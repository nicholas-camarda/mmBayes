library(logger)

#' Run the tournament simulation end-to-end
#'
#' @param config Optional project configuration list. Defaults to the loaded
#'   project config.
#'
#' @return A result bundle containing loaded data, fitted model, backtests,
#'   simulation outputs, candidate artifacts, and saved file paths.
#' @export
run_tournament_simulation <- function(config = NULL) {
    config <- config %||% load_project_config()
    output_dir <- config$output$path %||% "output"
    log_basename <- basename(config$output$log_path %||% "tournament_simulation.log")
    base_log_path <- file.path(output_dir, "logs", log_basename)
    run_log_path <- build_run_log_path(base_log_path)
    initialize_logging(run_log_path)
    model_cache_dir <- config$output$model_cache_path %||% file.path(output_dir, "model_cache")
    use_model_cache <- isTRUE(config$output$use_model_cache %||% TRUE)

    logger::log_info("Pipeline started")
    logger::log_info("Loading tournament data")
    data <- load_tournament_data(config)
    logger::log_info("Fitting tournament model")
    model_results <- fit_tournament_model(
        historical_matchups = data$historical_matchups,
        predictor_columns = config$model$required_predictors,
        random_seed = config$model$random_seed,
        cache_dir = model_cache_dir,
        use_cache = use_model_cache,
        interaction_terms = config$model$interaction_terms %||% NULL,
        prior_type = config$model$prior_type %||% "normal"
    )
    logger::log_info("Fitting total-points model")
    total_points_model <- fit_total_points_model(
        historical_total_points = build_total_points_training_rows(data$historical_actual_results),
        random_seed = config$model$random_seed,
        cache_dir = model_cache_dir,
        use_cache = use_model_cache
    )
    logger::log_info("Running backtest")
    backtest_results <- if (isTRUE(config$model$backtest)) {
        run_rolling_backtest(
            historical_teams = data$historical_teams,
            historical_actual_results = data$historical_actual_results,
            predictor_columns = config$model$required_predictors,
            random_seed = config$model$random_seed,
            draws = config$model$n_draws,
            cache_dir = model_cache_dir,
            use_cache = use_model_cache,
            interaction_terms = config$model$interaction_terms %||% NULL,
            prior_type = config$model$prior_type %||% "normal"
        )
    } else {
        NULL
    }
    logger::log_info("Simulating bracket")
    simulation_results <- simulate_full_bracket(
        all_teams = data$current_teams,
        model_results = model_results,
        draws = config$model$n_draws,
        actual_play_in_results = data$current_play_in_results,
        log_matchups = FALSE
    )
    logger::log_info("Generating bracket candidates")
    candidate_results <- generate_bracket_candidates(
        all_teams = data$current_teams,
        model_results = model_results,
        draws = config$model$n_draws,
        actual_play_in_results = data$current_play_in_results,
        n_candidates = 2L,
        n_simulations = 50L,
        random_seed = config$model$random_seed
    )
    decision_sheet <- build_decision_sheet(candidate_results)
    logger::log_info("Predicting candidate championship tiebreakers and matchup totals")
    total_points_predictions <- predict_candidate_total_points(
        candidates = candidate_results,
        current_teams = data$current_teams,
        total_points_model = total_points_model,
        draws = config$model$n_draws
    )

    result_bundle <- list(
        bracket_year = data$bracket_year,
        data = data,
        model = model_results,
        total_points_model = total_points_model,
        backtest = backtest_results,
        simulations = simulation_results,
        candidates = candidate_results,
        decision_sheet = decision_sheet,
        total_points_predictions = total_points_predictions,
        final_four = simulation_results$final_four,
        output = list(log_path = run_log_path)
    )

    result_bundle$output <- c(result_bundle$output, save_results(result_bundle, config$output))
    logger::log_info("Pipeline complete; decision sheet at {result_bundle$output$decision_sheet}")

    result_bundle
}
