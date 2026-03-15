library(logger)

#' Run the tournament simulation end-to-end
#'
#' @param config Optional project configuration list. Defaults to the loaded
#'   project config.
#'
#' @return A result bundle containing loaded data, fitted model, backtests,
#'   simulation outputs, visualization, and saved file paths.
#' @export
run_tournament_simulation <- function(config = NULL) {
    config <- config %||% load_project_config()
    initialize_logging(config$output$log_path %||% file.path(config$output$path %||% "output", "logs", "tournament_simulation.log"))

    logger::log_info("Running tournament simulation pipeline")
    data <- load_tournament_data(config)
    model_results <- fit_tournament_model(
        historical_matchups = data$historical_matchups,
        predictor_columns = config$model$required_predictors,
        random_seed = config$model$random_seed
    )
    backtest_results <- if (isTRUE(config$model$backtest)) {
        run_rolling_backtest(
            historical_teams = data$historical_teams,
            historical_actual_results = data$historical_actual_results,
            predictor_columns = config$model$required_predictors,
            random_seed = config$model$random_seed,
            draws = config$model$n_draws
        )
    } else {
        NULL
    }
    simulation_results <- simulate_full_bracket(
        all_teams = data$current_teams,
        model_results = model_results,
        draws = config$model$n_draws
    )
    visualization <- create_tournament_visualization(simulation_results)

    result_bundle <- list(
        bracket_year = data$bracket_year,
        data = data,
        model = model_results,
        backtest = backtest_results,
        simulations = simulation_results,
        final_four = simulation_results$final_four,
        visualization = visualization
    )

    result_bundle$output <- save_results(result_bundle, config$output)
    logger::log_info("Tournament simulation complete")

    result_bundle
}
