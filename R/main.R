library(logger)

#' Return a human-readable label for a model engine
#'
#' @param engine Engine identifier.
#'
#' @return A display label for the engine.
#' @keywords internal
engine_display_label <- function(engine) {
    if (identical(engine, "bart")) {
        "BART"
    } else {
        "Stan GLM"
    }
}

#' Resolve comparison-specific engine options
#'
#' @param alternate_engine Alternate model engine being compared.
#' @param interaction_terms Interaction terms from the primary configuration.
#' @param prior_type Prior type from the primary configuration.
#'
#' @return A list of engine-specific options suitable for the alternate fit.
#' @keywords internal
resolve_comparison_engine_options <- function(alternate_engine, interaction_terms, prior_type) {
    resolved <- list(
        interaction_terms = interaction_terms,
        prior_type = prior_type
    )

    if (identical(alternate_engine, "bart")) {
        resolved$interaction_terms <- NULL
        resolved$prior_type <- "normal"
    }

    resolved
}

#' Build an opt-in comparison bundle for Stan GLM and BART
#'
#' @param data Loaded tournament data bundle.
#' @param current_engine Engine used for the primary run.
#' @param current_model_results Fitted matchup model for the primary run.
#' @param current_total_points_model Fitted total-points model for the primary run.
#' @param current_backtest Backtest bundle for the primary run.
#' @param current_live_performance Live performance bundle for the primary run.
#' @param current_model_overview Model overview for the matchup model.
#' @param current_total_points_overview Model overview for the total-points model.
#' @param draws_budget Posterior draw budget used for scoring.
#' @param bart_config BART hyperparameter configuration.
#' @param random_seed Seed forwarded to alternate fits.
#' @param model_cache_dir Cache directory for fitted models.
#' @param use_model_cache Whether model caching is enabled.
#' @param interaction_terms Interaction terms forwarded to Stan fits.
#' @param prior_type Prior type forwarded to Stan fits.
#' @param matchup_predictors Predictor columns used for the matchup model.
#' @param run_backtest Whether the comparison should include a backtest.
#'
#' @return A comparison bundle or `NULL` when the alternate engine could not be fit.
#' @keywords internal
build_model_comparison_bundle <- function(data,
                                          current_engine,
                                          current_model_results,
                                          current_total_points_model,
                                          current_backtest,
                                          current_live_performance,
                                          current_model_overview,
                                          current_total_points_overview,
                                          draws_budget,
                                          bart_config,
                                          random_seed,
                                          model_cache_dir,
                                          use_model_cache,
                                          interaction_terms,
                                          prior_type,
                                          matchup_predictors,
                                          run_backtest = TRUE) {
    alternate_engine <- if (identical(current_engine, "bart")) "stan_glm" else "bart"
    current_label <- engine_display_label(current_engine)
    alternate_label <- engine_display_label(alternate_engine)
    alternate_engine_options <- resolve_comparison_engine_options(
        alternate_engine = alternate_engine,
        interaction_terms = interaction_terms,
        prior_type = prior_type
    )

    logger::log_info("Comparison mode enabled; fitting alternate {alternate_label} run")

    alternate_model_results <- tryCatch(
        fit_tournament_model(
            historical_matchups = data$historical_matchups,
            predictor_columns = matchup_predictors,
            engine = alternate_engine,
            bart_config = bart_config,
            random_seed = random_seed,
            include_diagnostics = if (identical(alternate_engine, "bart")) FALSE else TRUE,
            cache_dir = model_cache_dir,
            use_cache = use_model_cache,
            interaction_terms = alternate_engine_options$interaction_terms,
            prior_type = alternate_engine_options$prior_type
        ),
        error = function(e) {
            logger::log_warn("Alternate matchup model fit failed: {e$message}")
            NULL
        }
    )

    if (is.null(alternate_model_results)) {
        return(list(
            available = FALSE,
            attempted = TRUE,
            status = sprintf("%s could not be fit in this environment.", alternate_label),
            current_label = current_label,
            alternate_label = alternate_label,
            current = list(
                model_overview = current_model_overview,
                backtest = current_backtest,
                live_performance = current_live_performance
            ),
            alternate = NULL,
            backtest_comparison = tibble::tibble(),
            live_comparison = tibble::tibble(),
            summary = list(
                current_wins = 0L,
                alternate_wins = 0L,
                ties = 0L,
                text = sprintf("%s could not be fit, so no comparison is available.", alternate_label)
            ),
            notes = c(sprintf("%s comparison could not be completed.", alternate_label))
        ))
    }

    alternate_total_points_model <- tryCatch(
        fit_total_points_model(
            historical_total_points = build_total_points_training_rows(data$historical_actual_results),
            engine = alternate_engine,
            bart_config = bart_config,
            random_seed = random_seed,
            cache_dir = model_cache_dir,
            use_cache = use_model_cache
        ),
        error = function(e) {
            logger::log_warn("Alternate total-points model fit failed: {e$message}")
            NULL
        }
    )

    alternate_backtest <- if (isTRUE(run_backtest)) {
        tryCatch(
            run_rolling_backtest(
                historical_teams = data$historical_teams,
                historical_actual_results = data$historical_actual_results,
                predictor_columns = matchup_predictors,
                engine = alternate_engine,
                bart_config = bart_config,
                random_seed = random_seed,
                draws = draws_budget,
                cache_dir = model_cache_dir,
                use_cache = use_model_cache,
                interaction_terms = alternate_engine_options$interaction_terms,
                prior_type = alternate_engine_options$prior_type
            ),
            error = function(e) {
                logger::log_warn("Alternate backtest failed: {e$message}")
                NULL
            }
        )
    } else {
        NULL
    }

    alternate_live_performance <- tryCatch(
        summarize_live_tournament_performance(
            data = data,
            model_results = alternate_model_results,
            draws = draws_budget
        ),
        error = function(e) {
            logger::log_warn("Alternate live-performance summary failed: {e$message}")
            NULL
        }
    )

    alternate_model_overview <- list(
        matchup = summarize_model_overview(alternate_model_results, draws = draws_budget),
        totals = summarize_model_overview(alternate_total_points_model, draws = draws_budget)
    )

    backtest_comparison <- build_model_metric_comparison_table(
        current_summary = current_backtest$summary %||% tibble::tibble(),
        alternate_summary = alternate_backtest$summary %||% tibble::tibble(),
        current_label = current_label,
        alternate_label = alternate_label,
        kind = "backtest"
    )
    live_comparison <- build_model_metric_comparison_table(
        current_summary = current_live_performance$summary %||% tibble::tibble(),
        alternate_summary = alternate_live_performance$summary %||% tibble::tibble(),
        current_label = current_label,
        alternate_label = alternate_label,
        kind = "live"
    )

    list(
        available = TRUE,
        attempted = TRUE,
        status = sprintf("Comparison completed for %s and %s.", current_label, alternate_label),
        current_label = current_label,
        alternate_label = alternate_label,
        current = list(
            model_overview = as_model_overview_bundle(
                model_overview = current_model_overview,
                totals_overview = current_total_points_overview
            ),
            totals_overview = current_total_points_overview,
            backtest = current_backtest,
            live_performance = current_live_performance
        ),
        alternate = list(
            model_overview = as_model_overview_bundle(alternate_model_overview),
            totals_overview = alternate_model_overview$totals,
            backtest = alternate_backtest,
            live_performance = alternate_live_performance
        ),
        backtest_comparison = backtest_comparison,
        live_comparison = live_comparison,
        summary = if (nrow(backtest_comparison) > 0) {
            summarize_model_metric_comparison(
                comparison_table = backtest_comparison,
                current_label = current_label,
                alternate_label = alternate_label
            )
        } else {
            summarize_model_metric_comparison(
                comparison_table = live_comparison,
                current_label = current_label,
                alternate_label = alternate_label
            )
        },
        notes = c(
            if (identical(alternate_engine, "bart")) c(
                "This comparison uses the same base predictors for both engines.",
                "Explicit interaction terms are retained for Stan GLM.",
                "BART is fit without explicit interaction terms because it learns interactions implicitly through tree splits."
            ) else NULL
        )
    )
}

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
    output_dir <- config$output$path %||% default_runtime_output_root()
    engine <- config$model$engine %||% "stan_glm"
    compare_engines <- isTRUE(config$model$compare_engines %||% TRUE)
    bart_config <- config$model$bart %||% list()
    draws_budget <- if (identical(engine, "bart")) {
        as.integer(bart_config$n_post %||% 1000L)
    } else {
        as.integer(config$model$n_draws %||% 1000L)
    }
    log_basename <- basename(config$output$log_path %||% file.path(default_runtime_output_root(), "logs", "tournament_simulation.log"))
    base_log_path <- file.path(output_dir, "logs", log_basename)
    run_log_path <- build_run_log_path(base_log_path)
    initialize_logging(run_log_path)
    model_cache_dir <- config$output$model_cache_path %||% file.path(output_dir, "model_cache")
    use_model_cache <- isTRUE(config$output$use_model_cache %||% TRUE)

    logger::log_info("Pipeline started")
    logger::log_info("Loading tournament data")
    data <- load_tournament_data(config, include_betting_history = FALSE)
    data$historical_betting_features <- tibble::tibble()
    logger::log_info("Fitting tournament model")
    interaction_terms <- as.character(unlist(config$model$interaction_terms %||% character(0)))
    if (length(interaction_terms) == 0L) interaction_terms <- NULL
    matchup_predictors <- core_matchup_predictor_columns(config$model$required_predictors)
    model_results <- fit_tournament_model(
        historical_matchups = data$historical_matchups,
        predictor_columns = matchup_predictors,
        engine = engine,
        bart_config = bart_config,
        random_seed = config$model$random_seed,
        cache_dir = model_cache_dir,
        use_cache = use_model_cache,
        interaction_terms = interaction_terms,
        prior_type = config$model$prior_type %||% "normal"
    )
    model_results$betting_feature_context <- list(
        current_lines_matchups = tibble::tibble(),
        current_betting_features = tibble::tibble(),
        historical_betting_features = tibble::tibble(),
        source_label = NULL,
        used_api_call = FALSE,
        latest_lines_matchups_path = NULL,
        snapshot_path = NULL
    )
    logger::log_info("Fitting total-points model")
    total_points_model <- fit_total_points_model(
        historical_total_points = build_total_points_training_rows(data$historical_actual_results),
        engine = engine,
        bart_config = bart_config,
        random_seed = config$model$random_seed,
        cache_dir = model_cache_dir,
        use_cache = use_model_cache
    )
    total_points_model$betting_feature_context <- model_results$betting_feature_context
    logger::log_info("Running backtest")
    backtest_results <- if (isTRUE(config$model$backtest)) {
        run_rolling_backtest(
            historical_teams = data$historical_teams,
            historical_actual_results = data$historical_actual_results,
            predictor_columns = matchup_predictors,
            engine = engine,
            bart_config = bart_config,
            random_seed = config$model$random_seed,
            draws = draws_budget,
            cache_dir = model_cache_dir,
            use_cache = use_model_cache,
            interaction_terms = interaction_terms,
            prior_type = config$model$prior_type %||% "normal"
        )
    } else {
        NULL
    }
    logger::log_info("Simulating deterministic reference bracket")
    simulation_results <- simulate_full_bracket(
        all_teams = data$current_teams,
        model_results = model_results,
        draws = draws_budget,
        actual_play_in_results = data$current_play_in_results,
        log_matchups = FALSE,
        log_stage_progress = FALSE
    )
    candidate_simulations <- 50L
    logger::log_info("Generating bracket candidates from {candidate_simulations} stochastic simulations")
    candidate_results <- generate_bracket_candidates(
        all_teams = data$current_teams,
        model_results = model_results,
        draws = draws_budget,
        actual_play_in_results = data$current_play_in_results,
        n_candidates = 2L,
        n_simulations = candidate_simulations,
        random_seed = config$model$random_seed
    )
    decision_sheet <- build_decision_sheet(candidate_results)
    logger::log_info("Predicting candidate championship tiebreakers and matchup totals")
    total_points_predictions <- predict_candidate_total_points(
        candidates = candidate_results,
        current_teams = data$current_teams,
        total_points_model = total_points_model,
        draws = draws_budget
    )
    live_performance <- summarize_live_tournament_performance(
        data = data,
        model_results = model_results,
        draws = draws_budget
    )

    model_overview <- summarize_model_overview(model_results, draws = draws_budget)
    total_points_model_overview <- summarize_model_overview(total_points_model, draws = draws_budget)
    model_comparison <- if (compare_engines) {
        build_model_comparison_bundle(
            data = data,
            current_engine = engine,
            current_model_results = model_results,
            current_total_points_model = total_points_model,
            current_backtest = backtest_results,
            current_live_performance = live_performance,
            current_model_overview = model_overview,
            current_total_points_overview = total_points_model_overview,
            draws_budget = draws_budget,
            bart_config = bart_config,
            random_seed = config$model$random_seed,
            model_cache_dir = model_cache_dir,
            use_model_cache = use_model_cache,
            interaction_terms = interaction_terms,
            prior_type = config$model$prior_type %||% "normal",
            matchup_predictors = matchup_predictors,
            run_backtest = isTRUE(config$model$backtest)
        )
    } else {
        NULL
    }

    result_bundle <- list(
        bracket_year = data$bracket_year,
        data = data,
        model = model_results,
        total_points_model = total_points_model,
        model_overview = model_overview,
        total_points_model_overview = total_points_model_overview,
        model_comparison = model_comparison,
        draws_budget = draws_budget,
        backtest = backtest_results,
        simulations = simulation_results,
        candidates = candidate_results,
        decision_sheet = decision_sheet,
        total_points_predictions = total_points_predictions,
        live_performance = live_performance,
        final_four = simulation_results$final_four,
        betting = model_results$betting_feature_context,
        output = list(log_path = run_log_path)
    )

    result_bundle$output <- c(result_bundle$output, save_results(result_bundle, config$output))
    logger::log_info("Pipeline complete; decision sheet at {result_bundle$output$decision_sheet}")

    result_bundle
}
