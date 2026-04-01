test_that("BART engine fits matchup and total-points models and returns draw matrices", {
    if (!requireNamespace("BART", quietly = TRUE)) {
        skip("BART not installed")
    }

    team_file <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")
    team_data <- make_fixture_team_features(current_year = 2025, history_years = 2023:2024)
    results_data <- make_fixture_game_results(team_data, history_years = 2023:2024)
    fixture_paths <- write_fixture_data_files(team_file, results_file, team_data = team_data, results_data = results_data)

    config <- default_project_config()
    config$data$team_features_path <- fixture_paths$team_path
    config$data$game_results_path <- fixture_paths$results_path
    config$model$history_window <- 2L

    loaded <- load_tournament_data(config)

    bart_config <- list(
        n_trees = 20L,
        n_burn = 5L,
        n_post = 40L,
        k = 2,
        power = 2
    )

    fit_stdout <- capture.output(
        model_results <- fit_tournament_model(
            historical_matchups = loaded$historical_matchups,
            predictor_columns = config$model$required_predictors,
            engine = "bart",
            bart_config = bart_config,
            random_seed = 123,
            include_diagnostics = FALSE,
            use_cache = FALSE
        )
    )

    expect_equal(model_results$engine, "bart")
    expect_false(any(grepl("In main of C\\+\\+|Into main of|MCMC|done ", fit_stdout)))

    pred_stdout <- capture.output(
        predict_matchup_rows(loaded$historical_matchups[1:6, , drop = FALSE], model_results, draws = 20)
    )
    expect_false(any(grepl("In main of C\\+\\+ for bart prediction", pred_stdout)))

    prob_draws <- predict_matchup_rows(loaded$historical_matchups[1:6, , drop = FALSE], model_results, draws = 20)
    expect_true(is.matrix(prob_draws))
    expect_equal(nrow(prob_draws), 20)
    expect_equal(ncol(prob_draws), 6)
    expect_true(all(is.finite(prob_draws)))
    expect_true(all(prob_draws >= 0 & prob_draws <= 1))

    total_fit_stdout <- capture.output(
        total_points_model <- fit_total_points_model(
            historical_total_points = build_total_points_training_rows(loaded$historical_actual_results),
            engine = "bart",
            bart_config = bart_config,
            random_seed = 123,
            include_diagnostics = FALSE,
            use_cache = FALSE
        )
    )

    expect_equal(total_points_model$engine, "bart")
    expect_false(any(grepl("In main of C\\+\\+|Into main of|MCMC|done ", total_fit_stdout)))

    total_training <- build_total_points_training_rows(loaded$historical_actual_results)
    total_pred_stdout <- capture.output(
        predict_total_points_rows(total_training[1:6, , drop = FALSE], total_points_model, draws = 20)
    )
    expect_false(any(grepl("In main of C\\+\\+ for bart prediction", total_pred_stdout)))

    total_draws <- predict_total_points_rows(total_training[1:6, , drop = FALSE], total_points_model, draws = 20)
    expect_true(is.matrix(total_draws))
    expect_equal(nrow(total_draws), 20)
    expect_equal(ncol(total_draws), 6)
    expect_true(all(is.finite(total_draws)))
    expect_true(all(total_draws >= 0))

    model_overview <- summarize_model_overview(model_results, draws = 40L)
    overview_html <- render_model_overview_html(model_overview)
    expect_match(overview_html, "Model Overview")
    expect_match(overview_html, "Engine settings")
    expect_match(overview_html, "n_trees")
    expect_match(overview_html, "n_post")
})

test_that("run_rolling_backtest supports both stan_glm and bart engines", {
    if (!requireNamespace("BART", quietly = TRUE)) {
        skip("BART not installed")
    }

    team_file <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")
    team_data <- make_fixture_team_features(current_year = 2025, history_years = 2023:2024)
    results_data <- make_fixture_game_results(team_data, history_years = 2023:2024)
    fixture_paths <- write_fixture_data_files(team_file, results_file, team_data = team_data, results_data = results_data)

    config <- default_project_config()
    config$data$team_features_path <- fixture_paths$team_path
    config$data$game_results_path <- fixture_paths$results_path
    config$model$history_window <- 2L

    loaded <- load_tournament_data(config)

    old_options <- options(
        mmBayes.stan_chains = 1L,
        mmBayes.stan_iter = 30L,
        mmBayes.stan_refresh = 0L
    )
    on.exit(options(old_options), add = TRUE)

    bart_config <- list(
        n_trees = 20L,
        n_burn = 5L,
        n_post = 40L,
        k = 2,
        power = 2
    )

    stan_backtest <- run_rolling_backtest(
        historical_teams = loaded$historical_teams,
        historical_actual_results = loaded$historical_actual_results,
        predictor_columns = config$model$required_predictors,
        engine = "stan_glm",
        random_seed = 123,
        draws = 10L,
        use_cache = FALSE
    )
    expect_true(is.list(stan_backtest))
    expect_true(nrow(stan_backtest$summary) == 1)

    bart_backtest <- run_rolling_backtest(
        historical_teams = loaded$historical_teams,
        historical_actual_results = loaded$historical_actual_results,
        predictor_columns = config$model$required_predictors,
        engine = "bart",
        bart_config = bart_config,
        random_seed = 123,
        draws = 10L,
        include_diagnostics = FALSE,
        use_cache = FALSE
    )
    expect_true(is.list(bart_backtest))
    expect_true(nrow(bart_backtest$summary) == 1)
})
