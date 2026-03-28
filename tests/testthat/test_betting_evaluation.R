test_that("betting blend evaluation detects improvement on synthetic data", {
    predictions <- tibble::tibble(
        Year = c("2025", "2025", "2025", "2025"),
        round = c("Round of 64", "Round of 64", "Round of 64", "Round of 64"),
        teamA = c("Duke", "Kansas", "UCLA", "Gonzaga"),
        teamB = c("Siena", "Cal", "UCF", "Syracuse"),
        predicted_prob = c(0.55, 0.55, 0.55, 0.55),
        actual_outcome = c(1, 1, 0, 1)
    )

    closing_lines <- tibble::tibble(
        Year = c("2025", "2025", "2025", "2025"),
        round = c("Round of 64", "Round of 64", "Round of 64", "Round of 64"),
        teamA = c("Duke", "Kansas", "UCLA", "Gonzaga"),
        teamB = c("Siena", "Cal", "UCF", "Syracuse"),
        implied_prob_teamA = c(0.85, 0.80, 0.40, 0.75)
    )

    result <- evaluate_betting_blend_metrics(
        predictions = predictions,
        closing_lines = closing_lines,
        blend_weight = 0.5,
        rounds = "Round of 64"
    )

    expect_true(nrow(result) == 1)
    expect_true(result$n_games[[1]] == 4L)
    expect_true(result$delta_log_loss[[1]] < 0)
    expect_true(result$delta_brier[[1]] < 0)
})

test_that("optional closing-lines evaluation runs when local history exists", {
    closing_paths <- list.files(
        default_cloud_history_root(),
        pattern = "^closing_lines\\.csv$",
        recursive = TRUE,
        full.names = TRUE
    )
    if (length(closing_paths) == 0) {
        testthat::skip("No closing lines found under the configured cloud odds-history root")
    }
    if (!file.exists(file.path(default_cloud_output_root(), "model_quality", "latest_model_quality.rds"))) {
        testthat::skip("No cloud model-quality snapshot found; run the simulation pipeline first")
    }

    closing_lines <- dplyr::as_tibble(utils::read.csv(closing_paths[[1]], stringsAsFactors = FALSE))
    artifact <- readRDS(file.path(default_cloud_output_root(), "model_quality", "latest_model_quality.rds"))
    predictions <- artifact$backtest$predictions

    year <- as.character(unique(closing_lines$Year)[1])
    predictions_year <- predictions %>% dplyr::filter(Year == year)
    if (nrow(predictions_year) == 0) {
        testthat::skip(sprintf("No backtest predictions available for closing-lines year %s", year))
    }

    rounds <- unique(closing_lines$round)
    grid <- evaluate_betting_blend_weight_grid(
        predictions = predictions_year,
        closing_lines = closing_lines,
        weights = c(0, 0.1, 0.25, 0.35, 0.5),
        rounds = rounds
    )
    if (nrow(grid) == 0) {
        testthat::skip("No matched games between backtest predictions and closing lines")
    }
    if (max(grid$n_games, na.rm = TRUE) < 30L) {
        testthat::skip("Not enough matched games to evaluate odds blending (need >= 30)")
    }

    baseline <- grid %>% dplyr::filter(blend_weight == 0) %>% dplyr::slice(1)
    best <- grid %>% dplyr::arrange(log_loss_blend, brier_blend) %>% dplyr::slice(1)

    expect_lt(best$log_loss_blend[[1]], baseline$log_loss_model[[1]] - 1e-4)
    expect_lt(best$brier_blend[[1]], baseline$brier_model[[1]] - 1e-4)
})

test_that("betting ablation produces holdout metrics with betting predictors present", {
    team_file <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")
    runtime_root <- tempfile(pattern = "mmBayes-runtime-")
    team_data <- make_fixture_team_features(current_year = 2025, history_years = 2023:2024)
    results_data <- make_fixture_game_results(team_data, history_years = 2023:2024)
    fixture_paths <- write_fixture_data_files(team_file, results_file, team_data = team_data, results_data = results_data)

    config <- default_project_config()
    config$data$team_features_path <- fixture_paths$team_path
    config$data$game_results_path <- fixture_paths$results_path
    config$runtime$root <- runtime_root
    config$betting$history_dir <- file.path(runtime_root, "data", "odds_history")
    config$output$path <- file.path(runtime_root, "output")
    config <- normalize_project_paths(config)
    write_fixture_betting_history(config$betting$history_dir, team_data, results_data, current_year = 2025)

    loaded <- load_tournament_data(config)

    old_options <- options(
        mmBayes.stan_chains = 1L,
        mmBayes.stan_iter = 60L,
        mmBayes.stan_refresh = 0L
    )
    on.exit(options(old_options), add = TRUE)

    baseline <- run_rolling_backtest(
        historical_teams = loaded$historical_teams,
        historical_actual_results = loaded$historical_actual_results,
        predictor_columns = setdiff(config$model$required_predictors, betting_matchup_feature_columns()),
        random_seed = 123,
        draws = 20L,
        use_cache = FALSE,
        historical_betting_features = loaded$historical_betting_features
    )
    enhanced <- run_rolling_backtest(
        historical_teams = loaded$historical_teams,
        historical_actual_results = loaded$historical_actual_results,
        predictor_columns = config$model$required_predictors,
        random_seed = 123,
        draws = 20L,
        use_cache = FALSE,
        historical_betting_features = loaded$historical_betting_features
    )

    market_metrics <- compute_binary_metrics(
        enhanced$predictions$betting_prob_centered + 0.5,
        enhanced$predictions$actual_outcome
    )

    expect_true(all(betting_matchup_feature_columns() %in% names(loaded$historical_matchups)))
    expect_true(all(c("mean_log_loss", "mean_brier", "mean_accuracy") %in% names(baseline$summary)))
    expect_true(all(c("mean_log_loss", "mean_brier", "mean_accuracy") %in% names(enhanced$summary)))
    expect_true(is.finite(market_metrics$log_loss[[1]]))
    expect_true(is.finite(market_metrics$brier[[1]]))
})
