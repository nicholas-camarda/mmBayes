test_that("run_tournament_simulation writes outputs and backtest summaries", {
    team_file <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")
    fixture_paths <- write_fixture_data_files(team_file, results_file)
    runtime_root <- tempfile(pattern = "mmBayes-runtime-")
    output_dir <- file.path(runtime_root, "output")
    dir.create(output_dir, recursive = TRUE)

    config <- default_project_config()
    config$data$team_features_path <- fixture_paths$team_path
    config$data$game_results_path <- fixture_paths$results_path
    config$model$history_window <- 3L
    config$model$n_draws <- 25L
    config$runtime$root <- runtime_root
    config$betting$history_dir <- file.path(runtime_root, "data", "odds_history")
    config$output$path <- output_dir
    config <- normalize_project_paths(config)
    config$output$prefix <- "fixture"
    team_data <- read_table_file(fixture_paths$team_path)
    results_data <- read_table_file(fixture_paths$results_path)
    write_fixture_betting_history(config$betting$history_dir, team_data, results_data, current_year = 2025)

    old_options <- options(
        mmBayes.stan_chains = 1L,
        mmBayes.stan_iter = 60L,
        mmBayes.stan_refresh = 0L
    )
    on.exit(options(old_options), add = TRUE)

    results <- run_tournament_simulation(config)

    expect_true(file.exists(file.path(output_dir, "fixture.rds")))
    expect_true(file.exists(file.path(output_dir, "fixture_model_summary.txt")))
    expect_true(file.exists(file.path(output_dir, "fixture_backtest_summary.txt")))
    expect_false(file.exists(file.path(output_dir, "fixture_bracket.png")))
    expect_true(file.exists(file.path(output_dir, "bracket_dashboard.html")))
    expect_true(file.exists(file.path(output_dir, "technical_dashboard.html")))
    expect_true(file.exists(file.path(output_dir, "bracket_decision_sheet.csv")))
    expect_true(file.exists(file.path(output_dir, "bracket_candidate_1.csv")))
    expect_true(file.exists(file.path(output_dir, "model_quality", "latest_model_quality.rds")))
    expect_true(length(list.files(file.path(output_dir, "model_quality"), pattern = "^model_quality_.*_pid.*\\.rds$")) >= 1)
    expect_true(file.exists(results$output$log_path))
    expect_match(basename(results$output$log_path), "^tournament_simulation_.*\\.log$")
    expect_length(list.files(file.path(output_dir, "logs"), pattern = "\\.log$"), 1)
    expect_true(is.list(results$backtest))
    expect_true(nrow(results$backtest$summary) == 1)
    expect_true(file.exists(results$output$model_quality_latest))
    expect_true(file.exists(results$output$model_quality_archive))
    expect_true(nrow(results$model$betting_feature_context$current_betting_features) > 0)
    expect_null(results$visualization)
    expect_null(results$output$bracket_plot)
    expect_true(file.exists(results$output$technical_dashboard))
})
