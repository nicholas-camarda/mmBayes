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
    config$model$ensemble$enabled <- FALSE
    config$runtime$root <- runtime_root
    config$output$path <- output_dir
    config <- normalize_project_paths(config)
    config$output$prefix <- "fixture"

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
    expect_false(file.exists(file.path(output_dir, "model_comparison_dashboard.html")))
    expect_true(file.exists(file.path(output_dir, "bracket_decision_sheet.csv")))
    expect_true(file.exists(file.path(output_dir, "bracket_candidate_1.csv")))
    expect_true(file.exists(file.path(output_dir, "model_quality", "latest_model_quality.rds")))
    expect_equal(length(list.files(file.path(output_dir, "model_quality"), pattern = "^model_quality_.*_pid.*\\.rds$")), 0L)
    expect_true(file.exists(results$output$log_path))
    expect_match(basename(results$output$log_path), "^tournament_simulation_.*\\.log$")
    expect_length(list.files(file.path(output_dir, "logs"), pattern = "\\.log$"), 1)
    expect_true(is.list(results$backtest))
    expect_true(nrow(results$backtest$summary) == 1)
    expect_true(file.exists(results$output$model_quality_latest))
    expect_true(file.exists(results$output$model_quality_archive))
    expect_identical(results$output$model_quality_archive, results$output$model_quality_latest)
    expect_null(results$output$model_comparison_dashboard)
    expect_false(any(startsWith(results$model$predictor_columns, "betting_")))
    expect_false(any(startsWith(results$total_points_model$predictor_columns, "betting_")))
    expect_null(results$visualization)
    expect_null(results$output$bracket_plot)
    expect_true(file.exists(results$output$technical_dashboard))

    repo_output_dir <- file.path(tempfile(pattern = "mmBayes-repo-output-"))
    dir.create(repo_output_dir, recursive = TRUE, showWarnings = FALSE)
    unlink(file.path(output_dir, "bracket_dashboard.html"))
    unlink(file.path(output_dir, "technical_dashboard.html"))

    testthat::local_mocked_bindings(
        run_tournament_simulation = function(...) stop("regeneration should not rerun the full simulation"),
        fit_tournament_model = function(...) stop("regeneration should not fit the matchup model"),
        fit_total_points_model = function(...) stop("regeneration should not fit the total-points model"),
        run_rolling_backtest = function(...) stop("regeneration should not run the rolling backtest"),
        generate_bracket_candidates = function(...) stop("regeneration should not regenerate candidates")
    )

    dashboard_build_metadata <- build_dashboard_build_metadata(
        project_root = normalizePath(file.path(testthat::test_path(), "..", "..")),
        rendered_at = as.POSIXct("2026-04-23 13:15:00", tz = "America/New_York"),
        repo_snapshot_synced = TRUE
    )
    regenerated <- regenerate_dashboard_outputs_from_results(
        results = results,
        output_dir = output_dir,
        repo_output_dir = repo_output_dir,
        dashboard_build_metadata = dashboard_build_metadata
    )

    expect_true(file.exists(regenerated$dashboard))
    expect_true(file.exists(regenerated$technical_dashboard))
    expect_null(regenerated$model_comparison_dashboard)
    expect_length(regenerated$repo_output_files, 2L)
    expect_true(all(file.exists(regenerated$repo_output_files)))
    expect_equal(
        basename(regenerated$repo_output_files),
        dashboard_html_manifest()
    )

    rendered_dashboard_html <- paste(readLines(regenerated$dashboard, warn = FALSE), collapse = "\n")
    synced_dashboard_html <- paste(readLines(regenerated$repo_output_files[[1]], warn = FALSE), collapse = "\n")
    expect_match(rendered_dashboard_html, "Updated")
    expect_match(rendered_dashboard_html, "April 23, 2026")
    expect_match(rendered_dashboard_html, "Built from commit")
    expect_match(rendered_dashboard_html, "Repo snapshot")
    expect_match(rendered_dashboard_html, "Synced to tracked repo output in this run")
    expect_match(synced_dashboard_html, "Synced to tracked repo output in this run")
    if (!is.na(dashboard_build_metadata$commit_short)) {
        expect_match(rendered_dashboard_html, dashboard_build_metadata$commit_short)
    }
})
