#!/usr/bin/env Rscript
# Regenerates the checked-in dashboard payload fixtures under tests/fixtures/.
# Run from the repo root: Rscript scripts/generate_dashboard_payload_fixtures.R

suppressPackageStartupMessages(library(pkgload))
pkgload::load_all(".")
source(file.path("tests", "testthat", "helper-fixtures.R"))

scratch_dir <- file.path(tempdir(), "payload_fixture_scratch")
dir.create(scratch_dir, recursive = TRUE, showWarnings = FALSE)
fixture_paths <- write_fixture_data_files(
    file.path(scratch_dir, "fixture_team_features.xlsx"),
    file.path(scratch_dir, "fixture_game_results.xlsx")
)

config <- default_project_config()
config$data$team_features_path <- fixture_paths$team_path
config$data$game_results_path <- fixture_paths$results_path
config$model$history_window <- 3L

old_options <- options(
    mmBayes.stan_chains = 1L,
    mmBayes.stan_iter = 60L,
    mmBayes.stan_refresh = 0L
)
on.exit(options(old_options), add = TRUE)

loaded <- load_tournament_data(config)
model_results <- fit_tournament_model(
    loaded$historical_matchups,
    config$model$required_predictors,
    random_seed = 123,
    include_diagnostics = FALSE
)
candidates <- generate_bracket_candidates(
    all_teams = loaded$current_teams,
    model_results = model_results,
    draws = 25L,
    n_candidates = 2L,
    n_simulations = 8L,
    random_seed = 123
)
candidates <- prepare_bracket_tree_fixture_candidates(candidates)
decision_sheet <- build_decision_sheet(candidates)
play_in_resolution <- summarize_play_in_resolution(
    current_teams = loaded$current_teams,
    actual_play_in_results = loaded$current_play_in_results
)
model_overview <- summarize_model_overview(
    model_results,
    draws = 25L,
    history_summary = loaded$history_summary %||% tibble::tibble()
)
fixture_backtest <- list(
    summary = tibble::tibble(
        mean_log_loss = 0.451,
        mean_brier = 0.192,
        mean_accuracy = 0.708,
        mean_bracket_score = 83.6,
        mean_correct_picks = 41.2
    ),
    calibration = tibble::tibble(
        bin = c("(0.4,0.5]", "(0.5,0.6]", "(0.6,0.7]", "(0.7,0.8]"),
        mean_predicted = c(0.45, 0.55, 0.65, 0.75),
        empirical_rate = c(0.42, 0.58, 0.63, 0.72),
        n_games = c(120L, 180L, 150L, 90L)
    ),
    predictions = tibble::tibble(
        round = c("Round of 64", "Round of 64", "Sweet 16", "Elite 8"),
        predicted_prob = c(0.7, 0.55, 0.62, 0.48),
        actual_outcome = c(1, 0, 1, 0)
    )
)
model_quality_context <- list(
    backtest = fixture_backtest,
    source_label = "fixture backtest",
    used_cached_quality = FALSE
)

build_metadata <- list(
    git_commit = "fixture",
    render_timestamp = "2026-01-01T00:00:00+0000",
    repo_snapshot_synced = TRUE
)
dashboard_context <- build_bracket_dashboard_context(
    current_teams = loaded$current_teams,
    decision_sheet = decision_sheet,
    candidates = candidates,
    play_in_resolution = play_in_resolution,
    dashboard_build_metadata = build_metadata
)

bracket_payload <- build_bracket_dashboard_payload(
    bracket_year = 2026L,
    candidates = candidates,
    decision_sheet = decision_sheet,
    dashboard_context = dashboard_context,
    play_in_resolution = play_in_resolution
)
technical_payload <- build_technical_dashboard_payload(
    bracket_year = 2026L,
    decision_sheet = decision_sheet,
    candidates = candidates,
    model_quality_context = model_quality_context,
    build_metadata = build_metadata,
    model_overview = model_overview,
    play_in_resolution = play_in_resolution,
    backtest = fixture_backtest
)
# Pin generated_at so fixture regeneration is deterministic.
bracket_payload$generated_at <- "2026-01-01T00:00:00+0000"
technical_payload$generated_at <- "2026-01-01T00:00:00+0000"

validate_dashboard_payload(bracket_payload, "bracket")
validate_dashboard_payload(technical_payload, "technical")

fixture_dir <- file.path("tests", "fixtures")
dir.create(fixture_dir, recursive = TRUE, showWarnings = FALSE)
writeLines(
    dashboard_payload_json(bracket_payload),
    file.path(fixture_dir, "dashboard_payload_bracket.json"),
    useBytes = TRUE
)
writeLines(
    dashboard_payload_json(technical_payload),
    file.path(fixture_dir, "dashboard_payload_technical.json"),
    useBytes = TRUE
)
cat("Wrote tests/fixtures/dashboard_payload_bracket.json\n")
cat("Wrote tests/fixtures/dashboard_payload_technical.json\n")
