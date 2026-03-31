suppressPackageStartupMessages({
    library(pkgload)
})

pkgload::load_all(".")

source(file.path("tests", "testthat", "helper-fixtures.R"))

args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args) >= 1 && nzchar(args[[1]])) args[[1]] else tempdir()
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

team_file <- file.path(output_dir, "fixture_team_features.xlsx")
results_file <- file.path(output_dir, "fixture_game_results.xlsx")
fixture_paths <- write_fixture_data_files(team_file, results_file)

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

decision_sheet <- build_decision_sheet(candidates)
if (nrow(decision_sheet) > 0) {
    main_bracket_indices <- which(as.character(decision_sheet$round) != "First Four")
    if (length(main_bracket_indices) > 0) {
        main_bracket_index <- main_bracket_indices[[1]]
        decision_sheet$teamA[[main_bracket_index]] <- "St. John's"
        decision_sheet$teamB[[main_bracket_index]] <- "Texas A&M"
        decision_sheet$rationale_short[[main_bracket_index]] <- "Don't break HTML attributes."
    }
}

play_in_resolution <- summarize_play_in_resolution(
    current_teams = loaded$current_teams,
    actual_play_in_results = loaded$current_play_in_results
)

dashboard_html <- create_bracket_dashboard_html(
    bracket_year = 2026L,
    decision_sheet = decision_sheet,
    candidates = candidates,
    current_teams = NULL,
    backtest = NULL,
    play_in_resolution = play_in_resolution
)

output_path <- file.path(output_dir, "bracket_dashboard.html")
writeLines(dashboard_html, output_path, useBytes = TRUE)

cat(normalizePath(output_path, winslash = "/", mustWork = TRUE), "\n")
