suppressPackageStartupMessages({
    library(pkgload)
})

pkgload::load_all(".")

source(file.path("tests", "testthat", "helper-fixtures.R"))

override_candidate_matchup <- function(matchups, slot_key, values) {
    row_index <- which(as.character(matchups$slot_key) == slot_key)
    if (length(row_index) != 1L) {
        stop(sprintf("Expected exactly one matchup row for slot %s", slot_key))
    }

    for (field_name in names(values)) {
        matchups[[field_name]][[row_index]] <- values[[field_name]]
    }

    win_prob_a <- safe_numeric(matchups$win_prob_A[[row_index]], default = NA_real_)
    if (is.na(win_prob_a)) {
        win_prob_a <- 0.5
        matchups$win_prob_A[[row_index]] <- win_prob_a
    }

    if (win_prob_a >= 0.5) {
        matchups$posterior_favorite[[row_index]] <- matchups$teamA[[row_index]]
        matchups$favorite_seed[[row_index]] <- matchups$teamA_seed[[row_index]]
        matchups$underdog[[row_index]] <- matchups$teamB[[row_index]]
        matchups$underdog_seed[[row_index]] <- matchups$teamB_seed[[row_index]]
        matchups$win_prob_favorite[[row_index]] <- win_prob_a
        matchups$win_prob_underdog[[row_index]] <- 1 - win_prob_a
    } else {
        matchups$posterior_favorite[[row_index]] <- matchups$teamB[[row_index]]
        matchups$favorite_seed[[row_index]] <- matchups$teamB_seed[[row_index]]
        matchups$underdog[[row_index]] <- matchups$teamA[[row_index]]
        matchups$underdog_seed[[row_index]] <- matchups$teamA_seed[[row_index]]
        matchups$win_prob_favorite[[row_index]] <- 1 - win_prob_a
        matchups$win_prob_underdog[[row_index]] <- win_prob_a
    }

    matchups$upset[[row_index]] <- identical(
        as.character(matchups$winner[[row_index]]),
        as.character(matchups$underdog[[row_index]])
    )

    matchups
}

prepare_bracket_tree_fixture_candidates <- function(candidates) {
    for (candidate_index in seq_along(candidates)) {
        matchups <- candidates[[candidate_index]]$matchups
        first_main_game <- which(as.character(matchups$round) != "First Four")[[1]]
        matchups <- override_candidate_matchup(matchups, as.character(matchups$slot_key[[first_main_game]]), list(
            teamA = "St. John's",
            teamB = "Texas A&M",
            teamA_seed = 5L,
            teamB_seed = 12L,
            winner = "St. John's",
            win_prob_A = 0.62,
            rationale_short = "Don't break HTML attributes."
        ))
        candidates[[candidate_index]]$matchups <- matchups
    }

    candidates[[1]]$matchups <- override_candidate_matchup(candidates[[1]]$matchups, "East|Round of 64|5", list(
        teamA = "Louisville",
        teamB = "South Florida",
        teamA_seed = 6L,
        teamB_seed = 11L,
        winner = "Louisville",
        win_prob_A = 0.757,
        confidence_tier = "Lean"
    ))
    candidates[[2]]$matchups <- override_candidate_matchup(candidates[[2]]$matchups, "East|Round of 64|5", list(
        teamA = "Louisville",
        teamB = "South Florida",
        teamA_seed = 6L,
        teamB_seed = 11L,
        winner = "South Florida",
        win_prob_A = 0.757,
        confidence_tier = "Lean"
    ))

    candidates[[1]]$matchups <- override_candidate_matchup(candidates[[1]]$matchups, "East|Round of 32|3", list(
        teamA = "Louisville",
        teamB = "Michigan State",
        teamA_seed = 6L,
        teamB_seed = 3L,
        winner = "Louisville",
        win_prob_A = 0.56,
        confidence_tier = "Toss-up"
    ))
    candidates[[2]]$matchups <- override_candidate_matchup(candidates[[2]]$matchups, "East|Round of 32|3", list(
        teamA = "South Florida",
        teamB = "Michigan State",
        teamA_seed = 11L,
        teamB_seed = 3L,
        winner = "Michigan State",
        win_prob_A = 0.44,
        confidence_tier = "Toss-up"
    ))

    candidates[[1]]$matchups <- override_candidate_matchup(candidates[[1]]$matchups, "East|Sweet 16|2", list(
        teamA = "Louisville",
        teamB = "Connecticut",
        teamA_seed = 6L,
        teamB_seed = 2L,
        winner = "Connecticut",
        win_prob_A = 0.38,
        confidence_tier = "Toss-up"
    ))
    candidates[[2]]$matchups <- override_candidate_matchup(candidates[[2]]$matchups, "East|Sweet 16|2", list(
        teamA = "Michigan State",
        teamB = "Connecticut",
        teamA_seed = 3L,
        teamB_seed = 2L,
        winner = "Connecticut",
        win_prob_A = 0.38,
        confidence_tier = "Toss-up"
    ))

    candidates
}

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
candidates <- prepare_bracket_tree_fixture_candidates(candidates)

decision_sheet <- build_decision_sheet(candidates)

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
