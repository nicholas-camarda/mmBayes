#!/usr/bin/env Rscript

#' Find the absolute path to the current script
#'
#' @return A normalized absolute path to the running script or working directory.
#' @keywords internal
find_script_path <- function() {
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("^--file=", args, value = TRUE)
    if (length(file_arg) == 0) {
        return(normalizePath(getwd()))
    }
    normalizePath(sub("^--file=", "", file_arg[1]))
}

#' Parse an optional `--year=` argument
#'
#' @param args Trailing command-line args.
#'
#' @return The parsed year (integer) or `NULL`.
#' @keywords internal
parse_year_arg <- function(args) {
    year_arg <- grep("^--year=", args, value = TRUE)
    if (length(year_arg) == 0) {
        return(NULL)
    }
    value <- sub("^--year=", "", year_arg[1])
    parsed <- suppressWarnings(as.integer(value))
    if (!is.finite(parsed) || is.na(parsed)) {
        stop_with_message(sprintf("Invalid --year value: %s", value))
    }
    parsed
}

script_path <- find_script_path()
project_root <- normalizePath(file.path(dirname(script_path), ".."))
setwd(project_root)

pkgload::load_all(project_root, export_all = TRUE, helpers = FALSE, quiet = TRUE)
load_dotenv_file(".env", override = FALSE)

args <- commandArgs(trailingOnly = TRUE)
year_override <- parse_year_arg(args)

config <- load_project_config("config.yml")
output_dir <- config$output$path %||% "output"
output_prefix <- config$output$prefix %||% "tournament_sim"

loaded <- load_tournament_data(config)
bracket_year <- as.integer(year_override %||% loaded$bracket_year)

paths <- build_odds_history_paths(bracket_year, history_dir = config$betting$history_dir %||% "data/odds_history")
if (!file.exists(paths$closing_lines)) {
    stop_with_message(
        sprintf(
            "Missing %s. Run scripts/build_closing_lines.R --year=%s first.",
            paths$closing_lines,
            bracket_year
        )
    )
}

results_rds_path <- file.path(output_dir, paste0(output_prefix, ".rds"))
if (!file.exists(results_rds_path)) {
    stop_with_message(
        sprintf(
            "Missing %s. Run the simulation pipeline (scripts/run_simulation.R) first.",
            results_rds_path
        )
    )
}

closing_lines <- dplyr::as_tibble(utils::read.csv(paths$closing_lines, stringsAsFactors = FALSE)) %>%
    dplyr::filter(isTRUE(closing_before_commence) | is.na(closing_before_commence))

#' Build predicted probabilities for current-year played games
#'
#' This evaluation uses the fitted model from the last pipeline run and scores
#' only games with known winners in the current tournament year.
#'
#' @param results_bundle A results bundle read from `output/<prefix>.rds`.
#' @param bracket_year Tournament year to evaluate.
#' @param draws Number of posterior draws to use when scoring.
#'
#' @return A tibble suitable for [evaluate_betting_blend_metrics()].
#' @keywords internal
build_current_year_predictions <- function(results_bundle, bracket_year, draws = 500L) {
    data_bundle <- results_bundle$data %||% list()
    game_results <- data_bundle$game_results %||% tibble::tibble()
    if (nrow(game_results) == 0) {
        return(tibble::tibble())
    }

    played <- game_results %>%
        dplyr::filter(Year == as.character(bracket_year)) %>%
        dplyr::filter(!is.na(winner), nzchar(as.character(winner)))
    if (nrow(played) == 0) {
        return(tibble::tibble())
    }

    all_teams <- dplyr::bind_rows(
        data_bundle$historical_teams %||% tibble::tibble(),
        data_bundle$current_teams %||% tibble::tibble()
    )

    joined <- build_actual_game_reference(all_teams, played)
    matchup_rows <- actual_results_to_matchup_rows(joined)
    if (nrow(matchup_rows) == 0) {
        return(tibble::tibble())
    }

    draw_matrix <- predict_matchup_rows(matchup_rows, results_bundle$model, draws = draws)
    predicted_prob <- colMeans(draw_matrix)

    matchup_rows %>%
        dplyr::mutate(predicted_prob = predicted_prob) %>%
        dplyr::select(Year, round, teamA, teamB, predicted_prob, actual_outcome)
}

results_bundle <- readRDS(results_rds_path)
predictions_year <- build_current_year_predictions(results_bundle, bracket_year = bracket_year, draws = config$model$n_draws %||% 500L)
if (nrow(predictions_year) == 0) {
    stop_with_message(sprintf("No played games with results found for year %s in %s", bracket_year, config$data$game_results_path))
}

rounds <- unique(closing_lines$round)
weights <- c(0, 0.1, 0.25, 0.35, 0.5)
grid <- evaluate_betting_blend_weight_grid(
    predictions = predictions_year,
    closing_lines = closing_lines,
    weights = weights,
    rounds = rounds
)
if (nrow(grid) == 0) {
    stop_with_message("No matched games between closing lines and current-year played games (did you capture odds before those games started?).")
}

eval_path <- file.path(output_dir, sprintf("odds_blend_evaluation_%s.csv", bracket_year))
utils::write.csv(grid, eval_path, row.names = FALSE)

baseline <- grid %>% dplyr::filter(blend_weight == 0) %>% dplyr::slice(1)
best <- grid %>% dplyr::arrange(log_loss_blend, brier_blend) %>% dplyr::slice(1)

model_no_odds <- results_bundle$model
model_no_odds$betting$enabled <- FALSE
model_with_odds <- results_bundle$model
model_with_odds$betting$enabled <- TRUE

bracket_no_odds <- simulate_full_bracket(
    all_teams = results_bundle$data$current_teams,
    model_results = model_no_odds,
    draws = config$model$n_draws %||% 500L,
    actual_play_in_results = results_bundle$data$current_play_in_results,
    log_matchups = FALSE
)
bracket_with_odds <- simulate_full_bracket(
    all_teams = results_bundle$data$current_teams,
    model_results = model_with_odds,
    draws = config$model$n_draws %||% 500L,
    actual_play_in_results = results_bundle$data$current_play_in_results,
    log_matchups = FALSE
)
actual_results_year <- results_bundle$data$game_results %>%
    dplyr::filter(Year == as.character(bracket_year)) %>%
    dplyr::filter(!is.na(winner), nzchar(as.character(winner)))

score_no_odds <- score_bracket_against_results(flatten_matchup_results(bracket_no_odds), actual_results_year)$summary
score_with_odds <- score_bracket_against_results(flatten_matchup_results(bracket_with_odds), actual_results_year)$summary
score_tbl <- tibble::tibble(
    bracket_year = as.integer(bracket_year),
    total_games_scored = score_no_odds$total_games[[1]],
    correct_picks_model = score_no_odds$correct_picks[[1]],
    bracket_score_model = score_no_odds$bracket_score[[1]],
    correct_picks_blend = score_with_odds$correct_picks[[1]],
    bracket_score_blend = score_with_odds$bracket_score[[1]],
    delta_correct_picks = score_with_odds$correct_picks[[1]] - score_no_odds$correct_picks[[1]],
    delta_bracket_score = score_with_odds$bracket_score[[1]] - score_no_odds$bracket_score[[1]]
)
score_path <- file.path(output_dir, sprintf("odds_blend_bracket_score_%s.csv", bracket_year))
utils::write.csv(score_tbl, score_path, row.names = FALSE)

cat("Odds blend evaluation:\n")
cat(sprintf("- Output: %s\n", eval_path))
cat(sprintf("- Bracket score output: %s\n", score_path))
cat(sprintf("- Matched games: %s\n", best$n_games[[1]]))
cat(sprintf("- Baseline (w=0): log_loss=%.4f brier=%.4f\n", baseline$log_loss_model[[1]], baseline$brier_model[[1]]))
cat(sprintf(
    "- Best (w=%.2f): log_loss=%.4f brier=%.4f (delta_log_loss=%.4f delta_brier=%.4f)\n",
    best$blend_weight[[1]],
    best$log_loss_blend[[1]],
    best$brier_blend[[1]],
    best$delta_log_loss[[1]],
    best$delta_brier[[1]]
))
cat(sprintf(
    "- Bracket score so far: model=%s (correct=%s/%s) blend=%s (correct=%s/%s)\n",
    score_tbl$bracket_score_model[[1]],
    score_tbl$correct_picks_model[[1]],
    score_tbl$total_games_scored[[1]],
    score_tbl$bracket_score_blend[[1]],
    score_tbl$correct_picks_blend[[1]],
    score_tbl$total_games_scored[[1]]
))
