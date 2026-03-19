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

script_path <- find_script_path()
project_root <- normalizePath(file.path(dirname(script_path), ".."))
setwd(project_root)

pkgload::load_all(project_root, export_all = TRUE, helpers = FALSE, quiet = TRUE)

config <- load_project_config("config.yml")
config$model$backtest <- FALSE
model_cache_dir <- config$output$model_cache_path %||% file.path(config$output$path %||% "output", "model_cache")

log_path <- file.path(config$output$path %||% "output", "logs", "bracket_candidates.log")
initialize_logging(log_path)

logger::log_info("Running fast bracket-candidate pipeline")

data <- load_tournament_data(config)
model_results <- fit_tournament_model(
    historical_matchups = data$historical_matchups,
    predictor_columns = config$model$required_predictors,
    random_seed = config$model$random_seed,
    cache_dir = model_cache_dir,
    use_cache = isTRUE(config$output$use_model_cache %||% TRUE)
)
total_points_model <- fit_total_points_model(
    historical_total_points = build_total_points_training_rows(data$historical_actual_results),
    random_seed = config$model$random_seed,
    cache_dir = model_cache_dir,
    use_cache = isTRUE(config$output$use_model_cache %||% TRUE)
)

candidates <- generate_bracket_candidates(
    all_teams = data$current_teams,
    model_results = model_results,
    draws = config$model$n_draws,
    actual_play_in_results = data$current_play_in_results,
    n_candidates = 2L,
    n_simulations = 25L,
    random_seed = config$model$random_seed
)
total_points_predictions <- predict_candidate_total_points(
    candidates = candidates,
    current_teams = data$current_teams,
    total_points_model = total_points_model,
    draws = config$model$n_draws
)

output_dir <- config$output$path %||% "output"
play_in_resolution <- summarize_play_in_resolution(
    current_teams = data$current_teams,
    actual_play_in_results = data$current_play_in_results
)
decision_outputs <- save_decision_outputs(
    bracket_year = data$bracket_year,
    candidates = candidates,
    output_dir = output_dir,
    backtest = NULL,
    play_in_resolution = play_in_resolution,
    total_points_predictions = total_points_predictions
)

cat("\n=============================================\n")
cat("Bracket Candidates\n")
cat("=============================================\n\n")
for (candidate in candidates) {
    cat(sprintf(
        "- Candidate %s [%s]: champion=%s | final four=%s\n",
        candidate$candidate_id,
        candidate$type,
        candidate$champion,
        candidate$final_four
    ))
    tiebreaker_row <- total_points_predictions$candidate_summaries %>%
        dplyr::filter(candidate_id == candidate$candidate_id)
    if (nrow(tiebreaker_row) == 1L) {
        cat(sprintf(
            "  Championship tiebreaker: %s for %s (80%% interval %.1f-%.1f)\n",
            tiebreaker_row$recommended_tiebreaker_points[[1]],
            tiebreaker_row$championship_matchup[[1]],
            tiebreaker_row$predicted_total_80_lower[[1]],
            tiebreaker_row$predicted_total_80_upper[[1]]
        ))
    }
}
cat(sprintf("\nDashboard: %s\n", decision_outputs$dashboard))
cat(sprintf("Technical dashboard: %s\n", decision_outputs$technical_dashboard))
cat(sprintf("Decision sheet: %s\n", decision_outputs$decision_sheet_path))
cat(sprintf("Summary: %s\n", decision_outputs$candidate_summary))
cat(sprintf("RDS: %s\n", decision_outputs$candidates_rds))
if (!is.null(decision_outputs$model_quality_source_label)) {
    cat(sprintf("Model quality source: %s\n", decision_outputs$model_quality_source_label))
}
if (!is.null(decision_outputs$model_quality_source_path)) {
    cat(sprintf("Model quality path: %s\n", decision_outputs$model_quality_source_path))
}
if (isTRUE(decision_outputs$model_quality_used_fallback)) {
    cat("Model quality: used latest saved snapshot fallback\n")
}
for (index in seq_along(decision_outputs$candidate_csvs)) {
    cat(sprintf("Candidate %s CSV: %s\n", index, decision_outputs$candidate_csvs[[index]]))
}
cat(sprintf("Tiebreaker summary CSV: %s\n", decision_outputs$championship_tiebreaker_summary))
cat(sprintf("Tiebreaker distribution CSV: %s\n", decision_outputs$championship_tiebreaker_distribution))
cat(sprintf("Matchup totals CSV: %s\n", decision_outputs$matchup_total_points))
cat(sprintf("Log: %s\n", log_path))
logger::log_info("Fast bracket-candidate pipeline complete; dashboard at {decision_outputs$dashboard}")
