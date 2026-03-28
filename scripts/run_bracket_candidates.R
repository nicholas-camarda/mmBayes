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

load_dotenv_file(".env", override = FALSE)

config <- load_project_config("config.yml")
config$model$backtest <- FALSE
engine <- config$model$engine %||% "stan_glm"
bart_config <- config$model$bart %||% list()
draws_budget <- if (identical(engine, "bart")) {
    as.integer(bart_config$n_post %||% 1000L)
} else {
    as.integer(config$model$n_draws %||% 1000L)
}
model_cache_dir <- config$output$model_cache_path %||% file.path(config$output$path %||% default_runtime_output_root(), "model_cache")

log_path <- file.path(config$output$path %||% default_runtime_output_root(), "logs", "bracket_candidates.log")
initialize_logging(log_path)

logger::log_info("Running fast bracket-candidate pipeline")

data <- load_tournament_data(config, include_betting_history = FALSE)
data$historical_betting_features <- tibble::tibble()
matchup_predictors <- core_matchup_predictor_columns(config$model$required_predictors)
model_results <- fit_tournament_model(
    historical_matchups = data$historical_matchups,
    predictor_columns = matchup_predictors,
    engine = engine,
    bart_config = bart_config,
    random_seed = config$model$random_seed,
    cache_dir = model_cache_dir,
    use_cache = isTRUE(config$output$use_model_cache %||% TRUE)
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
total_points_model <- fit_total_points_model(
    historical_total_points = build_total_points_training_rows(data$historical_actual_results),
    engine = engine,
    bart_config = bart_config,
    random_seed = config$model$random_seed,
    cache_dir = model_cache_dir,
    use_cache = isTRUE(config$output$use_model_cache %||% TRUE)
)
total_points_model$betting_feature_context <- model_results$betting_feature_context
model_overview <- list(
    matchup = summarize_model_overview(model_results, draws = draws_budget),
    totals = summarize_model_overview(total_points_model, draws = draws_budget)
)
quality_signature <- build_model_quality_signature(list(
    bracket_year = data$bracket_year,
    draws_budget = draws_budget,
    model = model_results,
    data = data
))

candidates <- generate_bracket_candidates(
    all_teams = data$current_teams,
    model_results = model_results,
    draws = draws_budget,
    actual_play_in_results = data$current_play_in_results,
    n_candidates = 2L,
    n_simulations = 25L,
    random_seed = config$model$random_seed
)
total_points_predictions <- predict_candidate_total_points(
    candidates = candidates,
    current_teams = data$current_teams,
    total_points_model = total_points_model,
    draws = draws_budget
)
live_performance <- summarize_live_tournament_performance(
    data = data,
    model_results = model_results,
    draws = draws_budget
)

output_dir <- config$output$path %||% default_runtime_output_root()
play_in_resolution <- summarize_play_in_resolution(
    current_teams = data$current_teams,
    actual_play_in_results = data$current_play_in_results
)
decision_outputs <- save_decision_outputs(
    bracket_year = data$bracket_year,
    candidates = candidates,
    output_dir = output_dir,
    backtest = NULL,
    model_overview = model_overview,
    quality_signature = quality_signature,
    play_in_resolution = play_in_resolution,
    total_points_predictions = total_points_predictions,
    live_performance = live_performance
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
if (isTRUE(decision_outputs$model_quality_used_cached_quality)) {
    cat("Model quality: used cached identical validation snapshot\n")
}
for (index in seq_along(decision_outputs$candidate_csvs)) {
    cat(sprintf("Candidate %s CSV: %s\n", index, decision_outputs$candidate_csvs[[index]]))
}
cat(sprintf("Tiebreaker summary CSV: %s\n", decision_outputs$championship_tiebreaker_summary))
cat(sprintf("Tiebreaker distribution CSV: %s\n", decision_outputs$championship_tiebreaker_distribution))
cat(sprintf("Matchup totals CSV: %s\n", decision_outputs$matchup_total_points))
cat(sprintf("Log: %s\n", log_path))
logger::log_info("Fast bracket-candidate pipeline complete; dashboard at {decision_outputs$dashboard}")
