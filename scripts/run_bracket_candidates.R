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

candidates <- generate_bracket_candidates(
    all_teams = data$current_teams,
    model_results = model_results,
    draws = config$model$n_draws,
    n_candidates = 2L,
    n_simulations = 25L,
    random_seed = config$model$random_seed
)

output_dir <- config$output$path %||% "output"
decision_outputs <- save_decision_outputs(
    bracket_year = data$bracket_year,
    candidates = candidates,
    output_dir = output_dir,
    backtest = NULL
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
}
cat(sprintf("\nDashboard: %s\n", decision_outputs$dashboard))
cat(sprintf("Decision sheet: %s\n", decision_outputs$decision_sheet_path))
cat(sprintf("Summary: %s\n", decision_outputs$candidate_summary))
cat(sprintf("RDS: %s\n", decision_outputs$candidates_rds))
for (index in seq_along(decision_outputs$candidate_csvs)) {
    cat(sprintf("Candidate %s CSV: %s\n", index, decision_outputs$candidate_csvs[[index]]))
}
cat(sprintf("Log: %s\n", log_path))
logger::log_info("Fast bracket-candidate pipeline complete; dashboard at {decision_outputs$dashboard}")
