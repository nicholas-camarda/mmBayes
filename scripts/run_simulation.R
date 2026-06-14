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
results <- run_tournament_simulation(config)
runtime_dashboard_dir <- path.expand(normalize_project_paths(config)$output$path)
repo_output_dir <- file.path(project_root, "output")
dashboard_build_metadata <- build_dashboard_build_metadata(
    project_root = project_root,
    repo_snapshot_synced = TRUE
)
rendered_dashboards <- regenerate_dashboard_outputs_from_results(
    results = results,
    output_dir = runtime_dashboard_dir,
    repo_output_dir = repo_output_dir,
    dashboard_build_metadata = dashboard_build_metadata
)
synced_app_dirs <- sync_frontend_app(
    project_root = project_root,
    runtime_output_dir = runtime_dashboard_dir,
    repo_output_dir = repo_output_dir
)

champion <- results$final_four$champion
cat("\n=============================================\n")
cat("March Madness Tournament Prediction Results\n")
cat("=============================================\n\n")
cat("Bracket year:", results$bracket_year, "\n")
cat("Model engine:", results$model$engine, "\n")
cat("Predicted Champion:", champion$Team[[1]], "\n")
cat("Champion Strength:", round(compute_team_strength(champion), 4), "\n\n")
cat("Final Four Teams:\n")

for (region in names(results$final_four$semifinalists)) {
    team <- results$final_four$semifinalists[[region]]
    cat(sprintf("- %s: %s\n", region, team$Team[[1]]))
}

if (!is.null(results$candidates) && length(results$candidates) > 0) {
    cat("\nBracket candidates:\n")
    for (candidate in results$candidates) {
        cat(sprintf("- Candidate %s [%s]: champion=%s | final four=%s\n",
            candidate$candidate_id,
            candidate$type,
            candidate$champion,
            candidate$final_four
        ))
    }
}

cat("\nOutputs:\n")
cat(sprintf("- Results: %s\n", results$output$results))
cat(sprintf("- Model summary: %s\n", results$output$model_summary))
if (!is.null(results$output$backtest_summary)) {
    cat(sprintf("- Backtest summary: %s\n", results$output$backtest_summary))
}
if (!is.null(results$output$candidate_summary)) {
    cat(sprintf("- Candidate brackets: %s\n", results$output$candidate_summary))
}
cat(sprintf("- Dashboard app: %s\n", file.path(runtime_dashboard_dir, "app", "index.html")))
cat(sprintf("- Repo dashboard app dir: %s\n", file.path(repo_output_dir, "app")))
for (path in rendered_dashboards$repo_output_files) {
    cat(sprintf("- Synced app entry: %s\n", path))
}
if (!is.null(synced_app_dirs)) {
    for (app_dir in synced_app_dirs) {
        cat(sprintf("- Frontend app synced: %s\n", app_dir))
    }
}
cat(sprintf("- Technical dashboard app: %s\n", file.path(runtime_dashboard_dir, "app", "technical.html")))
if (!is.null(results$output$model_quality_latest)) {
    cat(sprintf("- Model quality latest: %s\n", results$output$model_quality_latest))
}
if (!is.null(results$output$model_quality_archive)) {
    cat(sprintf("- Model quality archive: %s\n", results$output$model_quality_archive))
}
if (!is.null(results$output$model_quality_source_label)) {
    cat(sprintf("- Model quality source: %s\n", results$output$model_quality_source_label))
}
if (isTRUE(results$output$model_quality_used_cached_quality)) {
    cat("- Model quality: used cached identical validation snapshot\n")
}
if (!is.null(results$output$decision_sheet)) {
    cat(sprintf("- Decision sheet: %s\n", results$output$decision_sheet))
}
if (!is.null(results$output$candidate_csvs)) {
    for (index in seq_along(results$output$candidate_csvs)) {
        cat(sprintf("- Candidate %s CSV: %s\n", index, results$output$candidate_csvs[[index]]))
    }
}
cat(sprintf("- Bracket plot: %s\n", results$output$bracket_plot))
cat(sprintf("- Log: %s\n", results$output$log_path))
