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
log_path <- config$output$log_path %||% file.path(config$output$path %||% "output", "logs", "tournament_simulation.log")
initialize_logging(log_path)
results <- run_tournament_simulation(config)

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

cat("\nOutputs:\n")
cat(sprintf("- Results: %s\n", results$output$results))
cat(sprintf("- Model summary: %s\n", results$output$model_summary))
if (!is.null(results$output$backtest_summary)) {
    cat(sprintf("- Backtest summary: %s\n", results$output$backtest_summary))
}
cat(sprintf("- Bracket plot: %s\n", results$output$bracket_plot))
cat(sprintf("- Log: %s\n", log_path))
