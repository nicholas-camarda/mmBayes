#!/usr/bin/env Rscript

#' Find the absolute path to the current script
#'
#' @return A normalized absolute path to the running script or working
#'   directory.
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
team_features <- read_table_file(config$data$team_features_path)
game_results <- read_table_file(config$data$game_results_path)
report <- evaluate_canonical_data_quality(team_features, game_results)

cat("=== Canonical Data Quality ===\n")
print(report$summary)

cat("\n=== Games Per Year ===\n")
print(report$games_per_year, n = Inf)

cat("\n=== Round Counts ===\n")
print(report$round_counts, n = Inf)

if (nrow(report$round_count_issues) > 0) {
    cat("\n=== Round Count Issues ===\n")
    print(report$round_count_issues, n = Inf)
}

if (nrow(report$suspicious_first_four) > 0) {
    cat("\n=== Suspicious First Four Rows ===\n")
    print(report$suspicious_first_four, n = Inf, width = Inf)
}

if (nrow(report$unresolved_teams) > 0) {
    cat("\n=== Unresolved Result Teams ===\n")
    print(report$unresolved_teams, n = Inf, width = Inf)
}

if (!isTRUE(report$passed)) {
    quit(save = "no", status = 1)
}
