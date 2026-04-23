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
report <- evaluate_canonical_tournament_audit(team_features, game_results)

cat("=== Canonical Tournament Audit ===\n")
print(report$summary)

cat("\n=== Historical Participation Summary ===\n")
print(report$historical_participation_summary, n = Inf)

if (!is.null(report$roster_validation_issue)) {
    cat("\n=== Roster Validation Issue ===\n")
    cat(report$roster_validation_issue, "\n")
}

if (nrow(report$duplicate_game_keys) > 0) {
    cat("\n=== Duplicate Game Keys ===\n")
    print(report$duplicate_game_keys, n = Inf)
}

if (nrow(report$historical_team_participation_issues) > 0) {
    cat("\n=== Historical Teams Missing From Results ===\n")
    print(report$historical_team_participation_issues, n = Inf, width = Inf)
}

if (nrow(report$result_seed_mismatches) > 0) {
    cat("\n=== Result Seed Mismatches ===\n")
    print(report$result_seed_mismatches, n = Inf, width = Inf)
}

if (nrow(report$current_year_unplayed_teams) > 0) {
    cat("\n=== Current-Year Teams Without Completed Games Yet ===\n")
    print(report$current_year_unplayed_teams, n = Inf, width = Inf)
}

if (!isTRUE(report$passed)) {
    quit(save = "no", status = 1)
}
