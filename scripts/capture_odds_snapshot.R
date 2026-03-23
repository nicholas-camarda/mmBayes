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
config$betting$enabled <- TRUE

loaded <- load_tournament_data(config)
bracket_year <- as.integer(loaded$bracket_year)

log_path <- file.path(config$output$path %||% "output", "logs", sprintf("odds_snapshot_%s.log", bracket_year))
initialize_logging(log_path)

snapshot <- capture_tournament_odds_snapshot(
    config = config,
    bracket_year = bracket_year,
    current_teams = loaded$current_teams
)

headers <- snapshot$headers %||% list()
remaining <- headers[["x-requests-remaining"]] %||% NA_character_
used <- headers[["x-requests-used"]] %||% NA_character_
last_cost <- headers[["x-requests-last"]] %||% NA_character_

cat("Captured Odds API snapshot for bracket year:", bracket_year, "\n")
cat(sprintf("- Snapshot JSON: %s\n", snapshot$snapshot_path))
cat(sprintf("- Latest matchup lines: %s\n", snapshot$latest_lines_matchups_path))
cat(sprintf("- Long history: %s\n", build_odds_history_paths(bracket_year, history_dir = config$betting$history_dir)$lines_long))
cat(sprintf("- Matchup history: %s\n", build_odds_history_paths(bracket_year, history_dir = config$betting$history_dir)$lines_matchups))
cat(sprintf("- Requests remaining: %s\n", remaining))
cat(sprintf("- Requests used: %s\n", used))
cat(sprintf("- Last request cost: %s\n", last_cost))
cat(sprintf("- Log: %s\n", log_path))
