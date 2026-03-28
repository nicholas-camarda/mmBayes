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

#' Parse a `--force` CLI flag
#'
#' @param args Trailing command-line args.
#'
#' @return `TRUE` when `--force` is present.
#' @keywords internal
parse_force_flag <- function(args) {
    any(args %in% "--force")
}

script_path <- find_script_path()
project_root <- normalizePath(file.path(dirname(script_path), ".."))
setwd(project_root)

pkgload::load_all(project_root, export_all = TRUE, helpers = FALSE, quiet = TRUE)
load_dotenv_file(".env", override = FALSE)

args <- commandArgs(trailingOnly = TRUE)
force_collection <- parse_force_flag(args)

config <- load_project_config("config.yml")
result <- run_tournament_odds_collector(config = config, force = force_collection)

state <- result$state %||% list()
plan <- result$plan %||% list()
snapshot <- result$snapshot %||% list()
schedule <- result$schedule %||% list()

cat("Tournament odds collector\n")
cat(sprintf("- Bracket year: %s\n", result$bracket_year %||% "unknown"))
cat(sprintf("- Season status: %s\n", if (isTRUE(schedule$season_active %||% FALSE)) "active" else "inactive"))
cat(sprintf("- Schedule cache: %s\n", schedule$paths$schedule_events %||% "unknown"))
cat(sprintf("- Windows cache: %s\n", schedule$paths$schedule_windows %||% "unknown"))
cat(sprintf("- Collector state: %s\n", result$paths$collector_state %||% "unknown"))
cat(sprintf("- Next poll: %s\n", state$next_poll_at %||% "unknown"))
cat(sprintf("- Remaining credits: %s\n", state$remaining_credits %||% "unknown"))
cat(sprintf("- Reason: %s\n", state$last_capture_reason %||% plan$reason %||% "unknown"))

if (isTRUE(snapshot$skipped %||% FALSE)) {
    cat(sprintf("- Snapshot: skipped (%s)\n", snapshot$skip_reason %||% "unknown"))
} else if (!is.null(snapshot$snapshot_path)) {
    cat(sprintf("- Snapshot: %s\n", snapshot$snapshot_path))
    cat(sprintf("- Latest matchup lines: %s\n", snapshot$latest_lines_matchups_path %||% "unknown"))
}

cat(sprintf("- Log: %s\n", result$log_path %||% "unknown"))
