#!/usr/bin/env Rscript

# Replace a completed tournament year's results with the Sports-Reference
# bracket (issue #26).
#
# Usage: Rscript scripts/rebuild_completed_year_results.R YEAR
#
# During the tournament, current-year results come from the ESPN scoreboard
# feed, whose game order is not the bracket slot order. Once the tournament
# is over, run this to replace that year's rows with the Sports-Reference
# bracket page, which lists games in slot order. The games, winners and
# scores must match the stored rows exactly; only game numbering changes.
# Stored completion times are kept. The current workbook is archived in the
# Project Vault before it is replaced.

find_script_path <- function() {
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("^--file=", args, value = TRUE)
    if (length(file_arg) == 0) {
        return(normalizePath(getwd()))
    }
    normalizePath(sub("^--file=", "", file_arg[1]))
}

project_root <- normalizePath(file.path(dirname(find_script_path()), ".."))
suppressMessages(pkgload::load_all(project_root, export_all = TRUE, helpers = FALSE, quiet = TRUE))
library(dplyr)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
    stop_with_message("Usage: Rscript scripts/rebuild_completed_year_results.R YEAR")
}
year <- as.character(as.integer(args[[1]]))

config <- load_project_config(file.path(project_root, "config.yml"))
initialize_logging(config$output$refresh_log_path)
results_path <- config$data$game_results_path
features <- read_table_file(config$data$team_features_path)
stored_all <- read_table_file(results_path) %>% dplyr::mutate(Year = as.character(Year))
stored <- normalize_game_results(stored_all) %>% dplyr::filter(Year == year)
if (nrow(stored) == 0) {
    stop_with_message(sprintf("No stored results for %s", year))
}

game_key <- function(d) {
    paste(d$round, pmin(normalize_team_key(d$teamA), normalize_team_key(d$teamB)), pmax(normalize_team_key(d$teamA), normalize_team_key(d$teamB)))
}
parsed <- scrape_tournament_results(as.integer(year)) %>% dplyr::mutate(Year = year)
if (nrow(parsed) != nrow(stored) ||
    !setequal(game_key(parsed), game_key(stored)) ||
    !setequal(paste(game_key(parsed), normalize_team_key(parsed$winner), parsed$total_points), paste(game_key(stored), normalize_team_key(stored$winner), stored$total_points))) {
    stop_with_message(sprintf("Sports-Reference %s results do not match the stored games, winners and scores; not replacing", year))
}

extra_columns <- setdiff(names(stored_all), names(parsed))
if (length(extra_columns) > 0) {
    carried <- stored_all %>%
        dplyr::filter(Year == year) %>%
        normalize_game_results() %>%
        dplyr::mutate(.key = game_key(.)) %>%
        dplyr::select(.key, dplyr::any_of(extra_columns))
    parsed <- parsed %>%
        dplyr::mutate(.key = game_key(.)) %>%
        dplyr::left_join(carried, by = ".key") %>%
        dplyr::select(-.key)
}

combined <- dplyr::bind_rows(stored_all %>% dplyr::filter(Year != year), parsed) %>%
    dplyr::arrange(suppressWarnings(as.integer(Year)), round, region, game_index)
assert_canonical_data_quality(features, combined)

archive_dir <- file.path(
    dirname(dirname(results_path)),
    "archive",
    sprintf("%s-results-before-%s-bracket-order", format(Sys.Date(), "%Y-%m-%d"), year)
)
if (dir.exists(archive_dir)) {
    stop_with_message(sprintf("Archive folder already exists; refusing to overwrite: %s", archive_dir))
}
dir.create(archive_dir, recursive = TRUE)
file.copy(results_path, file.path(archive_dir, basename(results_path)))
writeLines(
    c(
        sprintf("# Game results before %s was rebuilt in bracket order", year),
        "",
        sprintf("Archived %s by scripts/rebuild_completed_year_results.R.", format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
        sprintf("SHA-256: %s", digest::digest(file = file.path(archive_dir, basename(results_path)), algo = "sha256")),
        "",
        sprintf("The %s rows came from the ESPN scoreboard feed, whose game numbering is not the bracket slot order. Same games, winners and scores; only game_index changed.", year)
    ),
    file.path(archive_dir, "README.md")
)
writexl::write_xlsx(combined, results_path)
cat(sprintf("Rebuilt %s results in bracket order (%d games). Previous workbook archived in %s\n", year, nrow(parsed), archive_dir))
