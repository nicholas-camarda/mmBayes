#!/usr/bin/env Rscript

# Add earlier tournament years to the canonical workbooks (issue #12).
#
# Usage: Rscript scripts/backfill_tournament_history.R [first_year] [last_year]
# Defaults: 2008 to the year before the earliest year already present.
#
# For each year: the tournament field (seeds, regions) and completed results
# come from the Sports-Reference postseason page; team ratings come from ESPN
# box scores through Selection Sunday. The combined workbooks must pass the
# canonical data-quality gate before anything is written, and the current
# workbooks are archived in the Project Vault first. Fails if any year cannot
# be built completely.

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

config <- load_project_config(file.path(project_root, "config.yml"))
initialize_logging(config$output$refresh_log_path)

features_path <- config$data$team_features_path
results_path <- config$data$game_results_path
current_features <- read_table_file(features_path) %>% dplyr::mutate(Year = as.character(Year))
current_results <- read_table_file(results_path) %>% dplyr::mutate(Year = as.character(Year))

args <- commandArgs(trailingOnly = TRUE)
first_year <- if (length(args) >= 1) as.integer(args[[1]]) else 2008L
last_year <- if (length(args) >= 2) as.integer(args[[2]]) else min(as.integer(current_features$Year)) - 1L
years <- setdiff(seq.int(first_year, last_year), 2020L)
if (length(years) == 0) {
    stop_with_message("No years to backfill")
}
logger::log_info("Backfilling tournament years {paste(years, collapse = ', ')}")

built <- lapply(years, function(year) {
    roster <- scrape_historical_tournament_roster(year)
    issues <- attr(roster, "refresh_issues", exact = TRUE)
    if (!is.null(issues) && nrow(issues) > 0) {
        stop_with_message(sprintf("Could not build the %s tournament field: %s", year, paste(issues$message, collapse = "; ")))
    }
    results <- scrape_tournament_results(year)
    ratings <- compute_season_team_ratings(year, raw_dir = config$data$espn_raw_dir)
    features <- build_team_feature_dataset(ratings, roster)
    logger::log_info("Built {year}: {nrow(features)} teams, {nrow(results)} games")
    list(features = features, results = results)
})

new_features <- dplyr::bind_rows(lapply(built, `[[`, "features"))
new_results <- dplyr::bind_rows(lapply(built, `[[`, "results"))
combined_features <- dplyr::bind_rows(
    current_features %>% dplyr::filter(!Year %in% as.character(years)),
    new_features
) %>%
    normalize_team_features() %>%
    dplyr::arrange(suppressWarnings(as.integer(Year)), Region, Seed, Team)
combined_results <- dplyr::bind_rows(
    current_results %>% dplyr::filter(!Year %in% as.character(years)),
    new_results
) %>%
    normalize_game_results() %>%
    dplyr::arrange(suppressWarnings(as.integer(Year)), round, region, game_index)

assert_canonical_data_quality(combined_features, combined_results)

archive_dir <- file.path(
    dirname(dirname(features_path)),
    "archive",
    sprintf("%s-workbooks-before-history-backfill", format(Sys.Date(), "%Y-%m-%d"))
)
if (dir.exists(archive_dir)) {
    stop_with_message(sprintf("Archive folder already exists; refusing to overwrite: %s", archive_dir))
}
dir.create(archive_dir, recursive = TRUE)
for (path in c(features_path, results_path)) {
    file.copy(path, file.path(archive_dir, basename(path)))
}
writeLines(
    c(
        "# Canonical workbooks before the history backfill",
        "",
        sprintf("Archived %s by scripts/backfill_tournament_history.R before adding %s.", format(Sys.time(), "%Y-%m-%d %H:%M %Z"), paste(years, collapse = ", ")),
        "",
        vapply(c(features_path, results_path), function(path) {
            sprintf("- %s SHA-256 %s", basename(path), digest::digest(file = file.path(archive_dir, basename(path)), algo = "sha256"))
        }, character(1))
    ),
    file.path(archive_dir, "README.md")
)

writexl::write_xlsx(combined_features, features_path)
writexl::write_xlsx(combined_results, results_path)
cat(sprintf(
    "Backfilled %s: %d team rows and %d games added.\nWorkbooks now cover %s.\nPrevious workbooks archived in %s\n",
    paste(years, collapse = ", "), nrow(new_features), nrow(new_results),
    paste(sort(unique(combined_features$Year)), collapse = ", "), archive_dir
))
