#!/usr/bin/env Rscript

# Recompute team ratings for every year in the canonical team-features
# workbook, keeping its tournament rosters (Year, Team, Seed, Region).
#
# Use this when the ratings method or cutoff changes. Ratings come from ESPN
# box scores through each year's Selection Sunday (see R/team_ratings.R).
# The current workbook is copied to the Project Vault archive before it is
# replaced; an existing archive folder is never overwritten.

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
current <- read_table_file(features_path) %>% dplyr::mutate(Year = as.character(Year))
results <- read_table_file(config$data$game_results_path)
years <- sort(unique(current$Year))
logger::log_info("Rebuilding team ratings for {paste(years, collapse = ', ')}")

roster <- current %>%
    dplyr::transmute(Year, Team, Seed = as.integer(Seed), Region, Conf = NA_character_)
ratings <- purrr::map_dfr(years, function(year) {
    logger::log_info("Computing team ratings for {year} through Selection Sunday {ratings_cutoff_date(year)}")
    compute_season_team_ratings(as.integer(year), raw_dir = config$data$espn_raw_dir)
})
rebuilt <- build_team_feature_dataset(ratings, roster) %>%
    normalize_team_features() %>%
    dplyr::arrange(suppressWarnings(as.integer(Year)), Region, Seed, Team)

stopifnot(nrow(rebuilt) == nrow(current))
assert_canonical_data_quality(rebuilt, results)

archive_dir <- file.path(
    dirname(dirname(features_path)),
    "archive",
    sprintf("%s-team-features-before-ratings-rebuild", format(Sys.Date(), "%Y-%m-%d"))
)
if (dir.exists(archive_dir)) {
    stop_with_message(sprintf("Archive folder already exists; refusing to overwrite: %s", archive_dir))
}
dir.create(archive_dir, recursive = TRUE)
file.copy(features_path, file.path(archive_dir, basename(features_path)))
writeLines(
    c(
        "# Team features before the ratings rebuild",
        "",
        sprintf("Archived %s by scripts/rebuild_team_ratings.R.", format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
        "",
        sprintf("SHA-256: %s", digest::digest(file = file.path(archive_dir, basename(features_path)), algo = "sha256")),
        "",
        "Replaced by team ratings computed from ESPN box scores through each year's Selection Sunday (issue #10).",
        "Workbooks without a ratings_cutoff column were scraped from Torvik with a May 1 end date and include NCAA tournament games."
    ),
    file.path(archive_dir, "README.md")
)
logger::log_info("Archived previous team features to {archive_dir}")

writexl::write_xlsx(rebuilt, features_path)
logger::log_info("Wrote rebuilt team features ({nrow(rebuilt)} rows) to {features_path}")
cat(sprintf("Rebuilt %d team-feature rows for %s.\nPrevious workbook archived in %s\n", nrow(rebuilt), paste(years, collapse = ", "), archive_dir))
