#!/usr/bin/env Rscript

find_script_path <- function() {
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("^--file=", args, value = TRUE)
    if (length(file_arg) == 0) {
        return(normalizePath(getwd()))
    }
    normalizePath(sub("^--file=", "", file_arg[1]))
}

parse_years_arg <- function(args) {
    years_arg <- grep("^--years=", args, value = TRUE)
    if (length(years_arg) == 0L) {
        return(NULL)
    }
    years <- strsplit(sub("^--years=", "", years_arg[[1]]), ",", fixed = TRUE)[[1]]
    parsed <- suppressWarnings(as.integer(trimws(years)))
    parsed[is.finite(parsed) & !is.na(parsed)]
}

script_path <- find_script_path()
project_root <- normalizePath(file.path(dirname(script_path), ".."))
setwd(project_root)

pkgload::load_all(project_root, export_all = TRUE, helpers = FALSE, quiet = TRUE)
load_dotenv_file(".env", override = FALSE)

args <- commandArgs(trailingOnly = TRUE)
years <- parse_years_arg(args)
config <- load_project_config("config.yml")

result <- import_historical_oddspapi_closing_lines(config = config, years = years, preflight = TRUE)
skipped_years <- attr(result, "skipped_years") %||% tibble::tibble(year = integer(), reason = character())

cat("Historical OddsPapi import\n")
if (length(result) == 0L) {
    cat("- No eligible tournament seasons were available for import within the configured v4 retention window.\n")
}
if (nrow(skipped_years) > 0L) {
    for (index in seq_len(nrow(skipped_years))) {
        cat(sprintf("- Skipped %s: %s\n", skipped_years$year[[index]], skipped_years$reason[[index]]))
    }
}
for (year_name in names(result)) {
    year_result <- result[[year_name]]
    report <- year_result$report %||% list()
    cat(sprintf("- %s: closing_lines=%s status=%s matched=%s/%s both=%s moneyline=%s spread=%s\n",
        year_name,
        year_result$paths$closing_lines %||% "unknown",
        report$season_status %||% "unknown",
        report$matched_games %||% 0,
        report$target_games %||% 0,
        report$recovered_both_games %||% 0,
        report$recovered_moneyline_games %||% 0,
        report$recovered_spread_games %||% 0
    ))
    cat(sprintf(
        "  completeness: moneyline=%.1f%% spread=%.1f%% full=%.1f%%\n",
        100 * (report$moneyline_completeness_rate %||% 0),
        100 * (report$spread_completeness_rate %||% 0),
        100 * (report$full_completeness_rate %||% 0)
    ))
    cat(sprintf("  summary=%s\n", year_result$paths$import_summary %||% "unknown"))
    cat(sprintf("  report=%s\n", year_result$paths$import_report %||% "unknown"))
}
