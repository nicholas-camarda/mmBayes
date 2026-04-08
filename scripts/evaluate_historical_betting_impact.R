#!/usr/bin/env Rscript

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
result <- evaluate_historical_betting_bracket_impact(config = config)

summary_tbl <- result$summary %||% tibble::tibble()
baseline_best <- summary_tbl %>%
    dplyr::filter(model == "baseline_no_betting") %>%
    dplyr::arrange(dplyr::desc(bracket_score), dplyr::desc(correct_picks)) %>%
    dplyr::slice(1)
betting_best <- summary_tbl %>%
    dplyr::filter(model == "historical_betting_features") %>%
    dplyr::arrange(dplyr::desc(bracket_score), dplyr::desc(correct_picks)) %>%
    dplyr::slice(1)

cat("Historical betting impact\n")
cat(sprintf("- Bracket year: %s\n", result$bracket_year %||% "unknown"))
if (nrow(summary_tbl) > 0L && nrow(baseline_best) > 0L && nrow(betting_best) > 0L) {
    cat(sprintf(
        "- Best-candidate score: baseline=%s betting=%s outcome=%s\n",
        baseline_best$bracket_score[[1]] %||% NA,
        betting_best$bracket_score[[1]] %||% NA,
        classify_betting_impact(baseline_best$bracket_score[[1]], betting_best$bracket_score[[1]])
    ))
}
cat(sprintf("- Summary: %s\n", result$output_paths$summary %||% "unknown"))
cat(sprintf("- Differences: %s\n", result$output_paths$differences %||% "unknown"))
cat(sprintf("- Report: %s\n", result$output_paths$report %||% "unknown"))
