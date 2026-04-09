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
deployable_rows <- summary_tbl %>%
    dplyr::filter(analysis == "deployable_historical_training")
baseline_selected <- deployable_rows %>%
    dplyr::filter(model == "baseline_no_betting") %>%
    dplyr::slice(1)
betting_selected <- deployable_rows %>%
    dplyr::filter(model == "historical_betting_features") %>%
    dplyr::slice(1)
direct_row <- summary_tbl %>%
    dplyr::filter(analysis == "retrospective_current_year_direct_substitution") %>%
    dplyr::slice(1)
overlay_row <- summary_tbl %>%
    dplyr::filter(
        analysis == "retrospective_current_year_candidate_overlay",
        model == "current_year_closing_lines_overlay"
    ) %>%
    dplyr::slice(1)

cat("Historical betting impact\n")
cat(sprintf("- Bracket year: %s\n", result$bracket_year %||% "unknown"))
if (nrow(summary_tbl) > 0L) {
    cat(sprintf(
        "- Current-year archive status: %s (moneyline %.1f%%, spread %.1f%%, full %.1f%%)\n",
        summary_tbl$current_year_season_status[[1]] %||% "unknown",
        100 * (summary_tbl$moneyline_completeness_rate[[1]] %||% 0),
        100 * (summary_tbl$spread_completeness_rate[[1]] %||% 0),
        100 * (summary_tbl$full_completeness_rate[[1]] %||% 0)
    ))
}
if (nrow(baseline_selected) > 0L && nrow(betting_selected) > 0L) {
    cat(sprintf(
        "- Deployable top-ranked bracket: baseline=%s betting=%s outcome=%s\n",
        baseline_selected$bracket_score[[1]] %||% NA,
        betting_selected$bracket_score[[1]] %||% NA,
        classify_betting_impact(baseline_selected$bracket_score[[1]], betting_selected$bracket_score[[1]])
    ))
}
if (nrow(direct_row) > 0L) {
    cat(sprintf(
        "- Retrospective direct substitution: score=%s outcome=%s\n",
        direct_row$bracket_score[[1]] %||% NA,
        direct_row$outcome_vs_baseline[[1]] %||% "unknown"
    ))
}
if (nrow(overlay_row) > 0L) {
    cat(sprintf(
        "- Retrospective candidate overlay: score=%s outcome=%s\n",
        overlay_row$bracket_score[[1]] %||% NA,
        overlay_row$outcome_vs_baseline[[1]] %||% "unknown"
    ))
}
cat(sprintf("- Summary: %s\n", result$output_paths$summary %||% "unknown"))
cat(sprintf("- Differences: %s\n", result$output_paths$differences %||% "unknown"))
cat(sprintf("- Report: %s\n", result$output_paths$report %||% "unknown"))
