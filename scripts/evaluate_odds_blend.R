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

#' Parse an optional `--year=` argument
#'
#' @param args Trailing command-line args.
#'
#' @return The parsed year (integer) or `NULL`.
#' @keywords internal
parse_year_arg <- function(args) {
    year_arg <- grep("^--year=", args, value = TRUE)
    if (length(year_arg) == 0) {
        return(NULL)
    }
    value <- sub("^--year=", "", year_arg[1])
    parsed <- suppressWarnings(as.integer(value))
    if (!is.finite(parsed) || is.na(parsed)) {
        stop_with_message(sprintf("Invalid --year value: %s", value))
    }
    parsed
}

script_path <- find_script_path()
project_root <- normalizePath(file.path(dirname(script_path), ".."))
setwd(project_root)

pkgload::load_all(project_root, export_all = TRUE, helpers = FALSE, quiet = TRUE)
load_dotenv_file(".env", override = FALSE)

args <- commandArgs(trailingOnly = TRUE)
year_override <- parse_year_arg(args)

config <- load_project_config("config.yml")
output_dir <- config$output$path %||% "output"

loaded <- load_tournament_data(config)
bracket_year <- as.integer(year_override %||% loaded$bracket_year)

paths <- build_odds_history_paths(bracket_year, history_dir = config$betting$history_dir %||% "data/odds_history")
if (!file.exists(paths$closing_lines)) {
    stop_with_message(
        sprintf(
            "Missing %s. Run scripts/build_closing_lines.R --year=%s first.",
            paths$closing_lines,
            bracket_year
        )
    )
}

artifact_path <- file.path(output_dir, "model_quality", "latest_model_quality.rds")
if (!file.exists(artifact_path)) {
    stop_with_message(
        sprintf(
            "Missing %s. Run the simulation pipeline (scripts/run_simulation.R) first.",
            artifact_path
        )
    )
}

closing_lines <- dplyr::as_tibble(utils::read.csv(paths$closing_lines, stringsAsFactors = FALSE))
artifact <- readRDS(artifact_path)
predictions <- artifact$backtest$predictions %||% tibble::tibble()

predictions_year <- predictions %>%
    dplyr::filter(Year == as.character(bracket_year))
if (nrow(predictions_year) == 0) {
    stop_with_message(sprintf("No backtest predictions found for year %s in %s", bracket_year, artifact_path))
}

rounds <- unique(closing_lines$round)
weights <- c(0, 0.1, 0.25, 0.35, 0.5)
grid <- evaluate_betting_blend_weight_grid(
    predictions = predictions_year,
    closing_lines = closing_lines,
    weights = weights,
    rounds = rounds
)
if (nrow(grid) == 0) {
    stop_with_message("No matched games between closing lines and backtest predictions.")
}

eval_path <- file.path(output_dir, sprintf("odds_blend_evaluation_%s.csv", bracket_year))
utils::write.csv(grid, eval_path, row.names = FALSE)

baseline <- grid %>% dplyr::filter(blend_weight == 0) %>% dplyr::slice(1)
best <- grid %>% dplyr::arrange(log_loss_blend, brier_blend) %>% dplyr::slice(1)

cat("Odds blend evaluation:\n")
cat(sprintf("- Output: %s\n", eval_path))
cat(sprintf("- Matched games: %s\n", best$n_games[[1]]))
cat(sprintf("- Baseline (w=0): log_loss=%.4f brier=%.4f\n", baseline$log_loss_model[[1]], baseline$brier_model[[1]]))
cat(sprintf(
    "- Best (w=%.2f): log_loss=%.4f brier=%.4f (delta_log_loss=%.4f delta_brier=%.4f)\n",
    best$blend_weight[[1]],
    best$log_loss_blend[[1]],
    best$brier_blend[[1]],
    best$delta_log_loss[[1]],
    best$delta_brier[[1]]
))

