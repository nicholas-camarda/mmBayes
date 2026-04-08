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

stop_with_message(deprecated_live_betting_workflow_message())
