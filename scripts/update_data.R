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

config <- load_project_config("config.yml")
log_path <- config$output$refresh_log_path %||% file.path(config$output$path %||% default_runtime_output_root(), "logs", "data_refresh.log")

initialize_logging(log_path)
refresh_result <- update_tournament_data(config = config)

cat(paste(format_refresh_status_summary(refresh_result, log_path = log_path), collapse = "\n"))
cat("\n")
