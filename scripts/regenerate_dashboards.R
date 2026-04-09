#!/usr/bin/env Rscript

warning(
    "scripts/regenerate_dashboards.R is deprecated; use scripts/regenerate_and_sync_dashboards.R instead.",
    call. = FALSE
)

script_path <- file.path(getwd(), "scripts", "regenerate_and_sync_dashboards.R")
if (!file.exists(script_path)) {
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("^--file=", args, value = TRUE)
    if (length(file_arg) > 0) {
        current_script <- normalizePath(sub("^--file=", "", file_arg[1]))
        script_path <- file.path(dirname(current_script), "regenerate_and_sync_dashboards.R")
    }
}

source(script_path, local = FALSE)
