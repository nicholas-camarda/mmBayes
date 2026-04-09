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

#' Parse an optional `--date=` argument
#'
#' @param args Trailing command-line args.
#'
#' @return The parsed date or `NULL` when the flag is absent.
#' @keywords internal
parse_date_arg <- function(args) {
    date_arg <- grep("^--date=", args, value = TRUE)
    if (length(date_arg) == 0) {
        return(NULL)
    }
    value <- sub("^--date=", "", date_arg[1])
    parsed <- suppressWarnings(as.Date(value))
    if (is.na(parsed)) {
        stop_with_message(sprintf("Invalid --date value: %s", value))
    }
    parsed
}

#' Parse an optional `--publish-root=` argument
#'
#' @param args Trailing command-line args.
#'
#' @return The parsed publish root or `NULL` when the flag is absent.
#' @keywords internal
parse_publish_root_arg <- function(args) {
    root_arg <- grep("^--publish-root=", args, value = TRUE)
    if (length(root_arg) == 0) {
        return(NULL)
    }
    normalizePath(sub("^--publish-root=", "", root_arg[1]), mustWork = FALSE)
}

script_path <- find_script_path()
project_root <- normalizePath(file.path(dirname(script_path), ".."))
setwd(project_root)

pkgload::load_all(project_root, export_all = TRUE, helpers = FALSE, quiet = TRUE)

load_dotenv_file(".env", override = FALSE)

args <- commandArgs(trailingOnly = TRUE)
release_date <- parse_date_arg(args) %||% Sys.Date()
publish_root <- parse_publish_root_arg(args) %||% default_publish_root()
config <- load_project_config("config.yml")

result <- publish_release_bundle(
    config = config,
    release_date = release_date,
    publish_root = publish_root
)

cat("Published release bundle\n")
cat(sprintf("- Release root: %s\n", result$release_root))
cat(sprintf("- Deliverables: %s\n", result$deliverables_dir))
cat(sprintf("- Manifest: %s\n", result$manifest_path))
