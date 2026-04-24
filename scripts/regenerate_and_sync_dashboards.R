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
repo_output_dir <- file.path(project_root, "output")
dashboard_build_metadata <- build_dashboard_build_metadata(
    project_root = project_root,
    repo_snapshot_synced = TRUE
)
rendered <- regenerate_dashboards_from_saved_results(
    config = config,
    repo_output_dir = repo_output_dir,
    dashboard_build_metadata = dashboard_build_metadata
)

cat(sprintf("Loaded cached results bundle: %s\n", rendered$results_bundle_path))
cat("Regenerated runtime dashboards:\n")
cat(sprintf("- %s\n", rendered$dashboard))
cat(sprintf("- %s\n", rendered$technical_dashboard))
cat("Synced tracked repo dashboards:\n")
for (path in rendered$repo_output_files) {
    cat(sprintf("- %s\n", path))
}
