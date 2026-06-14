#!/usr/bin/env Rscript

# Internal helper: copy an already-rendered React dashboard app into tracked repo
# output/app files. This script does not regenerate dashboards and is not the
# authoritative publish workflow.

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

dashboard_app_dir <- "app"
runtime_output_root <- path.expand(Sys.getenv(
    "MMBAYES_PAGES_SOURCE",
    unset = default_runtime_output_root()
))

required_paths <- c(
    file.path(runtime_output_root, dashboard_app_dir, "index.html"),
    file.path(runtime_output_root, dashboard_app_dir, "technical.html"),
    file.path(runtime_output_root, dashboard_app_dir, "dashboard_payloads.js")
)
if (!dir.exists(runtime_output_root) || !all(file.exists(required_paths))) {
    stop_with_message(
        sprintf(
            paste(
                "Internal helper scripts/publish_github_pages.R requires an already-rendered React dashboard app at %s.",
                "Use `Rscript scripts/run_simulation.R` for the authoritative full workflow or",
                "`Rscript scripts/regenerate_and_sync_dashboards.R` to rebuild the app from the saved results bundle."
            ),
            runtime_output_root
        )
    )
}

cat("Internal helper: copying already-rendered React dashboard app into tracked repo output/app.\n")
cat(sprintf("Source bundle: %s\n", runtime_output_root))
source_app_dir <- file.path(runtime_output_root, dashboard_app_dir)
target_app_dir <- file.path(project_root, "output", dashboard_app_dir)
unlink(target_app_dir, recursive = TRUE)
dir.create(target_app_dir, recursive = TRUE, showWarnings = FALSE)
app_entries <- list.files(source_app_dir, full.names = TRUE)
copied <- file.copy(app_entries, target_app_dir, recursive = TRUE, overwrite = TRUE)
if (!all(copied)) {
    stop_with_message(sprintf("Failed to sync React dashboard app from %s to %s", source_app_dir, target_app_dir))
}
synced_app <- file.path(target_app_dir, basename(app_entries))
for (path in synced_app) {
    cat(sprintf(
        "Synced %s -> %s\n",
        file.path(source_app_dir, basename(path)),
        path
    ))
}

cat("\nGitHub Pages dashboard files are now synced into the repo.\n")
cat("This script does not regenerate dashboards; commit and push output/app to update the public links.\n")
