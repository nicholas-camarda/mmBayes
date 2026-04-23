#!/usr/bin/env Rscript

# Internal helper: copy an already-rendered dashboard bundle into tracked repo
# output files. This script does not regenerate dashboards and is not the
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

dashboard_files <- dashboard_html_manifest()
runtime_output_root <- path.expand(Sys.getenv(
    "MMBAYES_PAGES_SOURCE",
    unset = default_runtime_output_root()
))

if (!dir.exists(runtime_output_root) || !all(file.exists(file.path(runtime_output_root, dashboard_files)))) {
    stop_with_message(
        sprintf(
            paste(
                "Internal helper scripts/publish_github_pages.R requires an already-rendered dashboard bundle at %s.",
                "Use `Rscript scripts/run_simulation.R` for the authoritative full workflow or",
                "`Rscript scripts/regenerate_and_sync_dashboards.R` to rebuild HTML from the saved results bundle."
            ),
            runtime_output_root
        )
    )
}

cat("Internal helper: copying already-rendered dashboard HTML into tracked repo output.\n")
cat(sprintf("Source bundle: %s\n", runtime_output_root))
synced_paths <- sync_dashboard_html_files(
    source_dir = runtime_output_root,
    destination_dir = file.path(project_root, "output"),
    dashboard_files = dashboard_files
)
for (filename in dashboard_files) {
    cat(sprintf("Synced %s -> %s\n", file.path(runtime_output_root, filename), synced_paths[[filename]]))
}

cat("\nGitHub Pages dashboard files are now synced into the repo.\n")
cat("This script does not regenerate dashboards; commit and push these tracked HTML files to update the public links.\n")
