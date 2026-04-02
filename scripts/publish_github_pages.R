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

candidate_roots <- unique(c(
    path.expand(Sys.getenv("MMBAYES_PAGES_SOURCE", unset = "")),
    default_runtime_output_root(),
    file.path(default_runtime_output_root(), "workflow_redesign_preview"),
    default_cloud_output_root()
))
candidate_roots <- candidate_roots[nzchar(candidate_roots)]
dashboard_files <- dashboard_html_manifest()

source_candidates <- candidate_roots[vapply(candidate_roots, function(root) {
    dir.exists(root) && all(file.exists(file.path(root, dashboard_files)))
}, logical(1))]

if (length(source_candidates) == 0L) {
    stop_with_message(
        sprintf(
            "No dashboard bundle found in any candidate source: %s",
            paste(candidate_roots, collapse = ", ")
        )
    )
}

bundle_mtime <- vapply(source_candidates, function(root) {
    as.numeric(max(file.info(file.path(root, dashboard_files))$mtime, na.rm = TRUE))
}, numeric(1))
runtime_output_root <- source_candidates[[which.max(bundle_mtime)]]
cat(sprintf("Using dashboard source bundle: %s\n", runtime_output_root))
synced_paths <- sync_dashboard_html_files(
    source_dir = runtime_output_root,
    destination_dir = file.path(project_root, "output"),
    dashboard_files = dashboard_files
)
for (filename in dashboard_files) {
    cat(sprintf("Synced %s -> %s\n", file.path(runtime_output_root, filename), synced_paths[[filename]]))
}

cat("\nGitHub Pages dashboard files are now synced into the repo.\n")
cat("Commit and push these tracked HTML files to update the public links.\n")
