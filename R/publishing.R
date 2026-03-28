#' Return the file names that are safe to publish as deliverables
#'
#' The manifest stays intentionally small and human-facing. It excludes runtime
#' caches, logs, `.rds` bundles, and other scratch artifacts.
#'
#' @return A character vector of publishable deliverable file names.
#' @keywords internal
release_deliverable_manifest <- function() {
    c(
        "bracket_dashboard.html",
        "technical_dashboard.html",
        "bracket_decision_sheet.csv",
        "bracket_candidate_1.csv",
        "bracket_candidate_2.csv",
        "candidate_matchup_total_points.csv",
        "championship_tiebreaker_summary.csv",
        "championship_tiebreaker_distribution.csv",
        "tournament_sim_candidate_brackets.txt",
        "tournament_sim_model_summary.txt",
        "tournament_sim_backtest_summary.txt",
        "bracket_candidates.txt"
    )
}

#' Copy a file or directory into a destination directory
#'
#' @param source Path to a file or directory to copy.
#' @param destination_dir Directory that should receive the copied artifact.
#'
#' @return The destination path that was written.
#' @keywords internal
copy_release_artifact <- function(source, destination_dir) {
    if (!file.exists(source) && !dir.exists(source)) {
        stop_with_message(sprintf("Missing release artifact: %s", source))
    }

    dir.create(destination_dir, recursive = TRUE, showWarnings = FALSE)
    destination_path <- file.path(destination_dir, basename(source))
    copied <- file.copy(source, destination_dir, recursive = TRUE, overwrite = FALSE)
    if (!isTRUE(copied)) {
        stop_with_message(sprintf("Failed to copy release artifact from %s to %s", source, destination_dir))
    }

    destination_path
}

#' Publish a dated release bundle to the cloud-backup root
#'
#' @param config Optional project configuration list.
#' @param release_date A date or date-like value used to name the release.
#' @param publish_root Base OneDrive publish root.
#'
#' @return A list describing the created publish folder and copied artifacts.
#' @keywords internal
publish_release_bundle <- function(config = NULL,
                                   release_date = Sys.Date(),
                                   publish_root = default_publish_root()) {
    config <- config %||% load_project_config()
    output_dir <- path.expand(config$output$path %||% default_cloud_output_root())
    history_dir <- path.expand(config$betting$history_dir %||% default_cloud_history_root())
    release_root <- project_publish_release_root(release_date, publish_root = publish_root)

    if (dir.exists(release_root)) {
        stop_with_message(sprintf("Release folder already exists: %s", release_root))
    }
    if (!dir.exists(output_dir)) {
        stop_with_message(sprintf("Runtime output directory does not exist: %s", output_dir))
    }
    if (!dir.exists(history_dir)) {
        stop_with_message(sprintf("Runtime odds-history directory does not exist: %s", history_dir))
    }

    deliverables_dir <- file.path(release_root, "deliverables")
    data_snapshot_dir <- file.path(release_root, "data_snapshot")
    dir.create(deliverables_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(data_snapshot_dir, recursive = TRUE, showWarnings = FALSE)

    deliverable_paths <- vapply(release_deliverable_manifest(), function(filename) {
        source <- file.path(output_dir, filename)
        if (!file.exists(source)) {
            stop_with_message(sprintf("Missing deliverable file: %s", source))
        }
        copy_release_artifact(source, deliverables_dir)
    }, character(1))

    snapshot_path <- copy_release_artifact(history_dir, data_snapshot_dir)
    manifest_path <- file.path(release_root, "release_manifest.txt")
    manifest_lines <- c(
        sprintf("release_date: %s", format(as.Date(release_date), "%Y-%m-%d")),
        sprintf("output_dir: %s", output_dir),
        sprintf("history_dir: %s", history_dir),
        "deliverables:",
        paste0("  - ", release_deliverable_manifest()),
        sprintf("data_snapshot: %s", snapshot_path)
    )
    writeLines(manifest_lines, manifest_path, useBytes = TRUE)

    list(
        release_root = release_root,
        deliverables_dir = deliverables_dir,
        data_snapshot_dir = data_snapshot_dir,
        manifest_path = manifest_path,
        deliverables = unname(deliverable_paths),
        snapshot_path = snapshot_path,
        output_dir = output_dir,
        history_dir = history_dir
    )
}
