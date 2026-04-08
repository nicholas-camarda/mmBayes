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

#' Run a command and capture output
#'
#' @param command The executable path.
#' @param args Command-line arguments.
#'
#' @return A list with `output` and `status`.
#' @keywords internal
run_command <- function(command, args) {
    output <- system2(command, args, stdout = TRUE, stderr = TRUE)
    status <- attr(output, "status")
    if (is.null(status)) {
        status <- 0L
    }
    list(output = output, status = as.integer(status))
}

script_path <- find_script_path()
project_root <- normalizePath(file.path(dirname(script_path), ".."))
setwd(project_root)

pkgload::load_all(project_root, export_all = TRUE, helpers = FALSE, quiet = TRUE)

stop_with_message(deprecated_live_betting_workflow_message())

label <- "com.ncamarda.mmBayes.odds_collector"
uid_result <- run_command("/usr/bin/id", c("-u"))
if (length(uid_result$output) == 0) {
    stop_with_message("Unable to determine the current user ID for launchctl bootout.")
}
uid <- suppressWarnings(as.integer(uid_result$output[[1]]))
if (length(uid) == 0 || is.na(uid) || !is.finite(uid)) {
    stop_with_message("Unable to determine the current user ID for launchctl bootout.")
}

launch_agents_dir <- path.expand("~/Library/LaunchAgents")
plist_path <- file.path(launch_agents_dir, paste0(label, ".plist"))
bootstrap_domain <- sprintf("gui/%s", uid)
launchctl_label <- file.path(bootstrap_domain, label)

bootout <- run_command("/bin/launchctl", c("bootout", bootstrap_domain, plist_path))
if (!identical(bootout$status, 0L) && length(bootout$output) > 0) {
    cat(paste(bootout$output, collapse = "\n"), "\n")
}

if (file.exists(plist_path)) {
    unlink(plist_path)
}

disable <- run_command("/bin/launchctl", c("disable", launchctl_label))
if (!identical(disable$status, 0L) && length(disable$output) > 0) {
    cat(paste(disable$output, collapse = "\n"), "\n")
}

cat("Removed mmBayes odds collector LaunchAgent\n")
cat(sprintf("- Plist: %s\n", plist_path))
cat(sprintf("- Label: %s\n", label))
cat(sprintf("- Bootstrap domain: %s\n", bootstrap_domain))
