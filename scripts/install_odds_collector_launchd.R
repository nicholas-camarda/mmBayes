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

#' Escape text for XML output
#'
#' @param x A character vector.
#'
#' @return XML-safe text.
#' @keywords internal
xml_escape <- function(x) {
    x <- as.character(x)
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    gsub("\"", "&quot;", x, fixed = TRUE)
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

#' Fail when a command returns a non-zero exit status
#'
#' @param result Output from [run_command()].
#' @param label Human-readable command label.
#'
#' @return This function does not return when the command failed.
#' @keywords internal
stop_if_failed <- function(result, label) {
    if (!identical(result$status, 0L)) {
        stop(sprintf("%s failed:\n%s", label, paste(result$output, collapse = "\n")), call. = FALSE)
    }
}

script_path <- find_script_path()
project_root <- normalizePath(file.path(dirname(script_path), ".."))
setwd(project_root)

pkgload::load_all(project_root, export_all = TRUE, helpers = FALSE, quiet = TRUE)
load_dotenv_file(".env", override = FALSE)

stop_with_message(deprecated_live_betting_workflow_message())

label <- "com.ncamarda.mmBayes.odds_collector"
uid_result <- run_command("/usr/bin/id", c("-u"))
if (length(uid_result$output) == 0) {
    stop_with_message("Unable to determine the current user ID for launchctl bootstrap.")
}
uid <- suppressWarnings(as.integer(uid_result$output[[1]]))
if (length(uid) == 0 || is.na(uid) || !is.finite(uid)) {
    stop_with_message("Unable to determine the current user ID for launchctl bootstrap.")
}

rscript_path <- Sys.which("Rscript")
if (!nzchar(rscript_path)) {
    stop_with_message("Rscript was not found on PATH. Install R or add Rscript to PATH before installing the LaunchAgent.")
}

repo_script <- file.path(project_root, "scripts", "run_odds_collector.R")
if (!file.exists(repo_script)) {
    stop_with_message(sprintf("Odds collector script not found at %s", repo_script))
}

launch_agents_dir <- path.expand("~/Library/LaunchAgents")
dir.create(launch_agents_dir, recursive = TRUE, showWarnings = FALSE)
plist_path <- file.path(launch_agents_dir, paste0(label, ".plist"))
runtime_root <- path.expand(file.path("~", "ProjectsRuntime", "mmBayes"))
out_log <- file.path(runtime_root, "output", "logs", "odds_collector.launchd.out.log")
err_log <- file.path(runtime_root, "output", "logs", "odds_collector.launchd.err.log")
dir.create(dirname(out_log), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(err_log), recursive = TRUE, showWarnings = FALSE)

plist <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>\n',
    '<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">\n',
    '<plist version="1.0">\n',
    '<dict>\n',
    '    <key>Label</key>\n',
    '    <string>', xml_escape(label), '</string>\n',
    '    <key>ProgramArguments</key>\n',
    '    <array>\n',
    '        <string>', xml_escape(rscript_path), '</string>\n',
    '        <string>', xml_escape(repo_script), '</string>\n',
    '    </array>\n',
    '    <key>WorkingDirectory</key>\n',
    '    <string>', xml_escape(project_root), '</string>\n',
    '    <key>StartCalendarInterval</key>\n',
    '    <array>\n',
    '        <dict>\n',
    '            <key>Month</key>\n',
    '            <integer>3</integer>\n',
    '            <key>Minute</key>\n',
    '            <integer>0</integer>\n',
    '        </dict>\n',
    '        <dict>\n',
    '            <key>Month</key>\n',
    '            <integer>3</integer>\n',
    '            <key>Minute</key>\n',
    '            <integer>30</integer>\n',
    '        </dict>\n',
    '        <dict>\n',
    '            <key>Month</key>\n',
    '            <integer>4</integer>\n',
    '            <key>Minute</key>\n',
    '            <integer>0</integer>\n',
    '        </dict>\n',
    '        <dict>\n',
    '            <key>Month</key>\n',
    '            <integer>4</integer>\n',
    '            <key>Minute</key>\n',
    '            <integer>30</integer>\n',
    '        </dict>\n',
    '    </array>\n',
    '    <key>StandardOutPath</key>\n',
    '    <string>', xml_escape(out_log), '</string>\n',
    '    <key>StandardErrorPath</key>\n',
    '    <string>', xml_escape(err_log), '</string>\n',
    '</dict>\n',
    '</plist>\n'
)

writeLines(plist, plist_path, useBytes = TRUE)
Sys.chmod(plist_path, mode = "0644")

lint <- run_command("/usr/bin/plutil", c("-lint", plist_path))
stop_if_failed(lint, "plutil -lint")

bootstrap_domain <- sprintf("gui/%s", uid)
launchctl_label <- file.path(bootstrap_domain, label)

bootout <- run_command("/bin/launchctl", c("bootout", bootstrap_domain, plist_path))
if (!identical(bootout$status, 0L) && length(bootout$output) > 0) {
    cat(paste(bootout$output, collapse = "\n"), "\n")
}
bootstrap <- run_command("/bin/launchctl", c("bootstrap", bootstrap_domain, plist_path))
stop_if_failed(bootstrap, "launchctl bootstrap")

enable <- run_command("/bin/launchctl", c("enable", launchctl_label))
if (!identical(enable$status, 0L) && length(enable$output) > 0) {
    cat(paste(enable$output, collapse = "\n"), "\n")
}

kickstart <- run_command("/bin/launchctl", c("kickstart", "-k", launchctl_label))
if (!identical(kickstart$status, 0L) && length(kickstart$output) > 0) {
    cat(paste(kickstart$output, collapse = "\n"), "\n")
}

cat("Installed mmBayes odds collector LaunchAgent\n")
cat(sprintf("- Plist: %s\n", plist_path))
cat(sprintf("- Label: %s\n", label))
cat(sprintf("- Rscript: %s\n", rscript_path))
cat(sprintf("- Script: %s\n", repo_script))
cat(sprintf("- Bootstrap domain: %s\n", bootstrap_domain))
cat(sprintf("- Log out: %s\n", out_log))
cat(sprintf("- Log err: %s\n", err_log))
