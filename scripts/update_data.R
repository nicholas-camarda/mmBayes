#!/usr/bin/env Rscript

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

initialize_logging("tournament_simulation.log")
paths <- update_tournament_data()

cat("Updated tournament data files:\n")
cat(sprintf("- Bart data: %s\n", paths$bart_data))
cat(sprintf("- Conference assignments: %s\n", paths$conf_assignments))
cat(sprintf("- Canonical model data: %s\n", paths$final_data))
