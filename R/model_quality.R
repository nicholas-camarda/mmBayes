library(dplyr)
library(tibble)

#' Check whether a backtest bundle contains usable quality metrics
#'
#' @param backtest A backtest result bundle or `NULL`.
#'
#' @return `TRUE` when the bundle has a non-empty `summary` table.
#' @keywords internal
model_quality_has_backtest <- function(backtest) {
    !is.null(backtest) &&
        is.list(backtest) &&
        !is.null(backtest$summary) &&
        inherits(backtest$summary, "data.frame") &&
        nrow(backtest$summary) > 0
}

#' Build the model-quality artifact directory
#'
#' @param output_dir Base output directory.
#'
#' @return A path to the model-quality subdirectory.
#' @keywords internal
build_model_quality_directory <- function(output_dir = "output") {
    file.path(output_dir %||% "output", "model_quality")
}

#' Build canonical model-quality artifact paths
#'
#' @param output_dir Base output directory.
#' @param timestamp Timestamp used to name the archived snapshot.
#' @param process_id Process identifier used to reduce collisions.
#'
#' @return A named list with quality directory and file paths.
#' @keywords internal
build_model_quality_paths <- function(output_dir = "output", timestamp = Sys.time(), process_id = Sys.getpid()) {
    quality_dir <- build_model_quality_directory(output_dir)
    stamp <- gsub("\\.", "", format(timestamp, "%Y%m%d_%H%M%OS6"))
    archive_path <- file.path(quality_dir, sprintf("model_quality_%s_pid%s.rds", stamp, process_id))
    latest_path <- file.path(quality_dir, "latest_model_quality.rds")

    list(
        quality_dir = quality_dir,
        archive_path = archive_path,
        latest_path = latest_path
    )
}

#' Save a backtest bundle as the latest model-quality artifact
#'
#' @param backtest A backtest result bundle.
#' @param output_dir Base output directory.
#' @param timestamp Timestamp used to name the archived snapshot.
#' @param process_id Process identifier used to reduce collisions.
#'
#' @return A list containing the saved paths and the stored artifact, or `NULL`
#'   when no usable backtest summary was supplied.
#' @keywords internal
save_model_quality_artifact <- function(backtest, output_dir = "output", timestamp = Sys.time(), process_id = Sys.getpid()) {
    if (!model_quality_has_backtest(backtest)) {
        return(NULL)
    }

    paths <- build_model_quality_paths(output_dir, timestamp = timestamp, process_id = process_id)
    dir.create(paths$quality_dir, recursive = TRUE, showWarnings = FALSE)

    artifact <- list(
        version = 1L,
        generated_at = as.POSIXct(timestamp, tz = "UTC"),
        source = "current run backtest",
        backtest = backtest,
        archive_path = paths$archive_path,
        latest_path = paths$latest_path
    )

    saveRDS(artifact, paths$archive_path)
    saveRDS(artifact, paths$latest_path)

    c(paths, list(artifact = artifact))
}

#' Load the latest saved model-quality artifact
#'
#' @param output_dir Base output directory.
#'
#' @return A stored model-quality artifact, or `NULL` if no snapshot exists.
#' @keywords internal
load_latest_model_quality_artifact <- function(output_dir = "output") {
    paths <- build_model_quality_paths(output_dir)
    if (!file.exists(paths$latest_path)) {
        return(NULL)
    }

    artifact <- readRDS(paths$latest_path)
    if (is.null(artifact$latest_path)) {
        artifact$latest_path <- paths$latest_path
    }
    if (is.null(artifact$archive_path) && !is.null(artifact$latest_path)) {
        artifact$archive_path <- artifact$latest_path
    }

    artifact
}

#' Resolve the backtest used for dashboard rendering
#'
#' @param backtest An optional backtest bundle from the current run.
#' @param output_dir Base output directory.
#' @param allow_fallback Whether to fall back to the latest saved snapshot.
#'
#' @return A list containing the effective backtest bundle and source labels.
#' @keywords internal
resolve_model_quality_context <- function(backtest = NULL, output_dir = "output", allow_fallback = TRUE) {
    if (model_quality_has_backtest(backtest)) {
        return(list(
            backtest = backtest,
            source_label = "Current run backtest",
            source_path = NULL,
            used_fallback = FALSE
        ))
    }

    if (isFALSE(allow_fallback)) {
        return(list(
            backtest = NULL,
            source_label = "Backtest unavailable",
            source_path = NULL,
            used_fallback = FALSE
        ))
    }

    artifact <- load_latest_model_quality_artifact(output_dir)
    if (!is.null(artifact) && model_quality_has_backtest(artifact$backtest)) {
        generated_at <- artifact$generated_at
        if (is.null(generated_at)) {
            source_label <- "Latest saved model-quality snapshot"
        } else {
            source_label <- sprintf(
                "Latest saved model-quality snapshot from %s",
                format(as.POSIXct(generated_at, tz = "UTC"), "%Y-%m-%d %H:%M UTC")
            )
        }

        return(list(
            backtest = artifact$backtest,
            source_label = source_label,
            source_path = artifact$archive_path %||% artifact$latest_path,
            used_fallback = TRUE
        ))
    }

    list(
        backtest = NULL,
        source_label = "Backtest unavailable",
        source_path = NULL,
        used_fallback = FALSE
    )
}

#' Format the confidence-tier mix for a summary sentence
#'
#' @param decision_sheet A decision-sheet data frame.
#' @param top_n Number of rows to consider.
#'
#' @return A short plain-text summary of the tier mix.
#' @keywords internal
summarize_ranked_decision_board <- function(decision_sheet, top_n = 12L) {
    if (nrow(decision_sheet) == 0) {
        return("No decision rows were available.")
    }

    top_rows <- decision_sheet %>%
        dplyr::arrange(dplyr::desc(decision_score), round, region, matchup_number) %>%
        dplyr::slice_head(n = top_n)

    tier_counts <- top_rows %>%
        dplyr::count(confidence_tier, name = "n") %>%
        dplyr::mutate(confidence_tier = factor(confidence_tier, levels = c("Lock", "Lean", "Toss-up", "Volatile"))) %>%
        dplyr::arrange(confidence_tier)

    tier_text <- paste(
        sprintf("%s %s", tier_counts$n, as.character(tier_counts$confidence_tier)),
        collapse = ", "
    )
    late_rounds <- sum(as.character(top_rows$round) %in% c("Sweet 16", "Elite 8", "Final Four", "Championship"))
    uncertain_rows <- sum(top_rows$confidence_tier %in% c("Toss-up", "Volatile"))

    if (late_rounds >= ceiling(top_n / 2)) {
        sprintf(
            "The ranked board is late-round heavy: %s of the top %s rows are Sweet 16 or later. The color mix is %s, so the review queue is being driven mostly by uncertainty rather than locks.",
            late_rounds,
            top_n,
            tier_text
        )
    } else {
        sprintf(
            "The ranked board is mixed but still noisy: %s of the top %s rows are Toss-ups or Volatile and %s are Sweet 16 or later. The color mix is %s, so the dashboard is surfacing the games most likely to move the bracket.",
            uncertain_rows,
            top_n,
            late_rounds,
            tier_text
        )
    }
}

#' Summarize the upset-opportunity board in plain English
#'
#' @param decision_sheet A decision-sheet data frame.
#' @param top_n Number of rows to consider.
#'
#' @return A short plain-text summary of the upset board.
#' @keywords internal
summarize_upset_opportunity_board <- function(decision_sheet, top_n = 10L) {
    if (nrow(decision_sheet) == 0) {
        return("No upset opportunities were available.")
    }

    top_rows <- decision_sheet %>%
        dplyr::arrange(dplyr::desc(upset_leverage), dplyr::desc(decision_score), round, matchup_number) %>%
        dplyr::slice_head(n = top_n)
    top_row <- top_rows %>% dplyr::slice_head(n = 1)
    late_rounds <- sum(as.character(top_rows$round) %in% c("Sweet 16", "Elite 8", "Final Four", "Championship"))
    early_rounds <- sum(as.character(top_rows$round) %in% c("First Four", "Round of 64", "Round of 32"))

    if (nrow(top_row) == 1L) {
        top_row_text <- sprintf(
            "%s over %s in the %s",
            top_row$underdog[[1]],
            top_row$posterior_favorite[[1]],
            as.character(top_row$round[[1]])
        )
    } else {
        top_row_text <- "the top leverage game"
    }

    if (late_rounds >= early_rounds) {
        sprintf(
            "The biggest upset opportunities are mostly late-round: %s of the top %s rows are Sweet 16 or later, and the top leverage spot is %s. That means this board is more about a few high-payoff pivots than about early-round chaos.",
            late_rounds,
            top_n,
            top_row_text
        )
    } else {
        sprintf(
            "The upset board is leaning earlier in the bracket: %s of the top %s rows are Round of 32 or earlier, and the top leverage spot is %s. That means the board is pointing to bracket swings that can compound downstream.",
            early_rounds,
            top_n,
            top_row_text
        )
    }
}

#' Summarize candidate divergence in plain English
#'
#' @param decision_sheet A decision-sheet data frame.
#'
#' @return A short plain-text summary of how Candidate 1 and Candidate 2 differ.
#' @keywords internal
summarize_candidate_divergence <- function(decision_sheet) {
    if (nrow(decision_sheet) == 0) {
        return("No candidate comparison rows were available.")
    }

    diff_rows <- decision_sheet %>%
        dplyr::filter(candidate_diff_flag)

    if (nrow(diff_rows) == 0) {
        return("Candidate 1 and Candidate 2 match on every slot, so there is no bracket divergence to inspect.")
    }

    early_rounds <- sum(as.character(diff_rows$round) %in% c("First Four", "Round of 64", "Round of 32"))
    late_rounds <- sum(as.character(diff_rows$round) %in% c("Sweet 16", "Elite 8", "Final Four", "Championship"))
    first_diff <- diff_rows %>% dplyr::arrange(round, region, matchup_number) %>% dplyr::slice_head(n = 1)
    first_diff_text <- if (nrow(first_diff) == 1L) {
        sprintf(
            "%s vs %s",
            first_diff$teamA[[1]],
            first_diff$teamB[[1]]
        )
    } else {
        "the first differing slot"
    }

    if (late_rounds >= early_rounds) {
        sprintf(
            "The candidates differ in %s slots, with %s of those changes in Sweet 16 or later. The alternate path is concentrating its changes in higher-value games, starting with %s.",
            nrow(diff_rows),
            late_rounds,
            first_diff_text
        )
    } else {
        sprintf(
            "The candidates differ in %s slots, with %s of those changes in the first two rounds. The alternate path is changing the bracket earlier, starting with %s.",
            nrow(diff_rows),
            early_rounds,
            first_diff_text
        )
    }
}

#' Summarize model-quality metrics in plain English
#'
#' @param backtest A backtest result bundle.
#'
#' @return A short plain-text quality summary.
#' @keywords internal
summarize_model_quality <- function(backtest) {
    if (!model_quality_has_backtest(backtest)) {
        return("Backtest quality metrics were not available for this run.")
    }

    summary_row <- backtest$summary %>% dplyr::slice_head(n = 1)
    log_loss <- safe_numeric(summary_row$mean_log_loss[[1]], default = NA_real_)
    brier <- safe_numeric(summary_row$mean_brier[[1]], default = NA_real_)
    accuracy <- safe_numeric(summary_row$mean_accuracy[[1]], default = NA_real_) * 100

    calibration_tbl <- backtest$calibration
    if (inherits(calibration_tbl, "data.frame") && nrow(calibration_tbl) > 0) {
        gaps <- abs(safe_numeric(calibration_tbl$empirical_rate, default = NA_real_) - safe_numeric(calibration_tbl$mean_predicted, default = NA_real_))
        max_gap <- max(gaps, na.rm = TRUE)
        mean_gap <- mean(gaps, na.rm = TRUE)
        calibration_text <- if (is.finite(max_gap) && max_gap <= 0.03) {
            sprintf("The calibration curve hugs the diagonal closely. Each point shows the observed win rate, or empirical rate, in a probability bin, and the mean bin gap is %.3f.", mean_gap)
        } else if (is.finite(max_gap) && max_gap <= 0.06) {
            sprintf("The calibration curve stays fairly close to the diagonal. Each point shows the observed win rate, or empirical rate, in a probability bin, and the mean bin gap is %.3f.", mean_gap)
        } else {
            sprintf("The calibration curve shows a noticeable wobble. Each point shows the observed win rate, or empirical rate, in a probability bin, and the mean bin gap is %.3f.", mean_gap)
        }
    } else {
        calibration_text <- "A bin-level calibration chart is not available for this snapshot."
    }

    sprintf(
        "Log loss is %.3f, Brier score is %.3f, and accuracy is %.1f%%. %s Lower log loss and Brier are better, so this panel is the quick check on whether the model is both sharp and reasonably calibrated.",
        log_loss,
        brier,
        accuracy,
        calibration_text
    )
}

