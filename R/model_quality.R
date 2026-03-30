library(dplyr)
library(tibble)

#' Hash an R object into a stable fingerprint
#'
#' @param object Any serializable R object.
#'
#' @return A single MD5 hash string.
#' @keywords internal
hash_serialized_object <- function(object) {
    tmp <- tempfile(fileext = ".rds")
    on.exit(unlink(tmp), add = TRUE)
    saveRDS(object, tmp)
    tools::md5sum(tmp)[[1]]
}

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
build_model_quality_directory <- function(output_dir = default_runtime_output_root()) {
    file.path(output_dir %||% default_runtime_output_root(), "model_quality")
}

#' Build a quality signature for a run bundle
#'
#' @param results A run bundle returned by [run_tournament_simulation()].
#'
#' @return A stable fingerprint used to match cached quality artifacts.
#' @keywords internal
build_model_quality_signature <- function(results) {
    if (is.null(results) || !is.list(results)) {
        return(NULL)
    }

    model_results <- results$model %||% list()
    data_results <- results$data %||% list()

    signature <- list(
        bracket_year = results$bracket_year %||% NULL,
        draws_budget = results$draws_budget %||% NULL,
        model = list(
            engine = model_results$engine %||% NULL,
            predictor_columns = model_results$predictor_columns %||% character(0),
            interaction_terms = model_results$interaction_terms %||% character(0),
            prior_type = model_results$prior_type %||% NULL,
            bart_config = model_results$bart_config %||% NULL,
            scaling_reference = model_results$scaling_reference %||% NULL,
            model_matrix_columns = model_results$model_matrix_columns %||% character(0),
            round_levels = model_results$round_levels %||% character(0)
        ),
        data = list(
            historical_teams = data_results$historical_teams %||% tibble::tibble(),
            historical_actual_results = data_results$historical_actual_results %||% tibble::tibble()
        )
    )

    hash_serialized_object(signature)
}

#' Summarize the model configuration for dashboard display
#'
#' @param model_results A fitted model bundle from [fit_tournament_model()].
#' @param draws Posterior draw budget used for simulation and scoring.
#'
#' @return A named list of display-friendly model details.
#' @keywords internal
summarize_model_overview <- function(model_results, draws = NULL) {
    if (is.null(model_results)) {
        return(list())
    }

    engine <- model_results$engine %||% "unknown"
    predictor_columns <- model_results$predictor_columns %||% character(0)
    feature_columns <- setdiff(predictor_columns, "round")
    betting_predictor_count <- sum(startsWith(feature_columns, "betting_"), na.rm = TRUE)
    bart_config <- model_results$bart_config %||% list()
    draw_budget <- draws %||% (if (identical(engine, "bart")) safe_numeric(bart_config$n_post, default = NA_real_) else NA_real_)

    list(
        engine = engine,
        engine_label = if (identical(engine, "bart")) "BART" else "Stan GLM",
        prior_type = model_results$prior_type %||% NULL,
        draw_budget = draw_budget,
        predictor_count = length(feature_columns),
        betting_predictor_count = betting_predictor_count,
        predictor_summary = if (length(feature_columns) == 0) {
            "No predictors were recorded."
        } else {
            head_labels <- head(feature_columns, n = 6L)
            suffix <- if (length(feature_columns) > length(head_labels)) " ..." else ""
            sprintf("%s predictors: %s%s", length(feature_columns), paste(head_labels, collapse = ", "), suffix)
        },
        bart_config = bart_config,
        interaction_terms = model_results$interaction_terms %||% character(0),
        cache_path = model_results$cache_path %||% NULL
    )
}

#' Build a display table comparing two model summaries
#'
#' @param current_summary A one-row summary tibble for the current model.
#' @param alternate_summary A one-row summary tibble for the alternate model.
#' @param current_label Human-readable label for the current model.
#' @param alternate_label Human-readable label for the alternate model.
#' @param kind Comparison kind, either `"backtest"` or `"live"`.
#'
#' @return A tibble with formatted values, deltas, and a winner column.
#' @keywords internal
build_model_metric_comparison_table <- function(current_summary,
                                                alternate_summary,
                                                current_label,
                                                alternate_label,
                                                kind = c("backtest", "live")) {
    kind <- match.arg(kind)
    current_summary <- current_summary %||% tibble::tibble()
    alternate_summary <- alternate_summary %||% tibble::tibble()
    if (!inherits(current_summary, "data.frame") || !inherits(alternate_summary, "data.frame")) {
        return(tibble::tibble())
    }
    if (nrow(current_summary) == 0 || nrow(alternate_summary) == 0) {
        return(tibble::tibble())
    }

    metric_specs <- switch(
        kind,
        backtest = tibble::tibble(
            metric = c("Log loss", "Brier score", "Accuracy", "Bracket score", "Correct picks"),
            field = c("mean_log_loss", "mean_brier", "mean_accuracy", "mean_bracket_score", "mean_correct_picks"),
            direction = c("lower", "lower", "higher", "higher", "higher"),
            digits = c(3L, 3L, 1L, 1L, 1L),
            percent = c(FALSE, FALSE, TRUE, FALSE, FALSE)
        ),
        live = tibble::tibble(
            metric = c("Games played", "Log loss", "Brier score", "Accuracy"),
            field = c("games_played", "log_loss", "brier", "accuracy"),
            direction = c("higher", "lower", "lower", "higher"),
            digits = c(0L, 3L, 3L, 1L),
            percent = c(FALSE, FALSE, FALSE, TRUE)
        )
    )

    current_row <- current_summary[1, , drop = FALSE]
    alternate_row <- alternate_summary[1, , drop = FALSE]

    extract_metric <- function(row, field) {
        if (!field %in% names(row)) {
            return(NA_real_)
        }
        row[[field]][[1]]
    }

    format_metric_value <- function(value, digits, percent = FALSE) {
        value <- safe_numeric(value, default = NA_real_)
        if (!is.finite(value)) {
            return("n/a")
        }
        if (isTRUE(percent)) {
            return(format_probability(value))
        }
        if (digits <= 0L) {
            return(as.character(as.integer(round(value))))
        }
        sprintf(paste0("%.", digits, "f"), value)
    }

    format_metric_delta <- function(delta, digits, percent = FALSE) {
        delta <- safe_numeric(delta, default = NA_real_)
        if (!is.finite(delta)) {
            return("n/a")
        }
        if (isTRUE(percent)) {
            return(sprintf("%+.1f pp", 100 * delta))
        }
        if (digits <= 0L) {
            return(sprintf("%+d", as.integer(round(delta))))
        }
        sprintf(paste0("%+.", digits, "f"), delta)
    }

    purrr::pmap_dfr(metric_specs, function(metric, field, direction, digits, percent) {
        current_value <- safe_numeric(extract_metric(current_row, field), default = NA_real_)
        alternate_value <- safe_numeric(extract_metric(alternate_row, field), default = NA_real_)
        delta <- alternate_value - current_value
        winner <- if (!is.finite(current_value) || !is.finite(alternate_value)) {
            "Unavailable"
        } else if (abs(delta) < 1e-12) {
            "Tie"
        } else if ((identical(direction, "lower") && alternate_value < current_value) ||
            (identical(direction, "higher") && alternate_value > current_value)) {
            alternate_label
        } else {
            current_label
        }

        row <- tibble::tibble(
            Metric = metric,
            `Current` = format_metric_value(current_value, digits, percent),
            `Alternate` = format_metric_value(alternate_value, digits, percent),
            Delta = format_metric_delta(delta, digits, percent),
            Winner = winner
        )
        names(row)[names(row) == "Current"] <- current_label
        names(row)[names(row) == "Alternate"] <- alternate_label
        row
    })
}

#' Summarize which model wins more metrics in a comparison table
#'
#' @param comparison_table A tibble returned by [build_model_metric_comparison_table()].
#' @param current_label Human-readable label for the current model.
#' @param alternate_label Human-readable label for the alternate model.
#'
#' @return A named list with counts and a short explanation.
#' @keywords internal
summarize_model_metric_comparison <- function(comparison_table, current_label, alternate_label) {
    if (!inherits(comparison_table, "data.frame") || nrow(comparison_table) == 0) {
        return(list(
            current_wins = 0L,
            alternate_wins = 0L,
            ties = 0L,
            text = "No comparable metrics were available."
        ))
    }

    counts <- table(comparison_table$Winner %||% character())
    current_wins <- if (current_label %in% names(counts)) as.integer(counts[[current_label]]) else 0L
    alternate_wins <- if (alternate_label %in% names(counts)) as.integer(counts[[alternate_label]]) else 0L
    ties <- if ("Tie" %in% names(counts)) as.integer(counts[["Tie"]]) else 0L
    unavailable <- if ("Unavailable" %in% names(counts)) as.integer(counts[["Unavailable"]]) else 0L

    text <- if (unavailable == nrow(comparison_table)) {
        "No comparable metrics were available."
    } else {
        parts <- c()
        if (current_wins > 0L) {
            parts <- c(parts, sprintf("%s wins %s metric%s", current_label, current_wins, if (current_wins == 1L) "" else "s"))
        }
        if (alternate_wins > 0L) {
            parts <- c(parts, sprintf("%s wins %s metric%s", alternate_label, alternate_wins, if (alternate_wins == 1L) "" else "s"))
        }
        if (ties > 0L) {
            parts <- c(parts, sprintf("%s tie%s", ties, if (ties == 1L) "" else "s"))
        }
        if (length(parts) == 0L) {
            "No clear winner emerged."
        } else {
            paste(parts, collapse = "; ")
        }
    }

    list(
        current_wins = current_wins,
        alternate_wins = alternate_wins,
        ties = ties,
        text = text
    )
}

#' Summarize prediction performance by round
#'
#' @param predictions A prediction table containing `round`, `predicted_prob`,
#'   and `actual_outcome`.
#'
#' @return A tibble with one row per round and key metrics.
#' @keywords internal
summarize_prediction_round_performance <- function(predictions) {
    if (!inherits(predictions, "data.frame") || nrow(predictions) == 0 || !all(c("round", "predicted_prob", "actual_outcome") %in% names(predictions))) {
        return(tibble::tibble())
    }

    predictions %>%
        dplyr::mutate(round = factor(as.character(round), levels = round_levels())) %>%
        dplyr::group_by(round) %>%
        dplyr::group_modify(function(.x, .y) {
            metrics <- compute_binary_metrics(.x$predicted_prob, .x$actual_outcome)
            dplyr::mutate(
                metrics,
                games = nrow(.x),
                mean_predicted = mean(.x$predicted_prob),
                empirical_rate = mean(.x$actual_outcome)
            )
        }) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(round = as.character(round))
}

format_calibration_bin <- function(bin_value) {
    label <- as.character(bin_value %||% "")
    if (!nzchar(label)) {
        return("unknown bin")
    }

    match <- regexec("^\\(?([0-9.]+),([0-9.]+)\\]?$", label)
    parts <- regmatches(label, match)[[1]]
    if (length(parts) == 3) {
        lower <- safe_numeric(parts[2], default = NA_real_) * 100
        upper <- safe_numeric(parts[3], default = NA_real_) * 100
        if (is.finite(lower) && is.finite(upper)) {
            return(sprintf("%.0f%% to %.0f%%", lower, upper))
        }
    }

    label
}

#' Summarize backtest performance for diagnostics
#'
#' @param backtest A backtest bundle returned by [run_rolling_backtest()].
#'
#' @return A list of display-friendly diagnostics tables and notes.
#' @keywords internal
summarize_backtest_diagnostics <- function(backtest) {
    if (!model_quality_has_backtest(backtest)) {
        return(list(
            round_summary = tibble::tibble(),
            calibration = tibble::tibble(),
            strengths = character(),
            weaknesses = character(),
            calibration_notes = character(),
            best_round = NULL,
            worst_round = NULL,
            best_bin = NULL,
            worst_bin = NULL
        ))
    }

    predictions <- backtest$predictions %||% tibble::tibble()
    round_summary <- summarize_prediction_round_performance(predictions)

    calibration <- backtest$calibration %||% tibble::tibble()
    calibration <- if (nrow(calibration) > 0 && all(c("mean_predicted", "empirical_rate") %in% names(calibration))) {
        calibration %>%
            dplyr::mutate(
                bin = if ("bin" %in% names(.)) {
                    as.character(bin)
                } else {
                    sprintf("%.0f%% bucket", 100 * safe_numeric(mean_predicted, default = NA_real_))
                }
            ) %>%
            dplyr::mutate(calibration_gap = abs(safe_numeric(empirical_rate, default = NA_real_) - safe_numeric(mean_predicted, default = NA_real_)))
    } else {
        tibble::tibble()
    }

    strengths <- character()
    weaknesses <- character()
    calibration_notes <- character()
    best_round <- NULL
    worst_round <- NULL
    best_bin <- NULL
    worst_bin <- NULL

    if (nrow(round_summary) > 0) {
        best_round <- round_summary %>%
            dplyr::arrange(dplyr::desc(accuracy), brier) %>%
            dplyr::slice_head(n = 1)
        worst_round <- round_summary %>%
            dplyr::arrange(accuracy, dplyr::desc(brier)) %>%
            dplyr::slice_head(n = 1)

        if (nrow(best_round) == 1) {
            strengths <- c(
                strengths,
                sprintf(
                    "Strongest round: %s with %.1f%% accuracy and %.3f Brier.",
                    best_round$round[[1]],
                    100 * safe_numeric(best_round$accuracy[[1]], default = NA_real_),
                    safe_numeric(best_round$brier[[1]], default = NA_real_)
                )
            )
        }
        if (nrow(worst_round) == 1) {
            weaknesses <- c(
                weaknesses,
                sprintf(
                    "Weakest round: %s with %.1f%% accuracy and %.3f Brier.",
                    worst_round$round[[1]],
                    100 * safe_numeric(worst_round$accuracy[[1]], default = NA_real_),
                    safe_numeric(worst_round$brier[[1]], default = NA_real_)
                )
            )
        }
    }

    if (nrow(calibration) > 0) {
        best_bin <- calibration %>%
            dplyr::arrange(calibration_gap, dplyr::desc(n_games)) %>%
            dplyr::slice_head(n = 1)
        worst_bin <- calibration %>%
            dplyr::arrange(dplyr::desc(calibration_gap), dplyr::desc(n_games)) %>%
            dplyr::slice_head(n = 1)

        if (nrow(best_bin) == 1) {
            strengths <- c(
                strengths,
                sprintf(
                    "Most calibrated bin: the %s bucket had a mean predicted win rate of %.0f%% and an observed win rate of %.0f%%, so the gap was %.3f (%.1f percentage points). Smaller gaps mean better calibration.",
                    format_calibration_bin(best_bin$bin[[1]]),
                    100 * safe_numeric(best_bin$mean_predicted[[1]], default = NA_real_),
                    100 * safe_numeric(best_bin$empirical_rate[[1]], default = NA_real_),
                    safe_numeric(best_bin$calibration_gap[[1]], default = NA_real_),
                    100 * safe_numeric(best_bin$calibration_gap[[1]], default = NA_real_)
                )
            )
        }
        if (nrow(worst_bin) == 1) {
            weaknesses <- c(
                weaknesses,
                sprintf(
                    "Least calibrated bin: the %s bucket had a mean predicted win rate of %.0f%% and an observed win rate of %.0f%%, so the gap was %.3f (%.1f percentage points). Larger gaps mean the model is less calibrated in that range.",
                    format_calibration_bin(worst_bin$bin[[1]]),
                    100 * safe_numeric(worst_bin$mean_predicted[[1]], default = NA_real_),
                    100 * safe_numeric(worst_bin$empirical_rate[[1]], default = NA_real_),
                    safe_numeric(worst_bin$calibration_gap[[1]], default = NA_real_),
                    100 * safe_numeric(worst_bin$calibration_gap[[1]], default = NA_real_)
                )
            )
        }
    }

    if (nrow(round_summary) > 0) {
        calibration_notes <- c(
            calibration_notes,
            sprintf("Round table covers %s completed games.", sum(round_summary$games, na.rm = TRUE))
        )
    }

    list(
        round_summary = round_summary,
        calibration = calibration,
        strengths = strengths,
        weaknesses = weaknesses,
        calibration_notes = calibration_notes,
        best_round = best_round,
        worst_round = worst_round,
        best_bin = best_bin,
        worst_bin = worst_bin
    )
}

#' Build canonical model-quality artifact paths
#'
#' @param output_dir Base output directory.
#' @param timestamp Timestamp used when the current artifact is written.
#' @param process_id Process identifier used to reduce collisions.
#'
#' @return A named list with quality directory and latest-file paths.
#' @keywords internal
build_model_quality_paths <- function(output_dir = default_runtime_output_root(), timestamp = Sys.time(), process_id = Sys.getpid()) {
    quality_dir <- build_model_quality_directory(output_dir)
    latest_path <- file.path(quality_dir, "latest_model_quality.rds")

    list(
        quality_dir = quality_dir,
        artifact_path = latest_path,
        archive_path = latest_path,
        latest_path = latest_path
    )
}

#' Save a backtest bundle as the latest model-quality artifact
#'
#' @param backtest A backtest result bundle.
#' @param output_dir Base output directory.
#' @param timestamp Timestamp used to name the archived snapshot.
#' @param process_id Process identifier used to reduce collisions.
#' @param quality_signature A stable fingerprint for the current model/data snapshot.
#'
#' @return A list containing the saved paths and the stored artifact, or `NULL`
#'   when no usable backtest summary was supplied.
#' @keywords internal
save_model_quality_artifact <- function(backtest, output_dir = default_runtime_output_root(), timestamp = Sys.time(), process_id = Sys.getpid(), quality_signature = NULL) {
    if (!model_quality_has_backtest(backtest)) {
        return(NULL)
    }

    paths <- build_model_quality_paths(output_dir, timestamp = timestamp, process_id = process_id)
    dir.create(paths$quality_dir, recursive = TRUE, showWarnings = FALSE)

    artifact <- list(
        version = 2L,
        generated_at = as.POSIXct(timestamp, tz = "UTC"),
        source = "current run backtest",
        quality_signature = quality_signature %||% NULL,
        backtest = backtest,
        artifact_path = paths$latest_path,
        archive_path = paths$latest_path,
        latest_path = paths$latest_path
    )

    tmp_path <- tempfile(tmpdir = paths$quality_dir, fileext = ".rds")
    saveRDS(artifact, tmp_path)
    if (file.exists(paths$latest_path)) {
        unlink(paths$latest_path)
    }
    if (!file.rename(tmp_path, paths$latest_path)) {
        unlink(tmp_path)
        stop_with_message(sprintf("Failed to write model-quality artifact to %s", paths$latest_path))
    }

    paths$archive_path <- paths$latest_path
    paths$artifact_path <- paths$latest_path
    c(paths, list(artifact = artifact))
}

#' Load the latest saved model-quality artifact
#'
#' @param output_dir Base output directory.
#'
#' @return A stored model-quality artifact, or `NULL` if no snapshot exists.
#' @keywords internal
load_latest_model_quality_artifact <- function(output_dir = default_runtime_output_root()) {
    paths <- build_model_quality_paths(output_dir)
    if (!file.exists(paths$latest_path)) {
        return(NULL)
    }

    artifact <- readRDS(paths$latest_path)
    if (is.null(artifact$artifact_path)) {
        artifact$artifact_path <- paths$latest_path
    }
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
#' @param quality_signature A stable fingerprint for the current model/data snapshot.
#' @param allow_fallback Whether a matching cached snapshot may be used.
#' @param require_exact_match Whether cached reuse must match the fingerprint exactly.
#'
#' @return A list containing the effective backtest bundle and source labels.
#' @keywords internal
resolve_model_quality_context <- function(backtest = NULL, output_dir = default_runtime_output_root(), quality_signature = NULL, allow_fallback = TRUE, require_exact_match = TRUE) {
    if (model_quality_has_backtest(backtest)) {
        return(list(
            backtest = backtest,
            source_label = "Current run backtest",
            source_path = NULL,
            used_fallback = FALSE,
            used_cached_quality = FALSE,
            quality_signature = quality_signature %||% NULL
        ))
    }

    if (isFALSE(allow_fallback)) {
        stop_with_message("Model-quality fallback is disabled and no current-run backtest was supplied.")
    }

    if (is.null(quality_signature) && isTRUE(require_exact_match)) {
        stop_with_message("A model-quality fingerprint is required to reuse cached statistics.")
    }

    artifact <- load_latest_model_quality_artifact(output_dir)
    if (!is.null(artifact) && model_quality_has_backtest(artifact$backtest)) {
        matches_signature <- identical(artifact$quality_signature %||% NULL, quality_signature)
        if (!isTRUE(matches_signature) && isTRUE(require_exact_match)) {
            stop_with_message("The latest saved model-quality snapshot does not match the current model/data fingerprint.")
        }
        if (!isTRUE(matches_signature)) {
            stop_with_message("A matching model-quality snapshot was not found for this run.")
        }

        generated_at <- artifact$generated_at
        if (is.null(generated_at)) {
            source_label <- "Cached identical validation snapshot"
        } else {
            source_label <- sprintf(
                "Cached identical validation snapshot from %s",
                format(as.POSIXct(generated_at, tz = "UTC"), "%Y-%m-%d %H:%M UTC")
            )
        }

        return(list(
            backtest = artifact$backtest,
            source_label = source_label,
            source_path = artifact$artifact_path %||% artifact$latest_path,
            used_fallback = TRUE,
            used_cached_quality = TRUE,
            quality_signature = artifact$quality_signature %||% NULL
        ))
    }

    if (!is.null(quality_signature) || isTRUE(require_exact_match) || isTRUE(allow_fallback)) {
        return(list(
            backtest = list(),
            source_label = "Backtest not available for this run.",
            source_path = NULL,
            used_fallback = FALSE,
            used_cached_quality = FALSE,
            quality_signature = quality_signature %||% NULL
        ))
    }

    stop_with_message("No usable model-quality snapshot was available.")
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
            sprintf("The calibration curve hugs the diagonal closely. Each point is a probability bin: the x-axis is the model's average predicted win rate in that bin, the y-axis is the observed win rate, and the mean bin gap is %.3f.", mean_gap)
        } else if (is.finite(max_gap) && max_gap <= 0.06) {
            sprintf("The calibration curve stays fairly close to the diagonal. Each point is a probability bin: the x-axis is the model's average predicted win rate in that bin, the y-axis is the observed win rate, and the mean bin gap is %.3f.", mean_gap)
        } else {
            sprintf("The calibration curve shows a noticeable wobble. Each point is a probability bin: the x-axis is the model's average predicted win rate in that bin, the y-axis is the observed win rate, and the mean bin gap is %.3f.", mean_gap)
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

#' Summarize live current-year tournament performance
#'
#' @param data A loaded tournament-data bundle from [load_tournament_data()].
#' @param model_results A fitted matchup-model result bundle.
#' @param draws Number of posterior draws to use when scoring current-year games.
#'
#' @return A list with a text summary, summary metrics, and a recent-game table.
#' @keywords internal
summarize_live_tournament_performance <- function(data, model_results, draws = 1000) {
    if (is.null(data) || is.null(model_results)) {
        return(NULL)
    }

    bracket_year <- data$bracket_year %||% NA_integer_
    current_completed_results <- data$current_completed_results %||% tibble::tibble()
    if (nrow(current_completed_results) == 0 && !is.null(data$game_results)) {
        current_completed_results <- data$game_results %>%
            dplyr::filter(
                Year == as.character(bracket_year),
                !is.na(winner),
                !is.na(teamA_score),
                !is.na(teamB_score)
            )
    }

    if (nrow(current_completed_results) == 0) {
        return(list(
            status = "No completed current-year games have been recorded yet.",
            monitoring_note = "Monitoring only: current-year results are reported for evaluation and commentary, not for model refits.",
            interpretive_status = "no completed games",
            summary = tibble::tibble(),
            main_bracket_summary = tibble::tibble(),
            round_summary = tibble::tibble(),
            games = tibble::tibble(),
            games_played = 0L,
            main_bracket_games_played = 0L,
            rounds_played = character()
        ))
    }

    current_reference <- build_actual_game_reference(data$current_teams, current_completed_results)
    matchup_rows <- actual_results_to_matchup_rows(
        current_reference,
        historical_betting_features = data$historical_betting_features %||% tibble::tibble(),
        current_betting_features = model_results$betting_feature_context$current_betting_features %||% tibble::tibble()
    )

    if (nrow(matchup_rows) == 0) {
        return(list(
            status = "Current-year games exist, but no matchup rows could be constructed for live scoring.",
            monitoring_note = "Monitoring only: current-year results are reported for evaluation and commentary, not for model refits.",
            interpretive_status = "no scored live rows",
            summary = tibble::tibble(),
            main_bracket_summary = tibble::tibble(),
            round_summary = tibble::tibble(),
            games = tibble::tibble(),
            games_played = 0L,
            main_bracket_games_played = 0L,
            rounds_played = character()
        ))
    }

    prediction_draws <- predict_matchup_rows(matchup_rows, model_results, draws = draws)
    predicted_prob <- colMeans(prediction_draws)
    recency_lookup <- current_completed_results %>%
        dplyr::transmute(
            Year = as.character(Year),
            region = as.character(region),
            round = as.character(round),
            game_index = as.integer(game_index),
            teamA = as.character(teamA),
            teamB = as.character(teamB),
            completed_at = if ("completed_at" %in% names(current_completed_results)) as.character(completed_at) else NA_character_
        )
    scored_games <- matchup_rows %>%
        dplyr::left_join(
            recency_lookup,
            by = c("Year", "region", "round", "game_index", "teamA", "teamB")
        ) %>%
        dplyr::mutate(
            predicted_prob = predicted_prob,
            model_pick = dplyr::if_else(predicted_prob >= 0.5, teamA, teamB),
            model_correct = model_pick == winner,
            round = factor(round, levels = round_levels())
        ) %>%
        dplyr::arrange(round, region, game_index)

    overall_metrics <- compute_binary_metrics(scored_games$predicted_prob, scored_games$actual_outcome)
    main_bracket_games <- scored_games %>%
        dplyr::filter(round != "First Four")
    main_bracket_metrics <- if (nrow(main_bracket_games) > 0) {
        compute_binary_metrics(main_bracket_games$predicted_prob, main_bracket_games$actual_outcome)
    } else {
        tibble::tibble(
            log_loss = NA_real_,
            brier = NA_real_,
            accuracy = NA_real_
        )
    }
    round_summary <- scored_games %>%
        dplyr::group_by(round) %>%
        dplyr::summarise(
            games = dplyr::n(),
            accuracy = mean(model_correct),
            mean_predicted_prob = mean(predicted_prob),
            .groups = "drop"
        ) %>%
        dplyr::mutate(round = as.character(round))

    interpretive_status <- dplyr::case_when(
        nrow(main_bracket_games) == 0 ~ "early read",
        nrow(main_bracket_games) < 8L ~ "main-bracket underway",
        nrow(main_bracket_games) < 24L ~ "meaningful live sample still limited",
        TRUE ~ "meaningful live sample"
    )
    coverage_text <- if (any(scored_games$round == "First Four") && nrow(main_bracket_games) == 0) {
        "Only First Four games have completed so far."
    } else if (any(scored_games$round == "First Four") && nrow(main_bracket_games) > 0) {
        "Live sample includes First Four and main-bracket games."
    } else {
        "Live sample reflects main-bracket games."
    }

    status <- sprintf(
        "Live tournament performance through %s completed games across %s. %s Monitoring only: current-year outcomes are not fed back into the bracket model.",
        nrow(scored_games),
        paste(unique(as.character(scored_games$round)), collapse = ", "),
        coverage_text
    )

    recent_games_have_timestamps <- any(!is.na(scored_games$completed_at) & nzchar(scored_games$completed_at))
    recent_games_title <- if (recent_games_have_timestamps) {
        "Recent Games"
    } else {
        "Latest Available Games"
    }
    recent_games_note <- if (recent_games_have_timestamps) {
        "Ordered by recorded completion time."
    } else {
        "Ordered by the best available bracket progression metadata because completion timestamps were not supplied."
    }
    recent_games_tbl <- if (recent_games_have_timestamps) {
        scored_games %>%
            dplyr::mutate(completed_at_sort = suppressWarnings(as.POSIXct(completed_at, tz = "UTC"))) %>%
            dplyr::arrange(dplyr::desc(completed_at_sort), dplyr::desc(game_index), region)
    } else {
        scored_games %>%
            dplyr::arrange(dplyr::desc(round), dplyr::desc(game_index), region)
    }

    list(
        status = status,
        monitoring_note = "Monitoring only: current-year outcomes are for evaluation and commentary. They do not retrain the current tournament model or alter pre-tournament matchup features.",
        interpretive_status = interpretive_status,
        summary = tibble::tibble(
            games_played = nrow(scored_games),
            log_loss = overall_metrics$log_loss[[1]],
            brier = overall_metrics$brier[[1]],
            accuracy = overall_metrics$accuracy[[1]]
        ),
        main_bracket_summary = tibble::tibble(
            games_played = nrow(main_bracket_games),
            log_loss = main_bracket_metrics$log_loss[[1]],
            brier = main_bracket_metrics$brier[[1]],
            accuracy = main_bracket_metrics$accuracy[[1]]
        ),
        round_summary = round_summary,
        games = recent_games_tbl %>%
            dplyr::mutate(
                actual_winner = winner,
                model_pick = model_pick,
                model_pick_note = dplyr::if_else(model_correct, "Correct", "Miss")
            ) %>%
            dplyr::select(
                Year, region, round, game_index, teamA, teamB, actual_winner, model_pick,
                predicted_prob, model_pick_note, actual_outcome, completed_at
            ) %>%
            dplyr::slice_head(n = 8L),
        recent_games_title = recent_games_title,
        recent_games_note = recent_games_note,
        games_played = nrow(scored_games),
        main_bracket_games_played = nrow(main_bracket_games),
        rounds_played = unique(as.character(scored_games$round))
    )
}
