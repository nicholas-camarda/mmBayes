library(dplyr)
library(ggplot2)
library(logger)
library(tibble)

stop_with_message <- function(message) {
    stop(message, call. = FALSE)
}

#' Validate input data for model fitting and simulation
#' @export
validate_input_data <- function(data, metrics_to_use = NULL) {
    if (!is.data.frame(data)) {
        stop_with_message("Input must be a data frame")
    }

    required_cols <- c("Team", "Seed", "Region", "Conf")
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
        stop_with_message(
            sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", "))
        )
    }

    if (!is.null(metrics_to_use)) {
        missing_metrics <- setdiff(metrics_to_use, names(data))
        if (length(missing_metrics) > 0) {
            stop_with_message(
                sprintf("Missing required metrics: %s", paste(missing_metrics, collapse = ", "))
            )
        }
    }

    if (any(is.na(data$Seed)) || any(data$Seed < 1 | data$Seed > 16)) {
        stop_with_message("Invalid seed values detected")
    }

    TRUE
}

safe_numeric <- function(x, default = 0) {
    value <- suppressWarnings(as.numeric(x))
    ifelse(is.na(value) | !is.finite(value), default, value)
}

impute_numeric_columns <- function(data, reference = NULL) {
    numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]

    for (col in numeric_cols) {
        if (!anyNA(data[[col]])) {
            next
        }

        fill_value <- stats::median(data[[col]], na.rm = TRUE)
        if (!is.finite(fill_value) && !is.null(reference) && col %in% names(reference)) {
            fill_value <- stats::median(reference[[col]], na.rm = TRUE)
        }
        if (!is.finite(fill_value)) {
            fill_value <- 0
        }

        data[[col]][is.na(data[[col]])] <- fill_value
    }

    data
}

`%||%` <- function(x, y) {
    if (is.null(x)) y else x
}

#' Compute team strength using multiple metrics
#' @export
compute_team_strength <- function(team, metrics = NULL) {
    if (!is.data.frame(team) || nrow(team) == 0) {
        stop_with_message("Team data must contain at least one row")
    }

    if (is.null(metrics)) {
        metrics <- c(
            "overall_strength", "barthag_logit", "AdjOE", "AdjDE",
            "Clutch_Index", "Conf_Strength", "Upset_Factor", "Turnover_Edge"
        )
    }

    available <- metrics[metrics %in% names(team)]
    if (!"Seed" %in% available && "Seed" %in% names(team)) {
        available <- c(available, "Seed")
    }

    if (length(available) == 0) {
        return(0.5)
    }

    values <- vapply(available, function(metric) safe_numeric(team[[metric]][1]), numeric(1))
    if ("Seed" %in% available) {
        seed_idx <- which(available == "Seed")
        values[seed_idx] <- (17 - values[seed_idx]) / 16
    }

    mean(values, na.rm = TRUE)
}

#' Fix play-in seed issues when a region contains duplicate seeds
#' @export
fix_region_seeds <- function(region_teams) {
    region_teams <- as.data.frame(region_teams)
    region_teams$Assigned_Seed <- region_teams$Seed

    expected <- 1:16
    missing <- setdiff(expected, unique(region_teams$Seed))
    dup_seeds <- as.numeric(names(which(table(region_teams$Seed) > 1)))

    if (length(missing) == 0 && length(dup_seeds) == 0) {
        return(region_teams)
    }

    if (length(missing) == 1 && length(dup_seeds) == 1) {
        dup_indices <- which(region_teams$Seed == dup_seeds[1])
        strengths <- vapply(
            dup_indices,
            function(idx) compute_team_strength(region_teams[idx, , drop = FALSE]),
            numeric(1)
        )
        loser_idx <- dup_indices[which.min(strengths)]
        region_teams$Assigned_Seed[loser_idx] <- missing[1]
        return(region_teams)
    }

    stop_with_message("Region contains unsupported seed structure; expected a standard 16-team bracket")
}

standard_bracket_order <- function() {
    c(1, 16, 8, 9, 5, 12, 4, 13, 6, 11, 3, 14, 7, 10, 2, 15)
}

#' Flatten matchup results into a single data frame
#' @export
flatten_matchup_results <- function(simulation_results) {
    region_rows <- purrr::imap_dfr(simulation_results$region_results, function(region_result, region_name) {
        purrr::imap_dfr(region_result$results, function(round_result, round_name) {
            dplyr::mutate(round_result, region = region_name, round = round_name)
        })
    })

    final_rows <- dplyr::bind_rows(
        dplyr::mutate(simulation_results$final_four$semifinals[[1]], region = "Final Four", round = "Final Four"),
        dplyr::mutate(simulation_results$final_four$semifinals[[2]], region = "Final Four", round = "Final Four"),
        dplyr::mutate(simulation_results$final_four$championship, region = "Final Four", round = "Championship")
    )

    dplyr::bind_rows(region_rows, final_rows)
}

#' Save simulation results to disk
#' @export
save_results <- function(results, output_config) {
    output_dir <- output_config$path %||% "output"
    prefix <- output_config$prefix %||% "tournament_sim"
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

    rds_path <- file.path(output_dir, paste0(prefix, ".rds"))
    summary_path <- file.path(output_dir, paste0(prefix, "_model_summary.txt"))
    viz_path <- file.path(output_dir, paste0(prefix, "_bracket.png"))

    saveRDS(results, rds_path)

    sink(summary_path)
    print(summary(results$model$model))
    sink()

    ggplot2::ggsave(
        filename = viz_path,
        plot = results$visualization,
        width = 14,
        height = 10,
        dpi = 300
    )

    list(
        results = rds_path,
        model_summary = summary_path,
        bracket_plot = viz_path
    )
}
