library(dplyr)
library(ggplot2)
library(logger)
library(stringr)
library(tibble)

#' Stop with a simplified message
#'
#' @param message Error text to raise.
#'
#' @return This function does not return; it raises an error.
#' @keywords internal
stop_with_message <- function(message) {
    stop(message, call. = FALSE)
}

#' Provide a default for `NULL`
#'
#' @param x A value to test.
#' @param y A fallback used when `x` is `NULL`.
#'
#' @return `x` when non-`NULL`, otherwise `y`.
#' @keywords internal
`%||%` <- function(x, y) {
    if (is.null(x)) y else x
}

#' Safely coerce values to numeric
#'
#' @param x A vector to coerce.
#' @param default Fallback value for invalid or missing entries.
#'
#' @return A numeric vector.
#' @keywords internal
safe_numeric <- function(x, default = 0) {
    value <- suppressWarnings(as.numeric(x))
    ifelse(is.na(value) | !is.finite(value), default, value)
}

#' Build a raw normalization key for team names
#'
#' @param x A character vector of team names.
#'
#' @return A normalized character vector used for alias lookups.
#' @keywords internal
raw_team_name_key <- function(x) {
    x %>%
        as.character() %>%
        stringr::str_replace_all("[\u2018\u2019]", "'") %>%
        stringr::str_squish() %>%
        tolower() %>%
        gsub("&", "and", ., fixed = TRUE) %>%
        gsub("[^a-z0-9]", "", .)
}

#' Return canonical team-name aliases used across data sources
#'
#' @return A named character vector mapping normalized aliases to canonical team
#'   display names.
#' @keywords internal
team_name_aliases <- function() {
    c(
        "alabamast" = "Alabama State",
        "appalachianst" = "Appalachian State",
        "arizonast" = "Arizona State",
        "boisest" = "Boise State",
        "calstfullerton" = "Cal State Fullerton",
        "clevelandst" = "Cleveland State",
        "collegeofcharleston" = "Charleston",
        "coloradost" = "Colorado State",
        "fdu" = "Fairleigh Dickinson",
        "floridast" = "Florida State",
        "grambling" = "Grambling State",
        "georgiast" = "Georgia State",
        "gramblingst" = "Grambling State",
        "iowast" = "Iowa State",
        "jacksonvillest" = "Jacksonville State",
        "kansasst" = "Kansas State",
        "kennesawst" = "Kennesaw State",
        "kentst" = "Kent State",
        "longbeachst" = "Long Beach State",
        "loyolail" = "Loyola Chicago",
        "mcneesest" = "McNeese State",
        "miamifl" = "Miami (FL)",
        "michiganst" = "Michigan State",
        "mississippi" = "Mississippi",
        "mississippist" = "Mississippi State",
        "montanast" = "Montana State",
        "moreheadst" = "Morehead State",
        "murrayst" = "Murray State",
        "newmexicost" = "New Mexico State",
        "ncstate" = "NC State",
        "ncst" = "NC State",
        "ncstatewolfpack" = "NC State",
        "northdakotast" = "North Dakota State",
        "northcarolina" = "North Carolina",
        "northcarolinastate" = "NC State",
        "norfolkst" = "Norfolk State",
        "ohiost" = "Ohio State",
        "olemiss" = "Mississippi",
        "oklahomast" = "Oklahoma State",
        "oregonst" = "Oregon State",
        "pennst" = "Penn State",
        "pitt" = "Pittsburgh",
        "prairieview" = "Prairie View A&M",
        "prairieviewaandm" = "Prairie View A&M",
        "saintfrancispa" = "Saint Francis",
        "saintjohns" = "Saint John's",
        "saintpeters" = "Saint Peter's",
        "sandiegost" = "San Diego State",
        "southdakotast" = "South Dakota State",
        "southeastmissourist" = "Southeast Missouri State",
        "stfrancis" = "Saint Francis",
        "stfrancispa" = "Saint Francis",
        "stjohns" = "Saint John's",
        "stjohnsny" = "Saint John's",
        "stpeters" = "Saint Peter's",
        "texasaandmcorpuschris" = "Texas A&M Corpus Christi",
        "texasaandmcorpuschristi" = "Texas A&M Corpus Christi",
        "uconn" = "Connecticut",
        "ucsandiego" = "UC San Diego",
        "ucsb" = "UC Santa Barbara",
        "unc" = "North Carolina",
        "utahst" = "Utah State",
        "washingtonst" = "Washington State",
        "wichitast" = "Wichita State",
        "wrightst" = "Wright State",
        "omaha" = "Nebraska Omaha"
    )
}

#' Canonicalize team names across scraped sources
#'
#' @param x A character vector of team names.
#'
#' @return A character vector of canonical team display names.
#' @keywords internal
canonicalize_team_name <- function(x) {
    aliases <- team_name_aliases()
    keys <- raw_team_name_key(x)
    canonical <- unname(aliases[keys])
    fallback <- x %>%
        as.character() %>%
        stringr::str_replace_all("[\u2018\u2019]", "'") %>%
        stringr::str_squish()
    dplyr::coalesce(canonical, fallback)
}

#' Normalize team names for joins
#'
#' @param x A character vector of team names.
#'
#' @return A normalized character vector suitable for key-based joins.
#' @keywords internal
normalize_team_key <- function(x) {
    raw_team_name_key(canonicalize_team_name(x))
}

#' Return the ordered set of tournament round labels
#'
#' @return A character vector of round labels used throughout the package.
#' @keywords internal
round_levels <- function() {
    c("First Four", "Round of 64", "Round of 32", "Sweet 16", "Elite 8", "Final Four", "Championship")
}

#' List allowed pre-tournament feature columns
#'
#' @return A character vector of season-available team feature names.
#' @keywords internal
pre_tournament_feature_columns <- function() {
    c("barthag_logit", "AdjOE", "AdjDE", "WAB", "TOR", "TORD", "ORB", "DRB", "3P%", "3P%D", "Adj T.")
}

#' List leakage-prone columns excluded from modeling
#'
#' @return A character vector of disallowed post-tournament columns.
#' @keywords internal
leakage_columns <- function() {
    c("R64", "R32", "S16", "E8", "F4", "F2", "Champ", "Clutch_Index", "Conf_Strength", "historical_performance")
}

#' List continuous matchup-difference columns
#'
#' @return A character vector of continuous matchup-difference predictor names.
#' @keywords internal
continuous_matchup_diff_columns <- function() {
    c(
        "seed_diff",
        "barthag_logit_diff",
        "AdjOE_diff",
        "AdjDE_diff",
        "WAB_diff",
        "TOR_diff",
        "TORD_diff",
        "ORB_diff",
        "DRB_diff",
        "3P%_diff",
        "3P%D_diff",
        "Adj T._diff"
    )
}

#' Return default bracket scoring weights by round
#'
#' @return A named numeric vector of round weights.
#' @keywords internal
default_round_weights <- function() {
    c(
        "First Four" = 0,
        "Round of 64" = 1,
        "Round of 32" = 2,
        "Sweet 16" = 4,
        "Elite 8" = 8,
        "Final Four" = 16,
        "Championship" = 32
    )
}

#' Return expected game counts by round for a completed tournament
#'
#' @return A named integer vector of expected completed-tournament round counts.
#' @keywords internal
expected_completed_round_counts <- function() {
    c(
        "First Four" = 4L,
        "Round of 64" = 32L,
        "Round of 32" = 16L,
        "Sweet 16" = 8L,
        "Elite 8" = 4L,
        "Final Four" = 2L,
        "Championship" = 1L
    )
}

#' Build a single matchup feature row
#'
#' @param team_a A one-row team feature data frame for team A.
#' @param team_b A one-row team feature data frame for team B.
#' @param round_name The round label for the matchup.
#' @param actual_outcome Optional observed outcome for team A.
#' @param metadata Optional metadata such as year, region, team names, and winner.
#'
#' @return A one-row matchup feature tibble.
#' @keywords internal
build_matchup_feature_row <- function(team_a, team_b, round_name, actual_outcome = NA_real_, metadata = list()) {
    if (nrow(team_a) != 1 || nrow(team_b) != 1) {
        stop_with_message("Matchup feature rows require exactly one row for each team")
    }

    available_features <- intersect(pre_tournament_feature_columns(), intersect(names(team_a), names(team_b)))
    diff_values <- purrr::map_dbl(available_features, function(feature_name) {
        safe_numeric(team_a[[feature_name]][1]) - safe_numeric(team_b[[feature_name]][1])
    })
    names(diff_values) <- paste0(available_features, "_diff")

    tibble::tibble(
        Year = as.character(metadata$Year %||% team_a$Year[1]),
        region = as.character(metadata$region %||% team_a$Region[1]),
        round = as.character(round_name),
        game_index = safe_numeric(metadata$game_index %||% NA_real_, default = NA_real_),
        teamA = as.character(metadata$teamA %||% team_a$Team[1]),
        teamB = as.character(metadata$teamB %||% team_b$Team[1]),
        winner = as.character(metadata$winner %||% if (isTRUE(actual_outcome == 1)) team_a$Team[1] else if (isTRUE(actual_outcome == 0)) team_b$Team[1] else NA_character_),
        actual_outcome = safe_numeric(actual_outcome, default = NA_real_),
        same_conf = as.integer(as.character(team_a$Conf[1]) == as.character(team_b$Conf[1])),
        seed_diff = safe_numeric(team_a$Seed[1]) - safe_numeric(team_b$Seed[1]),
        barthag_logit_diff = diff_values[["barthag_logit_diff"]] %||% 0,
        AdjOE_diff = diff_values[["AdjOE_diff"]] %||% 0,
        AdjDE_diff = diff_values[["AdjDE_diff"]] %||% 0,
        WAB_diff = diff_values[["WAB_diff"]] %||% 0,
        TOR_diff = diff_values[["TOR_diff"]] %||% 0,
        TORD_diff = diff_values[["TORD_diff"]] %||% 0,
        ORB_diff = diff_values[["ORB_diff"]] %||% 0,
        DRB_diff = diff_values[["DRB_diff"]] %||% 0,
        `3P%_diff` = diff_values[["3P%_diff"]] %||% 0,
        `3P%D_diff` = diff_values[["3P%D_diff"]] %||% 0,
        `Adj T._diff` = diff_values[["Adj T._diff"]] %||% 0
    )
}

#' Read a supported tabular data file
#'
#' @param path Path to an `.xlsx`, `.xls`, or `.csv` file.
#'
#' @return A data frame read from disk.
#' @keywords internal
read_table_file <- function(path) {
    if (!file.exists(path)) {
        stop_with_message(sprintf("Required data file not found: %s", path))
    }

    extension <- tolower(tools::file_ext(path))
    if (extension %in% c("xlsx", "xls")) {
        return(readxl::read_excel(path))
    }
    if (extension == "csv") {
        return(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE))
    }

    stop_with_message(sprintf("Unsupported data file extension for %s", path))
}

#' Add safe derived pre-tournament features
#'
#' @param data A team-level feature table.
#'
#' @return The input data with safe derived fields added when available.
#' @keywords internal
add_safe_pre_tournament_features <- function(data) {
    data <- dplyr::mutate(
        data,
        Year = as.character(Year),
        Seed = safe_numeric(Seed),
        Seed = as.integer(Seed)
    )

    if (!"barthag_logit" %in% names(data) && "Barthag" %in% names(data)) {
        data <- dplyr::mutate(
            data,
            barthag_logit = log((pmin(pmax(Barthag, 1e-9), 1 - 1e-9)) / (1 - pmin(pmax(Barthag, 1e-9), 1 - 1e-9)))
        )
    }

    data
}

#' Validate required team-level input columns
#'
#' @param data A team-level data frame.
#' @param metrics_to_use Optional required feature columns.
#'
#' @return `TRUE` if validation passes.
#' @export
validate_input_data <- function(data, metrics_to_use = NULL) {
    if (!is.data.frame(data)) {
        stop_with_message("Input must be a data frame")
    }

    required_cols <- c("Year", "Team", "Seed", "Region", "Conf")
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

#' Impute missing numeric columns
#'
#' @param data A data frame to impute.
#' @param reference Optional reference data used when the current column has no
#'   finite median.
#'
#' @return The input data with missing numeric values imputed.
#' @keywords internal
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

#' Compute a simple composite team strength
#'
#' @param team A one-row team feature data frame.
#' @param metrics Optional feature names to use.
#'
#' @return A numeric scalar summarizing team strength.
#' @export
compute_team_strength <- function(team, metrics = NULL) {
    if (!is.data.frame(team) || nrow(team) == 0) {
        stop_with_message("Team data must contain at least one row")
    }

    metrics <- metrics %||% pre_tournament_feature_columns()
    available <- metrics[metrics %in% names(team)]
    if (length(available) == 0) {
        return(0.5)
    }

    values <- vapply(available, function(metric) safe_numeric(team[[metric]][1]), numeric(1))
    if ("AdjDE" %in% available) {
        values[available == "AdjDE"] <- -values[available == "AdjDE"]
    }

    mean(values, na.rm = TRUE)
}

#' Return the standard NCAA bracket seed order
#'
#' @return An integer vector describing the within-region seed order.
#' @keywords internal
standard_bracket_order <- function() {
    c(1, 16, 8, 9, 5, 12, 4, 13, 6, 11, 3, 14, 7, 10, 2, 15)
}

#' Flatten simulated matchup results into one table
#'
#' @param simulation_results A full bracket simulation result bundle.
#'
#' @return A data frame containing all regional and national matchup results.
#' @export
flatten_matchup_results <- function(simulation_results) {
    region_rows <- purrr::imap_dfr(simulation_results$region_results, function(region_result, region_name) {
        purrr::imap_dfr(region_result$results, function(round_result, round_name) {
            if (nrow(round_result) == 0) {
                return(tibble::tibble())
            }
            dplyr::mutate(round_result, region = region_name, round = round_name)
        })
    })

    final_rows <- dplyr::bind_rows(
        dplyr::mutate(simulation_results$final_four$semifinals[[1]], region = "National", round = "Final Four"),
        dplyr::mutate(simulation_results$final_four$semifinals[[2]], region = "National", round = "Final Four"),
        dplyr::mutate(simulation_results$final_four$championship, region = "National", round = "Championship")
    )

    dplyr::bind_rows(region_rows, final_rows)
}

#' Save simulation outputs to disk
#'
#' @param results A result bundle returned by the main pipeline.
#' @param output_config Output configuration values.
#'
#' @return A list of written output file paths.
#' @export
save_results <- function(results, output_config) {
    output_dir <- output_config$path %||% "output"
    prefix <- output_config$prefix %||% "tournament_sim"
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

    rds_path <- file.path(output_dir, paste0(prefix, ".rds"))
    model_summary_path <- file.path(output_dir, paste0(prefix, "_model_summary.txt"))
    backtest_summary_path <- file.path(output_dir, paste0(prefix, "_backtest_summary.txt"))
    viz_path <- file.path(output_dir, paste0(prefix, "_bracket.png"))

    saveRDS(results, rds_path)

    sink(model_summary_path)
    print(summary(results$model$model))
    sink()

    if (!is.null(results$backtest)) {
        sink(backtest_summary_path)
        print(results$backtest$summary)
        sink()
    }

    ggplot2::ggsave(
        filename = viz_path,
        plot = results$visualization,
        width = 14,
        height = 10,
        dpi = 300
    )

    list(
        results = rds_path,
        model_summary = model_summary_path,
        backtest_summary = if (!is.null(results$backtest)) backtest_summary_path else NULL,
        bracket_plot = viz_path
    )
}

#' Score a predicted bracket against actual results
#'
#' @param predicted_matchups A flattened predicted matchup table.
#' @param actual_results A table of actual tournament results.
#' @param round_weights Optional round scoring weights.
#'
#' @return A list containing game-level comparison rows and a one-row summary.
#' @export
score_bracket_against_results <- function(predicted_matchups, actual_results, round_weights = default_round_weights()) {
    actual_lookup <- actual_results %>%
        dplyr::select(region, round, matchup_number = game_index, actual_winner = winner)

    comparison <- predicted_matchups %>%
        dplyr::select(region, round, matchup_number, predicted_winner = winner) %>%
        dplyr::left_join(actual_lookup, by = c("region", "round", "matchup_number")) %>%
        dplyr::mutate(
            round_weight = unname(round_weights[round]),
            round_weight = dplyr::coalesce(round_weight, 0),
            correct = predicted_winner == actual_winner,
            score = ifelse(correct, round_weight, 0)
        )

    list(
        comparison = comparison,
        summary = tibble::tibble(
            correct_picks = sum(comparison$correct, na.rm = TRUE),
            total_games = sum(!is.na(comparison$actual_winner)),
            bracket_score = sum(comparison$score, na.rm = TRUE)
        )
    )
}

#' Summarize calibration bins for predictions
#'
#' @param predictions A data frame containing `predicted_prob` and `actual_outcome`.
#' @param bins Number of probability bins to use.
#'
#' @return A tibble of calibration summaries by bin.
#' @export
summarize_calibration <- function(predictions, bins = 10L) {
    if (nrow(predictions) == 0) {
        return(tibble::tibble())
    }

    clipped <- pmin(pmax(predictions$predicted_prob, 1e-6), 1 - 1e-6)
    breaks <- seq(0, 1, length.out = bins + 1L)

    tibble::tibble(
        predicted_prob = clipped,
        actual_outcome = predictions$actual_outcome
    ) %>%
        dplyr::mutate(
            bin = cut(predicted_prob, breaks = breaks, include.lowest = TRUE, right = TRUE)
        ) %>%
        dplyr::group_by(bin) %>%
        dplyr::summarise(
            mean_predicted = mean(predicted_prob),
            empirical_rate = mean(actual_outcome),
            n_games = dplyr::n(),
            .groups = "drop"
        )
}

#' Compute binary classification metrics
#'
#' @param predicted_prob Predicted probabilities for the positive class.
#' @param actual_outcome Observed binary outcomes.
#'
#' @return A one-row tibble containing log loss, Brier score, and accuracy.
#' @export
compute_binary_metrics <- function(predicted_prob, actual_outcome) {
    clipped <- pmin(pmax(predicted_prob, 1e-6), 1 - 1e-6)
    actual_numeric <- safe_numeric(actual_outcome)

    tibble::tibble(
        log_loss = -mean(actual_numeric * log(clipped) + (1 - actual_numeric) * log(1 - clipped)),
        brier = mean((clipped - actual_numeric)^2),
        accuracy = mean((clipped >= 0.5) == (actual_numeric >= 0.5))
    )
}
