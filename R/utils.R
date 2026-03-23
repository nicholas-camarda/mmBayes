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

#' Load environment variables from a `.env`-style file
#'
#' This helper supports simple `KEY=VALUE` lines, optionally quoted values, and
#' ignores blank lines or comments beginning with `#`. It is designed to avoid
#' introducing new dependencies while keeping secrets out of version control.
#'
#' @param path Path to the `.env` file.
#' @param override Whether to override existing environment variables.
#'
#' @return Invisibly returns a character vector of environment keys loaded.
#' @export
load_dotenv_file <- function(path = ".env", override = FALSE) {
    if (!file.exists(path)) {
        return(invisible(character()))
    }

    lines <- readLines(path, warn = FALSE)
    loaded <- character()

    for (line in lines) {
        line <- stringr::str_trim(line %||% "")
        if (!nzchar(line) || startsWith(line, "#")) {
            next
        }

        if (!grepl("=", line, fixed = TRUE)) {
            next
        }

        parts <- strsplit(line, "=", fixed = TRUE)[[1]]
        key <- stringr::str_trim(parts[[1]] %||% "")
        value <- paste(parts[-1], collapse = "=")
        value <- stringr::str_trim(value)
        if (!nzchar(key)) {
            next
        }

        if ((startsWith(value, "\"") && endsWith(value, "\"")) ||
            (startsWith(value, "'") && endsWith(value, "'"))) {
            value <- substr(value, 2, nchar(value) - 1)
        }

        if (!isTRUE(override) && nzchar(Sys.getenv(key, unset = ""))) {
            next
        }

        do.call(Sys.setenv, stats::setNames(list(value), key))
        loaded <- c(loaded, key)
    }

    invisible(unique(loaded))
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
        "liubrooklyn" = "LIU",
        "longislanduniversitybrooklyn" = "LIU",
        "loyolail" = "Loyola Chicago",
        "louisianalafayette" = "Louisiana",
        "mcneese" = "McNeese State",
        "mcneesest" = "McNeese State",
        "miamifl" = "Miami (FL)",
        "miamioh" = "Miami OH",
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
        "northcarolinast" = "NC State",
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

#' Return the bracket fill order for regions
#'
#' @return A character vector of region labels in ESPN bracket order.
#' @keywords internal
bracket_region_levels <- function() {
    c("East", "South", "Midwest", "West")
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
#' @param year Optional tournament year. When supplied, known exceptions such as
#'   the 2021 COVID no-contest are reflected in the expected counts.
#'
#' @return A named integer vector of expected completed-tournament round counts.
#' @keywords internal
expected_completed_round_counts <- function(year = NULL) {
    counts <- c(
        "First Four" = 4L,
        "Round of 64" = 32L,
        "Round of 32" = 16L,
        "Sweet 16" = 8L,
        "Elite 8" = 4L,
        "Final Four" = 2L,
        "Championship" = 1L
    )

    year_value <- suppressWarnings(as.integer(year))
    if (length(year_value) == 1L && !is.na(year_value) && year_value == 2021L) {
        counts["Round of 64"] <- 31L
    }

    counts
}

#' Return an empty tournament game-results table
#'
#' @return A zero-row tibble with the canonical tournament game-results schema.
#' @keywords internal
empty_game_results_table <- function() {
    tibble::tibble(
        Year = character(),
        region = character(),
        round = character(),
        game_index = integer(),
        teamA = character(),
        teamB = character(),
        teamA_seed = integer(),
        teamB_seed = integer(),
        teamA_score = integer(),
        teamB_score = integer(),
        total_points = integer(),
        winner = character()
    )
}

#' Summarize current-year First Four resolution status
#'
#' @param current_teams A current-year team feature table.
#' @param actual_play_in_results A normalized current-year First Four results
#'   table.
#'
#' @return A one-row tibble describing expected, resolved, and unresolved
#'   play-in slots.
#' @keywords internal
summarize_play_in_resolution <- function(current_teams, actual_play_in_results = NULL) {
    duplicate_slots <- current_teams %>%
        dplyr::count(Region, Seed, name = "n") %>%
        dplyr::filter(n > 1L) %>%
        dplyr::transmute(play_in_region = Region, slot_seed = Seed)

    actual_play_in_results <- actual_play_in_results %||%
        tibble::tibble(play_in_region = character(), slot_seed = integer())

    resolved_slots <- actual_play_in_results %>%
        dplyr::distinct(play_in_region, slot_seed)

    expected_slots <- nrow(duplicate_slots)
    resolved_count <- duplicate_slots %>%
        dplyr::inner_join(resolved_slots, by = c("play_in_region", "slot_seed")) %>%
        nrow()

    tibble::tibble(
        expected_slots = expected_slots,
        resolved_slots = resolved_count,
        unresolved_slots = max(expected_slots - resolved_count, 0L),
        has_unresolved_slots = max(expected_slots - resolved_count, 0L) > 0L
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

    conf_a <- stringr::str_squish(dplyr::coalesce(as.character(team_a$Conf[1]), ""))
    conf_b <- stringr::str_squish(dplyr::coalesce(as.character(team_b$Conf[1]), ""))
    same_conf_value <- if (nzchar(conf_a) && nzchar(conf_b) && conf_a == conf_b) 1L else 0L

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
        same_conf = same_conf_value,
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

#' Build a single total-points feature row
#'
#' @param team_a A one-row team feature data frame for team A.
#' @param team_b A one-row team feature data frame for team B.
#' @param round_name The round label for the matchup.
#' @param total_points Optional observed combined score for the matchup.
#' @param metadata Optional metadata such as year, region, game index, and team
#'   names.
#'
#' @return A one-row matchup feature tibble for total-points modeling.
#' @keywords internal
build_total_points_feature_row <- function(team_a, team_b, round_name, total_points = NA_real_, metadata = list()) {
    if (nrow(team_a) != 1 || nrow(team_b) != 1) {
        stop_with_message("Total-points feature rows require exactly one row for each team")
    }

    conf_a <- stringr::str_squish(dplyr::coalesce(as.character(team_a$Conf[1]), ""))
    conf_b <- stringr::str_squish(dplyr::coalesce(as.character(team_b$Conf[1]), ""))
    same_conf_value <- if (nzchar(conf_a) && nzchar(conf_b) && conf_a == conf_b) 1L else 0L

    sum_feature <- function(feature_name) {
        safe_numeric(team_a[[feature_name]][1]) + safe_numeric(team_b[[feature_name]][1])
    }
    mean_feature <- function(feature_name) {
        mean(c(safe_numeric(team_a[[feature_name]][1]), safe_numeric(team_b[[feature_name]][1])), na.rm = TRUE)
    }
    gap_feature <- function(feature_name) {
        abs(safe_numeric(team_a[[feature_name]][1]) - safe_numeric(team_b[[feature_name]][1]))
    }

    tibble::tibble(
        Year = as.character(metadata$Year %||% team_a$Year[1]),
        region = as.character(metadata$region %||% team_a$Region[1]),
        round = as.character(round_name),
        game_index = safe_numeric(metadata$game_index %||% NA_real_, default = NA_real_),
        teamA = as.character(metadata$teamA %||% team_a$Team[1]),
        teamB = as.character(metadata$teamB %||% team_b$Team[1]),
        same_conf = same_conf_value,
        seed_sum = safe_numeric(team_a$Seed[1]) + safe_numeric(team_b$Seed[1]),
        seed_gap = abs(safe_numeric(team_a$Seed[1]) - safe_numeric(team_b$Seed[1])),
        barthag_logit_sum = sum_feature("barthag_logit"),
        barthag_logit_gap = gap_feature("barthag_logit"),
        AdjOE_sum = sum_feature("AdjOE"),
        AdjDE_sum = sum_feature("AdjDE"),
        WAB_sum = sum_feature("WAB"),
        TOR_sum = sum_feature("TOR"),
        TORD_sum = sum_feature("TORD"),
        ORB_sum = sum_feature("ORB"),
        DRB_sum = sum_feature("DRB"),
        `3P%_sum` = sum_feature("3P%"),
        `3P%D_sum` = sum_feature("3P%D"),
        `Adj T._mean` = mean_feature("Adj T."),
        `Adj T._gap` = gap_feature("Adj T."),
        total_points = safe_numeric(total_points, default = NA_real_)
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

#' Compute the implied log probability of a flattened bracket
#'
#' @param flattened_matchups A flattened matchup table with `win_prob_A`.
#'
#' @return A one-row tibble with bracket-level log-probability summaries.
#' @keywords internal
summarize_bracket_probability <- function(flattened_matchups) {
    chosen_prob <- ifelse(
        flattened_matchups$winner == flattened_matchups$teamA,
        flattened_matchups$win_prob_A,
        1 - flattened_matchups$win_prob_A
    )
    chosen_prob <- pmin(pmax(chosen_prob, 1e-6), 1 - 1e-6)

    tibble::tibble(
        bracket_log_prob = sum(log(chosen_prob), na.rm = TRUE),
        mean_game_prob = mean(chosen_prob, na.rm = TRUE)
    )
}

#' Classify the confidence tier for a matchup
#'
#' @param favorite_prob Posterior win probability for the favored side.
#' @param ci_lower Lower credible bound for the favored side.
#' @param ci_upper Upper credible bound for the favored side.
#'
#' @return A character scalar describing the confidence tier.
#' @keywords internal
classify_confidence_tier <- function(favorite_prob, ci_lower, ci_upper) {
    interval_width <- safe_numeric(ci_upper) - safe_numeric(ci_lower)

    if (interval_width > 0.35) {
        return("Volatile")
    }
    if (safe_numeric(favorite_prob) >= 0.8 && safe_numeric(ci_lower) >= 0.6) {
        return("Lock")
    }
    if (safe_numeric(favorite_prob) >= 0.65 && interval_width <= 0.3) {
        return("Lean")
    }

    "Toss-up"
}

#' Build a short decision rationale for a matchup
#'
#' @param confidence_tier Confidence tier returned by
#'   [classify_confidence_tier()].
#' @param round_weight Standard scoring weight for the round.
#' @param favorite_prob Posterior win probability for the favored side.
#' @param underdog_prob Posterior win probability for the underdog.
#' @param interval_width Width of the favored-side credible interval.
#'
#' @return A short sentence suitable for dashboards and decision sheets.
#' @keywords internal
build_decision_rationale <- function(confidence_tier, round_weight, favorite_prob, underdog_prob, interval_width) {
    if (identical(confidence_tier, "Lock")) {
        return("Take the favorite. The posterior edge is strong and stable.")
    }
    if (safe_numeric(round_weight) >= 8 && safe_numeric(underdog_prob) >= 0.35) {
        return("Late-round leverage spot. The underdog path is still live.")
    }
    if (identical(confidence_tier, "Volatile")) {
        return("Wide posterior interval. This matchup is unstable enough to swing a bracket.")
    }
    if (safe_numeric(favorite_prob) < 0.6) {
        return("Near coin flip. Either side is defensible in pool play.")
    }
    if (safe_numeric(interval_width) > 0.25) {
        return("The favorite has the edge, but uncertainty is still meaningful.")
    }

    "Lean favorite. The safer side is clear, but it is not a lock."
}

#' Add decision-oriented metadata to matchup predictions
#'
#' @param matchups A flattened matchup table.
#' @param round_weights Optional named round-weight vector.
#'
#' @return The matchup table with favorite, uncertainty, and leverage columns.
#' @export
augment_matchup_decisions <- function(matchups, round_weights = default_round_weights()) {
    if (nrow(matchups) == 0) {
        return(tibble::tibble())
    }

    favorite_prob <- pmax(matchups$win_prob_A, 1 - matchups$win_prob_A)
    underdog_prob <- 1 - favorite_prob
    favorite_ci_lower <- ifelse(matchups$win_prob_A >= 0.5, matchups$ci_lower, 1 - matchups$ci_upper)
    favorite_ci_upper <- ifelse(matchups$win_prob_A >= 0.5, matchups$ci_upper, 1 - matchups$ci_lower)
    interval_width <- pmax(0, favorite_ci_upper - favorite_ci_lower)
    confidence_tier <- vapply(
        seq_len(nrow(matchups)),
        function(index) {
            classify_confidence_tier(
                favorite_prob = favorite_prob[[index]],
                ci_lower = favorite_ci_lower[[index]],
                ci_upper = favorite_ci_upper[[index]]
            )
        },
        character(1)
    )

    augmented <- matchups %>%
        dplyr::mutate(
            slot_key = sprintf("%s|%s|%s", region, round, matchup_number),
            round = factor(round, levels = round_levels()),
            round_weight = unname(round_weights[as.character(round)]),
            round_weight = dplyr::coalesce(round_weight, 0),
            posterior_favorite = ifelse(win_prob_A >= 0.5, teamA, teamB),
            favorite_seed = ifelse(win_prob_A >= 0.5, teamA_seed, teamB_seed),
            underdog = ifelse(win_prob_A >= 0.5, teamB, teamA),
            underdog_seed = ifelse(win_prob_A >= 0.5, teamB_seed, teamA_seed),
            win_prob_favorite = favorite_prob,
            win_prob_underdog = underdog_prob,
            favorite_ci_lower = favorite_ci_lower,
            favorite_ci_upper = favorite_ci_upper,
            interval_width = interval_width,
            posterior_edge = win_prob_favorite - 0.5,
            confidence_tier = confidence_tier,
            decision_score = round_weight * (win_prob_underdog + interval_width),
            upset_leverage = round_weight * win_prob_underdog * (1 + interval_width)
        ) %>%
        dplyr::mutate(
            inspection_level = dplyr::case_when(
                confidence_tier == "Toss-up" ~ "primary",
                confidence_tier == "Volatile" ~ "secondary",
                TRUE ~ "none"
            ),
            inspection_flag = inspection_level != "none",
            rationale_short = purrr::pmap_chr(
                list(confidence_tier, round_weight, win_prob_favorite, win_prob_underdog, interval_width),
                build_decision_rationale
            )
        )

    ranking <- augmented %>%
        dplyr::arrange(dplyr::desc(decision_score), dplyr::desc(interval_width), round, region, matchup_number) %>%
        dplyr::transmute(slot_key, decision_rank = dplyr::row_number())

    augmented %>%
        dplyr::left_join(ranking, by = "slot_key") %>%
        dplyr::mutate(
            region = factor(region, levels = bracket_region_levels()),
            round = factor(round, levels = round_levels())
        ) %>%
        dplyr::arrange(region, round, matchup_number)
}

#' Build a stable key for a full bracket path
#'
#' @param flattened_matchups A flattened matchup table.
#'
#' @return A character scalar uniquely identifying the bracket winners by game.
#' @keywords internal
build_bracket_key <- function(flattened_matchups) {
    flattened_matchups %>%
        dplyr::arrange(factor(round, levels = round_levels()), region, matchup_number) %>%
        dplyr::transmute(key = sprintf("%s|%s|%s|%s", round, region, matchup_number, winner)) %>%
        dplyr::pull(key) %>%
        paste(collapse = " || ")
}

#' Compare two candidate brackets slot by slot
#'
#' @param base_matchups The baseline candidate matchup table.
#' @param alt_matchups The alternate candidate matchup table.
#'
#' @return A joined comparison table keyed by bracket slot.
#' @keywords internal
compare_candidate_matchups <- function(base_matchups, alt_matchups) {
    base_prepped <- if ("slot_key" %in% names(base_matchups)) base_matchups else augment_matchup_decisions(base_matchups)
    alt_prepped <- if ("slot_key" %in% names(alt_matchups)) alt_matchups else augment_matchup_decisions(alt_matchups)

    base_prepped %>%
        dplyr::transmute(
            slot_key,
            region,
            round,
            matchup_number,
            candidate_1_matchup = sprintf("%s vs %s", teamA, teamB),
            candidate_1_pick = winner,
            candidate_1_upset = upset,
            confidence_tier,
            upset_leverage,
            decision_score
        ) %>%
        dplyr::left_join(
            alt_prepped %>%
                dplyr::transmute(
                    slot_key,
                    candidate_2_matchup = sprintf("%s vs %s", teamA, teamB),
                    candidate_2_pick = winner,
                    candidate_2_upset = upset
                ),
            by = "slot_key"
        ) %>%
        dplyr::mutate(
            candidate_2_matchup = dplyr::coalesce(candidate_2_matchup, candidate_1_matchup),
            candidate_2_pick = dplyr::coalesce(candidate_2_pick, candidate_1_pick),
            candidate_2_upset = dplyr::coalesce(candidate_2_upset, candidate_1_upset),
            candidate_diff_flag = candidate_1_pick != candidate_2_pick |
                candidate_1_matchup != candidate_2_matchup
        )
}

#' Build a short rationale for a candidate flip
#'
#' @param comparison_row A one-row comparison tibble from
#'   [compare_candidate_matchups()].
#'
#' @return A short sentence explaining why the alternate pick is plausible.
#' @keywords internal
build_flip_rationale <- function(comparison_row) {
    if (!isTRUE(comparison_row$candidate_diff_flag[[1]])) {
        return("Matches the primary bracket.")
    }

    if (isTRUE(comparison_row$candidate_2_upset[[1]]) && comparison_row$confidence_tier[[1]] %in% c("Toss-up", "Volatile")) {
        return("Live underdog pivot in a volatile matchup.")
    }
    if (comparison_row$round[[1]] %in% c("Sweet 16", "Elite 8", "Final Four", "Championship")) {
        return("High-leverage late-round pivot.")
    }
    if (comparison_row$confidence_tier[[1]] == "Toss-up") {
        return("Near coin flip, so the alternate bracket takes the other side.")
    }

    "Controlled alternate path with a plausible downstream payoff."
}

#' Summarize path-level details for a bracket candidate
#'
#' @param matchups A candidate matchup table.
#'
#' @return A one-row tibble describing the candidate's title path.
#' @keywords internal
summarize_candidate_path <- function(matchups) {
    title_path <- matchups %>%
        dplyr::filter(round %in% c("Final Four", "Championship")) %>%
        dplyr::transmute(
            round = as.character(round),
            chosen_prob = ifelse(winner == teamA, win_prob_A, 1 - win_prob_A)
        )

    tibble::tibble(
        title_path_mean_prob = mean(title_path$chosen_prob, na.rm = TRUE),
        title_path_min_prob = min(title_path$chosen_prob, na.rm = TRUE)
    )
}

#' Build the bracket decision sheet used by the dashboard and CSV export
#'
#' @param candidates A list of bracket candidate result objects.
#' @param round_weights Optional named round-weight vector.
#'
#' @return A sorted decision-sheet tibble.
#' @export
build_decision_sheet <- function(candidates, round_weights = default_round_weights()) {
    if (length(candidates) == 0) {
        return(tibble::tibble())
    }

    primary <- if ("slot_key" %in% names(candidates[[1]]$matchups)) {
        candidates[[1]]$matchups
    } else {
        augment_matchup_decisions(candidates[[1]]$matchups, round_weights = round_weights)
    }
    alternate <- if (length(candidates) >= 2) {
        if ("slot_key" %in% names(candidates[[2]]$matchups)) {
            candidates[[2]]$matchups
        } else {
            augment_matchup_decisions(candidates[[2]]$matchups, round_weights = round_weights)
        }
    } else {
        primary
    }

    comparison <- compare_candidate_matchups(primary, alternate)
    comparison$alternate_rationale <- vapply(
        seq_len(nrow(comparison)),
        function(index) build_flip_rationale(comparison[index, , drop = FALSE]),
        character(1)
    )

    primary %>%
        dplyr::transmute(
            slot_key,
            region,
            round = factor(round, levels = round_levels()),
            matchup_number,
            matchup_label = sprintf("%s vs %s", teamA, teamB),
            teamA,
            teamB,
            teamA_seed,
            teamB_seed,
            posterior_favorite,
            favorite_seed,
            underdog,
            underdog_seed,
            win_prob_A,
            win_prob_favorite,
            win_prob_underdog,
            ci_lower = favorite_ci_lower,
            ci_upper = favorite_ci_upper,
            interval_width,
            prediction_sd,
            confidence_tier,
            inspection_flag,
            inspection_level,
            round_weight = unname(round_weights[as.character(round)]),
            decision_score,
            upset_leverage,
            decision_rank,
            rationale_short,
            candidate_1_pick = winner,
            candidate_1_upset = upset
        ) %>%
        dplyr::left_join(
            comparison %>%
                dplyr::select(
                    slot_key,
                    candidate_2_matchup,
                    candidate_2_pick,
                    candidate_2_upset,
                    candidate_diff_flag,
                    alternate_rationale
                ),
            by = "slot_key"
        ) %>%
        dplyr::mutate(
            candidate_2_matchup = dplyr::coalesce(candidate_2_matchup, matchup_label),
            candidate_2_pick = dplyr::coalesce(candidate_2_pick, candidate_1_pick),
            candidate_2_upset = dplyr::coalesce(candidate_2_upset, candidate_1_upset),
            candidate_diff_flag = dplyr::coalesce(candidate_diff_flag, FALSE),
            upset_flag_if_picked = candidate_1_upset
        ) %>%
        dplyr::mutate(
            region_order = factor(region, levels = bracket_region_levels()),
            round_order = factor(round, levels = round_levels())
        ) %>%
        dplyr::arrange(region_order, round_order, matchup_number) %>%
        dplyr::select(-region_order, -round_order)
}

#' Summarize posterior total-points draws
#'
#' @param draws A numeric vector of posterior predictive total-points draws.
#'
#' @return A one-row tibble of mean, median, spread, and interval summaries.
#' @keywords internal
summarize_total_points_draws <- function(draws) {
    draws <- suppressWarnings(as.numeric(draws))
    draws <- draws[is.finite(draws)]

    if (length(draws) == 0) {
        return(tibble::tibble(
            predicted_total_mean = NA_real_,
            predicted_total_median = NA_real_,
            predicted_total_sd = NA_real_,
            predicted_total_50_lower = NA_real_,
            predicted_total_50_upper = NA_real_,
            predicted_total_80_lower = NA_real_,
            predicted_total_80_upper = NA_real_,
            predicted_total_95_lower = NA_real_,
            predicted_total_95_upper = NA_real_,
            recommended_tiebreaker_points = NA_integer_
        ))
    }

    tibble::tibble(
        predicted_total_mean = mean(draws),
        predicted_total_median = stats::median(draws),
        predicted_total_sd = stats::sd(draws),
        predicted_total_50_lower = as.numeric(stats::quantile(draws, 0.25)),
        predicted_total_50_upper = as.numeric(stats::quantile(draws, 0.75)),
        predicted_total_80_lower = as.numeric(stats::quantile(draws, 0.10)),
        predicted_total_80_upper = as.numeric(stats::quantile(draws, 0.90)),
        predicted_total_95_lower = as.numeric(stats::quantile(draws, 0.025)),
        predicted_total_95_upper = as.numeric(stats::quantile(draws, 0.975)),
        recommended_tiebreaker_points = as.integer(round(stats::median(draws)))
    )
}

#' Build total-points prediction rows for a candidate bracket
#'
#' @param candidate A candidate bracket object with a `matchups` table.
#' @param team_lookup A current-year team feature lookup table keyed by team.
#'
#' @return A matchup-level tibble ready for total-points prediction.
#' @keywords internal
build_candidate_total_points_rows <- function(candidate, team_lookup) {
    if (is.null(candidate$matchups) || nrow(candidate$matchups) == 0) {
        return(tibble::tibble())
    }

    purrr::pmap_dfr(
        candidate$matchups,
        function(...) {
            row <- tibble::as_tibble(list(...))
            team_a <- team_lookup %>%
                dplyr::filter(team_key == normalize_team_key(row$teamA[[1]]))
            team_b <- team_lookup %>%
                dplyr::filter(team_key == normalize_team_key(row$teamB[[1]]))

            if (nrow(team_a) != 1 || nrow(team_b) != 1) {
                stop_with_message(
                    sprintf(
                        "Could not build total-points features for %s vs %s",
                        row$teamA[[1]],
                        row$teamB[[1]]
                    )
                )
            }

            build_total_points_feature_row(
                team_a = team_a,
                team_b = team_b,
                round_name = as.character(row$round[[1]]),
                metadata = list(
                    Year = team_a$Year[[1]],
                    region = as.character(row$region[[1]]),
                    game_index = row$matchup_number[[1]],
                    teamA = row$teamA[[1]],
                    teamB = row$teamB[[1]]
                )
            ) %>%
                dplyr::mutate(
                    candidate_id = candidate$candidate_id,
                    candidate_type = candidate$type,
                    matchup_number = row$matchup_number[[1]],
                    confidence_tier = row$confidence_tier[[1]],
                    inspection_flag = row$inspection_flag[[1]],
                    inspection_level = row$inspection_level[[1]],
                    chosen_winner = row$winner[[1]]
                )
        }
    )
}

#' Predict candidate-level championship and matchup total points
#'
#' @param candidates A list of bracket candidate objects.
#' @param current_teams A current-year team feature table.
#' @param total_points_model A fitted total-points model result bundle.
#' @param draws Number of posterior predictive draws to retain per matchup.
#'
#' @return A list containing candidate summaries, championship distributions,
#'   and full matchup-level total summaries.
#' @export
predict_candidate_total_points <- function(candidates, current_teams, total_points_model, draws = 1000) {
    if (length(candidates) == 0 || is.null(total_points_model)) {
        return(list(
            candidate_summaries = tibble::tibble(),
            championship_distribution = tibble::tibble(),
            matchup_summaries = tibble::tibble()
        ))
    }

    team_lookup <- current_teams %>%
        dplyr::mutate(team_key = normalize_team_key(Team)) %>%
        dplyr::distinct(team_key, .keep_all = TRUE)

    candidate_rows <- purrr::map(candidates, build_candidate_total_points_rows, team_lookup = team_lookup)

    matchup_summaries <- purrr::map_dfr(candidate_rows, function(candidate_rows_tbl) {
        if (nrow(candidate_rows_tbl) == 0) {
            return(tibble::tibble())
        }

        draw_matrix <- predict_total_points_rows(candidate_rows_tbl, total_points_model, draws = draws)
        draw_summaries <- purrr::map_dfr(seq_len(ncol(draw_matrix)), function(index) {
            summarize_total_points_draws(draw_matrix[, index])
        })

        dplyr::bind_cols(candidate_rows_tbl, draw_summaries) %>%
            dplyr::transmute(
                candidate_id,
                candidate_type,
                round = as.character(round),
                region = as.character(region),
                matchup_number = as.integer(matchup_number),
                teamA,
                teamB,
                chosen_winner,
                confidence_tier,
                inspection_flag,
                inspection_level,
                predicted_total_mean,
                predicted_total_median,
                predicted_total_sd,
                predicted_total_50_lower,
                predicted_total_50_upper,
                predicted_total_80_lower,
                predicted_total_80_upper,
                predicted_total_95_lower,
                predicted_total_95_upper,
                recommended_tiebreaker_points
            )
    })

    candidate_summaries <- matchup_summaries %>%
        dplyr::filter(round == "Championship") %>%
        dplyr::transmute(
            candidate_id,
            candidate_type,
            championship_matchup = sprintf("%s vs %s", teamA, teamB),
            finalist_a = teamA,
            finalist_b = teamB,
            recommended_tiebreaker_points,
            predicted_total_mean,
            predicted_total_median,
            predicted_total_sd,
            predicted_total_50_lower,
            predicted_total_50_upper,
            predicted_total_80_lower,
            predicted_total_80_upper,
            predicted_total_95_lower,
            predicted_total_95_upper
        )

    championship_distribution <- purrr::map_dfr(candidate_rows, function(candidate_rows_tbl) {
        if (nrow(candidate_rows_tbl) == 0) {
            return(tibble::tibble())
        }

        championship_index <- which(as.character(candidate_rows_tbl$round) == "Championship")
        if (length(championship_index) != 1L) {
            return(tibble::tibble())
        }

        championship_row <- candidate_rows_tbl[championship_index, , drop = FALSE]
        championship_draws <- predict_total_points_rows(championship_row, total_points_model, draws = draws)[, 1]
        championship_draws <- pmax(0, round(championship_draws))
        distribution <- as.data.frame(table(championship_draws), stringsAsFactors = FALSE)
        names(distribution) <- c("total_points", "n")

        distribution %>%
            dplyr::as_tibble() %>%
            dplyr::mutate(
                candidate_id = championship_row$candidate_id[[1]],
                candidate_type = championship_row$candidate_type[[1]],
                championship_matchup = sprintf("%s vs %s", championship_row$teamA[[1]], championship_row$teamB[[1]]),
                total_points = as.integer(total_points),
                probability = n / sum(n)
            ) %>%
            dplyr::select(candidate_id, candidate_type, championship_matchup, total_points, probability) %>%
            dplyr::arrange(candidate_id, total_points)
    })

    list(
        candidate_summaries = candidate_summaries,
        championship_distribution = championship_distribution,
        matchup_summaries = matchup_summaries
    )
}

#' Generate the top bracket candidates for the current field
#'
#' @param all_teams A current-year team feature table.
#' @param model_results A fitted matchup-model result bundle.
#' @param draws Number of posterior draws per matchup.
#' @param actual_play_in_results Optional normalized current-year First Four
#'   results used to replace simulated duplicate-seed outcomes when available.
#' @param n_candidates Number of bracket candidates to retain.
#' @param n_simulations Number of stochastic brackets to explore.
#' @param random_seed Random seed for stochastic exploration.
#'
#' @return A list with candidate metadata and flattened matchup tables.
#' @export
generate_bracket_candidates <- function(all_teams, model_results, draws = 1000, actual_play_in_results = NULL, n_candidates = 2L, n_simulations = 250L, random_seed = 123) {
    deterministic_bracket <- simulate_full_bracket(
        all_teams = all_teams,
        model_results = model_results,
        draws = draws,
        actual_play_in_results = actual_play_in_results,
        deterministic = TRUE,
        log_matchups = FALSE
    )
    deterministic_flat <- augment_matchup_decisions(flatten_matchup_results(deterministic_bracket))
    deterministic_summary <- summarize_bracket_probability(deterministic_flat)
    deterministic_path <- summarize_candidate_path(deterministic_flat)

    candidates <- list(
        list(
            candidate_id = 1L,
            bracket_key = build_bracket_key(deterministic_flat),
            type = "safe",
            champion = deterministic_bracket$final_four$champion$Team[[1]],
            final_four = paste(vapply(deterministic_bracket$final_four$semifinalists, function(team) team$Team[[1]], character(1)), collapse = ", "),
            frequency = NA_integer_,
            bracket_log_prob = deterministic_summary$bracket_log_prob[[1]],
            mean_game_prob = deterministic_summary$mean_game_prob[[1]],
            title_path_mean_prob = deterministic_path$title_path_mean_prob[[1]],
            title_path_min_prob = deterministic_path$title_path_min_prob[[1]],
            diff_summary = "Primary bracket built from the higher posterior-mean pick in every slot.",
            simulation = deterministic_bracket,
            matchups = deterministic_flat
        )
    )

    if (n_candidates <= 1L || n_simulations <= 0L) {
        return(candidates)
    }

    set.seed(random_seed)
    simulated_rows <- vector("list", n_simulations)
    for (index in seq_len(n_simulations)) {
        bracket <- simulate_full_bracket(
            all_teams = all_teams,
            model_results = model_results,
            draws = draws,
            actual_play_in_results = actual_play_in_results,
            deterministic = FALSE,
            log_matchups = FALSE
        )
        flattened <- flatten_matchup_results(bracket)
        summary_row <- summarize_bracket_probability(flattened)
        simulated_rows[[index]] <- tibble::tibble(
            bracket_key = build_bracket_key(flattened),
            champion = bracket$final_four$champion$Team[[1]],
            final_four = paste(vapply(bracket$final_four$semifinalists, function(team) team$Team[[1]], character(1)), collapse = ", "),
            bracket_log_prob = summary_row$bracket_log_prob[[1]],
            mean_game_prob = summary_row$mean_game_prob[[1]],
            simulation = list(bracket),
            matchups = list(augment_matchup_decisions(flattened))
        )
    }

    ranked_candidates <- dplyr::bind_rows(simulated_rows) %>%
        dplyr::group_by(bracket_key) %>%
        dplyr::summarise(
            champion = champion[[1]],
            final_four = final_four[[1]],
            frequency = dplyr::n(),
            bracket_log_prob = max(bracket_log_prob),
            mean_game_prob = max(mean_game_prob),
            simulation = list(simulation[[which.max(bracket_log_prob)]]),
            matchups = list(matchups[[which.max(bracket_log_prob)]]),
            .groups = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(frequency), dplyr::desc(bracket_log_prob))

    alternate_candidates <- purrr::map_dfr(seq_len(nrow(ranked_candidates)), function(index) {
        row <- ranked_candidates[index, , drop = FALSE]
        comparison <- compare_candidate_matchups(deterministic_flat, row$matchups[[1]])
        diff_rows <- comparison %>%
            dplyr::filter(candidate_diff_flag)

        tibble::tibble(
            row_index = index,
            diff_count = nrow(diff_rows),
            round64_diff_count = sum(diff_rows$round == "Round of 64"),
            leverage_sum = sum(diff_rows$upset_leverage, na.rm = TRUE),
            candidate_score = row$bracket_log_prob[[1]] +
                (log1p(row$frequency[[1]]) / 3) +
                (sum(diff_rows$upset_leverage, na.rm = TRUE) / 25) -
                (nrow(diff_rows) / 8) -
                (max(sum(diff_rows$round == "Round of 64") - 6L, 0L) * 2),
            diff_summary = if (nrow(diff_rows) == 0) {
                "Matches the primary bracket."
            } else {
                paste(
                    sprintf(
                        "%s (%s)",
                        diff_rows$candidate_2_pick[seq_len(min(3L, nrow(diff_rows)))],
                        diff_rows$round[seq_len(min(3L, nrow(diff_rows)))]
                    ),
                    collapse = "; "
                )
            }
        )
    }) %>%
        dplyr::filter(diff_count > 0) %>%
        dplyr::arrange(dplyr::desc(candidate_score), round64_diff_count, diff_count)

    for (index in seq_len(min(n_candidates - 1L, nrow(alternate_candidates)))) {
        row <- ranked_candidates[alternate_candidates$row_index[[index]], , drop = FALSE]
        path_summary <- summarize_candidate_path(row$matchups[[1]])
        candidates[[length(candidates) + 1L]] <- list(
            candidate_id = length(candidates) + 1L,
            bracket_key = row$bracket_key[[1]],
            type = "alternate",
            champion = row$champion[[1]],
            final_four = row$final_four[[1]],
            frequency = row$frequency[[1]],
            bracket_log_prob = row$bracket_log_prob[[1]],
            mean_game_prob = row$mean_game_prob[[1]],
            title_path_mean_prob = path_summary$title_path_mean_prob[[1]],
            title_path_min_prob = path_summary$title_path_min_prob[[1]],
            diff_summary = alternate_candidates$diff_summary[[index]],
            simulation = row$simulation[[1]],
            matchups = row$matchups[[1]]
        )
    }

    candidates
}

#' Write candidate summaries, decision sheets, and dashboard artifacts
#'
#' @param bracket_year The active bracket year.
#' @param candidates A list of candidate bracket objects.
#' @param output_dir Directory used for output artifacts.
#' @param backtest Optional backtest result bundle.
#' @param play_in_resolution Optional one-row tibble from
#'   [summarize_play_in_resolution()] describing whether unresolved simulated
#'   First Four slots remain in the active bracket.
#' @param total_points_predictions Optional list returned by
#'   [predict_candidate_total_points()].
#'
#' @return A list of decision-artifact file paths and the in-memory decision
#'   sheet.
#' @export
save_decision_outputs <- function(bracket_year, candidates, output_dir = "output", backtest = NULL, play_in_resolution = NULL, total_points_predictions = NULL) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

    decision_sheet <- build_decision_sheet(candidates)
    model_quality_context <- resolve_model_quality_context(backtest = backtest, output_dir = output_dir)
    summary_path <- file.path(output_dir, "bracket_candidates.txt")
    rds_path <- file.path(output_dir, "bracket_candidates.rds")
    decision_sheet_path <- file.path(output_dir, "bracket_decision_sheet.csv")
    dashboard_path <- file.path(output_dir, "bracket_dashboard.html")
    technical_dashboard_path <- file.path(output_dir, "technical_dashboard.html")
    tiebreaker_summary_path <- file.path(output_dir, "championship_tiebreaker_summary.csv")
    championship_distribution_path <- file.path(output_dir, "championship_tiebreaker_distribution.csv")
    matchup_totals_path <- file.path(output_dir, "candidate_matchup_total_points.csv")

    saveRDS(candidates, rds_path)
    utils::write.csv(decision_sheet, decision_sheet_path, row.names = FALSE)

    if (!is.null(total_points_predictions)) {
        utils::write.csv(total_points_predictions$candidate_summaries, tiebreaker_summary_path, row.names = FALSE)
        utils::write.csv(total_points_predictions$championship_distribution, championship_distribution_path, row.names = FALSE)
        utils::write.csv(total_points_predictions$matchup_summaries, matchup_totals_path, row.names = FALSE)
    }

    candidate_csv_paths <- vapply(candidates, function(candidate) {
        path <- file.path(output_dir, sprintf("bracket_candidate_%s.csv", candidate$candidate_id))
        utils::write.csv(candidate$matchups, path, row.names = FALSE)
        path
    }, character(1))

    sink(summary_path)
    cat(sprintf("Bracket year: %s\n\n", bracket_year))
    for (candidate in candidates) {
        cat(sprintf("Candidate %s (%s)\n", candidate$candidate_id, candidate$type))
        cat(sprintf("Champion: %s\n", candidate$champion))
        cat(sprintf("Final Four: %s\n", candidate$final_four))
        cat(sprintf("Bracket log probability: %.4f\n", candidate$bracket_log_prob))
        cat(sprintf("Mean picked-game probability: %.4f\n", candidate$mean_game_prob))
        cat(sprintf("Title path mean probability: %.4f\n", candidate$title_path_mean_prob %||% NA_real_))
        if (!is.null(total_points_predictions) && nrow(total_points_predictions$candidate_summaries) > 0) {
            tiebreaker_row <- total_points_predictions$candidate_summaries %>%
                dplyr::filter(candidate_id == candidate$candidate_id)
            if (nrow(tiebreaker_row) == 1L) {
                cat(sprintf("Championship matchup: %s\n", tiebreaker_row$championship_matchup[[1]]))
                cat(sprintf("Recommended tiebreaker: %s\n", tiebreaker_row$recommended_tiebreaker_points[[1]]))
                cat(sprintf(
                    "Championship total 80%% interval: %.1f to %.1f\n",
                    tiebreaker_row$predicted_total_80_lower[[1]],
                    tiebreaker_row$predicted_total_80_upper[[1]]
                ))
            }
        }
        if (!is.na(candidate$frequency)) {
            cat(sprintf("Simulation frequency: %s\n", candidate$frequency))
        }
        if (!is.null(candidate$diff_summary)) {
            cat(sprintf("Alternate rationale: %s\n", candidate$diff_summary))
        }
        cat("\n")
    }
    sink()

    writeLines(
        create_bracket_dashboard_html(
            bracket_year = bracket_year,
            decision_sheet = decision_sheet,
            candidates = candidates,
            backtest = backtest,
            play_in_resolution = play_in_resolution,
            total_points_predictions = total_points_predictions,
            model_quality_context = model_quality_context
        ),
        dashboard_path
    )
    writeLines(
        create_technical_dashboard_html(
            bracket_year = bracket_year,
            decision_sheet = decision_sheet,
            candidates = candidates,
            backtest = backtest,
            total_points_predictions = total_points_predictions,
            play_in_resolution = play_in_resolution,
            model_quality_context = model_quality_context
        ),
        technical_dashboard_path
    )

    list(
        decision_sheet = decision_sheet,
        decision_sheet_path = decision_sheet_path,
        dashboard = dashboard_path,
        technical_dashboard = technical_dashboard_path,
        candidates_rds = rds_path,
        candidate_summary = summary_path,
        candidate_csvs = unname(candidate_csv_paths),
        championship_tiebreaker_summary = if (!is.null(total_points_predictions)) tiebreaker_summary_path else NULL,
        championship_tiebreaker_distribution = if (!is.null(total_points_predictions)) championship_distribution_path else NULL,
        matchup_total_points = if (!is.null(total_points_predictions)) matchup_totals_path else NULL,
        model_quality_source_label = model_quality_context$source_label %||% NULL,
        model_quality_source_path = model_quality_context$source_path %||% NULL,
        model_quality_used_fallback = isTRUE(model_quality_context$used_fallback)
    )
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
    candidate_summary_path <- file.path(output_dir, paste0(prefix, "_candidate_brackets.txt"))

    saveRDS(results, rds_path)

    sink(model_summary_path)
    print(summary(results$model$model))
    sink()

    if (!is.null(results$backtest)) {
        sink(backtest_summary_path)
        print(results$backtest$summary)
        sink()
    }

    model_quality_artifact <- save_model_quality_artifact(
        backtest = results$backtest,
        output_dir = output_dir
    )

    decision_outputs <- if (!is.null(results$candidates) && length(results$candidates) > 0) {
        play_in_resolution <- summarize_play_in_resolution(
            current_teams = results$data$current_teams,
            actual_play_in_results = results$data$current_play_in_results
        )
        save_decision_outputs(
            bracket_year = results$bracket_year,
            candidates = results$candidates,
            output_dir = output_dir,
            backtest = results$backtest,
            play_in_resolution = play_in_resolution,
            total_points_predictions = results$total_points_predictions
        )
    } else {
        NULL
    }

    if (!is.null(decision_outputs)) {
        file.copy(decision_outputs$candidate_summary, candidate_summary_path, overwrite = TRUE)
    }

    model_quality_archive_path <- if (!is.null(model_quality_artifact)) model_quality_artifact$archive_path else NULL
    model_quality_latest_path <- if (!is.null(model_quality_artifact)) model_quality_artifact$latest_path else NULL
    model_quality_source_label <- if (!is.null(decision_outputs)) decision_outputs$model_quality_source_label %||% NULL else NULL
    model_quality_source_path <- if (!is.null(decision_outputs)) decision_outputs$model_quality_source_path %||% NULL else NULL
    model_quality_used_fallback <- if (!is.null(decision_outputs)) isTRUE(decision_outputs$model_quality_used_fallback) else FALSE
    decision_dashboard_path <- if (!is.null(decision_outputs)) decision_outputs$dashboard %||% NULL else NULL
    decision_technical_dashboard_path <- if (!is.null(decision_outputs)) decision_outputs$technical_dashboard %||% NULL else NULL
    decision_sheet_path <- if (!is.null(decision_outputs)) decision_outputs$decision_sheet_path %||% NULL else NULL
    decision_csv_paths <- if (!is.null(decision_outputs)) decision_outputs$candidate_csvs %||% NULL else NULL
    decision_rds_path <- if (!is.null(decision_outputs)) decision_outputs$candidates_rds %||% NULL else NULL

    list(
        results = rds_path,
        model_summary = model_summary_path,
        backtest_summary = if (!is.null(results$backtest)) backtest_summary_path else NULL,
        candidate_summary = if (!is.null(results$candidates) && length(results$candidates) > 0) candidate_summary_path else NULL,
        dashboard = decision_dashboard_path,
        technical_dashboard = decision_technical_dashboard_path,
        decision_sheet = decision_sheet_path,
        candidate_csvs = decision_csv_paths,
        candidates_rds = decision_rds_path,
        model_quality_archive = model_quality_archive_path,
        model_quality_latest = model_quality_latest_path,
        model_quality_source_label = model_quality_source_label,
        model_quality_source_path = model_quality_source_path,
        model_quality_used_fallback = model_quality_used_fallback
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
