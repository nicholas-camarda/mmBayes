library(dplyr)
library(logger)
library(readxl)

add_derived_features <- function(data) {
    data <- dplyr::mutate(
        data,
        Year = as.character(Year),
        Seed = as.integer(Seed)
    )

    if (!"Turnover_Edge" %in% names(data) && all(c("TORD", "TOR") %in% names(data))) {
        data <- dplyr::mutate(data, Turnover_Edge = TORD - TOR)
    }

    if (!"Upset_Factor" %in% names(data) && all(c("3PR", "3P%") %in% names(data))) {
        scale_value <- stats::sd(data$`3P%`, na.rm = TRUE) / mean(data$`3P%`, na.rm = TRUE)
        data <- dplyr::mutate(data, Upset_Factor = `3PR` * scale_value)
    }

    if (!"Clutch_Index" %in% names(data) && all(c("R32", "S16", "E8", "F4", "F2", "Champ") %in% names(data))) {
        data <- dplyr::mutate(
            data,
            Clutch_Index = (0.1 * R32) + (0.2 * S16) + (0.4 * E8) +
                (0.7 * F4) + (1.0 * F2) + (1.5 * Champ)
        )
    }

    if (!"barthag_logit" %in% names(data) && "Barthag" %in% names(data)) {
        data <- dplyr::mutate(
            data,
            barthag_logit = log((Barthag + 1e-9) / (1 - Barthag + 1e-9))
        )
    }

    if (!"overall_strength" %in% names(data) && all(c("AdjOE", "AdjDE", "WAB") %in% names(data))) {
        data <- dplyr::mutate(
            data,
            overall_strength = (AdjOE - 100) - (AdjDE - 100) + WAB
        )
    }

    if (!"Conf_Strength" %in% names(data) && all(c("Conf", "R32") %in% names(data))) {
        conf_strength <- data %>%
            dplyr::group_by(Conf) %>%
            dplyr::summarise(Conf_Strength = mean(R32, na.rm = TRUE), .groups = "drop")
        data <- dplyr::left_join(data, conf_strength, by = "Conf")
    }

    data
}

#' Load and prepare all tournament data
#' @export
load_tournament_data <- function(config) {
    logger::log_info("Starting data loading process")
    raw_data <- load_raw_data(config$data_path)
    validate_tournament_data(raw_data)

    bracket_year <- get_bracket_year(raw_data)
    logger::log_info("Using bracket year: {bracket_year}")

    split_data <- split_tournament_data(raw_data, bracket_year)
    historical <- preprocess_historical_data(split_data$historical)
    current <- preprocess_current_data(split_data$current, historical)

    list(
        bracket_year = bracket_year,
        historical = historical,
        current = current
    )
}

#' Get the active bracket year from available data
#' @export
get_bracket_year <- function(data = NULL) {
    preferred_year <- format(Sys.Date(), "%Y")
    if (is.null(data)) {
        return(preferred_year)
    }

    available_years <- sort(unique(as.character(data$Year)))
    if (preferred_year %in% available_years) {
        preferred_year
    } else {
        max(available_years)
    }
}

#' Load raw data from Excel file
#' @export
load_raw_data <- function(data_path) {
    if (!file.exists(data_path)) {
        stop_with_message(sprintf("Tournament data file not found: %s", data_path))
    }

    tryCatch(
        add_derived_features(readxl::read_excel(data_path)),
        error = function(e) {
            logger::log_error("Failed to load tournament data: {e$message}")
            stop_with_message("Failed to load tournament data")
        }
    )
}

#' Validate loaded tournament data
#' @export
validate_tournament_data <- function(data) {
    required_cols <- c(
        "Year", "Team", "Seed", "Region", "Conf", "R64", "Champ",
        "overall_strength", "barthag_logit", "AdjOE", "AdjDE",
        "Clutch_Index", "Conf_Strength", "Upset_Factor", "Turnover_Edge"
    )
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
        stop_with_message(
            sprintf("Data validation failed; missing columns: %s", paste(missing_cols, collapse = ", "))
        )
    }

    validate_input_data(data, default_project_config()$metrics_to_use)
    TRUE
}

#' Split data into historical and current year
#' @export
split_tournament_data <- function(data, bracket_year) {
    current <- data %>%
        dplyr::filter(Year == bracket_year, R64 > 0.5)

    historical <- data %>%
        dplyr::filter(Year != bracket_year)

    if (nrow(current) == 0) {
        stop_with_message(sprintf("No tournament teams found for bracket year %s", bracket_year))
    }

    list(historical = historical, current = current)
}

#' Preprocess historical data
#' @export
preprocess_historical_data <- function(historical_data) {
    historical_data <- add_derived_features(historical_data)
    historical_data <- impute_numeric_columns(historical_data)

    conf_power <- historical_data %>%
        dplyr::group_by(Conf) %>%
        dplyr::summarise(conf_power = mean(overall_strength, na.rm = TRUE), .groups = "drop")

    team_history <- historical_data %>%
        dplyr::group_by(Team) %>%
        dplyr::summarise(historical_performance = mean(Champ, na.rm = TRUE), .groups = "drop")

    historical_data %>%
        dplyr::left_join(conf_power, by = "Conf") %>%
        dplyr::left_join(team_history, by = "Team") %>%
        dplyr::mutate(
            seed_strength = (17 - Seed) / 16,
            Conf = factor(Conf),
            Champ = safe_numeric(Champ),
            R64 = safe_numeric(R64)
        )
}

#' Preprocess current year data
#' @export
preprocess_current_data <- function(current_data, historical_reference = NULL) {
    current_data <- add_derived_features(current_data)
    current_data <- impute_numeric_columns(current_data, historical_reference)

    conf_power <- if (!is.null(historical_reference) && nrow(historical_reference) > 0) {
        historical_reference %>%
            dplyr::group_by(Conf) %>%
            dplyr::summarise(conf_power = mean(overall_strength, na.rm = TRUE), .groups = "drop")
    } else {
        current_data %>%
            dplyr::group_by(Conf) %>%
            dplyr::summarise(conf_power = mean(overall_strength, na.rm = TRUE), .groups = "drop")
    }

    team_history <- if (!is.null(historical_reference) && nrow(historical_reference) > 0) {
        historical_reference %>%
            dplyr::group_by(Team) %>%
            dplyr::summarise(historical_performance = mean(Champ, na.rm = TRUE), .groups = "drop")
    } else {
        tibble::tibble(Team = character(), historical_performance = numeric())
    }

    conf_levels <- if (!is.null(historical_reference)) levels(historical_reference$Conf) else sort(unique(current_data$Conf))

    current_data %>%
        dplyr::left_join(conf_power, by = "Conf") %>%
        dplyr::left_join(team_history, by = "Team") %>%
        dplyr::mutate(
            seed_strength = (17 - Seed) / 16,
            historical_performance = dplyr::coalesce(historical_performance, 0),
            conf_power = dplyr::coalesce(conf_power, overall_strength),
            Conf = factor(Conf, levels = conf_levels),
            Champ = safe_numeric(Champ),
            R64 = safe_numeric(R64)
        )
}

#' Calculate conference power ratings
#' @export
calculate_conference_power <- function(data) {
    data %>%
        dplyr::group_by(Conf) %>%
        dplyr::summarise(conf_power = mean(overall_strength, na.rm = TRUE), .groups = "drop") %>%
        dplyr::right_join(data, by = "Conf") %>%
        dplyr::pull(conf_power)
}

#' Calculate historical performance metrics
#' @export
calculate_historical_performance <- function(data) {
    data %>%
        dplyr::group_by(Team) %>%
        dplyr::summarise(historical_performance = mean(Champ, na.rm = TRUE), .groups = "drop") %>%
        dplyr::right_join(data, by = "Team") %>%
        dplyr::pull(historical_performance)
}
