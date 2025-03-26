library(tidyverse)
library(readxl)
library(logger)
library(here)

#' Load and prepare all tournament data
#' @export
load_tournament_data <- function(config) {
    log_info("Starting data loading process")
    
    # Get current year
    bracket_year <- get_bracket_year()
    log_info("Loading data for bracket year: {bracket_year}")
    
    # Load historical and current data
    data <- load_raw_data(config$data_path, bracket_year)
    
    # Validate loaded data
    validate_tournament_data(data)
    
    # Split into historical and current
    data_split <- split_tournament_data(data, bracket_year)
    
    # Preprocess both datasets
    processed_data <- list(
        historical = preprocess_historical_data(data_split$historical),
        current = preprocess_current_data(data_split$current)
    )
    
    log_info("Data loading and preprocessing completed")
    processed_data
}

#' Get current bracket year
#' @return Character string of current year
get_bracket_year <- function() {
    str_split(Sys.Date(), pattern = "-", simplify = TRUE)[, 1]
}

#' Load raw data from Excel file
#' @param data_path Path to data file
#' @param bracket_year Current year
#' @return Combined dataset
load_raw_data <- function(data_path, bracket_year) {
    tryCatch({
        read_excel(data_path) %>%
            mutate(
                Year = as.character(Year),
                is_current = Year == bracket_year
            )
    }, error = function(e) {
        log_error("Error loading data: {e$message}")
        stop("Failed to load tournament data")
    })
}

#' Validate loaded tournament data
#' @param data Raw tournament data
validate_tournament_data <- function(data) {
    required_cols <- c(
        "Year", "Team", "Seed", "Region", "Conf",
        "overall_strength", "barthag_logit", "AdjOE", "AdjDE",
        "Clutch_Index", "Conf_Strength", "Upset_Factor", "Turnover_Edge"
    )
    
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
        log_error("Missing required columns: {paste(missing_cols, collapse=', ')}")
        stop("Data validation failed: missing columns")
    }
    
    # Validate data types and ranges
    assertthat::assert_that(
        all(data$Seed >= 1 & data$Seed <= 16),
        msg = "Invalid seed values detected"
    )
    
    # Check for missing values in key metrics
    missing_metrics <- data %>%
        select(all_of(required_cols)) %>%
        summarise(across(everything(), ~sum(is.na(.)))) %>%
        gather(metric, missing_count) %>%
        filter(missing_count > 0)
    
    if (nrow(missing_metrics) > 0) {
        log_warn("Missing values detected in metrics: {paste(missing_metrics$metric, collapse=', ')}")
    }
}

#' Split data into historical and current year
#' @param data Combined tournament data
#' @param bracket_year Current year
#' @return List containing historical and current data
split_tournament_data <- function(data, bracket_year) {
    list(
        historical = data %>% 
            filter(Year != bracket_year) %>%
            mutate(R64 = as.factor(if_else(R64 == 1, 1, 0))),
        current = data %>% 
            filter(Year == bracket_year, R64 > 0.5)
    )
}

#' Preprocess historical data
#' @param historical_data Historical tournament data
#' @return Processed historical data
preprocess_historical_data <- function(historical_data) {
    historical_data %>%
        # Create derived features
        mutate(
            seed_strength = (17 - Seed) / 16,  # Normalize seed strength
            conf_power = calculate_conference_power(.), # Calculate conference power
            historical_performance = calculate_historical_performance(.)
        ) %>%
        # Handle missing values
        mutate(across(where(is.numeric), ~if_else(is.na(.), mean(., na.rm = TRUE), .)))
}

#' Preprocess current year data
#' @param current_data Current year tournament data
#' @return Processed current year data
preprocess_current_data <- function(current_data) {
    current_data %>%
        # Apply same transformations as historical
        mutate(
            seed_strength = (17 - Seed) / 16,
            conf_power = calculate_conference_power(.),
            historical_performance = calculate_historical_performance(.)
        ) %>%
        # Handle missing values
        mutate(across(where(is.numeric), ~if_else(is.na(.), mean(., na.rm = TRUE), .)))
}

#' Calculate conference power ratings
#' @param data Tournament data
#' @return Vector of conference power ratings
calculate_conference_power <- function(data) {
    data %>%
        group_by(Conf) %>%
        summarise(
            conf_power = mean(overall_strength, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        right_join(data, by = "Conf") %>%
        pull(conf_power)
}

#' Calculate historical performance metrics
#' @param data Tournament data
#' @return Vector of historical performance metrics
calculate_historical_performance <- function(data) {
    data %>%
        group_by(Team) %>%
        summarise(
            hist_perf = mean(Champ, na.rm = TRUE),
            .groups = "drop"
        ) %>%
        right_join(data, by = "Team") %>%
        pull(hist_perf)
} 