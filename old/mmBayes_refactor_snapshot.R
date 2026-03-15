#!/usr/bin/env Rscript

##############################################
# mmBayes - Bayesian March Madness Prediction
# Based on original script with enhanced visualization
##############################################

# Add after the data loading section
scrape_new_data <- FALSE # Toggle for scraping new data
force_overwrite <- FALSE # Toggle to force overwrite existing data

# Check if being run as a script or sourced
is_script <- !interactive() && 
  is.null(parent.frame()) ||
  identical(parent.frame(), .GlobalEnv)

# Load required packages
required_packages <- c(
  "tidyverse", "here", "logger", "rstan", "rstanarm", 
  "bayesplot", "loo", "readxl", "config", "ggrepel",
  "future", "progressr", "GetoptLong", "writexl", "tictoc",
  "pROC", "ggplot2", "glue", "rvest", "furrr"
)

# Initialize logging first
library(logger)
log_threshold(INFO)
log_appender(appender_file("tournament_simulation.log"))

# Install missing packages if running as script
if (is_script) {
  log_info("Checking for missing packages...")
  missing_packages <- setdiff(required_packages, installed.packages()[,"Package"])
  if (length(missing_packages) > 0) {
    log_info("Installing missing packages: {paste(missing_packages, collapse=', ')}")
    install.packages(missing_packages, repos="https://cran.r-project.org")
  }
}

# Load remaining packages
log_info("Loading required packages...")
lapply(setdiff(required_packages, "logger"), library, character.only = TRUE)

# Set Stan options to improve performance
log_info("Configuring Stan options...")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Configuration
bracket_year <- str_split(Sys.Date(), pattern = "-", simplify = TRUE)[, 1]
n_addtl_brackets <- 5  # if this number is 0, then no additional brackets are simulated
random_seed <- 123
set.seed(random_seed)

# Prior metrics
metrics_to_use <- c(
    # Core metrics
    "overall_strength", "barthag_logit", "AdjOE", "AdjDE", 
    "Clutch_Index", "Conf_Strength", "Upset_Factor", "Turnover_Edge",
    # Additional BartTorvik metrics
    "EFG%", "EFGD%", "TOR", "TORD", "ORB", "DRB", "FTR", "FTRD",
    "WAB",
    # Interaction terms
    "AdjOE:EFG%",    # Offensive efficiency and shooting
    "AdjDE:EFGD%",   # Defensive efficiency and shooting defense
    "TOR:TORD",     # Turnover battle
    "ORB:DRB",      # Rebounding battle
    "FTR:FTRD"      # Free throw battle
)

# File paths
priors_file <- sprintf("priors/priors_%s.rds", bracket_year)
data_fn <- "data/bayesian_model_data.xlsx"

# Scraping functions
scrape_conf_assignments <- function(year) {
    url <- glue::glue(
        "https://barttorvik.com/tourneytime.php?year={year}&sort=7&conlimit=All",
        year = year
    )
    log_info("Scraping conf assignments: {url}")

    page <- rvest::read_html(url)
    tbl_region <- page %>%
        rvest::html_element("table") %>%
        rvest::html_table(fill = TRUE)

    final_tbl_region <- tbl_region %>%
        mutate(
            Year = year,
            Region = factor(Region, levels = c("East", "Midwest", "South", "West"))
        ) %>%
        arrange(Region, Seed) %>%
        mutate(across(
            c(R64, R32, S16, E8, F4, F2, Champ),
            ~ as.numeric(ifelse(as.character(.) == "✓", "1", as.character(.)))
        ))
    return(final_tbl_region)
}

scrape_bart_data <- function(year, region) {
    # Use year minus one for the begin date to avoid issues in the URL
    url <- glue::glue(
        "https://barttorvik.com/?year={year}",
        "&sort=&hteam=&t2value=",
        "&conlimit={region}",
        "&state=All&begin={year_minus_one}1101",
        "&end={year}0501",
        "&top=0&revquad=0&quad=5&venue=All&type=All&mingames=0#",
        year = year, region = region, year_minus_one = year - 1
    )
    log_info("Scraping Bart data: {url}")

    page <- rvest::read_html(url)
    tbl <- page %>%
        rvest::html_element(xpath = '//*[@id="content"]//table') %>%
        rvest::html_table()

    # First row contains column names
    colnames_new <- tbl[1, ] %>% as.character()
    colnames(tbl) <- colnames_new

    final_tbl <- tbl %>%
        slice(-1) %>%
        mutate(
            Year = year,
            Region = region,
            .before = 1
        ) %>%
        # Remove duplicate tail rows if present
        anti_join(tbl %>% slice_tail(n = 1), by = join_by(
            Rk, Team, Conf, G, Rec, AdjOE, AdjDE,
            Barthag, `EFG%`, `EFGD%`, TOR, TORD, ORB, DRB, FTR, FTRD,
            `2P%`, `2P%D`, `3P%`, `3P%D`, `3PR`, `3PRD`, `Adj T.`, WAB
        )) %>%
        mutate(
            Rk = as.numeric(Rk),
            G = as.numeric(G),
            across(`AdjOE`:`WAB`, as.numeric),
            temp = str_match(Team, "^(.*?)\\s+(\\d+)\\s*seed"),
            Team_clean = str_trim(temp[, 2]),
            Seed_clean = as.numeric(temp[, 3])
        ) %>%
        select(-temp) %>%
        mutate(
            Team = Team_clean,
            Seed = Seed_clean, .before = 5
        ) %>%
        select(-Team_clean, -Seed_clean)

    return(final_tbl)
}

update_tournament_data <- function(start_year, bracket_year) {
    # Define file paths
    bart_data_file <- file.path("data", "bart_data_all_years.xlsx")
    conf_assign_file <- file.path("data", "bart_conference_assignments_all_years.xlsx")
    
    # Read existing data if available and not forcing overwrite
    if (file.exists(bart_data_file) && !force_overwrite) {
        all_bart_data <- read_xlsx(bart_data_file) %>%
            arrange(desc(Year))
        log_info("Loaded existing Bart data from disk.")
    } else {
        all_bart_data <- data.frame()
        log_info("Starting fresh Bart data collection.")
    }
    
    if (file.exists(conf_assign_file) && !force_overwrite) {
        all_conf_assignments <- read_xlsx(conf_assign_file) %>%
            arrange(desc(Year))
        log_info("Loaded existing conference assignments from disk.")
    } else {
        all_conf_assignments <- data.frame()
        log_info("Starting fresh conference assignments collection.")
    }
    
    # Identify years to scrape
    years_to_check <- seq(start_year, bracket_year)
    if (force_overwrite) {
        years_missing <- years_to_check
        conf_years_missing <- years_to_check
        log_info("Force overwrite enabled. Will scrape all years from {start_year} to {bracket_year}")
    } else {
        existing_years <- unique(all_bart_data$Year)
        years_missing <- setdiff(years_to_check, existing_years)
        existing_conf_years <- unique(all_conf_assignments$Year)
        conf_years_missing <- setdiff(years_to_check, existing_conf_years)
    }
    
    if (length(years_missing) > 0) {
        log_info("Scraping Bart data for years: {paste(years_missing, collapse=', ')}")
        new_bart_data <- map_dfr(years_missing, function(year) {
            regions <- c("East", "West", "Midwest", "South")
            furrr::future_map_dfr(regions, function(region) {
                tryCatch(
                    {
                        scrape_bart_data(year, region)
                    },
                    error = function(e) {
                        log_warn("Failed to scrape Bart data for year={year}, region={region}. Error: {e}")
                        NULL
                    }
                )
            })
        })
        all_bart_data <- bind_rows(all_bart_data, new_bart_data) %>%
            arrange(desc(Year))
        write_xlsx(all_bart_data, bart_data_file)
        log_info("Updated Bart data saved to disk.")
    } else {
        log_info("All Bart data for years {start_year} to {bracket_year} are already scraped.")
    }
    
    # Update conference assignments
    if (length(conf_years_missing) > 0) {
        log_info("Scraping conference assignments for years: {paste(conf_years_missing, collapse=', ')}")
        new_conf_assignments <- map_dfr(conf_years_missing, function(year) {
            tryCatch(
                {
                    scrape_conf_assignments(year)
                },
                error = function(e) {
                    log_warn("Failed to scrape conference assignments for year={year}. Error: {e}")
                    NULL
                }
            )
        })
        all_conf_assignments <- bind_rows(all_conf_assignments, new_conf_assignments) %>%
            arrange(desc(Year))
        write_xlsx(all_conf_assignments, conf_assign_file)
        log_info("Updated conference assignments saved to disk.")
    } else {
        log_info("All conference assignments for years {start_year} to {bracket_year} are already scraped.")
    }
    
    # Build the final dataset
    combined_data <- left_join(all_bart_data, all_conf_assignments,
        by = c("Year", "Team", "Seed", "Region", "Conf")
    ) %>%
        mutate(
            Upset_Factor = `3PR` * (sd(`3P%`) / mean(`3P%`, na.rm = TRUE)),
            Turnover_Edge = TORD - TOR,
            Clutch_Index = (0.1 * R32) + (0.2 * S16) + (0.4 * E8) +
                (0.7 * F4) + (1.0 * F2) + (1.5 * Champ) +
                (0.5 * Turnover_Edge) + (0.3 * FTR) +
                (0.4 * (`EFG%` - `EFGD%`)),
            combined_metric = 0.5 * (AdjOE - 100) - 0.5 * (AdjDE - 100) + 5 * (Barthag - 0.5),
            barthag_logit = log((Barthag + 1e-9) / (1 - Barthag + 1e-9)),
            overall_strength = (AdjOE - 100) - (AdjDE - 100) + WAB
        )
    
    # Calculate conference strength
    conf_strength <- combined_data %>%
        group_by(Conf) %>%
        summarize(Conf_Strength = mean(R32, na.rm = TRUE), by = "Conf")
    
    # Combine with conference strength
    final_data <- combined_data %>% left_join(conf_strength, by = join_by(Conf))
    
    # Save the final dataset
    write_xlsx(final_data, "data/bayesian_model_data.xlsx")
    log_info("Final dataset saved to data/bayesian_model_data.xlsx")
    
    return(final_data)
}

# Update the data loading section
if (scrape_new_data) {
    log_info("Updating tournament data...")
    all_data <- update_tournament_data(start_year = 2021, bracket_year = bracket_year)
    log_info("Data update complete. Please run the script again with scrape_new_data = FALSE")
    quit()
}

# Load data
log_info("Loading data from {data_fn}...")

# Add data validation and debugging
if (!file.exists(data_fn)) {
    log_error("Data file {data_fn} does not exist!")
    stop("Data file not found")
}

# Load historical data with validation
historical_data <- tryCatch({
    data <- read_excel(data_fn)
    if (nrow(data) == 0) {
        log_error("Historical data file is empty!")
        stop("Empty data file")
    }
    log_info("Raw data loaded: {nrow(data)} rows")
    
    # Filter and process historical data
    processed_data <- data %>%
    filter(Year != bracket_year) %>%
    mutate(
        R64 = as.factor(if_else(R64 == 1, 1, 0)),
        # Create interaction terms
        `AdjOE:EFG%` = AdjOE * `EFG%`,
        `AdjDE:EFGD%` = AdjDE * `EFGD%`,
        `TOR:TORD` = TOR * TORD,
        `ORB:DRB` = ORB * DRB,
        `FTR:FTRD` = FTR * FTRD
    )

    log_info("Historical data processed: {nrow(processed_data)} rows")
    processed_data
}, error = function(e) {
    log_error("Error loading historical data: {e$message}")
    stop("Failed to load historical data")
})

# Load current year data with validation
data_init <- tryCatch({
    data <- read_excel(data_fn)
    if (nrow(data) == 0) {
        log_error("Current year data file is empty!")
        stop("Empty data file")
    }
    
    # Filter and process current year data
    processed_data <- data %>%
        filter(Year == bracket_year)
    
    log_info("Current year data before R64 filter: {nrow(processed_data)} rows")
    
    if (nrow(processed_data) == 0) {
        log_error("No data found for year {bracket_year}")
        stop("No current year data")
    }
    
    # Process the data
    final_data <- processed_data %>%
        filter(R64 > 0.5) %>%
        mutate(
            # Create interaction terms for current year
            `AdjOE:EFG%` = AdjOE * `EFG%`,
            `AdjDE:EFGD%` = AdjDE * `EFGD%`,
            `TOR:TORD` = TOR * TORD,
            `ORB:DRB` = ORB * DRB,
            `FTR:FTRD` = FTR * FTRD
        )
    
    log_info("Current year data after R64 filter: {nrow(final_data)} rows")
    
    if (nrow(final_data) == 0) {
        log_error("No teams found after R64 filter for year {bracket_year}")
        stop("No teams found after R64 filter")
    }
    
    # Validate required columns
    required_cols <- c("Team", "Seed", "Region", "Conf", metrics_to_use)
    missing_cols <- setdiff(required_cols, names(final_data))
    if (length(missing_cols) > 0) {
        log_error("Missing required columns: {paste(missing_cols, collapse=', ')}")
        stop("Missing required columns in data")
    }
    
    # Validate team distribution
    region_counts <- table(final_data$Region)
    log_info("Teams per region: {paste(names(region_counts), region_counts, sep=':', collapse=', ')}")
    if (any(region_counts != 16)) {
        log_error("Invalid team distribution across regions")
        stop("Each region must have exactly 16 teams")
    }
    
    # Validate seed distribution
    for (region in unique(final_data$Region)) {
        region_data <- final_data[final_data$Region == region,]
        seed_counts <- table(region_data$Seed)
        log_info("Region {region} seed distribution: {paste(names(seed_counts), seed_counts, sep=':', collapse=', ')}")
        if (any(seed_counts > 1)) {
            log_error("Duplicate seeds found in region {region}")
            stop("Each seed must appear exactly once in each region")
        }
        if (length(seed_counts) != 16) {
            log_error("Missing seeds in region {region}")
            stop("Each region must have all seeds 1-16")
        }
    }
    
    # Validate metrics
    for (metric in metrics_to_use) {
        if (any(is.na(final_data[[metric]]))) {
            log_error("Missing values found in metric {metric}")
            stop("All metrics must have valid values")
        }
        log_info("Metric {metric} range: [{min(final_data[[metric]])}, {max(final_data[[metric]])}]")
    }
    
    final_data
}, error = function(e) {
    log_error("Error loading current year data: {e$message}")
    stop("Failed to load current year data")
})

# Debug: Print information about loaded data
log_info("Data loading summary:")
log_info("Historical data: {nrow(historical_data)} rows")
log_info("Current year data: {nrow(data_init)} rows")
log_info("Current year teams:")
log_info(paste(data_init$Team, collapse=", "))
log_info("Regions in data:")
log_info(paste(unique(data_init$Region), collapse=", "))
log_info("Metrics available:")
log_info(paste(names(data_init), collapse=", "))

if (scrape_new_data) {
    log_info("Scraping new data...")
    source("old/load_v2.R")
    log_info("Data scraping complete. Please run the script again with scrape_new_data = FALSE")
    quit()
}

# Derive explicit bayesian priors
derive_explicit_priors <- function(historical_data, metrics_to_use) {
    # Scale the predictor variables
    scaled_data <- historical_data %>%
        mutate(across(all_of(metrics_to_use), ~scale(.) %>% as.vector()))
    
    # Create formula with scaled predictors and random effects for conference
    # Properly escape interaction terms
    formula_str <- paste("Champ ~", paste(paste0("`", metrics_to_use, "`"), collapse = " + "), "+ (1 | Conf)")
    formula_obj <- as.formula(formula_str)
    
    # Fit model with scaled data and adjusted initialization
    stan_model <- stan_glmer(
        formula_obj,
        data = scaled_data,
        family = binomial(link = "logit"),
        prior = normal(0, 2.5),  # Wider prior
        prior_intercept = normal(0, 2.5),
        init_r = 0.1,  # Smaller initial range
        chains = 4,
        iter = 2000,
        seed = 42
    )
    
    # Extract posterior means and standard deviations
    posterior_summary <- summary(stan_model)
    
    # Create priors data frame
    priors <- data.frame(
        mean = posterior_summary[, "mean"],
        sd = posterior_summary[, "sd"],
        row.names = rownames(posterior_summary)
    )
    
    return(priors)
}

# Load or generate priors
if (file.exists(priors_file)) {
    log_info("Loading existing priors for year: {bracket_year}")
    loaded_priors <- tryCatch({
        readRDS(priors_file)
    }, error = function(e) {
        log_warn("Error loading priors: {e$message}. Regenerating...")
        new_priors <- derive_explicit_priors(historical_data, metrics_to_use)
        saveRDS(new_priors, priors_file)
        new_priors
    })
    
    if (!is.data.frame(loaded_priors) || nrow(loaded_priors) == 0) {
        log_warn("Loaded priors are invalid, regenerating...")
        loaded_priors <- derive_explicit_priors(historical_data, metrics_to_use)
        saveRDS(loaded_priors, priors_file)
    }
    priors <- loaded_priors
} else {
    log_info("Generating new priors for year: {bracket_year}")
    priors <- derive_explicit_priors(historical_data, metrics_to_use)
    dir.create("priors", showWarnings = FALSE)
    saveRDS(priors, priors_file)
}

# Validate priors after loading/generation
if (!is.data.frame(priors) || nrow(priors) == 0) {
    log_error("Failed to generate valid priors")
    stop("Invalid priors data frame")
}

# Debug: Print information about metrics and priors
log_info("Available metrics for simulation:")
log_info(paste(metrics_to_use, collapse=", "))
log_info("Available prior names:")
log_info(paste(rownames(priors), collapse=", "))

# Compute team strength
compute_team_strength <- function(team) {
    # Get all available metrics from the team data
    available_metrics <- intersect(metrics_to_use, names(team))
    
    if (length(available_metrics) == 0) {
        log_error("No metrics found for team {team$Team[1]}")
        return(NULL)
    }
    
    # Define default weights for all metrics
    metric_weights <- sapply(available_metrics, function(metric) {
        if (metric == "overall_strength") 2.0
        else if (metric == "barthag_logit") 1.5
        else if (metric == "AdjOE") 1.0
        else if (metric == "AdjDE") -1.0  # Negative because lower is better
        else if (metric == "Clutch_Index") 0.8
        else if (metric == "Conf_Strength") 0.7
        else if (metric == "Upset_Factor") 0.5
        else if (metric == "Turnover_Edge") 0.4
        else if (metric == "WAB") 0.6
        else if (grepl(":", metric)) 0.3  # Interaction terms get lower weight
        else 0.5  # Default weight for other metrics
    })
    
    # Log metrics for debugging
    log_info("Computing strength for team {team$Team[1]} (Seed {team$Seed[1]})")
    log_info("Available metrics: {paste(available_metrics, collapse=', ')}")
    
    # Calculate weighted sum with validation
    weighted_sum <- 0
    total_weight <- 0
    
    for (metric in available_metrics) {
        value <- as.numeric(team[[metric]][1])
        if (!is.na(value)) {
            # Get metric statistics from data_init for scaling
            metric_stats <- if (metric %in% names(data_init)) {
                c(mean = mean(data_init[[metric]], na.rm = TRUE),
                  sd = sd(data_init[[metric]], na.rm = TRUE))
            } else {
                c(mean = 0, sd = 1)
            }
            
            # Scale the metric based on its typical range
            scaled_value <- if (metric == "AdjOE" || metric == "AdjDE") {
                (value - 100) / 15  # Center around 100 and scale by typical SD
            } else if (metric == "barthag_logit") {
                value / 2  # Already in log-odds scale
            } else if (metric %in% c("EFG%", "EFGD%", "TOR", "TORD", "ORB", "DRB", "FTR", "FTRD")) {
                (value - 0.5) * 2  # Scale percentages to [-1, 1]
            } else if (grepl(":", metric)) {
                (value - metric_stats["mean"]) / metric_stats["sd"]  # Z-score for interaction terms
            } else {
                (value - metric_stats["mean"]) / metric_stats["sd"]  # Z-score for other metrics
            }
            
            if (!is.na(scaled_value) && is.finite(scaled_value)) {
                weighted_sum <- weighted_sum + (scaled_value * metric_weights[metric])
                total_weight <- total_weight + abs(metric_weights[metric])
                log_info("Metric {metric}: raw={value}, scaled={scaled_value}, weight={metric_weights[metric]}")
            } else {
                log_warn("Invalid scaled value for metric {metric}")
            }
        } else {
            log_warn("Missing value for metric {metric}")
        }
    }
    
    if (total_weight == 0) {
        log_error("No valid metrics found for team {team$Team[1]}")
        return(NULL)
    }
    
    strength <- weighted_sum / total_weight
    log_info("Final strength for {team$Team[1]}: {strength}")
    
    if (!is.finite(strength)) {
        log_error("Invalid strength value computed for team {team$Team[1]}")
        return(NULL)
    }
    
    return(strength)
}

# Here we assume the bracket order is the standard 1 vs 16, 8 vs 9, etc.
fix_region_seeds <- function(region_teams, draws = 1000, sd = 10) {
    expected <- 1:16
    actual <- region_teams$Seed
    missing <- setdiff(expected, unique(actual))
    dup_seeds <- names(which(table(actual) > 1))

    if (length(missing) == 0 && length(dup_seeds) == 0) {
        return(region_teams %>% mutate(Assigned_Seed = Seed))
    }
    if (length(missing) == 1 && length(dup_seeds) == 1) {
        dup_val <- as.numeric(dup_seeds)
        missing_val <- missing[1]
        dup_indices <- which(region_teams$Seed == dup_val)
        if (length(dup_indices) != 2) stop("Unexpected number of duplicate entries found.")
        team1 <- region_teams[dup_indices[1], , drop = FALSE]
        team2 <- region_teams[dup_indices[2], , drop = FALSE]
        play_in_result <- simulate_matchup(team1, team2, "Seed Play-In", 0, priors, draws, sd)
        if (play_in_result$win_prob_A > 0.5) {
            region_teams$Assigned_Seed <- region_teams$Seed
            region_teams$Assigned_Seed[dup_indices[1]] <- dup_val
            region_teams$Assigned_Seed[dup_indices[2]] <- missing_val
            message("Seed Play-In: ", team1$Team, " wins over ", team2$Team)
        } else {
            region_teams$Assigned_Seed <- region_teams$Seed
            region_teams$Assigned_Seed[dup_indices[1]] <- missing_val
            region_teams$Assigned_Seed[dup_indices[2]] <- dup_val
            message("Seed Play-In: ", team2$Team, " wins over ", team1$Team)
        }
        return(region_teams)
    } else {
        warning("More complex seed issues detected; manual assignment may be needed.")
        return(region_teams %>% mutate(Assigned_Seed = Seed))
    }
}

# Simulate region
simulate_region <- function(region_teams, region_name, priors, draws = 1000, sd = 10) {
    log_info("Simulating region: {region_name}")
    
    # Validate number of teams
    if (nrow(region_teams) != 16) {
        log_error("Region {region_name} must have exactly 16 teams, but has {nrow(region_teams)}")
        return(NULL)
    }
    
    # Standard March Madness bracket order
    bracket_order <- c(1, 16, 8, 9, 5, 12, 4, 13, 6, 11, 3, 14, 7, 10, 2, 15)
    region_teams <- region_teams[order(match(region_teams$Seed, bracket_order)), ]
    
    # Initialize results storage
    results <- list()
    remaining_teams <- region_teams
    
    # Simulate each round
    rounds <- c("R64", "R32", "S16", "E8")
    for (round in rounds) {
        log_info("Simulating {round} in {region_name}")
        num_matchups <- nrow(remaining_teams) / 2
        round_results <- list()
        
        log_info("Round {round} has {num_matchups} matchups")
        
        for (i in 1:num_matchups) {
            teamA_idx <- (i * 2) - 1
            teamB_idx <- i * 2
            
            teamA <- remaining_teams[teamA_idx, ]
            teamB <- remaining_teams[teamB_idx, ]
            
            log_info("Matchup {i}: {teamA$Team[1]} (Seed {teamA$Seed[1]}) vs {teamB$Team[1]} (Seed {teamB$Seed[1]})")
            
            matchup_result <- simulate_matchup(teamA, teamB, round, i, priors, draws, sd)
            if (is.null(matchup_result)) {
                log_error("Failed to simulate matchup between {teamA$Team[1]} and {teamB$Team[1]}")
                next
            }
            
            # Store matchup result in structured format
            round_results[[i]] <- list(
                round = round,
                matchup = i,
                teamA = teamA$Team[1],
                teamB = teamB$Team[1],
                seedA = teamA$Seed[1],
                seedB = teamB$Seed[1],
                scoreA = matchup_result$teamA_composite,
                scoreB = matchup_result$teamB_composite,
                win_prob_A = matchup_result$win_prob_A,
                winner = matchup_result$winner
            )
            
            log_info("Stored matchup result: {teamA$Team[1]} vs {teamB$Team[1]} -> {matchup_result$winner}")
            
            # Update remaining teams for next round
            if (matchup_result$winner == teamA$Team[1]) {
                remaining_teams[i, ] <- teamA
            } else {
                remaining_teams[i, ] <- teamB
            }
        }
        
        # Validate number of teams advancing
        expected_teams <- 16 / (2^which(rounds == round))
        if (nrow(remaining_teams) != expected_teams) {
            log_error("Invalid number of teams advancing in {round}. Expected {expected_teams}, got {nrow(remaining_teams)}")
            return(NULL)
        }
        
        # Store round results
        results[[round]] <- round_results
        log_info("Round {round} complete. {length(round_results)} matchups stored")
        remaining_teams <- remaining_teams[1:expected_teams, ]
    }
    
    # Set champion
    champion <- remaining_teams[1, ]
    log_info("Region {region_name} champion: {champion$Team[1]} (Seed {champion$Seed[1]})")
    
    # Debug: Print final results structure
    log_info("Final results structure:")
    for (round in names(results)) {
        log_info("  {round}: {length(results[[round]])} matchups")
    }
    
    list(
        results = results,
        champion = champion
    )
}

# Simulate final four and championship
simulate_final_four <- function(region_results, draws = 1000, sd = 10) {
    # Debug: Print region results structure
    log_info("Starting Final Four simulation")
    log_info("Regional Champions:")
    regional_champs <- list()
    
    # Get regional champions with validation
    for (region in c("South", "West", "East", "Midwest")) {
        if (!region %in% names(region_results)) {
            log_error("Missing region {region} in results")
            stop("Missing region in results")
        }
        if (!"Champ" %in% names(region_results[[region]])) {
            log_error("No champion found for region {region}")
            stop("Missing champion in region")
        }
        
        # Get the champion team data
        champ_team <- region_results[[region]]$Champ
        regional_champs[[region]] <- champ_team
        log_info("{region} Champion: {champ_team$Team}")
    }
    
    # Create semifinal matchups (South vs West, East vs Midwest)
    log_info("Simulating Final Four - Semifinal 1: {regional_champs$South$Team} (South) vs {regional_champs$West$Team} (West)")
    
    semifinal1 <- simulate_matchup(
        regional_champs$South,
        regional_champs$West,
        "F4_Semifinal1",
        1,
        priors,
        draws,
        sd
    )
    log_info("Semifinal 1 Result: {semifinal1$winner} advances")
    
    log_info("Simulating Final Four - Semifinal 2: {regional_champs$East$Team} (East) vs {regional_champs$Midwest$Team} (Midwest)")
    
    semifinal2 <- simulate_matchup(
        regional_champs$East,
        regional_champs$Midwest,
        "F4_Semifinal2",
        2,
        priors,
        draws,
        sd
    )
    log_info("Semifinal 2 Result: {semifinal2$winner} advances")
    
    # Championship game
    log_info("Simulating Championship Game: {semifinal1$winner} vs {semifinal2$winner}")
    
    # Get the winning teams' data for the championship
    champ_team1 <- if (semifinal1$winner == regional_champs$South$Team) {
        regional_champs$South
    } else {
        regional_champs$West
    }
    
    champ_team2 <- if (semifinal2$winner == regional_champs$East$Team) {
        regional_champs$East
    } else {
        regional_champs$Midwest
    }
    
    championship <- simulate_matchup(
        champ_team1,
        champ_team2,
        "Championship",
        1,
        priors,
        draws,
        sd
    )
    log_info("Championship Result: {championship$winner} wins the tournament!")
    
    return(list(
        regional_champs = regional_champs,
        semifinal1 = semifinal1,
        semifinal2 = semifinal2,
        championship = championship
    ))
}

# Simulate matchup
simulate_matchup <- function(teamA, teamB, round_name, matchup_number, priors, draws = 1000, sd = 10) {
    # Validate team data
    if (nrow(teamA) == 0 || nrow(teamB) == 0) {
        log_error("Invalid team data: empty data frame")
        return(NULL)
    }
    
    # Ensure we have all required metrics
    required_metrics <- metrics_to_use
    if (!all(required_metrics %in% names(teamA)) || !all(required_metrics %in% names(teamB))) {
        log_error("Missing required metrics for teams")
        return(NULL)
    }
    
    # Compute composite strengths for each team
    strength_A <- compute_team_strength(teamA)
    strength_B <- compute_team_strength(teamB)
    
    # Get the fixed effects (metrics)
    beta_names <- metrics_to_use[metrics_to_use %in% rownames(priors)]
    
    # Get the intercept and conference effects
    intercept_mean <- priors["(Intercept)", "mean"]
    intercept_sd <- priors["(Intercept)", "sd"]
    
    conf_A_name <- paste0("b[(Intercept) Conf:", teamA$Conf[1], "]")
    conf_B_name <- paste0("b[(Intercept) Conf:", teamB$Conf[1], "]")
    
    # Handle missing conference effects gracefully
    conf_effect_A <- if (conf_A_name %in% rownames(priors)) priors[conf_A_name, "mean"] else 0
    conf_effect_B <- if (conf_B_name %in% rownames(priors)) priors[conf_B_name, "mean"] else 0
    conf_effect_A_sd <- if (conf_A_name %in% rownames(priors)) priors[conf_A_name, "sd"] else 1
    conf_effect_B_sd <- if (conf_B_name %in% rownames(priors)) priors[conf_B_name, "sd"] else 1
    
    # Get fixed effects
    beta_means <- priors[beta_names, "mean"]
    beta_sds <- priors[beta_names, "sd"]

    # Generate draws for all parameters
    intercept_draws <- rnorm(draws, intercept_mean, intercept_sd)
    conf_draws_A <- rnorm(draws, conf_effect_A, conf_effect_A_sd)
    conf_draws_B <- rnorm(draws, conf_effect_B, conf_effect_B_sd)
    beta_draws <- sapply(seq_along(beta_means), function(i) rnorm(draws, beta_means[i], beta_sds[i]))
    colnames(beta_draws) <- beta_names

    # Team vectors - ensure numeric conversion
    teamA_vector <- sapply(beta_names, function(x) as.numeric(teamA[[x]][1]))
    teamB_vector <- sapply(beta_names, function(x) as.numeric(teamB[[x]][1]))

    # Check for NA values
    if (any(is.na(teamA_vector)) || any(is.na(teamB_vector))) {
        log_error("NA values found in team metrics")
        return(NULL)
    }

    # Compute win probability
    draws_A <- intercept_draws + conf_draws_A + rowSums(sweep(beta_draws, 2, teamA_vector, "*"))
    draws_B <- intercept_draws + conf_draws_B + rowSums(sweep(beta_draws, 2, teamB_vector, "*"))
    win_draws <- plogis(draws_A - draws_B)
    
    # Adjust probabilities based on seed difference
    seed_diff <- teamA$Seed[1] - teamB$Seed[1]
    seed_adjustment <- if (abs(seed_diff) >= 8) {
        ifelse(seed_diff > 0, -0.3, 0.3)  # Favor better seed
    } else if (abs(seed_diff) >= 4) {
        ifelse(seed_diff > 0, -0.15, 0.15)
    } else {
        0
    }
    
    # Apply seed adjustment and ensure probability is between 0.05 and 0.95
    win_prob_final <- pmin(pmax(mean(win_draws) + seed_adjustment, 0.05), 0.95)
    
    # Determine winner
    winner <- if (runif(1) < win_prob_final) {
        teamA$Team[1]
    } else {
        teamB$Team[1]
    }

    # Log the matchup
    log_info("Matchup: {teamA$Team[1]} (Seed {teamA$Seed[1]}) vs {teamB$Team[1]} (Seed {teamB$Seed[1]})")
    log_info("Win probability for {teamA$Team[1]}: {round(win_prob_final, 2)}")
    log_info("Winner: {winner}")

    # Return matchup result
    result <- data.frame(
        round = round_name,
        matchup_number = matchup_number,
        teamA = teamA$Team[1],
        teamA_seed = teamA$Seed[1],
        teamA_composite = strength_A,
        teamB = teamB$Team[1],
        teamB_seed = teamB$Seed[1],
        teamB_composite = strength_B,
        win_prob_A = round(win_prob_final, 2),
        win_prob_lower = round(quantile(win_draws, 0.025), 2),
        win_prob_upper = round(quantile(win_draws, 0.975), 2),
        winner = winner,
        upset = (winner == teamA$Team[1] && teamA$Seed[1] > teamB$Seed[1]) || 
                (winner == teamB$Team[1] && teamB$Seed[1] > teamA$Seed[1])
    )
    
    return(result)
}

# Predict R64 teams in each region
predict_r64_in_region <- function(data, priors, draws = 1000, sd = 10) {
    # Keep only needed columns and rename the composite metric column
    combined <- data %>%
        select(Year, Team, Seed, Region, Champ, all_of(metrics_to_use), Conf) # need to include Conf here as (1 | Conf) is in bayesian model
    # Group by Region and Seed. For each slot:
    predictions <- combined %>%
        group_by(Region, Seed) %>%
        group_modify(~ {
            slot <- unique(.x$Seed)
            reg <- unique(.x$Region)

            if (nrow(.x) == 1) {
                # Only one team in this Region/Seed slot wins by default.
                .x <- .x %>% mutate(predicted_R64 = 1)
            } else if (nrow(.x) == 2) {
                # Two teams: simulate a play-in matchup.
                sim <- simulate_matchup(.x[1, ], .x[2, ], "Play-In", 0, priors, draws, sd)
                log_info("Region: {reg} Seed: {slot} - Play-In matchup: {sim$teamA} vs {sim$teamB} | Winner: {sim$winner}")
                .x <- .x %>% mutate(predicted_R64 = if_else(Team == sim$winner, 1, 0))
            } else {
                # More than two teams: choose the one with the highest composite strength.
                composite_values <- sapply(1:nrow(.x), function(i) compute_team_strength(.x[i, ]))
                winner <- .x %>%
                    slice(which.max(composite_values)) %>%
                    pull(Team)
                log_info("Region: {reg} Seed: {slot} - Multiple teams: predicted winner is {winner}")
                .x <- .x %>% mutate(predicted_R64 = if_else(Team == winner, 1, 0))
            }
            .x
        }) %>%
        ungroup()
    
    # Add Assigned_Seed column
    predictions$Assigned_Seed <- predictions$Seed
    
    return(predictions)
}

# Simulate multiple brackets and compute ensemble statistics
simulate_multiple_brackets <- function(data_init, n_brackets = 1, draws = 1000, sd = 10) {
    log_info("Starting simulation of {n_brackets} brackets")
    all_brackets <- list()
    
    # Validate input data
    if (is.null(data_init) || nrow(data_init) == 0) {
        log_error("Invalid input data")
        stop("Input data cannot be NULL or empty")
    }
    
    # Ensure we have teams from all regions
    regions <- c("South", "West", "East", "Midwest")
    for (region in regions) {
        region_teams <- data_init %>% filter(Region == region)
        if (nrow(region_teams) == 0) {
            log_error("No teams found for region: {region}")
            stop(paste("Missing teams for region:", region))
        }
        log_info("Found {nrow(region_teams)} teams for region {region}")
    }
    
    # Simulate each bracket
    for (bracket_idx in 1:n_brackets) {
        log_info("Simulating bracket {bracket_idx}/{n_brackets}")
        
        # Initialize region results
        region_results <- list()
        
        # Simulate each region
        for (region in regions) {
            log_info("Simulating region: {region}")
            region_teams <- data_init %>% 
                filter(Region == region) %>%
                fix_region_seeds()
            
            if (nrow(region_teams) != 16) {
                log_error("Region {region} has {nrow(region_teams)} teams instead of 16")
                stop(paste("Invalid team count in region:", region))
            }
            
            result <- simulate_region(region_teams, region, priors, draws, sd)
            
            # Validate region results
            if (is.null(result)) {
                log_error("Failed to simulate region {region}")
                stop(paste("Region simulation failed:", region))
            }
            
            # Ensure we have a champion
            if (!"champion" %in% names(result)) {
                log_error("No champion found for region {region}")
                stop(paste("No champion found in region:", region))
            }
            
            region_results[[region]] <- result
        }
        
        # Simulate final four and championship
        final_results <- simulate_final_four(region_results, draws, sd)
        
        if (is.null(final_results) || is.null(final_results$championship)) {
            log_error("Failed to simulate final four")
            stop("Final four simulation failed")
        }
        
        # Store the complete bracket
        all_brackets[[bracket_idx]] <- list(
            region_results = region_results,
            final_results = final_results
        )
        log_info("Successfully completed bracket {bracket_idx}")
    }
    
    log_info("Successfully simulated {n_brackets} brackets")
    return(all_brackets)
}

# Compute ensemble statistics from simulated brackets
compute_ensemble_stats <- function(all_brackets, data_init) {
    log_info("Computing ensemble statistics from {length(all_brackets)} brackets")
    
    # Process the successful brackets
    team_stats <- list()
    matchup_stats <- list()
    final_four_stats <- list()
    championship_stats <- list()
    path_stats <- list()
    
    # Process each bracket
    for (bracket_idx in seq_along(all_brackets)) {
        log_info("Processing bracket {bracket_idx}/{length(all_brackets)}")
        
        # Validate bracket structure
        if (is.null(all_brackets[[bracket_idx]])) {
            log_error("Bracket {bracket_idx} is NULL")
            next
        }
        
        bracket <- all_brackets[[bracket_idx]]
        log_info("Bracket {bracket_idx} structure:")
        log_info("  Regions: {paste(names(bracket$region_results), collapse=', ')}")
        log_info("  Has final results: {!is.null(bracket$final_results)}")
        
        # Debug: Print detailed structure of region results
        for (region in names(bracket$region_results)) {
            log_info("Region {region} structure:")
            region_data <- bracket$region_results[[region]]
            log_info("  Rounds available: {paste(names(region_data), collapse=', ')}")
            for (round in names(region_data)) {
                round_data <- region_data[[round]]
                if (is.data.frame(round_data)) {
                    log_info("  Round {round}: {nrow(round_data)} matchups (data frame)")
                } else if (is.list(round_data)) {
                    log_info("  Round {round}: {length(round_data)} matchups (list)")
                } else {
                    log_info("  Round {round}: unknown type {class(round_data)}")
                }
            }
        }
        
        # Track regional results
        for (region in names(bracket$region_results)) {
            log_info("Processing region {region} in bracket {bracket_idx}")
            
            if (is.null(bracket$region_results[[region]])) {
                log_error("Region {region} is NULL in bracket {bracket_idx}")
                next
            }
            
            for (round in names(bracket$region_results[[region]])) {
                log_info("Processing round {round} in region {region}")
                
                round_matchups <- bracket$region_results[[region]][[round]]
                if (is.null(round_matchups)) {
                    log_error("Round {round} is NULL in region {region}")
                    next
                }
                
                # Debug: Print matchup structure
                if (is.data.frame(round_matchups)) {
                    log_info("Round {round} is a data frame with {nrow(round_matchups)} rows")
                    log_info("Columns: {paste(names(round_matchups), collapse=', ')}")
                } else if (is.list(round_matchups)) {
                    log_info("Round {round} is a list with {length(round_matchups)} elements")
                    if (length(round_matchups) > 0) {
                        log_info("First matchup structure: {paste(names(round_matchups[[1]]), collapse=', ')}")
                    }
                }
                
                # Convert data frame to list if necessary
                if (is.data.frame(round_matchups)) {
                    round_matchups <- split(round_matchups, seq(nrow(round_matchups)))
                }
                
                log_info("Found {length(round_matchups)} matchups in {region} {round}")
                
                for (matchup_idx in seq_along(round_matchups)) {
                    matchup <- round_matchups[[matchup_idx]]
                    
                    # Debug: Print matchup details
                    log_info("Matchup {matchup_idx}:")
                    log_info("  Structure: {paste(names(matchup), collapse=', ')}")
                    log_info("  TeamA: {matchup$teamA}, TeamB: {matchup$teamB}")
                    
                    # Skip invalid or NA matchups
                    if (is.null(matchup) || !all(c("teamA", "teamB", "winner", "win_prob_A") %in% names(matchup)) ||
                        any(is.na(c(matchup$teamA, matchup$teamB, matchup$winner, matchup$win_prob_A)))) {
                        log_warn("Invalid matchup in bracket {bracket_idx}, region {region}, round {round}, matchup {matchup_idx}")
                        next
                    }
                    
                    # Skip champion matchups (they're handled separately)
                    if (round == "Champ") {
                        log_info("Skipping champion matchup in region {region}")
                        next
                    }
                    
                    # Update team and matchup stats with extra debug info
                    update_stats(team_stats, matchup_stats, matchup, paste0(region, "_", round))
                }
            }
        }
        
        # Track Final Four results
        if (!is.null(bracket$final_results)) {
            log_info("Processing Final Four results for bracket {bracket_idx}")
            
            # Process Semifinal 1
            if (!is.null(bracket$final_results$semifinal1)) {
                matchup <- bracket$final_results$semifinal1
                log_info("Processing Semifinal 1: {matchup$teamA} vs {matchup$teamB}")
                update_stats(team_stats, final_four_stats, matchup, "F4_Semifinal1")
            }
            
            # Process Semifinal 2
            if (!is.null(bracket$final_results$semifinal2)) {
                matchup <- bracket$final_results$semifinal2
                log_info("Processing Semifinal 2: {matchup$teamA} vs {matchup$teamB}")
                update_stats(team_stats, final_four_stats, matchup, "F4_Semifinal2")
            }
            
            # Process Championship
            if (!is.null(bracket$final_results$championship)) {
                matchup <- bracket$final_results$championship
                log_info("Processing Championship: {matchup$teamA} vs {matchup$teamB}, Winner: {matchup$winner}")
                update_stats(team_stats, championship_stats, matchup, "Championship")
                
                # Track championship path
                champ <- matchup$winner
                path <- get_championship_path(bracket, champ)
                path_key <- paste(path, collapse = " → ")
                log_info("Championship path for bracket {bracket_idx}: {path_key}")
                path_stats[[path_key]] <- (path_stats[[path_key]] %||% 0) + 1
            } else {
                log_warn("No championship results found in bracket {bracket_idx}")
            }
        } else {
            log_warn("No Final Four results found in bracket {bracket_idx}")
        }
        
        log_info("Completed processing bracket {bracket_idx}")
    }
    
    # Log the size of each stats collection
    log_info("Stats collection sizes:")
    log_info("Team stats: {length(team_stats)} entries")
    log_info("Matchup stats: {length(matchup_stats)} entries")
    log_info("Final Four stats: {length(final_four_stats)} entries")
    log_info("Championship stats: {length(championship_stats)} entries")
    log_info("Path stats: {length(path_stats)} entries")
    
    # Convert stats to data frames
    log_info("Converting statistics to data frames...")
    team_df <- create_team_summary(team_stats)
    matchup_df <- create_matchup_summary(matchup_stats, "Regular")
    final_four_df <- create_matchup_summary(final_four_stats, "Final Four")
    championship_df <- create_matchup_summary(championship_stats, "Championship")
    path_df <- create_path_summary(path_stats, length(all_brackets))
    
    # Log the size of each data frame
    log_info("Data frame sizes:")
    log_info("Team df: {nrow(team_df)} rows")
    log_info("Matchup df: {nrow(matchup_df)} rows")
    log_info("Final Four df: {nrow(final_four_df)} rows")
    log_info("Championship df: {nrow(championship_df)} rows")
    log_info("Path df: {nrow(path_df)} rows")
    
    # Create model-averaged bracket
    log_info("Creating model-averaged bracket...")
    averaged_bracket <- create_averaged_bracket(team_df, matchup_df, final_four_df, championship_df)
    
    # Save results
    log_info("Saving ensemble statistics...")
    write_xlsx(list(
        "Team Statistics" = team_df,
        "Regular Matchups" = matchup_df,
        "Final Four Matchups" = final_four_df,
        "Championship Matchups" = championship_df,
        "Championship Paths" = path_df,
        "Model Averaged Bracket" = averaged_bracket
    ), "output/ensemble/ensemble_statistics.xlsx")
    
    log_info("Ensemble statistics computation complete")
    return(list(
        team_stats = team_df,
        matchup_stats = matchup_df,
        final_four_stats = final_four_df,
        championship_stats = championship_df,
        path_stats = path_df,
        averaged_bracket = averaged_bracket
    ))
}

# Helper function to update stats
update_stats <- function(team_stats, matchup_stats, matchup, round) {
    log_info("Updating stats for matchup: {matchup$teamA} vs {matchup$teamB} in round {round}")
    
    # Validate matchup data
    if (is.null(matchup) || !all(c("teamA", "teamB", "winner", "win_prob_A") %in% names(matchup))) {
        log_error("Invalid matchup data structure")
        return()
    }
    
    # Update team stats for both teams
    for (team in c(matchup$teamA, matchup$teamB)) {
        if (!team %in% names(team_stats)) {
            log_info("Initializing stats for new team: {team}")
            team_stats[[team]] <- list(
                appearances = 0,
                wins = 0,
                rounds_reached = character(),
                win_probs = numeric(),
                opponents = character(),
                round_results = list()
            )
        }
        
        # Update basic stats
        team_stats[[team]]$appearances <- team_stats[[team]]$appearances + 1
        team_stats[[team]]$win_probs <- c(team_stats[[team]]$win_probs, 
                                        ifelse(team == matchup$teamA, 
                                               matchup$win_prob_A, 
                                               1 - matchup$win_prob_A))
        
        # Update opponent and round info
        opponent <- ifelse(team == matchup$teamA, matchup$teamB, matchup$teamA)
        team_stats[[team]]$opponents <- c(team_stats[[team]]$opponents, opponent)
        
        # Initialize round_results list for this round if it doesn't exist
        if (!round %in% names(team_stats[[team]]$round_results)) {
            team_stats[[team]]$round_results[[round]] <- character()
        }
        
        # Update round results
        team_stats[[team]]$round_results[[round]] <- c(
            team_stats[[team]]$round_results[[round]],
            ifelse(team == matchup$winner, "W", "L")
        )
        
        if (team == matchup$winner) {
            team_stats[[team]]$wins <- team_stats[[team]]$wins + 1
            team_stats[[team]]$rounds_reached <- c(team_stats[[team]]$rounds_reached, round)
            log_info("Team {team} won in round {round}. Current record: {team_stats[[team]]$wins}/{team_stats[[team]]$appearances}")
        }
    }
    
    # Update matchup stats
    matchup_key <- paste(sort(c(matchup$teamA, matchup$teamB)), collapse = " vs ")
    if (!matchup_key %in% names(matchup_stats)) {
        log_info("Initializing stats for new matchup: {matchup_key}")
        matchup_stats[[matchup_key]] <- list(
            count = 0,
            teamA_wins = 0,
            win_probs = numeric(),
            rounds = character(),
            winners = character()
        )
    }
    
    # Update matchup statistics
    matchup_stats[[matchup_key]]$count <- matchup_stats[[matchup_key]]$count + 1
    matchup_stats[[matchup_key]]$win_probs <- c(matchup_stats[[matchup_key]]$win_probs, matchup$win_prob_A)
    matchup_stats[[matchup_key]]$rounds <- c(matchup_stats[[matchup_key]]$rounds, round)
    matchup_stats[[matchup_key]]$winners <- c(matchup_stats[[matchup_key]]$winners, matchup$winner)
    
    if (matchup$winner == matchup$teamA) {
        matchup_stats[[matchup_key]]$teamA_wins <- matchup_stats[[matchup_key]]$teamA_wins + 1
        log_info("Team A won matchup {matchup_key}. Current record: {matchup_stats[[matchup_key]]$teamA_wins}/{matchup_stats[[matchup_key]]$count}")
    }
    
    # Log current stats sizes
    log_info("Current stats sizes - Teams: {length(team_stats)}, Matchups: {length(matchup_stats)}")
}

# Helper function to create team summary
create_team_summary <- function(team_stats) {
    # Check if team_stats is empty
    if (length(team_stats) == 0) {
        log_warn("No team statistics found")
        return(data.frame(
            Team = character(),
            Appearances = integer(),
            Wins = integer(),
            Win_Rate = numeric(),
            Avg_Win_Prob = numeric(),
            Most_Common_Round = character(),
            Opponents = list(),
            Round_Results = list()
        ))
    }
    
    # Log the teams we're processing
    log_info("Processing team statistics for {length(team_stats)} teams")
    log_info("Teams: {paste(names(team_stats), collapse=', ')}")
    
    # Create the data frame with all columns
    team_df <- data.frame(
        Team = names(team_stats),
        Appearances = sapply(team_stats, function(x) x$appearances),
        Wins = sapply(team_stats, function(x) x$wins),
        Win_Rate = sapply(team_stats, function(x) x$wins / x$appearances),
        Avg_Win_Prob = sapply(team_stats, function(x) mean(x$win_probs)),
        Most_Common_Round = sapply(team_stats, function(x) {
            if (length(x$rounds_reached) == 0) return("None")
            names(sort(table(x$rounds_reached), decreasing = TRUE)[1])
        }),
        Opponents = sapply(team_stats, function(x) x$opponents),
        Round_Results = sapply(team_stats, function(x) x$round_results)
    )
    
    # Log the size of the resulting data frame
    log_info("Created team summary with {nrow(team_df)} rows")
    
    # Now arrange the completed data frame
    team_df <- arrange(team_df, desc(.data$Win_Rate))
    
    return(team_df)
}

# Helper function to create matchup summary
create_matchup_summary <- function(matchup_stats, type) {
    if (length(matchup_stats) == 0) {
        log_warn("No matchup statistics found for type: {type}")
        return(data.frame(
            Type = character(),
            Matchup = character(),
            Count = integer(),
            TeamA_Win_Rate = numeric(),
            Avg_Win_Prob = numeric(),
            Rounds = list(),
            Winners = list()
        ))
    }
    
    # Log the matchups we're processing
    log_info("Processing {type} matchup statistics for {length(matchup_stats)} matchups")
    
    # Create the data frame
    matchup_df <- data.frame(
        Type = type,
        Matchup = names(matchup_stats),
        Count = sapply(matchup_stats, function(x) x$count),
        TeamA_Win_Rate = sapply(matchup_stats, function(x) x$teamA_wins / x$count),
        Avg_Win_Prob = sapply(matchup_stats, function(x) mean(x$win_probs)),
        Rounds = sapply(matchup_stats, function(x) x$rounds),
        Winners = sapply(matchup_stats, function(x) x$winners)
    )
    
    # Log the size of the resulting data frame
    log_info("Created {type} matchup summary with {nrow(matchup_df)} rows")
    
    # Arrange by count
    matchup_df <- arrange(matchup_df, desc(Count))
    
    return(matchup_df)
}

# Helper function to create path summary
create_path_summary <- function(path_stats, total_brackets) {
    # Check if path_stats is empty
    if (length(path_stats) == 0) {
        return(data.frame(
            Path = character(),
            Count = integer(),
            Probability = numeric(),
            stringsAsFactors = FALSE
        ))
    }
    
    data.frame(
        Path = names(path_stats),
        Count = unlist(path_stats),
        Probability = unlist(path_stats) / total_brackets,
        stringsAsFactors = FALSE
    ) %>% arrange(desc(Count))
}

# Helper function to create averaged bracket
create_averaged_bracket <- function(team_df, matchup_df, final_four_df, championship_df) {
    # Handle empty team_df
    if (nrow(team_df) == 0) {
        return(data.frame(
            Team = character(),
            Appearances = integer(),
            Wins = integer(),
            Win_Rate = numeric(),
            Avg_Win_Prob = numeric(),
            Most_Common_Round = character(),
            Final_Four_Appearances = integer(),
            Championship_Appearances = integer(),
            Championship_Wins = numeric()
        ))
    }
    
    # Combine all matchups
    all_matchups <- bind_rows(
        matchup_df %>% mutate(Stage = "Regular"),
        final_four_df %>% mutate(Stage = "Final Four"),
        championship_df %>% mutate(Stage = "Championship")
    )
    
    # Create summary for each team
    averaged_bracket <- team_df %>%
        mutate(
            Final_Four_Appearances = sapply(Team, function(t) {
                if (nrow(final_four_df) == 0) return(0)
                sum(grepl(t, final_four_df$Matchup))
            }),
            Championship_Appearances = sapply(Team, function(t) {
                if (nrow(championship_df) == 0) return(0)
                sum(grepl(t, championship_df$Matchup))
            }),
            Championship_Wins = sapply(Team, function(t) {
                if (nrow(championship_df) == 0) return(0)
                sum(championship_df$TeamA_Win_Rate[grepl(t, championship_df$Matchup)])
            })
        ) %>%
        arrange(desc(Championship_Wins), desc(Championship_Appearances), 
                desc(Final_Four_Appearances), desc(Win_Rate))
    
    return(averaged_bracket)
}

# Helper function to get championship path
get_championship_path <- function(bracket, champ) {
    path <- character()
    
    # Start with the champion
    path <- c(path, champ)
    
    # Find which semifinal the champion came from
    if (bracket$final_results$semifinal1$winner == champ) {
        # Champion came from semifinal 1 (South vs West)
        path <- c(path, bracket$final_results$semifinal1$teamA, 
                 bracket$final_results$semifinal1$teamB)
        
        # Find which region champion came from
        if (bracket$final_results$regional_champs$South$Team == champ) {
            region <- "South"
            # Add the teams they beat in their region
            region_results <- bracket$region_results$South
            for (round in c("E8", "S16", "R32", "R64")) {
                if (round %in% names(region_results)) {
                    matchups <- region_results[[round]]
                    if (is.data.frame(matchups)) {
                        # Handle data frame format
                        if (any(matchups$winner == champ)) {
                            match_row <- matchups[matchups$winner == champ,]
                            path <- c(path, match_row$teamA, match_row$teamB)
                        }
                    } else if (is.list(matchups)) {
                        # Handle list format
                        for (matchup in matchups) {
                            if (matchup$winner == champ) {
                                path <- c(path, matchup$teamA, matchup$teamB)
                            }
                        }
                    }
                }
            }
        } else if (bracket$final_results$regional_champs$West$Team == champ) {
            region <- "West"
            # Add the teams they beat in their region
            region_results <- bracket$region_results$West
            for (round in c("E8", "S16", "R32", "R64")) {
                if (round %in% names(region_results)) {
                    matchups <- region_results[[round]]
                    if (is.data.frame(matchups)) {
                        # Handle data frame format
                        if (any(matchups$winner == champ)) {
                            match_row <- matchups[matchups$winner == champ,]
                            path <- c(path, match_row$teamA, match_row$teamB)
                        }
                    } else if (is.list(matchups)) {
                        # Handle list format
                        for (matchup in matchups) {
                            if (matchup$winner == champ) {
                                path <- c(path, matchup$teamA, matchup$teamB)
                            }
                        }
                    }
                }
            }
        }
    } else if (bracket$final_results$semifinal2$winner == champ) {
        # Champion came from semifinal 2 (East vs Midwest)
        path <- c(path, bracket$final_results$semifinal2$teamA, 
                 bracket$final_results$semifinal2$teamB)
        
        # Find which region champion came from
        if (bracket$final_results$regional_champs$East$Team == champ) {
            region <- "East"
            # Add the teams they beat in their region
            region_results <- bracket$region_results$East
            for (round in c("E8", "S16", "R32", "R64")) {
                if (round %in% names(region_results)) {
                    matchups <- region_results[[round]]
                    if (is.data.frame(matchups)) {
                        # Handle data frame format
                        if (any(matchups$winner == champ)) {
                            match_row <- matchups[matchups$winner == champ,]
                            path <- c(path, match_row$teamA, match_row$teamB)
                        }
                    } else if (is.list(matchups)) {
                        # Handle list format
                        for (matchup in matchups) {
                            if (matchup$winner == champ) {
                                path <- c(path, matchup$teamA, matchup$teamB)
                            }
                        }
                    }
                }
            }
        } else if (bracket$final_results$regional_champs$Midwest$Team == champ) {
            region <- "Midwest"
            # Add the teams they beat in their region
            region_results <- bracket$region_results$Midwest
            for (round in c("E8", "S16", "R32", "R64")) {
                if (round %in% names(region_results)) {
                    matchups <- region_results[[round]]
                    if (is.data.frame(matchups)) {
                        # Handle data frame format
                        if (any(matchups$winner == champ)) {
                            match_row <- matchups[matchups$winner == champ,]
                            path <- c(path, match_row$teamA, match_row$teamB)
                        }
                    } else if (is.list(matchups)) {
                        # Handle list format
                        for (matchup in matchups) {
                            if (matchup$winner == champ) {
                                path <- c(path, matchup$teamA, matchup$teamB)
                            }
                        }
                    }
                }
            }
        }
    }
    
    # Remove duplicates while preserving order
    path <- unique(path)
    
    return(path)
}

# Main execution
if (is_script) {
    log_info("Starting March Madness simulation for year {bracket_year}")
    
    # Create output directories
    dir.create("output/model", showWarnings = FALSE, recursive = TRUE)
    dir.create("output/matchups", showWarnings = FALSE, recursive = TRUE)
    dir.create("output/ensemble", showWarnings = FALSE, recursive = TRUE)
    
    # Simulate multiple brackets
    log_info("Starting bracket simulation...")
    all_brackets <- simulate_multiple_brackets(data_init, n_brackets = n_addtl_brackets + 1)
    
    # Validate all_brackets before computing ensemble stats
    if (is.null(all_brackets) || length(all_brackets) == 0) {
        log_error("No brackets were generated")
        stop("Failed to generate any brackets")
    }
    
    log_info("Computing ensemble statistics...")
    ensemble_stats <- compute_ensemble_stats(all_brackets, data_init)
    
    # Validate ensemble stats
    if (is.null(ensemble_stats)) {
        log_error("Failed to compute ensemble statistics")
        stop("Ensemble statistics computation failed")
    }
    
    # Save ensemble statistics
    log_info("Saving ensemble statistics...")
    saveRDS(ensemble_stats, "output/ensemble/ensemble_statistics.rds")
    
    # Save detailed results
    log_info("Saving results...")
    
    # Save model diagnostics
    model_diagnostics <- list(
        priors = priors,
        metrics_used = metrics_to_use,
        convergence = list(
            rhat = summary(stan_glm(
                formula = as.formula(paste("Champ ~", paste(paste0("`", metrics_to_use, "`"), collapse = " + "))),
                data = historical_data,
                family = binomial(),
                prior = normal(0, 2.5),
                prior_intercept = normal(0, 5),
                chains = 4, iter = 4000, seed = random_seed
            ))[, "Rhat"]
        )
    )
    
    saveRDS(model_diagnostics, "output/model/model_diagnostics.rds")
    
    # Create a summary text file
    sink("output/ensemble/summary.txt")
    cat("March Madness Ensemble Simulation Summary\n")
    cat("=======================================\n\n")
    
    cat("Number of Brackets Simulated:", length(all_brackets), "\n\n")
    
    cat("Top 5 Teams by Win Rate:\n")
    print(head(ensemble_stats$team_stats, 5))
    
    cat("\nMost Common Championship Paths:\n")
    print(head(ensemble_stats$path_stats, 5))
    
    cat("\nMost Frequent Matchups:\n")
    print(head(ensemble_stats$matchup_stats, 5))
    
    sink()
    
    log_info("Simulation complete!")
}

