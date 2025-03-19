rm(list = ls())
library(tidyverse)
library(glue)
library(progressr)
library(GetoptLong)
library(readxl)
library(writexl)
library(tictoc)
library(rvest)
library(furrr)

# progressr info
handlers(handler_pbcol(
    adjust = 1.0,
    complete = function(s) cli::bg_red(cli::col_black(s)),
    incomplete = function(s) cli::bg_cyan(cli::col_black(s))
))

# Set up parallel processing: adjust workers as needed.
my_cores <- 4
plan(multisession, workers = my_cores)
message("Number of parallel workers: ", my_cores)

# Identify bracket year from system date (if relevant)
start_year <- 2021 # for historical data
bracket_year <- as.numeric(str_split(Sys.Date(), pattern = "-", simplify = TRUE)[, 1])
message("Bracket year (from system date): ", bracket_year)

###############################################################################
# SCRAPING FUNCTIONS (BartTorvik.com)
###############################################################################

# Function to scrape the conference assignment table for a given year.
scrape_conf_assignments <- function(year) {
    url <- glue::glue(
        "https://barttorvik.com/tourneytime.php?year={year}&sort=7&conlimit=All",
        year = year
    )
    message("Scraping conf assignments: ", url)

    page <- read_html(url)
    tbl_region <- page %>%
        html_element("table") %>%
        html_table(fill = TRUE)

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

# Function to scrape the detailed BartTorvik data for a given year and region.
scrape_bart_data <- function(year, region) {
    # Use year minus one for the begin date to avoid issues in the URL.
    url <- glue::glue(
        "https://barttorvik.com/?year={year}",
        "&sort=&hteam=&t2value=",
        "&conlimit={region}",
        "&state=All&begin={year_minus_one}1101",
        "&end={year}0501",
        "&top=0&revquad=0&quad=5&venue=All&type=All&mingames=0#",
        year = year, region = region, year_minus_one = year - 1
    )
    message("Scraping Bart data: ", url)

    page <- read_html(url)
    tbl <- page %>%
        html_element(xpath = '//*[@id="content"]//table') %>%
        html_table()

    # First row contains column names.
    colnames_new <- tbl[1, ] %>% as.character()
    colnames(tbl) <- colnames_new

    final_tbl <- tbl %>%
        slice(-1) %>%
        mutate(
            Year = year,
            Region = region,
            .before = 1
        ) %>%
        # Remove duplicate tail rows if present.
        anti_join(tbl %>% slice_tail(n = 1), by = join_by(
            Rk, Team, Conf, G, Rec, AdjOE, AdjDE,
            Barthag, `EFG%`, `EFGD%`, TOR, TORD, ORB, DRB, FTR, FTRD,
            `2P%`, `2P%D`, `3P%`, `3P%D`, `3PR`, `3PRD`, `Adj T.`, WAB
        )) %>%
        mutate(
            Rk = as.numeric(Rk),
            G = as.numeric(G),
            across(`AdjOE`:`WAB`, as.numeric),
            # Use str_match to capture two groups:
            #   group 1: the team name (non-greedy until the first run of spaces before a number)
            #   group 2: the seed number
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

###############################################################################
# UPDATE OR CREATE THE DATABASE OF SCRAPED DATA
###############################################################################

# Define file paths for combined data.
bart_data_file <- file.path("data", "bart_data_all_years.xlsx")
conf_assign_file <- file.path("data", "bart_conference_assignments_all_years.xlsx")

# Read in previously scraped Bart data if available.
if (file.exists(bart_data_file)) {
    all_bart_data <- read_xlsx(bart_data_file) %>%
        arrange(desc(Year))
    message("Loaded existing Bart data from disk.")
} else {
    all_bart_data <- tibble()
    message("No existing Bart data found. Starting fresh.")
}

# Read in previously scraped conference assignments if available.
if (file.exists(conf_assign_file)) {
    all_conf_assignments <- read_xlsx(conf_assign_file) %>%
        arrange(desc(Year))
    message("Loaded existing conference assignments from disk.")
} else {
    all_conf_assignments <- tibble()
    message("No existing conference assignments found. Starting fresh.")
}

# Define the range of years you want to have in your database.
# (Adjust the start year as appropriate for your data history.)
years_to_check <- seq(start_year, bracket_year)

# Identify which years are missing in the Bart data.
existing_years <- unique(all_bart_data$Year)
years_missing <- setdiff(years_to_check, existing_years)

if (length(years_missing) > 0) {
    message("Missing Bart data for years: ", paste(years_missing, collapse = ", "))
    new_bart_data <- map_dfr(years_missing, function(year) {
        regions <- c("East", "West", "Midwest", "South")
        future_map_dfr(regions, function(region) {
            tryCatch(
                {
                    scrape_bart_data(year, region)
                },
                error = function(e) {
                    warning("Failed to scrape Bart data for year=", year, ", region=", region, ". Error: ", e)
                    NULL
                }
            )
        })
    })
    all_bart_data <- bind_rows(all_bart_data, new_bart_data) %>%
        arrange(desc(Year))
    write_xlsx(all_bart_data, bart_data_file)
    message("Updated Bart data saved to disk.")
} else {
    message("All Bart data for years ", start_year, " to ", bracket_year, " are already scraped.")
}

# Repeat similar logic for conference assignments.
existing_conf_years <- unique(all_conf_assignments$Year)
conf_years_missing <- setdiff(years_to_check, existing_conf_years)

if (length(conf_years_missing) > 0) {
    message("Missing conference assignments for years: ", paste(conf_years_missing, collapse = ", "))
    new_conf_assignments <- map_dfr(conf_years_missing, function(year) {
        tryCatch(
            {
                scrape_conf_assignments(year)
            },
            error = function(e) {
                warning("Failed to scrape conference assignments for year=", year, ". Error: ", e)
                NULL
            }
        )
    })
    all_conf_assignments <- bind_rows(all_conf_assignments, new_conf_assignments) %>%
        arrange(desc(Year))
    write_xlsx(all_conf_assignments, conf_assign_file)
    message("Updated conference assignments saved to disk.")
} else {
    message("All conference assignments for years ", start_year, " to ", bracket_year, " are already scraped.")
}

###############################################################################
# BUILD THE BAYESIAN BRACKET SELECTOR DATA
###############################################################################

# Here we combine the Bart data with the conference assignments.
# (Assuming that the Team names are consistent across datasets.)
build_bayesian_bracket_selector <- function(bart_data, conf_assignments) {
    # Merge by Year and Team.
    # bart_data = all_bart_data; conf_assignments = all_conf_assignments
    combined_data <- left_join(bart_data, conf_assignments,
        by = c("Year", "Team", "Seed", "Region", "Conf")
    )

    # Generate some priors
    combined_data <- combined_data %>%
        mutate(
            # Experience_Index = 0.1 * R32 + 0.2 * S16 + 0.4 * E8 +
            #     0.7 * F4 + 1.0 * F2 + 1.5 * Champ,
            Upset_Factor = `3PR` * (sd(`3P%`) / mean(`3P%`, na.rm = TRUE)),
            Turnover_Edge = TORD - TOR,
            # Clutch Index Calculation
            Clutch_Index = (0.1 * R32) + (0.2 * S16) + (0.4 * E8) +
                (0.7 * F4) + (1.0 * F2) + (1.5 * Champ) +
                (0.5 * Turnover_Edge) + (0.3 * FTR) +
                (0.4 * (`EFG%` - `EFGD%`)),

            # 2) A weighted measure that centers AdjOE and AdjDE around ~100 and
            #    also includes “Barthag” (Bart Torvik’s overall team power metric).
            #    Feel free to tweak weights as desired.
            combined_metric = 0.5 * (AdjOE - 100) - 0.5 * (AdjDE - 100) + 5 * (Barthag - 0.5),

            # 3) Convert Barthag (which is like an expected win%) to log-odds.
            #    This is often a convenient form for input into Bayesian models.
            #    We add a tiny epsilon (1e-9) to avoid division-by-zero if Barthag is 1.0
            barthag_logit = log((Barthag + 1e-9) / (1 - Barthag + 1e-9)),

            # 4) Incorporate WAB (Wins Above Bubble) as a simple additive factor.
            #    For instance, you could treat WAB as a “bonus” that shifts raw_strength upward.
            #    Or combine it with other metrics in a formula.
            overall_strength = (AdjOE - 100) - (AdjDE - 100) + WAB
        )

    conf_strength <- combined_data %>%
        group_by(Conf) %>%
        summarize(Conf_Strength = mean(R32, na.rm = TRUE), by = "Conf")
    # In your full Bayesian model, you would use these columns to define your priors
    # and then simulate each matchup accordingly.
    return(combined_data %>% left_join(conf_strength, by = join_by(Conf)))
}


bayes_model_data <- build_bayesian_bracket_selector(all_bart_data, all_conf_assignments)
skimr::skim(bayes_model_data)
write_xlsx(bayes_model_data, file.path("data", "bayesian_model_data.xlsx"))
message("Bayesian model data prepared and saved.")


plan(sequential) # Reset to seque
