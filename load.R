library(tidyverse)
library(progressr)
library(GetoptLong)
library(ncaahoopR)
library(readxl)
library(fuzzyjoin)
library(tictoc)
library(foreach)
library(doParallel)

source(file.path("helper_functions.R"))
# progressr info
handlers(handler_pbcol(
    adjust = 1.0,
    complete = function(s) cli::bg_red(cli::col_black(s)),
    incomplete = function(s) cli::bg_cyan(cli::col_black(s))
))

# can't do more than 2 or overwhelms the server
my_cores <- 2
myCluster <- makeCluster(my_cores, # number of cores to use
    type = "PSOCK", outfile = ""
)
registerDoParallel(myCluster)

# type of cluster
num_seconds_to_sleep_between_teams <- 10
message("Number of parallel workers: ", my_cores)

# change this to change bracket years
bracket_year <- str_split(Sys.Date(), pattern = "-", simplify = TRUE)[, 1]

# change this each year
# REVIEW: Must get the seed list using the scraper for each year
# read the seed placement data
extract_text <- function(string) {
    pattern <- "(?:\\.|\\d)\\s(.*?)\\s\\("
    result <- str_match(string, pattern)
    return(result[, 2])
}
seed_placement_cache <- suppressMessages(read_excel(file.path("data", bracket_year, "ncaa_tournament_seed_data_2023.xlsx"), col_names = FALSE) %>%
    mutate(NCAA = extract_text(...1)) %>%
    dplyr::select(NCAA) %>%
    mutate(SEED = row_number()))

# devtools::install_github("lbenz730/ncaahoopR")

# A data frame for converting between team names from various sites.
data("ids")
# A data frame of team color hex codes, pulled from teamcolorcodes.com. Additional data coverage provided by
data("ncaa_colors")
# A data frame for converting between team names from various sites.
data("dict")

valid_teams <- ids
team_to_id_mapping <- dict

validated_teams_temp <- stringdist_join(seed_placement_cache, team_to_id_mapping,
    by = "NCAA", # match based on team
    mode = "left", # use left join
    method = "jw", # use jw distance metric
    max_dist = 0.5,
    distance_col = "dist"
) %>%
    group_by(NCAA.x) %>%
    slice_min(order_by = dist, n = 1, with_ties = FALSE) %>%
    mutate(same_name = toupper(NCAA.x) == toupper(NCAA.y), .before = 1)

# might want to manually validate these matches
validated_teams_temp %>% filter(!same_name)

validated_teams <- validated_teams_temp %>%
    mutate(TEAM = ESPN) %>%
    ungroup() %>%
    dplyr::select(-c(NCAA.x, NCAA.y, dist)) %>%
    filter(!is.na(SEED)) %>%
    mutate(SEED = as.integer(SEED)) %>%
    # in case the website has a team in twice, which has happened
    distinct()

# add in the ESPN BPI data
espn_bpi_data <- read_excel(file.path("data", bracket_year, qq("espn-bpi-@{bracket_year}.xlsx"))) %>%
    dplyr::select(ESPN = TEAM, BPI, BPI_RANK)

# need to fuzzy join on ESPN name for espn_bpi_data
validated_teams_with_bpi <- stringdist_join(validated_teams, espn_bpi_data,
    by = "ESPN", # match based on team
    mode = "left", # use left join
    method = "jw", # use jw distance metric
    max_dist = 0.5,
    distance_col = "dist"
) %>%
    group_by(ESPN.x) %>%
    slice_min(order_by = dist, n = 1, with_ties = FALSE) %>%
    dplyr::select(TEAM, SEED, BPI, BPI_RANK, ESPN.x, ESPN.y, everything())

# Scrape NCAA basketball tournament data using ncaahoopR, only use those in the tournament this year
write_tsv(validated_teams_with_bpi, file.path("data", bracket_year, qq("validated_teams_with_bpi-@{bracket_year}.tsv")))
teams <- validated_teams_with_bpi$TEAM

# Set the years to scrape
years_seq_vec <- seq(2016, 2021, by = 1)
years <- tibble(year = years_seq_vec) %>%
    mutate(
        end_season_year = year + 1,
        season = str_c(year, substr(end_season_year, 3, 4), sep = "-")
    )

tic()
# with_progress(expr = {
res <- get_game_info_loop(
    teams,
    years,
    bracket_year,
    validated_teams_with_bpi,
    num_sec = num_seconds_to_sleep_between_teams
)
# })
toc()

# plan(sequential)
stopCluster(myCluster)
