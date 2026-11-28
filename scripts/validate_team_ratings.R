#!/usr/bin/env Rscript

# Validate ESPN-derived team ratings against Torvik and measure tournament
# leakage (issue #10).
#
# For each tournament year in the canonical team-features workbook:
#   1. compute our ratings through May 1 (full season, includes the tournament)
#      and compare with the stored Torvik values, which were scraped with the
#      same May 1 window;
#   2. compute our ratings through Selection Sunday and measure how much the
#      tournament games moved each feature, for Final Four teams versus
#      first-round losers.
#
# Writes CSVs to <runtime output>/ratings_validation/.

find_script_path <- function() {
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("^--file=", args, value = TRUE)
    if (length(file_arg) == 0) {
        return(normalizePath(getwd()))
    }
    normalizePath(sub("^--file=", "", file_arg[1]))
}

project_root <- normalizePath(file.path(dirname(find_script_path()), ".."))
suppressMessages(pkgload::load_all(project_root, export_all = TRUE, helpers = FALSE, quiet = TRUE))
library(dplyr)

config <- load_project_config(file.path(project_root, "config.yml"))
out_dir <- file.path(config$output$path, "ratings_validation")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

features <- c("Barthag", "AdjOE", "AdjDE", "WAB", "TOR", "TORD", "ORB", "DRB", "3P%", "3P%D", "Adj T.")
stored <- read_table_file(config$data$team_features_path) %>%
    dplyr::mutate(Year = as.character(Year), team_key = normalize_team_key(Team))
results <- normalize_game_results(read_table_file(config$data$game_results_path))
years <- sort(unique(stored$Year))

season_rows <- lapply(years, function(year) {
    logger::log_info("Computing ratings for {year}")
    paths <- download_espn_season_files(as.integer(year), config$data$espn_raw_dir)
    season <- read_espn_season_files(paths)
    games <- build_espn_team_games(season$team_box, season$schedule)
    conferences <- espn_conference_names(season$schedule)
    list(
        selection_sunday = compute_team_ratings(games, ratings_cutoff_date(year), conferences) %>% dplyr::mutate(Year = year),
        full_season = compute_team_ratings(games, as.Date(sprintf("%s-05-01", year)), conferences) %>% dplyr::mutate(Year = year)
    )
})
ours_ss <- dplyr::bind_rows(lapply(season_rows, `[[`, "selection_sunday")) %>% dplyr::mutate(team_key = normalize_team_key(Team))
ours_full <- dplyr::bind_rows(lapply(season_rows, `[[`, "full_season")) %>% dplyr::mutate(team_key = normalize_team_key(Team))

# 1. Name matching: every tournament team must be found in our ratings.
unmatched <- stored %>%
    dplyr::anti_join(ours_ss, by = c("Year", "team_key")) %>%
    dplyr::select(Year, Team, Seed, Region)
readr::write_csv(unmatched, file.path(out_dir, "unmatched_tournament_teams.csv"))

# 2. Agreement with Torvik over the same full-season window.
long_compare <- stored %>%
    dplyr::select(Year, Team, team_key, dplyr::all_of(features)) %>%
    tidyr::pivot_longer(dplyr::all_of(features), names_to = "feature", values_to = "torvik") %>%
    dplyr::inner_join(
        ours_full %>%
            dplyr::select(Year, team_key, dplyr::all_of(features)) %>%
            tidyr::pivot_longer(dplyr::all_of(features), names_to = "feature", values_to = "ours"),
        by = c("Year", "team_key", "feature")
    )
agreement <- long_compare %>%
    dplyr::group_by(feature) %>%
    dplyr::summarise(
        teams = dplyr::n(),
        correlation = stats::cor(ours, torvik),
        mean_abs_diff = mean(abs(ours - torvik)),
        mean_diff_ours_minus_torvik = mean(ours - torvik),
        sd_ours = stats::sd(ours),
        sd_torvik = stats::sd(torvik),
        .groups = "drop"
    ) %>%
    dplyr::arrange(match(feature, features))
readr::write_csv(agreement, file.path(out_dir, "agreement_with_torvik_full_season.csv"))

# 3. Leak size: full season minus Selection Sunday, by tournament outcome.
tournament_games <- results %>%
    dplyr::filter(Year %in% years) %>%
    dplyr::mutate(loser = ifelse(winner == teamA, teamB, teamA))
final_four_games <- dplyr::filter(tournament_games, round == "Final Four")
first_round_games <- dplyr::filter(tournament_games, round == "Round of 64")
outcome_groups <- dplyr::bind_rows(
    dplyr::transmute(final_four_games, Year, Team = teamA, group = "Final Four"),
    dplyr::transmute(final_four_games, Year, Team = teamB, group = "Final Four"),
    dplyr::transmute(first_round_games, Year, Team = loser, group = "Lost Round of 64")
) %>%
    dplyr::mutate(team_key = normalize_team_key(Team)) %>%
    dplyr::distinct(Year, team_key, group)

leak <- ours_full %>%
    dplyr::select(Year, team_key, G, Barthag, AdjOE, AdjDE, WAB) %>%
    dplyr::inner_join(
        ours_ss %>% dplyr::select(Year, team_key, G, Barthag, AdjOE, AdjDE, WAB),
        by = c("Year", "team_key"),
        suffix = c("_full", "_ss")
    ) %>%
    dplyr::inner_join(outcome_groups, by = c("Year", "team_key"))
leak_summary <- leak %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(
        teams = dplyr::n(),
        extra_games = mean(G_full - G_ss),
        barthag_change = mean(Barthag_full - Barthag_ss),
        adjoe_change = mean(AdjOE_full - AdjOE_ss),
        adjde_change = mean(AdjDE_full - AdjDE_ss),
        wab_change = mean(WAB_full - WAB_ss),
        .groups = "drop"
    )
readr::write_csv(leak, file.path(out_dir, "leak_by_team.csv"))
readr::write_csv(leak_summary, file.path(out_dir, "leak_summary.csv"))

cat("\nTournament teams not matched to ESPN ratings:", nrow(unmatched), "\n")
if (nrow(unmatched) > 0) print(unmatched, n = 100)
cat("\nAgreement with stored Torvik values (same May 1 window):\n")
print(agreement %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3))))
cat("\nChange from Selection Sunday to May 1 (our ratings):\n")
print(leak_summary %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3))))
cat("\nOutputs written to", out_dir, "\n")
