#!/usr/bin/env Rscript

# Exploratory: does information beyond Barthag improve held-out prediction? (#12)
#
# Each candidate is added to the Barthag-only logistic model and evaluated
# forward in time on the same holdout tournaments as model selection
# (2016-2026), trained on all earlier seasons. Candidates, all as team A minus
# team B differences:
#   seed_diff        - tournament seed (committee information, e.g. injuries)
#   form_diff        - mean game efficiency margin above what full-season
#                      ratings predicted, Feb 1 to Selection Sunday (points
#                      per 100 possessions)
#   prev_barthag_diff - previous season's end-of-season Barthag logit
#   three_rate_diff  - share of field-goal attempts from three
#   ft_rate_diff     - free-throw attempts per field-goal attempt
#
# Results are exploratory: several candidates are compared on ten holdout
# tournaments, so small differences are expected by chance.

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
logger::log_threshold(logger::ERROR)

config <- load_project_config(file.path(project_root, "config.yml"))
out_dir <- file.path(config$output$path, "model_selection")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

features <- normalize_team_features(read_table_file(config$data$team_features_path))
results <- normalize_game_results(read_table_file(config$data$game_results_path))
years <- sort(unique(as.integer(features$Year)))

season_extras <- function(year) {
    paths <- download_espn_season_files(year, config$data$espn_raw_dir)
    season <- read_espn_season_files(paths)
    games <- build_espn_team_games(season$team_box, season$schedule)
    cutoff <- ratings_cutoff_date(year)
    window <- games %>% dplyr::filter(game_date <= cutoff, poss >= 40, poss <= 110)
    ratings <- compute_team_ratings(games, cutoff)
    mu <- mean(c(ratings$AdjOE, ratings$AdjDE))

    form <- window %>%
        dplyr::filter(game_date >= as.Date(sprintf("%d-02-01", year))) %>%
        dplyr::left_join(ratings %>% dplyr::select(team_id = espn_team_id, oe = AdjOE, de = AdjDE), by = "team_id") %>%
        dplyr::left_join(ratings %>% dplyr::select(opponent_team_id = espn_team_id, opp_oe = AdjOE, opp_de = AdjDE), by = "opponent_team_id") %>%
        dplyr::mutate(
            actual_margin = 100 * (team_score - opponent_team_score) / poss,
            expected_margin = (oe + opp_de - mu) - (opp_oe + de - mu)
        ) %>%
        dplyr::group_by(team_id) %>%
        dplyr::summarise(form = mean(actual_margin - expected_margin), .groups = "drop")

    shooting <- window %>%
        dplyr::group_by(team_id) %>%
        dplyr::summarise(
            three_rate = sum(three_point_field_goals_attempted) / sum(field_goals_attempted),
            ft_rate = sum(free_throws_attempted) / sum(field_goals_attempted),
            .groups = "drop"
        )

    ratings %>%
        dplyr::select(team_id = espn_team_id, Team) %>%
        dplyr::left_join(form, by = "team_id") %>%
        dplyr::left_join(shooting, by = "team_id") %>%
        dplyr::mutate(Year = as.character(year), team_key = normalize_team_key(Team)) %>%
        dplyr::select(Year, team_key, form, three_rate, ft_rate)
}

previous_season <- function(year) {
    paths <- download_espn_season_files(year - 1L - as.integer(year - 1L == 2020L), config$data$espn_raw_dir)
    season <- read_espn_season_files(paths)
    games <- build_espn_team_games(season$team_box, season$schedule)
    end_date <- max(games$game_date)
    compute_team_ratings(games, end_date) %>%
        dplyr::transmute(Year = as.character(year), team_key = normalize_team_key(Team), prev_barthag_logit = stats::qlogis(pmin(pmax(Barthag, 1e-6), 1 - 1e-6)))
}

extras <- dplyr::bind_rows(lapply(years, season_extras))
previous <- dplyr::bind_rows(lapply(years, previous_season))
team_extras <- features %>%
    dplyr::transmute(Year, team_key = normalize_team_key(Team)) %>%
    dplyr::left_join(extras, by = c("Year", "team_key")) %>%
    dplyr::left_join(previous, by = c("Year", "team_key"))
missing <- team_extras %>% dplyr::filter(dplyr::if_any(c(form, three_rate, ft_rate, prev_barthag_logit), is.na))
cat("Tournament team-seasons missing an exploratory feature:", nrow(missing), "of", nrow(team_extras), "\n")
team_extras <- team_extras %>% dplyr::mutate(dplyr::across(c(form, three_rate, ft_rate, prev_barthag_logit), ~ dplyr::coalesce(.x, stats::median(.x, na.rm = TRUE))))

matchups <- build_explicit_matchup_history(features, results) %>%
    dplyr::mutate(Year = as.character(Year), a = normalize_team_key(teamA), b = normalize_team_key(teamB)) %>%
    dplyr::left_join(team_extras %>% dplyr::rename_with(~ paste0(.x, "_a"), -c(Year, team_key)) %>% dplyr::rename(a = team_key), by = c("Year", "a")) %>%
    dplyr::left_join(team_extras %>% dplyr::rename_with(~ paste0(.x, "_b"), -c(Year, team_key)) %>% dplyr::rename(b = team_key), by = c("Year", "b")) %>%
    dplyr::mutate(
        form_diff = form_a - form_b,
        prev_barthag_diff = prev_barthag_logit_a - prev_barthag_logit_b,
        three_rate_diff = three_rate_a - three_rate_b,
        ft_rate_diff = ft_rate_a - ft_rate_b
    )

holdout_years <- setdiff(2016:2026, 2020)
formulas <- list(
    barthag = actual_outcome ~ 0 + barthag_logit_diff,
    plus_seed = actual_outcome ~ 0 + barthag_logit_diff + seed_diff,
    plus_form = actual_outcome ~ 0 + barthag_logit_diff + form_diff,
    plus_prev_barthag = actual_outcome ~ 0 + barthag_logit_diff + prev_barthag_diff,
    plus_three_rate = actual_outcome ~ 0 + barthag_logit_diff + three_rate_diff,
    plus_ft_rate = actual_outcome ~ 0 + barthag_logit_diff + ft_rate_diff
)
rows <- purrr::map_dfr(as.character(holdout_years), function(h) {
    train <- matchups %>% dplyr::filter(Year < h)
    test <- matchups %>% dplyr::filter(Year == h, as.character(round) != "First Four")
    purrr::imap_dfr(formulas, function(formula, name) {
        fit <- stats::glm(formula, family = stats::binomial(), data = train)
        p <- stats::predict(fit, newdata = test, type = "response")
        holdout_metrics(p, test$actual_outcome) %>% dplyr::mutate(model = name, holdout_year = h, .before = 1)
    })
})
base <- rows %>% dplyr::filter(model == "barthag") %>% dplyr::select(holdout_year, base = log_loss)
summary_tbl <- rows %>%
    dplyr::left_join(base, by = "holdout_year") %>%
    dplyr::group_by(model) %>%
    dplyr::summarise(
        mean_log_loss = mean(log_loss),
        change_vs_barthag = mean(log_loss - base),
        se_change = stats::sd(log_loss - base) / sqrt(dplyr::n()),
        years_better = sum(log_loss < base),
        mean_accuracy = mean(accuracy),
        .groups = "drop"
    ) %>%
    dplyr::arrange(mean_log_loss)
readr::write_csv(rows, file.path(out_dir, "exploratory_features_by_year.csv"))
readr::write_csv(summary_tbl, file.path(out_dir, "exploratory_features_summary.csv"))
print(summary_tbl %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4))), width = 160)
