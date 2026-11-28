#!/usr/bin/env Rscript

# Model selection (#12).
#
# Stage 1: history window and recency weighting. Fits maximum-likelihood
# logistic candidates forward in time (each holdout
# season uses only earlier seasons) and scores them on held-out main-bracket
# games. Candidates:
#   seed     - seed difference only
#   barthag  - Barthag logit difference only
#   full     - the production Stan GLM formula (all difference predictors and
#              their round interactions), fit by maximum likelihood
# plus the fixed log5 baseline (Barthag logit difference with slope 1).
#
# Stage 2: the Bayesian candidates, trained on every earlier season with equal
# weight (stage 1 shows window and weighting do not matter materially):
#   stan_barthag - Stan GLM on the Barthag logit difference only
#   stan_full    - the production Stan GLM formula
#   bart         - BART on all predictors
#   equal_blend  - logit-scale average of stan_full and bart
# Each is scored on held-out games and on the bracket it would have filled in
# (1-2-4-8-16-32 points, 192 maximum).
#
# Writes CSVs to <runtime output>/model_selection/. Pass --stage1-only to skip
# stage 2.

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
logger::log_threshold(logger::WARN)

config <- load_project_config(file.path(project_root, "config.yml"))
out_dir <- file.path(config$output$path, "model_selection")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

features <- normalize_team_features(read_table_file(config$data$team_features_path))
results <- normalize_game_results(read_table_file(config$data$game_results_path))
matchups <- build_explicit_matchup_history(features, results) %>% dplyr::mutate(Year = as.character(Year))

holdout_years <- setdiff(2016:2026, 2020)
formulas <- list(
    seed = actual_outcome ~ 0 + seed_diff,
    barthag = actual_outcome ~ 0 + barthag_logit_diff,
    full = build_model_formula(config$model$required_predictors, interaction_terms = config$model$interaction_terms)
)

grid <- evaluate_logistic_history_grid(matchups, holdout_years, formulas, windows = c(8, 12, Inf), half_lives = c(Inf, 3, 5, 8))

log5 <- purrr::map_dfr(as.character(holdout_years), function(year) {
    test <- matchups %>% dplyr::filter(Year == year, as.character(round) != "First Four")
    p <- stats::plogis(test$barthag_logit_diff)
    holdout_metrics(p, test$actual_outcome) %>%
        dplyr::mutate(model = "log5", window = NA_real_, half_life = NA_real_, holdout_year = year, training_seasons = 0L, training_games = 0L, .before = 1) %>%
        dplyr::mutate(predictions = list(tibble::tibble(p = p, y = test$actual_outcome)))
})
all_results <- dplyr::bind_rows(grid, log5)
summary_tbl <- summarise_history_grid(all_results)

readr::write_csv(all_results %>% dplyr::select(-predictions), file.path(out_dir, "history_grid_by_year.csv"))
readr::write_csv(summary_tbl, file.path(out_dir, "history_grid_summary.csv"))

fmt <- function(x) ifelse(is.infinite(x), "all", ifelse(is.na(x), "-", as.character(x)))
cat("\nHeld-out main-bracket games, holdouts", paste(range(holdout_years), collapse = "-"), "(", length(holdout_years), "tournaments )\n")
print(
    summary_tbl %>%
        dplyr::mutate(window = fmt(window), half_life = fmt(half_life), dplyr::across(where(is.numeric), ~ round(.x, 4))),
    n = 100, width = 160
)

if ("--stage1-only" %in% commandArgs(trailingOnly = TRUE)) {
    cat("\nOutputs written to", out_dir, "\n")
    quit(save = "no")
}

actual_reference <- build_actual_game_reference(features, results) %>% dplyr::mutate(Year = as.character(Year))
raw_result_columns <- c("Year", "region", "round", "game_index", "teamA", "teamB", "teamA_seed", "teamB_seed", "winner")
draws <- config$model$n_draws
stan_options <- list(
    cache_dir = config$output$model_cache_path,
    use_cache = TRUE,
    random_seed = config$model$random_seed,
    include_diagnostics = FALSE,
    prior_type = config$model$prior_type
)
fit_candidate <- function(train_matchups, engine, predictor_columns, interaction_terms) {
    fit_tournament_model(
        historical_matchups = train_matchups,
        predictor_columns = predictor_columns,
        engine = engine,
        bart_config = config$model$bart,
        random_seed = stan_options$random_seed,
        include_diagnostics = FALSE,
        cache_dir = stan_options$cache_dir,
        use_cache = TRUE,
        interaction_terms = interaction_terms,
        prior_type = stan_options$prior_type
    )
}

stage2 <- purrr::map(as.character(holdout_years), function(holdout_year) {
    message("Stage 2: holdout ", holdout_year)
    train_years <- training_seasons(unique(features$Year), holdout_year, Inf)
    train_teams <- features %>% dplyr::filter(Year %in% train_years)
    train_results <- results %>% dplyr::filter(Year %in% train_years) %>% dplyr::select(dplyr::all_of(raw_result_columns))
    train_matchups <- build_explicit_matchup_history(train_teams, train_results)
    holdout_teams <- features %>% dplyr::filter(Year == holdout_year)
    holdout_results <- actual_reference %>% dplyr::filter(Year == holdout_year)
    holdout_rows <- actual_results_to_matchup_rows(holdout_results %>% dplyr::filter(round != "First Four"))

    models <- list(
        stan_barthag = fit_candidate(train_matchups, "stan_glm", "barthag_logit_diff", NULL),
        stan_full = fit_candidate(train_matchups, "stan_glm", config$model$required_predictors, config$model$interaction_terms),
        bart = fit_candidate(train_matchups, "bart", config$model$required_predictors, NULL)
    )
    models$equal_blend <- list(
        engine = "ensemble",
        components = list(stan_glm = models$stan_full, bart = models$bart),
        combiner = list(weight_stan_glm = 0.5),
        predictor_columns = config$model$required_predictors,
        prediction_seed = config$model$random_seed
    )

    rows <- purrr::imap_dfr(models, function(model, name) {
        p <- colMeans(predict_matchup_rows(holdout_rows, model, draws = draws))
        bracket <- score_ensemble_validation_bracket(holdout_teams, holdout_results, model, name, holdout_year, draws)
        holdout_metrics(p, holdout_rows$actual_outcome) %>%
            dplyr::mutate(model = name, holdout_year = holdout_year, training_seasons = length(train_years), .before = 1) %>%
            dplyr::mutate(
                bracket_score = bracket$bracket_score[[1]],
                correct_picks = bracket$correct_picks[[1]],
                predictions = list(tibble::tibble(p = p, y = holdout_rows$actual_outcome))
            )
    })
    rows
})
stage2 <- dplyr::bind_rows(stage2)
stage2_summary <- stage2 %>%
    dplyr::group_by(model) %>%
    dplyr::group_modify(function(rows, key) {
        pooled <- dplyr::bind_rows(rows$predictions)
        tibble::tibble(
            holdout_years = nrow(rows),
            mean_log_loss = mean(rows$log_loss),
            se_log_loss = stats::sd(rows$log_loss) / sqrt(nrow(rows)),
            mean_brier = mean(rows$brier),
            mean_accuracy = mean(rows$accuracy),
            mean_bracket_score = mean(rows$bracket_score),
            se_bracket_score = stats::sd(rows$bracket_score) / sqrt(nrow(rows))
        ) %>%
            dplyr::bind_cols(calibration_fit(pooled$p, pooled$y))
    }) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(mean_log_loss)

paired <- stage2 %>%
    dplyr::select(model, holdout_year, log_loss) %>%
    tidyr::pivot_wider(names_from = model, values_from = log_loss)
best <- stage2_summary$model[[1]]
paired_diffs <- purrr::map_dfr(setdiff(stage2_summary$model, best), function(other) {
    d <- paired[[other]] - paired[[best]]
    tibble::tibble(model = other, versus = best, mean_log_loss_difference = mean(d), se_difference = stats::sd(d) / sqrt(length(d)), years_worse = sum(d > 0), years = length(d))
})

readr::write_csv(stage2 %>% dplyr::select(-predictions), file.path(out_dir, "bayesian_candidates_by_year.csv"))
readr::write_csv(stage2_summary, file.path(out_dir, "bayesian_candidates_summary.csv"))
readr::write_csv(paired_diffs, file.path(out_dir, "bayesian_candidates_paired_differences.csv"))

cat("\nStage 2: Bayesian candidates, all earlier seasons, equal weight\n")
print(stage2_summary %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4))), width = 200)
cat("\nPaired log-loss differences versus the best model (positive = worse):\n")
print(paired_diffs %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4))), width = 200)
cat("\nPer-year log loss:\n")
print(paired %>% dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3))))
cat("\nOutputs written to", out_dir, "\n")
