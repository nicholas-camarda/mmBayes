library(dplyr)
library(logger)
library(tibble)

# Forward-in-time evaluation used to choose the production winner model (#12).
#
# Every candidate is refit for each holdout year using only earlier seasons,
# then scored on that year's main-bracket games (First Four excluded). The
# primary selection metric is mean held-out log loss across holdout years.

#' Training weights for earlier seasons
#'
#' @param train_years Season of each training row.
#' @param holdout_year The season being predicted.
#' @param half_life Seasons after which a game counts half as much; `Inf` gives
#'   every season equal weight.
#'
#' @return A numeric vector of weights, one per training row.
#' @keywords internal
season_recency_weights <- function(train_years, holdout_year, half_life = Inf) {
    age <- as.numeric(holdout_year) - as.numeric(train_years)
    if (any(age <= 0)) {
        stop_with_message("Training seasons must be earlier than the holdout season")
    }
    if (is.infinite(half_life)) {
        return(rep(1, length(age)))
    }
    0.5^((age - 1) / half_life)
}

#' Seasons available for training under a history window
#'
#' @param available_years All seasons with data.
#' @param holdout_year The season being predicted.
#' @param window Number of most recent earlier seasons to use; `Inf` uses all.
#'
#' @return A sorted character vector of training seasons.
#' @keywords internal
training_seasons <- function(available_years, holdout_year, window = Inf) {
    earlier <- sort(as.integer(available_years[as.integer(available_years) < as.integer(holdout_year)]))
    if (is.finite(window)) {
        earlier <- utils::tail(earlier, window)
    }
    as.character(earlier)
}

#' Binary scoring metrics for held-out probabilities
#'
#' @param p Predicted probabilities that team A wins.
#' @param y Observed outcomes (1 if team A won).
#'
#' @return A one-row tibble with log loss, Brier score and accuracy.
#' @keywords internal
holdout_metrics <- function(p, y) {
    p <- pmin(pmax(as.numeric(p), 1e-9), 1 - 1e-9)
    tibble::tibble(
        games = length(y),
        log_loss = -mean(y * log(p) + (1 - y) * log(1 - p)),
        brier = mean((p - y)^2),
        accuracy = mean((p >= 0.5) == (y == 1))
    )
}

#' Calibration intercept and slope of pooled held-out predictions
#'
#' Fits `y ~ logit(p)`. A slope below 1 means predictions are too extreme
#' (overconfident); above 1, too timid. The intercept is near 0 for
#' order-invariant predictions.
#'
#' @param p Predicted probabilities.
#' @param y Observed outcomes.
#'
#' @return A one-row tibble with `calibration_intercept` and `calibration_slope`.
#' @keywords internal
calibration_fit <- function(p, y) {
    p <- pmin(pmax(as.numeric(p), 1e-9), 1 - 1e-9)
    fit <- stats::glm(y ~ stats::qlogis(p), family = stats::binomial())
    tibble::tibble(
        calibration_intercept = unname(stats::coef(fit)[[1]]),
        calibration_slope = unname(stats::coef(fit)[[2]])
    )
}

#' Log5 win probability from two Barthag ratings
#'
#' @param barthag_a Team A's Barthag.
#' @param barthag_b Team B's Barthag.
#'
#' @return The probability that A beats B if each rating is a win probability
#'   against an average team.
#' @keywords internal
log5_probability <- function(barthag_a, barthag_b) {
    (barthag_a - barthag_a * barthag_b) / (barthag_a + barthag_b - 2 * barthag_a * barthag_b)
}

#' Evaluate maximum-likelihood logistic candidates over a history grid
#'
#' @param matchups One row per historical game (see
#'   [build_explicit_matchup_history()]), all seasons.
#' @param holdout_years Seasons to predict.
#' @param formulas Named list of model formulas (no intercept).
#' @param windows Numeric vector of history windows (`Inf` = all seasons).
#' @param half_lives Numeric vector of recency half-lives (`Inf` = equal weight).
#'
#' @return A tibble with one row per model, window, half-life and holdout year:
#'   training seasons and rows, and the held-out metrics.
#' @keywords internal
evaluate_logistic_history_grid <- function(matchups, holdout_years, formulas, windows = c(8, 12, Inf), half_lives = c(Inf, 3, 5, 8)) {
    matchups <- matchups %>% dplyr::mutate(Year = as.character(Year))
    scaled_columns <- intersect(continuous_matchup_diff_columns(), names(matchups))
    grid <- tidyr::expand_grid(model = names(formulas), window = windows, half_life = half_lives, holdout_year = as.character(holdout_years))

    purrr::pmap_dfr(grid, function(model, window, half_life, holdout_year) {
        seasons <- training_seasons(unique(matchups$Year), holdout_year, window)
        train <- matchups %>% dplyr::filter(Year %in% seasons)
        test <- matchups %>% dplyr::filter(Year == holdout_year, as.character(round) != "First Four")
        scaling <- build_scaling_reference(train, scaled_columns, scaled_columns = scaled_columns, center = FALSE)
        train_scaled <- apply_scaling_reference(train, scaling)
        test_scaled <- apply_scaling_reference(test, scaling)
        train_scaled$.recency_weight <- season_recency_weights(train$Year, holdout_year, half_life)
        fit <- suppressWarnings(stats::glm(formulas[[model]], family = stats::binomial(), data = train_scaled, weights = .recency_weight))
        p <- stats::predict(fit, newdata = test_scaled, type = "response")
        holdout_metrics(p, test$actual_outcome) %>%
            dplyr::mutate(
                model = model,
                window = window,
                half_life = half_life,
                holdout_year = holdout_year,
                training_seasons = length(seasons),
                training_games = nrow(train),
                .before = 1
            ) %>%
            dplyr::mutate(predictions = list(tibble::tibble(p = as.numeric(p), y = test$actual_outcome)))
    })
}

#' Summarise a history grid by configuration
#'
#' @param grid_results Output of [evaluate_logistic_history_grid()].
#'
#' @return One row per model, window and half-life: mean and standard error
#'   of held-out log loss across holdout years, mean Brier and accuracy,
#'   pooled calibration, and the number of holdout years.
#' @keywords internal
summarise_history_grid <- function(grid_results) {
    grid_results %>%
        dplyr::group_by(model, window, half_life) %>%
        dplyr::group_modify(function(rows, key) {
            pooled <- dplyr::bind_rows(rows$predictions)
            tibble::tibble(
                holdout_years = nrow(rows),
                mean_log_loss = mean(rows$log_loss),
                se_log_loss = stats::sd(rows$log_loss) / sqrt(nrow(rows)),
                mean_brier = mean(rows$brier),
                mean_accuracy = mean(rows$accuracy)
            ) %>%
                dplyr::bind_cols(calibration_fit(pooled$p, pooled$y))
        }) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(model, mean_log_loss)
}

#' Full feature set evaluated against the production model
#'
#' The production winner model uses only `barthag_logit_diff` (#12). Model
#' selection keeps comparing it with the full difference-feature set and its
#' round interactions so the choice is re-checked as data accumulate.
#'
#' @return A list with `predictors` and `interaction_terms`.
#' @keywords internal
selection_full_feature_set <- function() {
    list(
        predictors = c(
            "seed_diff", "barthag_logit_diff", "AdjOE_diff", "AdjDE_diff", "WAB_diff",
            "TOR_diff", "TORD_diff", "ORB_diff", "DRB_diff", "3P%_diff", "3P%D_diff", "Adj T._diff"
        ),
        interaction_terms = c("barthag_logit_diff:round", "seed_diff:round")
    )
}

#' Score the bracket a model would have filled in for a past tournament
#'
#' Simulates the deterministic posterior-mean bracket for a completed season
#' and scores it against the actual results (1-2-4-8-16-32 points).
#'
#' @param holdout_teams The season's tournament field.
#' @param holdout_results The season's actual results joined to team features.
#' @param model_results A fitted model bundle.
#' @param model_label Label for the output row.
#' @param holdout_year The season.
#' @param draws Posterior draws to use.
#'
#' @return A one-row tibble with bracket score and correct picks.
#' @keywords internal
score_holdout_bracket <- function(holdout_teams, holdout_results, model_results, model_label, holdout_year, draws) {
    simulated_bracket <- simulate_full_bracket(
        all_teams = holdout_teams,
        model_results = model_results,
        draws = draws,
        log_matchups = FALSE,
        log_stage_progress = FALSE
    )
    predicted_matchups <- flatten_matchup_results(simulated_bracket) %>%
        dplyr::mutate(Year = as.character(holdout_year))
    actual_lookup <- holdout_results %>%
        dplyr::select(Year, region, round, game_index, winner)
    score_bracket_against_results(predicted_matchups, actual_lookup)$summary %>%
        dplyr::mutate(model = model_label, year = as.character(holdout_year))
}
