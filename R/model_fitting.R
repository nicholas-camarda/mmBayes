library(dplyr)
library(logger)

#' Require the Bayesian modeling packages
#'
#' @return Invisibly validates that required Bayesian packages are installed.
#' @keywords internal
require_bayesian_packages <- function() {
    required <- c("rstanarm", "bayesplot", "loo")
    missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]

    if (length(missing) > 0) {
        stop_with_message(
            sprintf(
                "Missing required Bayesian packages: %s. Install them before running mmBayes.",
                paste(missing, collapse = ", ")
            )
        )
    }
}

#' Build scaling statistics for matchup predictors
#'
#' @param data A matchup-level modeling table.
#' @param predictor_columns Predictor columns requested for the model.
#'
#' @return A named list of centering and scaling values for continuous predictors.
#' @keywords internal
build_scaling_reference <- function(data, predictor_columns) {
    scaled_columns <- intersect(continuous_matchup_diff_columns(), predictor_columns)

    purrr::map(scaled_columns, function(column_name) {
        values <- suppressWarnings(as.numeric(data[[column_name]]))
        center <- mean(values, na.rm = TRUE)
        scale <- stats::sd(values, na.rm = TRUE)
        if (!is.finite(center)) {
            center <- 0
        }
        if (!is.finite(scale) || scale == 0) {
            scale <- 1
        }
        list(center = center, scale = scale)
    }) %>%
        stats::setNames(scaled_columns)
}

#' Apply a scaling reference to matchup predictors
#'
#' @param data A matchup-level data frame.
#' @param scaling_reference A named scaling reference produced by
#'   [build_scaling_reference()].
#'
#' @return The input data with scaled continuous predictors.
#' @keywords internal
apply_scaling_reference <- function(data, scaling_reference) {
    for (column_name in names(scaling_reference)) {
        if (!column_name %in% names(data)) {
            next
        }
        center <- scaling_reference[[column_name]]$center
        scale <- scaling_reference[[column_name]]$scale
        data[[column_name]] <- (safe_numeric(data[[column_name]]) - center) / scale
    }

    data
}

#' Validate matchup rows before modeling or prediction
#'
#' @param data A matchup-level data frame.
#' @param predictor_columns Predictor columns required by the model.
#' @param require_outcome Whether `actual_outcome` must be present and binary.
#'
#' @return `TRUE` if validation passes.
#' @keywords internal
validate_matchup_rows <- function(data, predictor_columns, require_outcome = TRUE) {
    required_cols <- c("Year", "round", "same_conf", predictor_columns)
    if (require_outcome) {
        required_cols <- c(required_cols, "actual_outcome")
    }

    missing_cols <- setdiff(unique(required_cols), names(data))
    if (length(missing_cols) > 0) {
        stop_with_message(
            sprintf("Missing required matchup columns: %s", paste(missing_cols, collapse = ", "))
        )
    }

    if (require_outcome && any(!data$actual_outcome %in% c(0, 1))) {
        stop_with_message("Historical matchup rows must contain binary actual_outcome values")
    }

    TRUE
}

#' Prepare matchup rows for fitting or prediction
#'
#' @param data A matchup-level data frame.
#' @param predictor_columns Predictor columns required by the model.
#' @param scaling_reference Optional existing scaling reference.
#' @param require_outcome Whether the rows must include observed outcomes.
#'
#' @return A list with prepared data and the scaling reference used.
#' @keywords internal
prepare_model_data <- function(data, predictor_columns, scaling_reference = NULL, require_outcome = TRUE) {
    validate_matchup_rows(data, predictor_columns, require_outcome = require_outcome)

    prepared_data <- data %>%
        dplyr::mutate(
            round = factor(as.character(round), levels = round_levels()),
            same_conf = as.integer(same_conf)
        )

    scaling_reference <- scaling_reference %||% build_scaling_reference(prepared_data, predictor_columns)
    prepared_data <- apply_scaling_reference(prepared_data, scaling_reference)

    list(
        data = prepared_data,
        scaling_reference = scaling_reference
    )
}

#' Build the matchup-model formula
#'
#' @param predictor_columns Predictor columns to include in the model.
#'
#' @return A model formula for `actual_outcome`.
#' @keywords internal
build_model_formula <- function(predictor_columns) {
    predictor_terms <- predictor_columns[predictor_columns != "round"]
    formula_terms <- c("round", predictor_terms[predictor_terms != "round"])

    stats::as.formula(
        paste(
            "actual_outcome ~",
            paste(sprintf("`%s`", formula_terms), collapse = " + ")
        )
    )
}

#' Configure priors for the matchup model
#'
#' @return A list of prior specifications for fixed effects and the intercept.
#' @export
configure_priors <- function() {
    require_bayesian_packages()

    list(
        fixed = rstanarm::normal(0, 1.5, autoscale = FALSE),
        intercept = rstanarm::normal(0, 2.5, autoscale = TRUE)
    )
}

#' Fit the tournament matchup model
#'
#' @param historical_matchups A matchup-level historical training table.
#' @param predictor_columns Predictor columns to include in the model.
#' @param random_seed Random seed used during fitting.
#'
#' @return A list containing the fitted Bayesian model, formula, predictors,
#'   scaling reference, and diagnostics.
#' @export
fit_tournament_model <- function(historical_matchups, predictor_columns, random_seed = 123) {
    require_bayesian_packages()
    logger::log_info("Starting matchup-level Bayesian model fitting")
    set.seed(random_seed)

    prepared <- prepare_model_data(historical_matchups, predictor_columns, require_outcome = TRUE)
    formula_input <- build_model_formula(predictor_columns)
    chains <- getOption("mmBayes.stan_chains", 4L)
    iter <- getOption("mmBayes.stan_iter", 2000L)
    refresh <- getOption("mmBayes.stan_refresh", 0L)
    priors <- configure_priors()

    logger::log_info("Stan settings: chains={chains}, iter={iter}")
    model <- rstanarm::stan_glm(
        formula = formula_input,
        data = prepared$data,
        family = stats::binomial(link = "logit"),
        prior = priors$fixed,
        prior_intercept = priors$intercept,
        chains = chains,
        iter = iter,
        seed = random_seed,
        refresh = refresh
    )

    list(
        engine = "bayes",
        model = model,
        formula = formula_input,
        predictor_columns = predictor_columns,
        scaling_reference = prepared$scaling_reference,
        diagnostics = perform_model_diagnostics(model, "bayes")
    )
}

#' Compute diagnostics for a fitted model
#'
#' @param model A fitted model object.
#' @param engine The modeling engine label.
#'
#' @return A list containing summary, LOO, and posterior predictive diagnostics.
#' @export
perform_model_diagnostics <- function(model, engine = "bayes") {
    posterior_draws <- nrow(as.matrix(model))
    nreps <- max(1L, min(10L, posterior_draws))

    list(
        engine = engine,
        summary = summary(model),
        loo = loo::loo(model),
        pp_check = bayesplot::pp_check(model, plotfun = "stat", stat = "mean", nreps = nreps)
    )
}

#' Convert joined actual results back into matchup rows
#'
#' @param actual_results A game-results table with team A and team B features
#'   already joined.
#'
#' @return A matchup-level table suitable for holdout scoring.
#' @keywords internal
actual_results_to_matchup_rows <- function(actual_results) {
    if (nrow(actual_results) == 0) {
        return(tibble::tibble())
    }

    purrr::pmap_dfr(
        actual_results,
        function(...) {
            row <- tibble::as_tibble(list(...))
            team_a <- tibble::tibble(
                Year = row$Year,
                Team = row$teamA,
                Seed = row$Seed_teamA,
                Region = row$Region_teamA,
                Conf = row$Conf_teamA
            )
            team_b <- tibble::tibble(
                Year = row$Year,
                Team = row$teamB,
                Seed = row$Seed_teamB,
                Region = row$Region_teamB,
                Conf = row$Conf_teamB
            )

            for (feature_name in pre_tournament_feature_columns()) {
                team_a[[feature_name]] <- row[[paste0(feature_name, "_teamA")]]
                team_b[[feature_name]] <- row[[paste0(feature_name, "_teamB")]]
            }

            build_matchup_feature_row(
                team_a = team_a,
                team_b = team_b,
                round_name = row$round,
                actual_outcome = as.numeric(row$winner == row$teamA),
                metadata = list(
                    Year = row$Year,
                    region = row$region,
                    game_index = row$game_index,
                    teamA = row$teamA,
                    teamB = row$teamB,
                    winner = row$winner
                )
            )
        }
    ) %>%
        dplyr::mutate(round = factor(round, levels = round_levels()))
}

#' Predict posterior win probabilities for matchup rows
#'
#' @param matchup_rows A matchup-level prediction table.
#' @param model_results A fitted model result bundle.
#' @param draws Number of posterior draws to return.
#'
#' @return A draw-by-game matrix of posterior expected win probabilities.
#' @keywords internal
predict_matchup_rows <- function(matchup_rows, model_results, draws = 1000) {
    prepared <- prepare_model_data(
        matchup_rows,
        predictor_columns = model_results$predictor_columns,
        scaling_reference = model_results$scaling_reference,
        require_outcome = FALSE
    )
    predictor_terms <- all.vars(stats::delete.response(stats::terms(model_results$formula)))
    prediction_data <- prepared$data %>%
        dplyr::select(dplyr::all_of(unique(predictor_terms)))

    if ("round" %in% names(prediction_data) && "round" %in% names(model_results$model$xlevels)) {
        training_round_levels <- model_results$model$xlevels$round
        round_values <- as.character(prediction_data$round)
        round_values[!round_values %in% training_round_levels & round_values == "First Four"] <- "Round of 64"
        prediction_data$round <- factor(round_values, levels = training_round_levels)
    }

    posterior_draws <- nrow(as.matrix(model_results$model))
    draws <- max(1L, min(as.integer(draws), posterior_draws))

    rstanarm::posterior_epred(
        object = model_results$model,
        newdata = prediction_data,
        draws = draws
    )
}

#' Run a rolling tournament-year backtest
#'
#' @param historical_teams A historical team feature table.
#' @param historical_actual_results A historical actual-results table with joined
#'   team features.
#' @param predictor_columns Predictor columns to include in each backtest fit.
#' @param random_seed Random seed used during repeated fitting.
#' @param draws Number of posterior draws used for scoring and simulation.
#'
#' @return A list of year-level metrics, predictions, calibration, bracket
#'   scores, and summary metrics.
#' @export
run_rolling_backtest <- function(historical_teams, historical_actual_results, predictor_columns, random_seed = 123, draws = 1000) {
    years <- sort(unique(as.character(historical_actual_results$Year)))
    if (length(years) < 2) {
        return(list(
            yearly_metrics = tibble::tibble(),
            predictions = tibble::tibble(),
            calibration = tibble::tibble(),
            bracket_scores = tibble::tibble(),
            summary = tibble::tibble()
        ))
    }

    yearly_metrics <- vector("list", length(years) - 1L)
    prediction_rows <- vector("list", length(years) - 1L)
    bracket_scores <- vector("list", length(years) - 1L)

    raw_result_columns <- c("Year", "region", "round", "game_index", "teamA", "teamB", "teamA_seed", "teamB_seed", "winner")

    for (index in 2:length(years)) {
        holdout_year <- years[[index]]
        train_years <- years[seq_len(index - 1L)]

        train_teams <- historical_teams %>%
            dplyr::filter(Year %in% train_years)
        train_results <- historical_actual_results %>%
            dplyr::filter(Year %in% train_years) %>%
            dplyr::select(dplyr::all_of(raw_result_columns)) %>%
            dplyr::distinct()

        holdout_teams <- historical_teams %>%
            dplyr::filter(Year == holdout_year)
        holdout_results <- historical_actual_results %>%
            dplyr::filter(Year == holdout_year)

        model_results <- fit_tournament_model(
            historical_matchups = build_explicit_matchup_history(train_teams, train_results),
            predictor_columns = predictor_columns,
            random_seed = random_seed
        )

        holdout_rows <- actual_results_to_matchup_rows(holdout_results)
        draw_matrix <- predict_matchup_rows(holdout_rows, model_results, draws = draws)
        predicted_prob <- colMeans(draw_matrix)

        prediction_frame <- holdout_rows %>%
            dplyr::mutate(
                predicted_prob = predicted_prob,
                holdout_year = holdout_year
            )
        metrics <- compute_binary_metrics(predicted_prob, holdout_rows$actual_outcome) %>%
            dplyr::mutate(year = holdout_year)

        simulated_bracket <- simulate_full_bracket(
            all_teams = holdout_teams,
            model_results = model_results,
            draws = draws
        )
        predicted_matchups <- flatten_matchup_results(simulated_bracket)
        actual_lookup <- holdout_results %>%
            dplyr::select(Year, region, round, game_index, winner)
        bracket_score <- score_bracket_against_results(predicted_matchups, actual_lookup)$summary %>%
            dplyr::mutate(year = holdout_year)

        yearly_metrics[[index - 1L]] <- metrics
        prediction_rows[[index - 1L]] <- prediction_frame
        bracket_scores[[index - 1L]] <- bracket_score
    }

    all_predictions <- dplyr::bind_rows(prediction_rows)
    calibration <- summarize_calibration(all_predictions)
    yearly_metrics_tbl <- dplyr::bind_rows(yearly_metrics)
    bracket_scores_tbl <- dplyr::bind_rows(bracket_scores)

    summary_tbl <- yearly_metrics_tbl %>%
        dplyr::summarise(
            mean_log_loss = mean(log_loss),
            mean_brier = mean(brier),
            mean_accuracy = mean(accuracy)
        ) %>%
        dplyr::mutate(
            mean_bracket_score = mean(bracket_scores_tbl$bracket_score),
            mean_correct_picks = mean(bracket_scores_tbl$correct_picks)
        )

    list(
        yearly_metrics = yearly_metrics_tbl,
        predictions = all_predictions,
        calibration = calibration,
        bracket_scores = bracket_scores_tbl,
        summary = summary_tbl
    )
}
