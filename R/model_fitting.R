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
#' @param scaled_columns Optional subset of predictor columns to standardize.
#'
#' @return A named list of centering and scaling values for continuous predictors.
#' @keywords internal
build_scaling_reference <- function(data, predictor_columns, scaled_columns = NULL) {
    scaled_columns <- scaled_columns %||% intersect(continuous_matchup_diff_columns(), predictor_columns)

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
#' @param scaled_columns Optional subset of predictor columns to standardize.
#' @param require_outcome Whether the rows must include observed outcomes.
#'
#' @return A list with prepared data and the scaling reference used.
#' @keywords internal
prepare_model_data <- function(data, predictor_columns, scaling_reference = NULL, scaled_columns = NULL, require_outcome = TRUE) {
    validate_matchup_rows(data, predictor_columns, require_outcome = require_outcome)

    prepared_data <- data %>%
        dplyr::mutate(
            round = factor(as.character(round), levels = round_levels()),
            same_conf = as.integer(same_conf)
        )

    scaling_reference <- scaling_reference %||% build_scaling_reference(prepared_data, predictor_columns, scaled_columns = scaled_columns)
    prepared_data <- apply_scaling_reference(prepared_data, scaling_reference)

    list(
        data = prepared_data,
        scaling_reference = scaling_reference
    )
}

#' Build the matchup-model formula
#'
#' @param predictor_columns Predictor columns to include in the model.
#' @param outcome_column Outcome column name.
#' @param interaction_terms Optional character vector of interaction terms to
#'   append to the formula (e.g. `"barthag_logit_diff:seed_diff"`).  Each
#'   element is inserted verbatim so the caller is responsible for correct R
#'   formula syntax.
#'
#' @return A model formula for the requested outcome.
#' @keywords internal
build_model_formula <- function(predictor_columns, outcome_column = "actual_outcome", interaction_terms = NULL) {
    predictor_terms <- predictor_columns[predictor_columns != "round"]
    formula_terms <- c("round", predictor_terms[predictor_terms != "round"])

    base_formula <- paste(
        outcome_column,
        "~",
        paste(sprintf("`%s`", formula_terms), collapse = " + ")
    )

    if (!is.null(interaction_terms) && length(interaction_terms) > 0) {
        interaction_string <- paste(interaction_terms, collapse = " + ")
        base_formula <- paste(base_formula, "+", interaction_string)
    }

    stats::as.formula(base_formula)
}

#' Configure priors for the matchup model
#'
#' @param prior_type Prior type to use for fixed effects.  `"normal"` (default)
#'   uses a weakly informative Normal(0, 1.5) prior.  `"hs"` uses a regularized
#'   horseshoe prior that automatically shrinks less-informative coefficients
#'   toward zero, which can improve calibration when many correlated predictors
#'   are present.  Any other value raises an error.
#'
#' @return A list of prior specifications for fixed effects and the intercept.
#' @export
configure_priors <- function(prior_type = "normal") {
    require_bayesian_packages()

    valid_prior_types <- c("normal", "hs")

    if (!prior_type %in% valid_prior_types) {
        stop_with_message(sprintf(
            "Unknown prior_type '%s'. Must be one of: %s.",
            prior_type,
            paste(valid_prior_types, collapse = ", ")
        ))
    }

    if (identical(prior_type, "hs")) {
        list(
            fixed = rstanarm::hs(df = 1, df_global = 1, slab_df = 4, slab_scale = 2.5, autoscale = FALSE),
            intercept = rstanarm::normal(0, 2.5, autoscale = TRUE)
        )
    } else {
        list(
            fixed = rstanarm::normal(0, 1.5, autoscale = FALSE),
            intercept = rstanarm::normal(0, 2.5, autoscale = TRUE)
        )
    }
}

#' Build a cache key for a fitted model bundle
#'
#' @param historical_matchups Historical matchup training rows.
#' @param predictor_columns Predictor columns used for fitting.
#' @param random_seed Random seed used during fitting.
#' @param include_diagnostics Whether diagnostics are included in the bundle.
#' @param model_label Short model label used to namespace the cache entry.
#' @param interaction_terms Optional interaction terms included in the formula.
#' @param prior_type Prior type used during fitting.
#'
#' @return A cache key suitable for a file name.
#' @keywords internal
build_model_fit_cache_key <- function(historical_matchups, predictor_columns, random_seed, include_diagnostics, model_label = "matchup", interaction_terms = NULL, prior_type = "normal") {
    cache_signature <- list(
        model_label = model_label,
        historical_matchups = historical_matchups,
        predictor_columns = predictor_columns,
        random_seed = random_seed,
        include_diagnostics = include_diagnostics,
        interaction_terms = interaction_terms %||% character(0),
        prior_type = prior_type %||% "normal"
    )

    signature_file <- tempfile(fileext = ".rds")
    on.exit(unlink(signature_file), add = TRUE)
    saveRDS(cache_signature, signature_file)
    tools::md5sum(signature_file)[[1]]
}

#' Build the cache file path for a fitted model bundle
#'
#' @param cache_dir Directory used to store cached fitted models.
#' @param cache_key Cache key returned by [build_model_fit_cache_key()].
#' @param model_label Short model label used to namespace the cache file.
#'
#' @return A cache file path.
#' @keywords internal
build_model_fit_cache_path <- function(cache_dir, cache_key, model_label = "matchup") {
    file.path(cache_dir, paste0(model_label, "_fit_", cache_key, ".rds"))
}

#' Fit the tournament matchup model
#'
#' @param historical_matchups A matchup-level historical training table.
#' @param predictor_columns Predictor columns to include in the model.
#' @param random_seed Random seed used during fitting.
#' @param include_diagnostics Whether to compute expensive post-fit diagnostics.
#' @param cache_dir Optional directory used to store and reload fitted model bundles.
#' @param use_cache Whether to reuse a cached fit when available.
#' @param interaction_terms Optional character vector of interaction terms to
#'   append to the model formula (e.g. `"barthag_logit_diff:seed_diff"`).
#' @param prior_type Prior type for fixed effects: `"normal"` (default) or
#'   `"hs"` for a regularized horseshoe prior.
#'
#' @return A list containing the fitted Bayesian model, formula, predictors,
#'   scaling reference, and diagnostics.
#' @export
fit_tournament_model <- function(historical_matchups, predictor_columns, random_seed = 123, include_diagnostics = TRUE, cache_dir = NULL, use_cache = TRUE, interaction_terms = NULL, prior_type = "normal") {
    require_bayesian_packages()

    cache_path <- NULL
    if (isTRUE(use_cache) && !is.null(cache_dir)) {
        dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
        cache_key <- build_model_fit_cache_key(historical_matchups, predictor_columns, random_seed, include_diagnostics, model_label = "matchup", interaction_terms = interaction_terms, prior_type = prior_type)
        cache_path <- build_model_fit_cache_path(cache_dir, cache_key, model_label = "matchup")
        if (file.exists(cache_path)) {
            logger::log_info("Loading cached matchup-level model fit from {cache_path}")
            cached_result <- readRDS(cache_path)
            cached_result$cache_path <- cache_path
            return(cached_result)
        }
    }

    logger::log_info("Starting matchup-level Bayesian model fitting")
    set.seed(random_seed)

    prepared <- prepare_model_data(historical_matchups, predictor_columns, require_outcome = TRUE)
    formula_input <- build_model_formula(predictor_columns, outcome_column = "actual_outcome", interaction_terms = interaction_terms)
    chains <- getOption("mmBayes.stan_chains", 4L)
    iter <- getOption("mmBayes.stan_iter", 2000L)
    refresh <- getOption("mmBayes.stan_refresh", 0L)
    effective_prior_type <- prior_type %||% "normal"
    priors <- configure_priors(prior_type = effective_prior_type)

    logger::log_info("Stan settings: chains={chains}, iter={iter}, prior_type={effective_prior_type}")
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

    fit_result <- list(
        engine = "bayes",
        model = model,
        formula = formula_input,
        predictor_columns = predictor_columns,
        interaction_terms = interaction_terms %||% character(0),
        prior_type = effective_prior_type,
        scaling_reference = prepared$scaling_reference,
        diagnostics = if (isTRUE(include_diagnostics)) perform_model_diagnostics(model, "bayes") else NULL,
        cache_path = cache_path
    )

    if (!is.null(cache_path)) {
        saveRDS(fit_result, cache_path)
        logger::log_info("Saved matchup-level model fit cache to {cache_path}")
    }

    fit_result
}

#' Return the default predictor columns for total-points modeling
#'
#' @return A character vector of matchup-total predictor names.
#' @keywords internal
default_total_points_predictors <- function() {
    c(
        "round",
        "same_conf",
        "seed_sum",
        "seed_gap",
        "barthag_logit_sum",
        "barthag_logit_gap",
        "AdjOE_sum",
        "AdjDE_sum",
        "WAB_sum",
        "TOR_sum",
        "TORD_sum",
        "ORB_sum",
        "DRB_sum",
        "3P%_sum",
        "3P%D_sum",
        "Adj T._mean",
        "Adj T._gap"
    )
}

#' Validate total-points rows before modeling or prediction
#'
#' @param data A matchup-level data frame.
#' @param predictor_columns Predictor columns required by the model.
#' @param require_outcome Whether `total_points` must be present and finite.
#'
#' @return `TRUE` if validation passes.
#' @keywords internal
validate_total_points_rows <- function(data, predictor_columns, require_outcome = TRUE) {
    required_cols <- c("Year", "round", "same_conf", predictor_columns)
    if (require_outcome) {
        required_cols <- c(required_cols, "total_points")
    }

    missing_cols <- setdiff(unique(required_cols), names(data))
    if (length(missing_cols) > 0) {
        stop_with_message(
            sprintf("Missing required total-points columns: %s", paste(missing_cols, collapse = ", "))
        )
    }

    if (isTRUE(require_outcome)) {
        total_points <- suppressWarnings(as.numeric(data$total_points))
        if (any(!is.finite(total_points))) {
            stop_with_message("Historical total-points rows must contain finite total_points values")
        }
    }

    TRUE
}

#' Prepare total-points rows for fitting or prediction
#'
#' @param data A total-points modeling table.
#' @param predictor_columns Predictor columns required by the model.
#' @param scaling_reference Optional existing scaling reference.
#' @param require_outcome Whether the rows must include observed total points.
#'
#' @return A list with prepared data and the scaling reference used.
#' @keywords internal
prepare_total_points_model_data <- function(data, predictor_columns, scaling_reference = NULL, require_outcome = TRUE) {
    validate_total_points_rows(data, predictor_columns, require_outcome = require_outcome)

    scaled_columns <- setdiff(predictor_columns, c("round", "same_conf"))
    prepared_data <- data %>%
        dplyr::mutate(
            round = factor(as.character(round), levels = round_levels()),
            same_conf = as.integer(same_conf),
            total_points = safe_numeric(total_points, default = NA_real_)
        )

    scaling_reference <- scaling_reference %||% build_scaling_reference(prepared_data, predictor_columns, scaled_columns = scaled_columns)
    prepared_data <- apply_scaling_reference(prepared_data, scaling_reference)

    list(
        data = prepared_data,
        scaling_reference = scaling_reference
    )
}

#' Configure priors for the total-points model
#'
#' @return A list of prior specifications for fixed effects and the intercept.
#' @keywords internal
configure_total_points_priors <- function() {
    require_bayesian_packages()

    list(
        fixed = rstanarm::normal(0, 3, autoscale = FALSE),
        intercept = rstanarm::normal(140, 30, autoscale = FALSE)
    )
}

#' Build historical total-points training rows
#'
#' @param actual_results A score-bearing historical tournament result table with
#'   team A and team B features already joined.
#'
#' @return A matchup-level training table for total-points modeling.
#' @export
build_total_points_training_rows <- function(actual_results) {
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

            build_total_points_feature_row(
                team_a = team_a,
                team_b = team_b,
                round_name = row$round,
                total_points = row$total_points,
                metadata = list(
                    Year = row$Year,
                    region = row$region,
                    game_index = row$game_index,
                    teamA = row$teamA,
                    teamB = row$teamB
                )
            )
        }
    ) %>%
        dplyr::mutate(round = factor(round, levels = round_levels()))
}

#' Fit the tournament total-points model
#'
#' @param historical_total_points A matchup-level historical total-points table.
#' @param predictor_columns Predictor columns to include in the model.
#' @param random_seed Random seed used during fitting.
#' @param include_diagnostics Whether to compute expensive post-fit diagnostics.
#' @param cache_dir Optional directory used to store and reload fitted model bundles.
#' @param use_cache Whether to reuse a cached fit when available.
#'
#' @return A list containing the fitted Bayesian model, formula, predictors,
#'   scaling reference, and diagnostics.
#' @export
fit_total_points_model <- function(historical_total_points, predictor_columns = default_total_points_predictors(), random_seed = 123, include_diagnostics = FALSE, cache_dir = NULL, use_cache = TRUE) {
    require_bayesian_packages()

    cache_path <- NULL
    if (isTRUE(use_cache) && !is.null(cache_dir)) {
        dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
        cache_key <- build_model_fit_cache_key(historical_total_points, predictor_columns, random_seed, include_diagnostics, model_label = "totals")
        cache_path <- build_model_fit_cache_path(cache_dir, cache_key, model_label = "totals")
        if (file.exists(cache_path)) {
            logger::log_info("Loading cached total-points model fit from {cache_path}")
            cached_result <- readRDS(cache_path)
            cached_result$cache_path <- cache_path
            return(cached_result)
        }
    }

    logger::log_info("Starting total-points Bayesian model fitting")
    set.seed(random_seed)

    prepared <- prepare_total_points_model_data(historical_total_points, predictor_columns, require_outcome = TRUE)
    formula_input <- build_model_formula(predictor_columns, outcome_column = "total_points")
    chains <- getOption("mmBayes.stan_chains", 4L)
    iter <- getOption("mmBayes.stan_iter", 2000L)
    refresh <- getOption("mmBayes.stan_refresh", 0L)
    priors <- configure_total_points_priors()

    logger::log_info("Stan total-points settings: chains={chains}, iter={iter}")
    model <- rstanarm::stan_glm(
        formula = formula_input,
        data = prepared$data,
        family = stats::gaussian(),
        prior = priors$fixed,
        prior_intercept = priors$intercept,
        chains = chains,
        iter = iter,
        seed = random_seed,
        refresh = refresh
    )

    fit_result <- list(
        engine = "bayes",
        outcome = "total_points",
        model = model,
        formula = formula_input,
        predictor_columns = predictor_columns,
        scaling_reference = prepared$scaling_reference,
        diagnostics = if (isTRUE(include_diagnostics)) perform_model_diagnostics(model, "bayes_gaussian") else NULL,
        cache_path = cache_path
    )

    if (!is.null(cache_path)) {
        saveRDS(fit_result, cache_path)
        logger::log_info("Saved total-points model fit cache to {cache_path}")
    }

    fit_result
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
        scaled_columns = names(model_results$scaling_reference %||% list()),
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

    if (anyNA(prediction_data)) {
        missing_columns <- names(prediction_data)[colSums(is.na(prediction_data)) > 0]
        stop_with_message(
            sprintf(
                "Prediction rows contain missing values for: %s",
                paste(missing_columns, collapse = ", ")
            )
        )
    }

    posterior_draws <- nrow(as.matrix(model_results$model))
    draws <- max(1L, min(as.integer(draws), posterior_draws))

    rstanarm::posterior_epred(
        object = model_results$model,
        newdata = prediction_data,
        draws = draws
    )
}

#' Predict posterior total points for matchup rows
#'
#' @param matchup_rows A matchup-level prediction table for total points.
#' @param model_results A fitted total-points model result bundle.
#' @param draws Number of posterior predictive draws to return.
#'
#' @return A draw-by-game matrix of posterior predictive total points.
#' @export
predict_total_points_rows <- function(matchup_rows, model_results, draws = 1000) {
    prepared <- prepare_total_points_model_data(
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

    if (anyNA(prediction_data)) {
        missing_columns <- names(prediction_data)[colSums(is.na(prediction_data)) > 0]
        stop_with_message(
            sprintf(
                "Total-points prediction rows contain missing values for: %s",
                paste(missing_columns, collapse = ", ")
            )
        )
    }

    posterior_draws <- nrow(as.matrix(model_results$model))
    draws <- max(1L, min(as.integer(draws), posterior_draws))

    prediction_matrix <- rstanarm::posterior_predict(
        object = model_results$model,
        newdata = prediction_data,
        draws = draws
    )
    prediction_matrix <- as.matrix(prediction_matrix)

    prediction_matrix[prediction_matrix < 0] <- 0
    prediction_matrix
}

#' Run a rolling tournament-year backtest
#'
#' @param historical_teams A historical team feature table.
#' @param historical_actual_results A historical actual-results table with joined
#'   team features.
#' @param predictor_columns Predictor columns to include in each backtest fit.
#' @param random_seed Random seed used during repeated fitting.
#' @param draws Number of posterior draws used for scoring and simulation.
#' @param interaction_terms Optional character vector of interaction terms
#'   forwarded to [fit_tournament_model()].
#' @param prior_type Prior type forwarded to [fit_tournament_model()].
#'
#' @return A list of year-level metrics, predictions, calibration, bracket
#'   scores, and summary metrics.
#' @export
run_rolling_backtest <- function(historical_teams, historical_actual_results, predictor_columns, random_seed = 123, draws = 1000, cache_dir = NULL, use_cache = TRUE, interaction_terms = NULL, prior_type = "normal") {
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
        logger::log_info("Running rolling backtest holdout for {holdout_year}")

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
            random_seed = random_seed,
            include_diagnostics = FALSE,
            cache_dir = cache_dir,
            use_cache = use_cache,
            interaction_terms = interaction_terms,
            prior_type = prior_type
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
            draws = draws,
            log_matchups = FALSE
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

#' Compare two model configurations using rolling backtest
#'
#' Fits and evaluates two model configurations on the same rolling holdout
#' splits and returns a side-by-side comparison of their backtest metrics.
#' This is the primary tool for verifying that a model change improves
#' performance before promoting it to production.
#'
#' @param historical_teams A historical team feature table.
#' @param historical_actual_results A historical actual-results table with
#'   joined team features.
#' @param config_a A list with elements `predictor_columns`, `interaction_terms`
#'   (optional), and `prior_type` (optional) for the baseline model.
#' @param config_b A list with the same structure for the candidate model.
#' @param config_a_label Human-readable label for the baseline model.
#' @param config_b_label Human-readable label for the candidate model.
#' @param random_seed Random seed forwarded to each backtest.
#' @param draws Number of posterior draws forwarded to each backtest.
#' @param cache_dir Optional cache directory forwarded to each backtest.
#'
#' @return A list with elements `config_a`, `config_b`, `delta`, and
#'   `comparison_table`.  `delta` is a named numeric vector of
#'   `(config_b metric) - (config_a metric)`; negative deltas for log-loss and
#'   Brier score and positive deltas for accuracy and bracket-score indicate
#'   improvement.
#' @export
compare_model_configurations <- function(
    historical_teams,
    historical_actual_results,
    config_a,
    config_b,
    config_a_label = "Baseline",
    config_b_label = "Candidate",
    random_seed = 123,
    draws = 500,
    cache_dir = NULL
) {
    run_backtest <- function(cfg, label) {
        logger::log_info("compare_model_configurations: running backtest for '{label}'")
        run_rolling_backtest(
            historical_teams = historical_teams,
            historical_actual_results = historical_actual_results,
            predictor_columns = cfg$predictor_columns,
            random_seed = random_seed,
            draws = draws,
            cache_dir = cache_dir,
            use_cache = !is.null(cache_dir),
            interaction_terms = cfg$interaction_terms,
            prior_type = cfg$prior_type %||% "normal"
        )
    }

    backtest_a <- run_backtest(config_a, config_a_label)
    backtest_b <- run_backtest(config_b, config_b_label)

    summarize_backtest <- function(bt, label) {
        if (!model_quality_has_backtest(bt)) {
            return(tibble::tibble(
                label = label,
                mean_log_loss = NA_real_,
                mean_brier = NA_real_,
                mean_accuracy = NA_real_,
                mean_bracket_score = NA_real_,
                mean_correct_picks = NA_real_
            ))
        }
        dplyr::mutate(bt$summary, label = label) %>%
            dplyr::select(label, dplyr::everything())
    }

    summary_a <- summarize_backtest(backtest_a, config_a_label)
    summary_b <- summarize_backtest(backtest_b, config_b_label)

    metric_cols <- c("mean_log_loss", "mean_brier", "mean_accuracy", "mean_bracket_score", "mean_correct_picks")
    delta <- stats::setNames(
        vapply(metric_cols, function(m) {
            safe_numeric(summary_b[[m]][[1]]) - safe_numeric(summary_a[[m]][[1]])
        }, numeric(1)),
        metric_cols
    )

    comparison_table <- dplyr::bind_rows(summary_a, summary_b)

    list(
        config_a = list(label = config_a_label, summary = summary_a, backtest = backtest_a),
        config_b = list(label = config_b_label, summary = summary_b, backtest = backtest_b),
        delta = delta,
        comparison_table = comparison_table
    )
}
