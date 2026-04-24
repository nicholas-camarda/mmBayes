library(dplyr)
library(logger)
library(purrr)
library(tibble)

#' Return the default ensemble configuration
#'
#' @return A named list of ensemble defaults.
#' @keywords internal
default_ensemble_config <- function() {
    list(
        enabled = TRUE,
        component_engines = c("stan_glm", "bart"),
        combiner = "logit_weight",
        primary_acceptance_target = "bracket_score",
        log_loss_guardrail_delta = 0.005,
        brier_guardrail_delta = 0.005
    )
}

#' Resolve ensemble configuration defaults
#'
#' @param ensemble_config User configuration under `model$ensemble`.
#'
#' @return A complete ensemble configuration list.
#' @keywords internal
resolve_ensemble_config <- function(ensemble_config = NULL) {
    config <- merge_config_lists(default_ensemble_config(), ensemble_config %||% list())
    config$enabled <- isTRUE(config$enabled)
    config$component_engines <- as.character(unlist(config$component_engines %||% character()))
    config$combiner <- as.character(config$combiner %||% "logit_weight")
    config$primary_acceptance_target <- as.character(config$primary_acceptance_target %||% "bracket_score")
    config$log_loss_guardrail_delta <- safe_numeric(config$log_loss_guardrail_delta %||% 0.005, default = 0.005)
    config$brier_guardrail_delta <- safe_numeric(config$brier_guardrail_delta %||% 0.005, default = 0.005)
    config
}

#' Check whether ensemble bracket picking is enabled
#'
#' @param config Project configuration list.
#'
#' @return `TRUE` when the ensemble should be the primary matchup model.
#' @keywords internal
ensemble_enabled <- function(config) {
    isTRUE((config$model$ensemble %||% list())$enabled)
}

#' Validate ensemble model configuration
#'
#' @param config Project configuration list.
#'
#' @return The input config, invisibly.
#' @keywords internal
validate_ensemble_config <- function(config) {
    ensemble_config <- resolve_ensemble_config(config$model$ensemble %||% list())
    if (!isTRUE(ensemble_config$enabled)) {
        return(invisible(config))
    }

    if (!identical(sort(ensemble_config$component_engines), c("bart", "stan_glm"))) {
        stop_with_message("Ensemble mode requires component_engines to contain exactly stan_glm and bart")
    }
    if (!identical(ensemble_config$combiner, "logit_weight")) {
        stop_with_message("Ensemble mode currently supports only combiner: logit_weight")
    }
    if (!identical(ensemble_config$primary_acceptance_target, "bracket_score")) {
        stop_with_message("Ensemble mode currently supports only primary_acceptance_target: bracket_score")
    }
    if (is.null(config$model$bart) || !is.list(config$model$bart)) {
        stop_with_message("Ensemble mode requires model$bart settings")
    }
    if (ensemble_config$log_loss_guardrail_delta < 0 || ensemble_config$brier_guardrail_delta < 0) {
        stop_with_message("Ensemble guardrail deltas must be non-negative")
    }

    invisible(config)
}

#' Clip probabilities away from 0 and 1
#'
#' @param probability Numeric probabilities.
#'
#' @return Clipped numeric probabilities.
#' @keywords internal
clip_ensemble_probability <- function(probability) {
    pmin(pmax(as.numeric(probability), 1e-6), 1 - 1e-6)
}

#' Combine component probabilities on the logit scale
#'
#' @param stan_probability Stan GLM probabilities.
#' @param bart_probability BART probabilities.
#' @param intercept Optional intercept.
#' @param weight_stan_glm Weight assigned to Stan GLM.
#'
#' @return Combined probabilities.
#' @keywords internal
combine_ensemble_probabilities <- function(stan_probability, bart_probability, intercept = 0, weight_stan_glm = 0.5) {
    stats::plogis(
        intercept +
            weight_stan_glm * stats::qlogis(clip_ensemble_probability(stan_probability)) +
            (1 - weight_stan_glm) * stats::qlogis(clip_ensemble_probability(bart_probability))
    )
}

#' Combine component draw matrices on the logit scale
#'
#' @param stan_draws Draw-by-game Stan probability matrix.
#' @param bart_draws Draw-by-game BART probability matrix.
#' @param intercept Optional intercept.
#' @param weight_stan_glm Weight assigned to Stan GLM.
#'
#' @return A draw-by-game combined probability matrix.
#' @keywords internal
combine_ensemble_draws <- function(stan_draws, bart_draws, intercept = 0, weight_stan_glm = 0.5) {
    draw_count <- min(nrow(stan_draws), nrow(bart_draws))
    if (draw_count < 1L) {
        stop_with_message("No posterior draws were available for ensemble prediction")
    }
    game_count <- ncol(stan_draws)
    combined <- combine_ensemble_probabilities(
        stan_probability = stan_draws[seq_len(draw_count), , drop = FALSE],
        bart_probability = bart_draws[seq_len(draw_count), , drop = FALSE],
        intercept = intercept,
        weight_stan_glm = weight_stan_glm
    )
    matrix(combined, nrow = draw_count, ncol = game_count)
}

with_ensemble_prediction_seed <- function(seed, expression) {
    had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
    on.exit({
        if (had_seed) {
            assign(".Random.seed", old_seed, envir = .GlobalEnv)
        } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
            rm(".Random.seed", envir = .GlobalEnv)
        }
    }, add = TRUE)
    set.seed(as.integer(seed))
    force(expression)
}

#' Fit the constrained logit-scale ensemble combiner
#'
#' @param predictions Holdout prediction table with `stan_glm`, `bart`, and
#'   `actual_outcome`.
#'
#' @return A one-row tibble containing the fitted weights.
#' @keywords internal
fit_constrained_ensemble_combiner <- function(predictions) {
    required_columns <- c("actual_outcome", "stan_glm", "bart")
    missing_columns <- setdiff(required_columns, names(predictions))
    if (length(missing_columns) > 0) {
        stop_with_message(sprintf("Combiner training data is missing: %s", paste(missing_columns, collapse = ", ")))
    }
    if (nrow(predictions) < 10L) {
        stop_with_message("Combiner training requires at least 10 holdout rows")
    }

    objective <- function(parameters) {
        predicted <- combine_ensemble_probabilities(
            stan_probability = predictions$stan_glm,
            bart_probability = predictions$bart,
            intercept = parameters[[1]],
            weight_stan_glm = stats::plogis(parameters[[2]])
        )
        actual <- as.numeric(predictions$actual_outcome)
        -mean(actual * log(predicted) + (1 - actual) * log(1 - predicted))
    }

    fit <- stats::optim(c(0, 0), objective, method = "BFGS", control = list(maxit = 1000))
    if (!identical(fit$convergence, 0L)) {
        stop_with_message(sprintf("Constrained ensemble combiner did not converge: %s", fit$message %||% fit$convergence))
    }

    weight_stan_glm <- as.numeric(stats::plogis(fit$par[[2]]))
    tibble::tibble(
        intercept = as.numeric(fit$par[[1]]),
        weight_stan_glm = weight_stan_glm,
        weight_bart = 1 - weight_stan_glm,
        training_rows = nrow(predictions),
        training_years = paste(sort(unique(predictions$holdout_year)), collapse = ","),
        training_log_loss = as.numeric(fit$value)
    )
}

#' Predict matchup rows with an ensemble model bundle
#'
#' @param matchup_rows Matchup prediction rows.
#' @param model_results Ensemble model bundle.
#' @param draws Number of posterior draws.
#'
#' @return A draw-by-game probability matrix.
#' @keywords internal
predict_ensemble_matchup_rows <- function(matchup_rows, model_results, draws = 1000) {
    components <- model_results$components %||% list()
    stan_model <- components$stan_glm %||% stop_with_message("Ensemble model is missing the Stan GLM component")
    bart_model <- components$bart %||% stop_with_message("Ensemble model is missing the BART component")
    combiner <- model_results$combiner %||% list()
    prediction_seed <- as.integer(model_results$prediction_seed %||% model_results$random_seed %||% 1L)
    stan_draws <- with_ensemble_prediction_seed(
        prediction_seed,
        predict_matchup_rows(matchup_rows, stan_model, draws = draws)
    )
    bart_draws <- with_ensemble_prediction_seed(
        prediction_seed + 1L,
        predict_matchup_rows(matchup_rows, bart_model, draws = draws)
    )

    combine_ensemble_draws(
        stan_draws = stan_draws,
        bart_draws = bart_draws,
        intercept = safe_numeric(combiner$intercept %||% 0, default = 0),
        weight_stan_glm = safe_numeric(combiner$weight_stan_glm %||% 0.5, default = 0.5)
    )
}

ensemble_metric_rows <- function(predictions, model_label, probability_column) {
    predictions %>%
        dplyr::group_by(holdout_year) %>%
        dplyr::group_modify(~ compute_binary_metrics(.x[[probability_column]], .x$actual_outcome)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(model = model_label, year = holdout_year) %>%
        dplyr::select(model, year, log_loss, brier, accuracy)
}

score_ensemble_validation_bracket <- function(holdout_teams, holdout_results, model_results, model_label, holdout_year, draws) {
    simulated_bracket <- simulate_full_bracket(
        all_teams = holdout_teams,
        model_results = model_results,
        draws = draws,
        log_matchups = FALSE,
        log_stage_progress = FALSE
    )
    predicted_matchups <- flatten_matchup_results(simulated_bracket) %>%
        dplyr::mutate(Year = holdout_year)
    actual_lookup <- holdout_results %>%
        dplyr::select(Year, region, round, game_index, winner)
    score_bracket_against_results(predicted_matchups, actual_lookup)$summary %>%
        dplyr::mutate(model = model_label, year = holdout_year)
}

#' Fit rolling holdout ensemble evidence and combiner weights
#'
#' @param historical_teams Historical team feature rows.
#' @param historical_actual_results Joined historical game results.
#' @param predictor_columns Matchup predictor columns.
#' @param bart_config BART settings.
#' @param random_seed Seed for component fits.
#' @param draws Posterior draw budget.
#' @param cache_dir Model cache directory.
#' @param use_cache Whether component model cache is enabled.
#' @param interaction_terms Stan interaction terms.
#' @param prior_type Stan prior type.
#' @param ensemble_config Ensemble guardrail settings.
#'
#' @return A list with global combiner, validation backtest, and gate details.
#' @keywords internal
fit_ensemble_combiner_validation <- function(historical_teams,
                                             historical_actual_results,
                                             predictor_columns,
                                             bart_config = NULL,
                                             random_seed = 123,
                                             draws = 1000,
                                             cache_dir = NULL,
                                             use_cache = TRUE,
                                             interaction_terms = NULL,
                                             prior_type = "normal",
                                             ensemble_config = NULL) {
    ensemble_config <- resolve_ensemble_config(ensemble_config)
    years <- sort(unique(historical_actual_results$Year))
    if (length(years) < 3L) {
        stop_with_message("At least three historical years are required to train the ensemble combiner")
    }

    cache_path <- NULL
    if (isTRUE(use_cache) && !is.null(cache_dir)) {
        dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
        cache_key <- build_model_fit_cache_key(
            historical_matchups = historical_actual_results,
            predictor_columns = predictor_columns,
            random_seed = random_seed,
            include_diagnostics = FALSE,
            model_label = "ensemble_combiner",
            interaction_terms = interaction_terms,
            prior_type = prior_type,
            engine = "ensemble",
            engine_config = list(bart = resolve_bart_config(bart_config), ensemble = ensemble_config),
            encoding_metadata = NULL
        )
        cache_path <- build_model_fit_cache_path(cache_dir, cache_key, model_label = "ensemble_combiner")
        if (file.exists(cache_path)) {
            logger::log_info("Loading cached ensemble combiner validation from {cache_path}")
            cached_result <- readRDS(cache_path)
            cached_result$cache_path <- cache_path
            return(cached_result)
        }
    }

    raw_result_columns <- c("Year", "region", "round", "game_index", "teamA", "teamB", "teamA_seed", "teamB_seed", "winner")
    year_contexts <- vector("list", length(years) - 1L)
    names(year_contexts) <- as.character(years[-1])
    prediction_rows <- vector("list", length(year_contexts))
    component_scores <- vector("list", length(year_contexts) * 2L)

    for (index in 2:length(years)) {
        holdout_year <- years[[index]]
        train_years <- years[seq_len(index - 1L)]
        logger::log_info("Fitting ensemble validation components for holdout {holdout_year}")

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
        holdout_rows <- actual_results_to_matchup_rows(holdout_results)

        stan_model <- fit_tournament_model(
            historical_matchups = build_explicit_matchup_history(train_teams, train_results),
            predictor_columns = predictor_columns,
            engine = "stan_glm",
            bart_config = bart_config,
            random_seed = random_seed,
            include_diagnostics = FALSE,
            cache_dir = cache_dir,
            use_cache = use_cache,
            interaction_terms = interaction_terms,
            prior_type = prior_type
        )
        bart_model <- fit_tournament_model(
            historical_matchups = build_explicit_matchup_history(train_teams, train_results),
            predictor_columns = predictor_columns,
            engine = "bart",
            bart_config = bart_config,
            random_seed = random_seed,
            include_diagnostics = FALSE,
            cache_dir = cache_dir,
            use_cache = use_cache,
            interaction_terms = NULL,
            prior_type = "normal"
        )

        stan_draws <- predict_matchup_rows(holdout_rows, stan_model, draws = draws)
        bart_draws <- predict_matchup_rows(holdout_rows, bart_model, draws = draws)
        context_key <- as.character(holdout_year)
        year_contexts[[context_key]] <- list(
            year = holdout_year,
            holdout_teams = holdout_teams,
            holdout_results = holdout_results,
            stan_model = stan_model,
            bart_model = bart_model
        )
        prediction_rows[[context_key]] <- holdout_rows %>%
            dplyr::transmute(
                holdout_year = holdout_year,
                region = region,
                round = as.character(round),
                game_index = game_index,
                teamA = teamA,
                teamB = teamB,
                winner = winner,
                actual_outcome = actual_outcome,
                stan_glm = colMeans(stan_draws),
                bart = colMeans(bart_draws),
                equal_weight = colMeans(combine_ensemble_draws(stan_draws, bart_draws, intercept = 0, weight_stan_glm = 0.5))
            )

        component_scores[[(index - 2L) * 2L + 1L]] <- score_ensemble_validation_bracket(
            holdout_teams,
            holdout_results,
            stan_model,
            "stan_glm",
            holdout_year,
            draws
        )
        component_scores[[(index - 2L) * 2L + 2L]] <- score_ensemble_validation_bracket(
            holdout_teams,
            holdout_results,
            bart_model,
            "bart",
            holdout_year,
            draws
        )
    }

    all_predictions <- dplyr::bind_rows(prediction_rows)
    global_combiner <- fit_constrained_ensemble_combiner(all_predictions)
    parameter_rows <- purrr::map_dfr(
        sort(unique(all_predictions$holdout_year)),
        function(holdout_year) {
            all_predictions %>%
                dplyr::filter(holdout_year != !!holdout_year) %>%
                fit_constrained_ensemble_combiner() %>%
                dplyr::mutate(holdout_year = holdout_year, .before = 1)
        }
    )
    all_predictions <- all_predictions %>%
        dplyr::left_join(parameter_rows %>% dplyr::select(holdout_year, intercept, weight_stan_glm), by = "holdout_year") %>%
        dplyr::mutate(
            learned_ensemble = combine_ensemble_probabilities(stan_glm, bart, intercept = intercept, weight_stan_glm = weight_stan_glm)
        )

    probability_metrics <- dplyr::bind_rows(
        ensemble_metric_rows(all_predictions, "stan_glm", "stan_glm"),
        ensemble_metric_rows(all_predictions, "bart", "bart"),
        ensemble_metric_rows(all_predictions, "equal_weight", "equal_weight"),
        ensemble_metric_rows(all_predictions, "learned_ensemble", "learned_ensemble")
    )

    ensemble_scores <- purrr::map_dfr(
        sort(unique(all_predictions$holdout_year)),
        function(holdout_year) {
            context <- year_contexts[[as.character(holdout_year)]]
            params <- parameter_rows %>% dplyr::filter(holdout_year == !!holdout_year)
            equal_model <- list(
                engine = "ensemble",
                components = list(stan_glm = context$stan_model, bart = context$bart_model),
                combiner = list(intercept = 0, weight_stan_glm = 0.5),
                predictor_columns = predictor_columns
            )
            learned_model <- list(
                engine = "ensemble",
                components = list(stan_glm = context$stan_model, bart = context$bart_model),
                combiner = list(intercept = params$intercept[[1]], weight_stan_glm = params$weight_stan_glm[[1]]),
                predictor_columns = predictor_columns
            )
            dplyr::bind_rows(
                score_ensemble_validation_bracket(context$holdout_teams, context$holdout_results, equal_model, "equal_weight", holdout_year, draws),
                score_ensemble_validation_bracket(context$holdout_teams, context$holdout_results, learned_model, "learned_ensemble", holdout_year, draws)
            )
        }
    )

    bracket_scores <- dplyr::bind_rows(component_scores, ensemble_scores) %>%
        dplyr::select(model, year, bracket_score, correct_picks, total_games)
    yearly_metrics <- probability_metrics %>%
        dplyr::left_join(bracket_scores, by = c("model", "year")) %>%
        dplyr::arrange(year, factor(model, levels = c("stan_glm", "bart", "equal_weight", "learned_ensemble")))
    summary_tbl <- yearly_metrics %>%
        dplyr::group_by(model) %>%
        dplyr::summarise(
            mean_log_loss = mean(log_loss),
            mean_brier = mean(brier),
            mean_accuracy = mean(accuracy),
            mean_bracket_score = mean(bracket_score),
            mean_correct_picks = mean(correct_picks),
            .groups = "drop"
        ) %>%
        dplyr::arrange(factor(model, levels = c("stan_glm", "bart", "equal_weight", "learned_ensemble")))

    learned <- summary_tbl %>% dplyr::filter(model == "learned_ensemble")
    components <- summary_tbl %>% dplyr::filter(model %in% c("stan_glm", "bart"))
    baselines <- summary_tbl %>% dplyr::filter(model %in% c("stan_glm", "bart", "equal_weight"))
    gate_conditions <- tibble::tibble(
        condition = c(
            "learned_mean_bracket_score_strictly_beats_all_baselines",
            "learned_mean_correct_picks_no_worse_than_best_component",
            "learned_log_loss_within_guardrail",
            "learned_brier_within_guardrail"
        ),
        threshold = c(
            max(baselines$mean_bracket_score),
            max(components$mean_correct_picks),
            min(components$mean_log_loss) + ensemble_config$log_loss_guardrail_delta,
            min(components$mean_brier) + ensemble_config$brier_guardrail_delta
        ),
        observed = c(
            learned$mean_bracket_score[[1]],
            learned$mean_correct_picks[[1]],
            learned$mean_log_loss[[1]],
            learned$mean_brier[[1]]
        ),
        passed = c(
            learned$mean_bracket_score[[1]] > max(baselines$mean_bracket_score),
            learned$mean_correct_picks[[1]] >= max(components$mean_correct_picks),
            learned$mean_log_loss[[1]] <= min(components$mean_log_loss) + ensemble_config$log_loss_guardrail_delta,
            learned$mean_brier[[1]] <= min(components$mean_brier) + ensemble_config$brier_guardrail_delta
        )
    )
    gate_passed <- all(gate_conditions$passed)
    if (!isTRUE(gate_passed)) {
        stop_with_message("Ensemble validation gate failed; production ensemble model will not be fit")
    }

    result <- list(
        combiner = as.list(global_combiner[1, , drop = FALSE]),
        parameter_rows = parameter_rows,
        backtest = list(
            yearly_metrics = yearly_metrics %>% dplyr::filter(model == "learned_ensemble") %>% dplyr::select(-model),
            predictions = all_predictions,
            calibration = summarize_calibration(all_predictions %>% dplyr::mutate(predicted_prob = learned_ensemble)),
            round_summary = summarize_prediction_round_performance(all_predictions %>% dplyr::mutate(predicted_prob = learned_ensemble)),
            bracket_scores = bracket_scores %>% dplyr::filter(model == "learned_ensemble") %>% dplyr::select(-model),
            summary = summary_tbl %>% dplyr::filter(model == "learned_ensemble") %>% dplyr::select(-model)
        ),
        comparison = list(
            summary = summary_tbl,
            yearly_metrics = yearly_metrics,
            gate_conditions = gate_conditions,
            gate_passed = gate_passed,
            guardrails = list(
                log_loss_max_delta = ensemble_config$log_loss_guardrail_delta,
                brier_max_delta = ensemble_config$brier_guardrail_delta
            )
        ),
        cache_path = cache_path
    )

    if (!is.null(cache_path)) {
        saveRDS(result, cache_path)
        logger::log_info("Saved ensemble combiner validation cache to {cache_path}")
    }

    result
}

#' Fit the primary ensemble tournament model
#'
#' @param data Loaded tournament data bundle.
#' @param predictor_columns Matchup predictor columns.
#' @param bart_config BART settings.
#' @param random_seed Seed for component fits.
#' @param draws Posterior draw budget.
#' @param cache_dir Model cache directory.
#' @param use_cache Whether cache should be used.
#' @param interaction_terms Stan interaction terms.
#' @param prior_type Stan prior type.
#' @param ensemble_config Ensemble settings.
#'
#' @return A fitted ensemble model bundle.
#' @export
fit_ensemble_tournament_model <- function(data,
                                          predictor_columns,
                                          bart_config = NULL,
                                          random_seed = 123,
                                          draws = 1000,
                                          cache_dir = NULL,
                                          use_cache = TRUE,
                                          interaction_terms = NULL,
                                          prior_type = "normal",
                                          ensemble_config = NULL) {
    ensemble_config <- resolve_ensemble_config(ensemble_config)
    validation <- fit_ensemble_combiner_validation(
        historical_teams = data$historical_teams,
        historical_actual_results = data$historical_actual_results,
        predictor_columns = predictor_columns,
        bart_config = bart_config,
        random_seed = random_seed,
        draws = draws,
        cache_dir = cache_dir,
        use_cache = use_cache,
        interaction_terms = interaction_terms,
        prior_type = prior_type,
        ensemble_config = ensemble_config
    )

    logger::log_info("Fitting full-history Stan GLM component for primary ensemble")
    stan_model <- fit_tournament_model(
        historical_matchups = data$historical_matchups,
        predictor_columns = predictor_columns,
        engine = "stan_glm",
        bart_config = bart_config,
        random_seed = random_seed,
        include_diagnostics = FALSE,
        cache_dir = cache_dir,
        use_cache = use_cache,
        interaction_terms = interaction_terms,
        prior_type = prior_type
    )
    logger::log_info("Fitting full-history BART component for primary ensemble")
    bart_model <- fit_tournament_model(
        historical_matchups = data$historical_matchups,
        predictor_columns = predictor_columns,
        engine = "bart",
        bart_config = bart_config,
        random_seed = random_seed,
        include_diagnostics = FALSE,
        cache_dir = cache_dir,
        use_cache = use_cache,
        interaction_terms = NULL,
        prior_type = "normal"
    )

    list(
        engine = "ensemble",
        engine_label = "Stan GLM + BART ensemble",
        components = list(stan_glm = stan_model, bart = bart_model),
        combiner = list(
            type = ensemble_config$combiner,
            intercept = safe_numeric(validation$combiner$intercept %||% 0, default = 0),
            weight_stan_glm = safe_numeric(validation$combiner$weight_stan_glm %||% 0.5, default = 0.5),
            weight_bart = safe_numeric(validation$combiner$weight_bart %||% 0.5, default = 0.5)
        ),
        predictor_columns = predictor_columns,
        interaction_terms = interaction_terms %||% character(0),
        prior_type = prior_type %||% "normal",
        bart_config = resolve_bart_config(bart_config),
        ensemble_config = ensemble_config,
        validation = validation,
        random_seed = random_seed,
        prediction_seed = random_seed,
        cache_path = validation$cache_path %||% NULL
    )
}
