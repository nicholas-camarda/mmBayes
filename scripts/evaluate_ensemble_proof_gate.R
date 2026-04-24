#!/usr/bin/env Rscript

suppressPackageStartupMessages({
    library(dplyr)
    library(jsonlite)
    library(logger)
    library(purrr)
    library(tibble)
    library(tidyr)
})

if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("pkgload is required to run the ensemble proof-gate evaluator")
}

pkgload::load_all(".", export_all = TRUE, helpers = TRUE, quiet = TRUE)

clip_probability <- function(probability) {
    pmin(pmax(as.numeric(probability), 1e-6), 1 - 1e-6)
}

logit_probability <- function(probability) {
    stats::qlogis(clip_probability(probability))
}

combine_probabilities <- function(stan_probability, bart_probability, intercept = 0, weight = 0.5) {
    stats::plogis(
        intercept +
            weight * logit_probability(stan_probability) +
            (1 - weight) * logit_probability(bart_probability)
    )
}

combine_draw_matrices <- function(stan_draws, bart_draws, intercept = 0, weight = 0.5) {
    draw_count <- min(nrow(stan_draws), nrow(bart_draws))
    if (draw_count < 1L) {
        stop_with_message("No posterior draws were available for ensemble prediction")
    }
    combined <- combine_probabilities(
        stan_probability = stan_draws[seq_len(draw_count), , drop = FALSE],
        bart_probability = bart_draws[seq_len(draw_count), , drop = FALSE],
        intercept = intercept,
        weight = weight
    )
    matrix(combined, nrow = draw_count)
}

fit_constrained_combiner <- function(predictions) {
    required_columns <- c("actual_outcome", "stan_glm", "bart")
    missing_columns <- setdiff(required_columns, names(predictions))
    if (length(missing_columns) > 0) {
        stop_with_message(sprintf("Combiner training data is missing: %s", paste(missing_columns, collapse = ", ")))
    }
    if (nrow(predictions) < 10L) {
        stop_with_message("Combiner training requires at least 10 holdout rows")
    }

    objective <- function(parameters) {
        intercept <- parameters[[1]]
        weight <- stats::plogis(parameters[[2]])
        predicted <- combine_probabilities(
            stan_probability = predictions$stan_glm,
            bart_probability = predictions$bart,
            intercept = intercept,
            weight = weight
        )
        actual <- as.numeric(predictions$actual_outcome)
        -mean(actual * log(predicted) + (1 - actual) * log(1 - predicted))
    }

    fit <- stats::optim(c(0, 0), objective, method = "BFGS", control = list(maxit = 1000))
    if (!identical(fit$convergence, 0L)) {
        stop_with_message(sprintf("Constrained ensemble combiner did not converge: %s", fit$message %||% fit$convergence))
    }

    tibble::tibble(
        intercept = as.numeric(fit$par[[1]]),
        weight_stan_glm = as.numeric(stats::plogis(fit$par[[2]])),
        weight_bart = 1 - as.numeric(stats::plogis(fit$par[[2]])),
        training_rows = nrow(predictions),
        training_years = paste(sort(unique(predictions$holdout_year)), collapse = ","),
        training_log_loss = as.numeric(fit$value)
    )
}

calculate_ensemble_win_probabilities <- function(teamA, teamB, round_name, stan_model, bart_model, draws, intercept = 0, weight = 0.5) {
    prediction_row <- prepare_prediction_row(teamA, teamB, round_name)
    stan_draws <- predict_matchup_rows(prediction_row, stan_model, draws = draws)
    bart_draws <- predict_matchup_rows(prediction_row, bart_model, draws = draws)
    draw_probs <- as.numeric(combine_draw_matrices(stan_draws, bart_draws, intercept = intercept, weight = weight)[, 1])

    list(
        mean = mean(draw_probs),
        ci_lower = as.numeric(stats::quantile(draw_probs, 0.025)),
        ci_upper = as.numeric(stats::quantile(draw_probs, 0.975)),
        sd = stats::sd(draw_probs),
        draws = draw_probs,
        model_mean = mean(draw_probs),
        line_prob = NA_real_,
        blend_weight = 0,
        used_betting_line = FALSE
    )
}

simulate_matchup_ensemble <- function(teamA, teamB, round_name, matchup_number, stan_model, bart_model, draws, intercept = 0, weight = 0.5, deterministic = TRUE, posterior_draw_index = NULL) {
    win_probs <- calculate_ensemble_win_probabilities(
        teamA = teamA,
        teamB = teamB,
        round_name = round_name,
        stan_model = stan_model,
        bart_model = bart_model,
        draws = draws,
        intercept = intercept,
        weight = weight
    )
    matchup_stats <- generate_matchup_stats(teamA, teamB, win_probs)
    decision_prob_A <- if (isTRUE(deterministic) || is.null(posterior_draw_index)) {
        win_probs$mean
    } else {
        draw_index <- ((as.integer(posterior_draw_index) - 1L) %% length(win_probs$draws)) + 1L
        win_probs$draws[[draw_index]]
    }
    team_a_wins <- if (isTRUE(deterministic)) {
        win_probs$mean >= 0.5
    } else {
        stats::rbinom(1L, 1L, prob = decision_prob_A) == 1L
    }
    winner <- if (team_a_wins) teamA$Team[1] else teamB$Team[1]

    create_matchup_result(
        teamA = teamA,
        teamB = teamB,
        round_name = round_name,
        matchup_number = matchup_number,
        win_probs = win_probs,
        matchup_stats = matchup_stats,
        winner = winner,
        decision_prob_A = decision_prob_A,
        posterior_draw_index = posterior_draw_index %||% NA_integer_
    )
}

simulate_round_ensemble <- function(teams, round_name, stan_model, bart_model, draws, intercept = 0, weight = 0.5, deterministic = TRUE, posterior_draw_index = NULL) {
    if (nrow(teams) %% 2 != 0) {
        stop_with_message(sprintf("%s received an odd number of teams", round_name))
    }

    round_results <- vector("list", nrow(teams) / 2)
    advancing_teams <- vector("list", length(round_results))

    for (i in seq_along(round_results)) {
        team_a_idx <- (i * 2) - 1
        team_b_idx <- i * 2
        team_a <- teams[team_a_idx, , drop = FALSE]
        team_b <- teams[team_b_idx, , drop = FALSE]
        matchup <- simulate_matchup_ensemble(
            teamA = team_a,
            teamB = team_b,
            round_name = round_name,
            matchup_number = i,
            stan_model = stan_model,
            bart_model = bart_model,
            draws = draws,
            intercept = intercept,
            weight = weight,
            deterministic = deterministic,
            posterior_draw_index = posterior_draw_index
        )
        round_results[[i]] <- matchup
        advancing_teams[[i]] <- if (matchup$winner[1] == team_a$Team[1]) team_a else team_b
    }

    list(
        results = dplyr::bind_rows(round_results),
        winners = dplyr::bind_rows(advancing_teams)
    )
}

resolve_play_in_games_ensemble <- function(region_teams, stan_model, bart_model, draws, actual_play_in_results = NULL, intercept = 0, weight = 0.5, deterministic = TRUE, posterior_draw_index = NULL) {
    region_name <- unique(region_teams$Region)[1]
    duplicate_counts <- table(region_teams$Seed)
    dup_seeds <- as.integer(names(duplicate_counts[duplicate_counts > 1]))

    if (length(dup_seeds) == 0) {
        region_teams$Assigned_Seed <- region_teams$Seed
        return(list(teams = region_teams, results = tibble::tibble()))
    }

    play_in_results <- vector("list", length(dup_seeds))
    survivors <- region_teams

    for (i in seq_along(dup_seeds)) {
        seed_value <- dup_seeds[[i]]
        duplicate_rows <- survivors %>%
            dplyr::filter(Seed == seed_value)

        if (nrow(duplicate_rows) != 2) {
            stop_with_message("Play-in simulation expects exactly two teams for each duplicate seed")
        }

        actual_result <- actual_play_in_results %||%
            tibble::tibble(play_in_region = character(), slot_seed = integer(), winner = character())
        actual_result <- actual_result %>%
            dplyr::filter(play_in_region == region_name, slot_seed == seed_value)

        if (nrow(actual_result) > 1) {
            stop_with_message(sprintf("Multiple actual First Four results found for %s %s-seed slot", region_name, seed_value))
        }

        if (nrow(actual_result) == 1) {
            actual_winner <- canonicalize_team_name(actual_result$winner[[1]])
            if (!actual_winner %in% duplicate_rows$Team) {
                stop_with_message(sprintf("Actual First Four winner %s does not match the active %s %s-seed slot", actual_winner, region_name, seed_value))
            }
            win_probs <- calculate_ensemble_win_probabilities(
                duplicate_rows[1, , drop = FALSE],
                duplicate_rows[2, , drop = FALSE],
                "First Four",
                stan_model,
                bart_model,
                draws,
                intercept = intercept,
                weight = weight
            )
            matchup <- create_matchup_result(
                duplicate_rows[1, , drop = FALSE],
                duplicate_rows[2, , drop = FALSE],
                "First Four",
                i,
                win_probs,
                generate_matchup_stats(duplicate_rows[1, , drop = FALSE], duplicate_rows[2, , drop = FALSE], win_probs),
                actual_winner,
                decision_prob_A = win_probs$mean,
                posterior_draw_index = NA_integer_
            )
        } else {
            matchup <- simulate_matchup_ensemble(
                duplicate_rows[1, , drop = FALSE],
                duplicate_rows[2, , drop = FALSE],
                "First Four",
                i,
                stan_model,
                bart_model,
                draws,
                intercept = intercept,
                weight = weight,
                deterministic = deterministic,
                posterior_draw_index = posterior_draw_index
            )
        }

        play_in_results[[i]] <- matchup
        losing_team <- if (matchup$winner[1] == duplicate_rows$Team[1]) duplicate_rows$Team[2] else duplicate_rows$Team[1]
        survivors <- survivors %>%
            dplyr::filter(Team != losing_team)
    }

    survivors$Assigned_Seed <- survivors$Seed
    list(teams = survivors, results = dplyr::bind_rows(play_in_results))
}

simulate_region_ensemble <- function(region_teams, stan_model, bart_model, draws, actual_play_in_results = NULL, intercept = 0, weight = 0.5, deterministic = TRUE, posterior_draw_index = NULL) {
    region_name <- unique(region_teams$Region)[1]
    play_in <- resolve_play_in_games_ensemble(
        region_teams = region_teams,
        stan_model = stan_model,
        bart_model = bart_model,
        draws = draws,
        actual_play_in_results = actual_play_in_results,
        intercept = intercept,
        weight = weight,
        deterministic = deterministic,
        posterior_draw_index = posterior_draw_index
    )
    region_teams <- play_in$teams

    if (nrow(region_teams) != 16) {
        stop_with_message(sprintf("Region %s must contain exactly 16 teams after play-in resolution", region_name))
    }

    remaining <- region_teams %>%
        dplyr::arrange(match(Assigned_Seed, standard_bracket_order()))
    round_results <- list()
    if (nrow(play_in$results) > 0) {
        round_results[["First Four"]] <- play_in$results
    }

    for (round_name in c("Round of 64", "Round of 32", "Sweet 16", "Elite 8")) {
        simulated_round <- simulate_round_ensemble(
            teams = remaining,
            round_name = round_name,
            stan_model = stan_model,
            bart_model = bart_model,
            draws = draws,
            intercept = intercept,
            weight = weight,
            deterministic = deterministic,
            posterior_draw_index = posterior_draw_index
        )
        round_results[[round_name]] <- simulated_round$results
        remaining <- simulated_round$winners
    }

    list(region = region_name, results = round_results, champion = remaining[1, , drop = FALSE])
}

simulate_final_four_ensemble <- function(region_champions, stan_model, bart_model, draws, intercept = 0, weight = 0.5, deterministic = TRUE, posterior_draw_index = NULL) {
    semifinal1 <- simulate_matchup_ensemble(region_champions$South, region_champions$West, "Final Four", 1, stan_model, bart_model, draws, intercept = intercept, weight = weight, deterministic = deterministic, posterior_draw_index = posterior_draw_index)
    semifinal2 <- simulate_matchup_ensemble(region_champions$East, region_champions$Midwest, "Final Four", 2, stan_model, bart_model, draws, intercept = intercept, weight = weight, deterministic = deterministic, posterior_draw_index = posterior_draw_index)

    championship_team_a <- if (semifinal1$winner[1] == region_champions$South$Team[1]) region_champions$South else region_champions$West
    championship_team_b <- if (semifinal2$winner[1] == region_champions$East$Team[1]) region_champions$East else region_champions$Midwest
    championship <- simulate_matchup_ensemble(championship_team_a, championship_team_b, "Championship", 1, stan_model, bart_model, draws, intercept = intercept, weight = weight, deterministic = deterministic, posterior_draw_index = posterior_draw_index)
    champion <- if (championship$winner[1] == championship_team_a$Team[1]) championship_team_a else championship_team_b

    list(
        semifinalists = region_champions,
        semifinals = list(semifinal1, semifinal2),
        championship = championship,
        champion = champion
    )
}

simulate_full_bracket_ensemble <- function(all_teams, stan_model, bart_model, draws, actual_play_in_results = NULL, intercept = 0, weight = 0.5, deterministic = TRUE, posterior_draw_index = NULL) {
    regions <- c("East", "West", "South", "Midwest")
    region_counts <- table(all_teams$Region)
    for (region in regions) {
        if (region_counts[[region]] %||% 0 == 0) {
            stop_with_message(sprintf("Missing teams for region %s", region))
        }
    }

    region_results <- purrr::map(
        stats::setNames(regions, regions),
        function(region_name) {
            simulate_region_ensemble(
                region_teams = dplyr::filter(all_teams, Region == region_name),
                stan_model = stan_model,
                bart_model = bart_model,
                draws = draws,
                actual_play_in_results = actual_play_in_results,
                intercept = intercept,
                weight = weight,
                deterministic = deterministic,
                posterior_draw_index = posterior_draw_index
            )
        }
    )
    champions <- purrr::map(region_results, "champion")
    final_four <- simulate_final_four_ensemble(champions, stan_model, bart_model, draws, intercept = intercept, weight = weight, deterministic = deterministic, posterior_draw_index = posterior_draw_index)

    list(region_results = region_results, final_four = final_four)
}

score_component_bracket <- function(holdout_teams, holdout_results, model_results, model_label, holdout_year, draws) {
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

score_ensemble_bracket <- function(holdout_teams, holdout_results, stan_model, bart_model, model_label, holdout_year, draws, intercept = 0, weight = 0.5) {
    simulated_bracket <- simulate_full_bracket_ensemble(
        all_teams = holdout_teams,
        stan_model = stan_model,
        bart_model = bart_model,
        draws = draws,
        intercept = intercept,
        weight = weight
    )
    predicted_matchups <- flatten_matchup_results(simulated_bracket) %>%
        dplyr::mutate(Year = holdout_year)
    actual_lookup <- holdout_results %>%
        dplyr::select(Year, region, round, game_index, winner)
    score_bracket_against_results(predicted_matchups, actual_lookup)$summary %>%
        dplyr::mutate(model = model_label, year = holdout_year)
}

metric_rows <- function(predictions, model_label, probability_column) {
    predictions %>%
        dplyr::group_by(holdout_year) %>%
        dplyr::group_modify(~ compute_binary_metrics(.x[[probability_column]], .x$actual_outcome)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(model = model_label, year = holdout_year) %>%
        dplyr::select(model, year, log_loss, brier, accuracy)
}

format_metric <- function(value, digits = 4) {
    if (is.na(value)) {
        return("NA")
    }
    format(round(value, digits), nsmall = digits)
}

write_report <- function(report_path, summary_tbl, yearly_tbl, gate_tbl, parameters_tbl, output_paths, guardrails, gate_passed) {
    learned <- summary_tbl %>%
        dplyr::filter(model == "learned_ensemble")
    lines <- c(
        "# Ensemble Bracket Picker Proof Gate",
        "",
        sprintf("Gate result: **%s**", if (isTRUE(gate_passed)) "PASS" else "FAIL"),
        "",
        "This evaluator is isolated from production configuration, primary simulation routing, dashboard generation, saved-result schemas, publishing, release manifests, product-facing documentation, and tracked HTML snapshots.",
        "",
        "The learned ensemble is evaluated with leave-one-holdout-year-out combiner fitting over real rolling holdout predictions. The component models are fit on historical years preceding each holdout year; the combiner for a given holdout year is fit only on other holdout years' out-of-fold component predictions.",
        "",
        sprintf("Guardrails: learned log loss must be <= best component + %.4f; learned Brier must be <= best component + %.4f.", guardrails$log_loss_max_delta, guardrails$brier_max_delta),
        "",
        "## Summary",
        "",
        paste(capture.output(print(summary_tbl, n = Inf)), collapse = "\n"),
        "",
        "## Gate Conditions",
        "",
        paste(capture.output(print(gate_tbl, n = Inf)), collapse = "\n"),
        "",
        "## Learned Parameters",
        "",
        paste(capture.output(print(parameters_tbl, n = Inf)), collapse = "\n"),
        "",
        "## Output Files",
        "",
        paste(sprintf("- %s: `%s`", names(output_paths), unlist(output_paths)), collapse = "\n"),
        "",
        "## Decision",
        "",
        if (isTRUE(gate_passed)) {
            paste0(
                "The learned ensemble is eligible for production integration. Mean bracket score = ",
                format_metric(learned$mean_bracket_score[[1]]),
                "; mean correct picks = ",
                format_metric(learned$mean_correct_picks[[1]], digits = 2),
                "."
            )
        } else {
            "The learned ensemble is not eligible for production integration. Stop before dashboard, release, schema, routing, or product-doc changes."
        },
        "",
        "## Yearly Metrics",
        "",
        paste(capture.output(print(yearly_tbl, n = Inf)), collapse = "\n")
    )
    writeLines(lines, report_path)
}

config <- load_project_config("config.yml")
data <- load_tournament_data(config)

if (!requireNamespace("BART", quietly = TRUE)) {
    stop_with_message("BART must be installed to run the ensemble proof gate")
}

output_dir <- file.path(path.expand(config$output$path %||% default_runtime_output_root()), "ensemble_proof_gate")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

draws <- as.integer(config$model$n_draws %||% 1000L)
random_seed <- as.integer(config$model$random_seed %||% 42L)
model_cache_dir <- path.expand(config$output$model_cache_path %||% file.path(config$output$path %||% default_runtime_output_root(), "model_cache"))
use_model_cache <- isTRUE(config$output$use_model_cache %||% TRUE)
matchup_predictors <- core_matchup_predictor_columns(config$model$required_predictors)
bart_config <- config$model$bart %||% list()
interaction_terms <- as.character(unlist(config$model$interaction_terms %||% character(0)))
prior_type <- config$model$prior_type %||% "normal"
guardrails <- list(log_loss_max_delta = 0.005, brier_max_delta = 0.005)

years <- sort(unique(data$historical_actual_results$Year))
if (length(years) < 3L) {
    stop_with_message("At least three historical years are required for ensemble proof-gate validation")
}

logger::log_info("Running isolated ensemble proof gate for holdout years: {paste(years[-1], collapse = ', ')}")

year_contexts <- vector("list", length(years) - 1L)
names(year_contexts) <- as.character(years[-1])
prediction_rows <- vector("list", length(year_contexts))
component_bracket_scores <- vector("list", length(year_contexts) * 2L)

raw_result_columns <- c("Year", "region", "round", "game_index", "teamA", "teamB", "teamA_seed", "teamB_seed", "winner")

for (index in 2:length(years)) {
    holdout_year <- years[[index]]
    train_years <- years[seq_len(index - 1L)]
    logger::log_info("Fitting component models for ensemble proof holdout {holdout_year}")

    train_teams <- data$historical_teams %>%
        dplyr::filter(Year %in% train_years)
    train_results <- data$historical_actual_results %>%
        dplyr::filter(Year %in% train_years) %>%
        dplyr::select(dplyr::all_of(raw_result_columns)) %>%
        dplyr::distinct()
    holdout_teams <- data$historical_teams %>%
        dplyr::filter(Year == holdout_year)
    holdout_results <- data$historical_actual_results %>%
        dplyr::filter(Year == holdout_year)
    holdout_rows <- actual_results_to_matchup_rows(holdout_results)

    stan_model <- fit_tournament_model(
        historical_matchups = build_explicit_matchup_history(train_teams, train_results),
        predictor_columns = matchup_predictors,
        engine = "stan_glm",
        bart_config = bart_config,
        random_seed = random_seed,
        include_diagnostics = FALSE,
        cache_dir = model_cache_dir,
        use_cache = use_model_cache,
        interaction_terms = interaction_terms,
        prior_type = prior_type
    )
    bart_model <- fit_tournament_model(
        historical_matchups = build_explicit_matchup_history(train_teams, train_results),
        predictor_columns = matchup_predictors,
        engine = "bart",
        bart_config = bart_config,
        random_seed = random_seed,
        include_diagnostics = FALSE,
        cache_dir = model_cache_dir,
        use_cache = use_model_cache,
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
            bart = colMeans(bart_draws)
        )

    component_bracket_scores[[(index - 2L) * 2L + 1L]] <- score_component_bracket(
        holdout_teams = holdout_teams,
        holdout_results = holdout_results,
        model_results = stan_model,
        model_label = "stan_glm",
        holdout_year = holdout_year,
        draws = draws
    )
    component_bracket_scores[[(index - 2L) * 2L + 2L]] <- score_component_bracket(
        holdout_teams = holdout_teams,
        holdout_results = holdout_results,
        model_results = bart_model,
        model_label = "bart",
        holdout_year = holdout_year,
        draws = draws
    )
}

all_predictions <- dplyr::bind_rows(prediction_rows) %>%
    dplyr::mutate(
        equal_weight = combine_probabilities(stan_glm, bart, intercept = 0, weight = 0.5)
    )

parameter_rows <- purrr::map_dfr(
    sort(unique(all_predictions$holdout_year)),
    function(holdout_year) {
        training_predictions <- all_predictions %>%
            dplyr::filter(holdout_year != !!holdout_year)
        fit_constrained_combiner(training_predictions) %>%
            dplyr::mutate(holdout_year = holdout_year, .before = 1)
    }
)

all_predictions <- all_predictions %>%
    dplyr::left_join(parameter_rows %>% dplyr::select(holdout_year, intercept, weight_stan_glm), by = "holdout_year") %>%
    dplyr::mutate(
        learned_ensemble = combine_probabilities(stan_glm, bart, intercept = intercept, weight = weight_stan_glm)
    )

probability_metrics <- dplyr::bind_rows(
    metric_rows(all_predictions, "stan_glm", "stan_glm"),
    metric_rows(all_predictions, "bart", "bart"),
    metric_rows(all_predictions, "equal_weight", "equal_weight"),
    metric_rows(all_predictions, "learned_ensemble", "learned_ensemble")
)

ensemble_bracket_scores <- purrr::map_dfr(
    sort(unique(all_predictions$holdout_year)),
    function(holdout_year) {
        context <- year_contexts[[as.character(holdout_year)]]
        params <- parameter_rows %>%
            dplyr::filter(holdout_year == !!holdout_year)
        dplyr::bind_rows(
            score_ensemble_bracket(
                holdout_teams = context$holdout_teams,
                holdout_results = context$holdout_results,
                stan_model = context$stan_model,
                bart_model = context$bart_model,
                model_label = "equal_weight",
                holdout_year = holdout_year,
                draws = draws,
                intercept = 0,
                weight = 0.5
            ),
            score_ensemble_bracket(
                holdout_teams = context$holdout_teams,
                holdout_results = context$holdout_results,
                stan_model = context$stan_model,
                bart_model = context$bart_model,
                model_label = "learned_ensemble",
                holdout_year = holdout_year,
                draws = draws,
                intercept = params$intercept[[1]],
                weight = params$weight_stan_glm[[1]]
            )
        )
    }
)

bracket_scores <- dplyr::bind_rows(component_bracket_scores, ensemble_bracket_scores) %>%
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

learned <- summary_tbl %>%
    dplyr::filter(model == "learned_ensemble")
component_summary <- summary_tbl %>%
    dplyr::filter(model %in% c("stan_glm", "bart"))
all_baseline_summary <- summary_tbl %>%
    dplyr::filter(model %in% c("stan_glm", "bart", "equal_weight"))

gate_tbl <- tibble::tibble(
    condition = c(
        "learned_mean_bracket_score_strictly_beats_all_baselines",
        "learned_mean_correct_picks_no_worse_than_best_component",
        "learned_log_loss_within_guardrail",
        "learned_brier_within_guardrail"
    ),
    threshold = c(
        max(all_baseline_summary$mean_bracket_score),
        max(component_summary$mean_correct_picks),
        min(component_summary$mean_log_loss) + guardrails$log_loss_max_delta,
        min(component_summary$mean_brier) + guardrails$brier_max_delta
    ),
    observed = c(
        learned$mean_bracket_score[[1]],
        learned$mean_correct_picks[[1]],
        learned$mean_log_loss[[1]],
        learned$mean_brier[[1]]
    ),
    passed = c(
        learned$mean_bracket_score[[1]] > max(all_baseline_summary$mean_bracket_score),
        learned$mean_correct_picks[[1]] >= max(component_summary$mean_correct_picks),
        learned$mean_log_loss[[1]] <= min(component_summary$mean_log_loss) + guardrails$log_loss_max_delta,
        learned$mean_brier[[1]] <= min(component_summary$mean_brier) + guardrails$brier_max_delta
    )
)
gate_passed <- all(gate_tbl$passed)

summary_path <- file.path(output_dir, "ensemble_proof_gate_summary.csv")
yearly_path <- file.path(output_dir, "ensemble_proof_gate_yearly_metrics.csv")
predictions_path <- file.path(output_dir, "ensemble_proof_gate_predictions.csv")
parameters_path <- file.path(output_dir, "ensemble_proof_gate_parameters.csv")
conditions_path <- file.path(output_dir, "ensemble_proof_gate_conditions.csv")
json_path <- file.path(output_dir, "ensemble_proof_gate_result.json")
report_path <- file.path(output_dir, "ensemble_proof_gate_report.md")

utils::write.csv(summary_tbl, summary_path, row.names = FALSE)
utils::write.csv(yearly_metrics, yearly_path, row.names = FALSE)
utils::write.csv(
    all_predictions %>%
        dplyr::select(holdout_year, region, round, game_index, teamA, teamB, winner, actual_outcome, stan_glm, bart, equal_weight, learned_ensemble, intercept, weight_stan_glm),
    predictions_path,
    row.names = FALSE
)
utils::write.csv(parameter_rows, parameters_path, row.names = FALSE)
utils::write.csv(gate_tbl, conditions_path, row.names = FALSE)

output_paths <- list(
    summary = summary_path,
    yearly_metrics = yearly_path,
    predictions = predictions_path,
    parameters = parameters_path,
    conditions = conditions_path,
    json = json_path,
    report = report_path
)

result_payload <- list(
    gate_passed = gate_passed,
    guardrails = guardrails,
    holdout_years = sort(unique(all_predictions$holdout_year)),
    output_paths = output_paths,
    summary = summary_tbl,
    gate_conditions = gate_tbl,
    combiner_parameters = parameter_rows
)
jsonlite::write_json(result_payload, json_path, pretty = TRUE, auto_unbox = TRUE, dataframe = "rows")

write_report(
    report_path = report_path,
    summary_tbl = summary_tbl,
    yearly_tbl = yearly_metrics,
    gate_tbl = gate_tbl,
    parameters_tbl = parameter_rows,
    output_paths = output_paths,
    guardrails = guardrails,
    gate_passed = gate_passed
)

logger::log_info("Ensemble proof-gate report written to {report_path}")
logger::log_info("Ensemble proof-gate result: {if (gate_passed) 'PASS' else 'FAIL'}")

invisible(if (gate_passed) 0L else 1L)
