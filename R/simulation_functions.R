library(dplyr)
library(logger)
library(tibble)

prepare_prediction_rows <- function(team_rows, model_results) {
    prepared <- prepare_model_data(
        team_rows,
        metrics_to_use = model_results$metrics_to_use,
        scaling_reference = model_results$scaling_reference,
        conf_levels = model_results$conf_levels
    )

    prepared$data
}

predict_matchup_probabilities <- function(teamA, teamB, model_results, draws = 1000) {
    prediction_rows <- prepare_prediction_rows(
        dplyr::bind_rows(teamA, teamB),
        model_results
    )

    if (model_results$engine != "bayes") {
        stop_with_message("mmBayes requires the Bayesian model engine for tournament simulation")
    }

    posterior_draws <- nrow(as.matrix(model_results$model))
    draws <- max(1L, min(as.integer(draws), posterior_draws))

    linpred <- rstanarm::posterior_linpred(
        newdata = prediction_rows,
        object = model_results$model,
        draws = draws,
        re.form = NA
    )
    stats::plogis(linpred[, 1] - linpred[, 2])
}

#' Simulate a single matchup
#' @export
simulate_matchup <- function(teamA, teamB, round_name, matchup_number, model_results, draws = 1000) {
    logger::log_info("Simulating matchup: {teamA$Team[1]} vs {teamB$Team[1]}")

    win_probs <- calculate_win_probabilities(teamA, teamB, model_results, draws)
    matchup_stats <- generate_matchup_stats(teamA, teamB, win_probs)
    create_matchup_result(
        teamA = teamA,
        teamB = teamB,
        round_name = round_name,
        matchup_number = matchup_number,
        win_probs = win_probs,
        matchup_stats = matchup_stats
    )
}

#' Calculate matchup win probabilities
#' @export
calculate_win_probabilities <- function(teamA, teamB, model_results, draws = 1000) {
    draw_probs <- predict_matchup_probabilities(teamA, teamB, model_results, draws)

    list(
        mean = mean(draw_probs),
        ci_lower = as.numeric(stats::quantile(draw_probs, 0.025)),
        ci_upper = as.numeric(stats::quantile(draw_probs, 0.975)),
        sd = stats::sd(draw_probs),
        density = stats::density(draw_probs),
        draws = draw_probs
    )
}

#' Generate matchup statistics
#' @export
generate_matchup_stats <- function(teamA, teamB, win_probs) {
    tibble::tibble(
        teamA_strength = compute_team_strength(teamA),
        teamB_strength = compute_team_strength(teamB),
        win_prob_A = win_probs$mean,
        win_prob_B = 1 - win_probs$mean
    )
}

#' Create a matchup result row
#' @export
create_matchup_result <- function(teamA, teamB, round_name, matchup_number, win_probs, matchup_stats) {
    team_a_name <- teamA$Team[1]
    team_b_name <- teamB$Team[1]
    team_a_seed <- teamA$Seed[1]
    team_b_seed <- teamB$Seed[1]
    winner <- if (runif(1) <= win_probs$mean) teamA$Team[1] else teamB$Team[1]

    tibble::tibble(
        round = round_name,
        matchup_number = matchup_number,
        teamA = team_a_name,
        teamA_seed = team_a_seed,
        teamA_strength = matchup_stats$teamA_strength,
        teamB = team_b_name,
        teamB_seed = team_b_seed,
        teamB_strength = matchup_stats$teamB_strength,
        win_prob_A = win_probs$mean,
        ci_lower = win_probs$ci_lower,
        ci_upper = win_probs$ci_upper,
        prediction_sd = win_probs$sd,
        winner = winner,
        upset = (winner == team_a_name && team_a_seed > team_b_seed) ||
            (winner == team_b_name && team_b_seed > team_a_seed)
    )
}

simulate_round <- function(teams, round_name, model_results, draws) {
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

        matchup <- simulate_matchup(team_a, team_b, round_name, i, model_results, draws)
        round_results[[i]] <- matchup
        advancing_teams[[i]] <- if (matchup$winner[1] == team_a$Team[1]) team_a else team_b
    }

    list(
        results = dplyr::bind_rows(round_results),
        winners = dplyr::bind_rows(advancing_teams)
    )
}

#' Simulate a single region bracket
#' @export
simulate_region_bayesian <- function(region_teams, model_results, draws = 1000) {
    region_teams <- fix_region_seeds(region_teams)
    region_name <- unique(region_teams$Region)[1]

    if (nrow(region_teams) != 16) {
        stop_with_message(sprintf("Region %s must contain exactly 16 teams", region_name))
    }

    bracket_order <- standard_bracket_order()
    ordered_teams <- region_teams %>%
        dplyr::arrange(match(Assigned_Seed, bracket_order))

    rounds <- c("Round of 64", "Round of 32", "Sweet 16", "Elite 8")
    round_results <- list()
    remaining <- ordered_teams

    for (round_name in rounds) {
        simulated_round <- simulate_round(remaining, round_name, model_results, draws)
        round_results[[round_name]] <- simulated_round$results
        remaining <- simulated_round$winners
    }

    list(
        region = region_name,
        results = round_results,
        champion = remaining[1, , drop = FALSE]
    )
}

#' Simulate the Final Four and championship
#' @export
simulate_final_four <- function(region_champions, model_results, draws = 1000) {
    semifinal1 <- simulate_matchup(
        region_champions$South,
        region_champions$West,
        "Final Four",
        1,
        model_results,
        draws
    )
    semifinal2 <- simulate_matchup(
        region_champions$East,
        region_champions$Midwest,
        "Final Four",
        2,
        model_results,
        draws
    )

    championship_team_a <- if (semifinal1$winner[1] == region_champions$South$Team[1]) {
        region_champions$South
    } else {
        region_champions$West
    }
    championship_team_b <- if (semifinal2$winner[1] == region_champions$East$Team[1]) {
        region_champions$East
    } else {
        region_champions$Midwest
    }

    championship <- simulate_matchup(
        championship_team_a,
        championship_team_b,
        "Championship",
        1,
        model_results,
        draws
    )

    champion <- if (championship$winner[1] == championship_team_a$Team[1]) {
        championship_team_a
    } else {
        championship_team_b
    }

    list(
        semifinalists = region_champions,
        semifinals = list(semifinal1, semifinal2),
        championship = championship,
        champion = champion
    )
}

#' Simulate the full tournament bracket
#' @export
simulate_full_bracket <- function(all_teams, model_results, draws = 1000) {
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
            simulate_region_bayesian(
                region_teams = dplyr::filter(all_teams, Region == region_name),
                model_results = model_results,
                draws = draws
            )
        }
    )

    champions <- purrr::map(region_results, "champion")
    final_four <- simulate_final_four(champions, model_results, draws)

    list(
        region_results = region_results,
        final_four = final_four
    )
}
