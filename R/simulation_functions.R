library(dplyr)
library(logger)
library(tibble)

#' Build a single prediction row for a matchup
#'
#' @param teamA A one-row team feature data frame for team A.
#' @param teamB A one-row team feature data frame for team B.
#' @param round_name The round label for the matchup.
#'
#' @return A one-row matchup feature table.
#' @keywords internal
prepare_prediction_row <- function(teamA, teamB, round_name) {
    build_matchup_feature_row(
        team_a = teamA,
        team_b = teamB,
        round_name = round_name,
        actual_outcome = NA_real_,
        metadata = list(
            Year = teamA$Year[1],
            region = teamA$Region[1],
            teamA = teamA$Team[1],
            teamB = teamB$Team[1]
        )
    )
}

#' Predict matchup win probabilities
#'
#' @param teamA A one-row team feature data frame for team A.
#' @param teamB A one-row team feature data frame for team B.
#' @param round_name The round label for the matchup.
#' @param model_results A fitted matchup-model result bundle.
#' @param draws Number of posterior draws to use.
#' @param deterministic Whether to take the higher posterior-mean team or sample
#'   a stochastic winner from that probability.
#' @param log_matchup Whether to emit a log line for the matchup.
#'
#' @return A numeric vector of posterior win probabilities for team A.
#' @export
predict_matchup_probabilities <- function(teamA, teamB, round_name, model_results, draws = 1000) {
    prediction_row <- prepare_prediction_row(teamA, teamB, round_name)
    draw_matrix <- predict_matchup_rows(prediction_row, model_results, draws = draws)
    as.numeric(draw_matrix[, 1])
}

#' Summarize matchup win probabilities
#'
#' @param teamA A one-row team feature data frame for team A.
#' @param teamB A one-row team feature data frame for team B.
#' @param round_name The round label for the matchup.
#' @param model_results A fitted matchup-model result bundle.
#' @param draws Number of posterior draws to use.
#'
#' @return A list containing posterior summary statistics and draws.
#' @export
calculate_win_probabilities <- function(teamA, teamB, round_name, model_results, draws = 1000) {
    model_draw_probs <- predict_matchup_probabilities(teamA, teamB, round_name, model_results, draws)
    model_mean <- mean(model_draw_probs)

    betting <- model_results$betting_feature_context %||% list()
    line_prob <- NA_real_
    if (!is.null(betting$current_lines_matchups) && nrow(betting$current_lines_matchups) > 0) {
        line_prob <- lookup_line_prob_for_team_a(betting$current_lines_matchups, teamA$Team[1], teamB$Team[1])
    }

    list(
        mean = mean(model_draw_probs),
        ci_lower = as.numeric(stats::quantile(model_draw_probs, 0.025)),
        ci_upper = as.numeric(stats::quantile(model_draw_probs, 0.975)),
        sd = stats::sd(model_draw_probs),
        draws = model_draw_probs,
        model_mean = model_mean,
        line_prob = line_prob,
        blend_weight = 0,
        used_betting_line = FALSE
    )
}

#' Compute matchup summary statistics
#'
#' @param teamA A one-row team feature data frame for team A.
#' @param teamB A one-row team feature data frame for team B.
#' @param win_probs A win-probability summary list.
#'
#' @return A one-row tibble of matchup summary statistics.
#' @keywords internal
generate_matchup_stats <- function(teamA, teamB, win_probs) {
    tibble::tibble(
        teamA_strength = compute_team_strength(teamA),
        teamB_strength = compute_team_strength(teamB),
        win_prob_A = win_probs$mean,
        win_prob_B = 1 - win_probs$mean
    )
}

#' Create a matchup result row
#'
#' @param teamA A one-row team feature data frame for team A.
#' @param teamB A one-row team feature data frame for team B.
#' @param round_name The round label for the matchup.
#' @param matchup_number The game index within the round.
#' @param win_probs A summarized win-probability list.
#' @param matchup_stats A one-row tibble of derived matchup statistics.
#' @param winner The chosen winner name.
#'
#' @return A one-row tibble describing the simulated matchup result.
#' @keywords internal
create_matchup_result <- function(teamA, teamB, round_name, matchup_number, win_probs, matchup_stats, winner) {
    team_a_name <- teamA$Team[1]
    team_b_name <- teamB$Team[1]
    team_a_seed <- teamA$Seed[1]
    team_b_seed <- teamB$Seed[1]

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
        model_win_prob_A = win_probs$model_mean %||% NA_real_,
        line_prob_A = win_probs$line_prob %||% NA_real_,
        betting_blend_weight = win_probs$blend_weight %||% NA_real_,
        used_betting_line = isTRUE(win_probs$used_betting_line %||% FALSE),
        ci_lower = win_probs$ci_lower,
        ci_upper = win_probs$ci_upper,
        prediction_sd = win_probs$sd,
        winner = winner,
        upset = (winner == team_a_name && team_a_seed > team_b_seed) ||
            (winner == team_b_name && team_b_seed > team_a_seed)
    )
}

#' Simulate a deterministic matchup outcome
#'
#' @param teamA A one-row team feature data frame for team A.
#' @param teamB A one-row team feature data frame for team B.
#' @param round_name The round label for the matchup.
#' @param matchup_number The game index within the round.
#' @param model_results A fitted matchup-model result bundle.
#' @param draws Number of posterior draws to use.
#'
#' @return A one-row tibble describing the matchup result.
#' @export
simulate_matchup <- function(teamA, teamB, round_name, matchup_number, model_results, draws = 1000, deterministic = TRUE, log_matchup = TRUE) {
    if (isTRUE(log_matchup)) {
        logger::log_debug("Evaluating matchup: {teamA$Team[1]} vs {teamB$Team[1]} ({round_name})")
    }

    win_probs <- calculate_win_probabilities(teamA, teamB, round_name, model_results, draws)
    matchup_stats <- generate_matchup_stats(teamA, teamB, win_probs)
    team_a_wins <- if (isTRUE(deterministic)) {
        win_probs$mean >= 0.5
    } else {
        stats::rbinom(1L, 1L, prob = win_probs$mean) == 1L
    }
    winner <- if (team_a_wins) teamA$Team[1] else teamB$Team[1]

    create_matchup_result(
        teamA = teamA,
        teamB = teamB,
        round_name = round_name,
        matchup_number = matchup_number,
        win_probs = win_probs,
        matchup_stats = matchup_stats,
        winner = winner
    )
}

#' Simulate a tournament round
#'
#' @param teams A data frame of teams ordered into bracket slots.
#' @param round_name The round label to simulate.
#' @param model_results A fitted matchup-model result bundle.
#' @param draws Number of posterior draws to use.
#' @param deterministic Whether each matchup is resolved deterministically.
#' @param log_matchups Whether to emit per-matchup log lines.
#'
#' @return A list containing round results and advancing teams.
#' @keywords internal
simulate_round <- function(teams, round_name, model_results, draws, deterministic = TRUE, log_matchups = TRUE) {
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

        matchup <- simulate_matchup(team_a, team_b, round_name, i, model_results, draws, deterministic = deterministic, log_matchup = log_matchups)
        round_results[[i]] <- matchup
        advancing_teams[[i]] <- if (matchup$winner[1] == team_a$Team[1]) team_a else team_b
    }

    list(
        results = dplyr::bind_rows(round_results),
        winners = dplyr::bind_rows(advancing_teams)
    )
}

#' Resolve duplicate-seed play-in games
#'
#' @param region_teams A region-level team table.
#' @param model_results A fitted matchup-model result bundle.
#' @param draws Number of posterior draws to use.
#' @param actual_play_in_results Optional normalized current-year First Four
#'   results used to replace simulated duplicate-seed outcomes when an actual
#'   winner is already known.
#' @param deterministic Whether play-in winners are resolved deterministically.
#' @param log_matchups Whether to emit per-matchup log lines.
#'
#' @return A list containing resolved teams and optional First Four results.
#' @keywords internal
resolve_play_in_games <- function(region_teams, model_results, draws, actual_play_in_results = NULL, deterministic = TRUE, log_matchups = TRUE) {
    region_name <- unique(region_teams$Region)[1]
    duplicate_counts <- table(region_teams$Seed)
    dup_seeds <- as.integer(names(duplicate_counts[duplicate_counts > 1]))

    if (length(dup_seeds) == 0) {
        region_teams$Assigned_Seed <- region_teams$Seed
        return(list(
            teams = region_teams,
            results = tibble::tibble()
        ))
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
            dplyr::filter(
                play_in_region == region_name,
                slot_seed == seed_value
            )

        if (nrow(actual_result) > 1) {
            stop_with_message(sprintf("Multiple actual First Four results found for %s %s-seed slot", region_name, seed_value))
        }

        if (nrow(actual_result) == 1) {
            actual_winner <- canonicalize_team_name(actual_result$winner[[1]])
            if (!actual_winner %in% duplicate_rows$Team) {
                stop_with_message(sprintf("Actual First Four winner %s does not match the active %s %s-seed slot", actual_winner, region_name, seed_value))
            }

            win_probs <- calculate_win_probabilities(
                duplicate_rows[1, , drop = FALSE],
                duplicate_rows[2, , drop = FALSE],
                "First Four",
                model_results,
                draws
            )
            matchup_stats <- generate_matchup_stats(
                duplicate_rows[1, , drop = FALSE],
                duplicate_rows[2, , drop = FALSE],
                win_probs
            )
            matchup <- create_matchup_result(
                duplicate_rows[1, , drop = FALSE],
                duplicate_rows[2, , drop = FALSE],
                "First Four",
                i,
                win_probs,
                matchup_stats,
                actual_winner
            )
        } else {
            matchup <- simulate_matchup(
                duplicate_rows[1, , drop = FALSE],
                duplicate_rows[2, , drop = FALSE],
                "First Four",
                i,
                model_results,
                draws,
                deterministic = deterministic,
                log_matchup = log_matchups
            )
        }
        play_in_results[[i]] <- matchup
        losing_team <- if (matchup$winner[1] == duplicate_rows$Team[1]) duplicate_rows$Team[2] else duplicate_rows$Team[1]
        survivors <- survivors %>%
            dplyr::filter(Team != losing_team)
    }

    survivors$Assigned_Seed <- survivors$Seed

    list(
        teams = survivors,
        results = dplyr::bind_rows(play_in_results)
    )
}

#' Simulate a single regional bracket
#'
#' @param region_teams A region-level team table.
#' @param model_results A fitted matchup-model result bundle.
#' @param draws Number of posterior draws to use.
#' @param actual_play_in_results Optional normalized current-year First Four
#'   results used to replace simulated duplicate-seed outcomes when available.
#' @param deterministic Whether regional games are resolved deterministically.
#' @param log_matchups Whether to emit per-matchup log lines.
#'
#' @return A list containing regional round results and the region champion.
#' @export
simulate_region_bayesian <- function(region_teams, model_results, draws = 1000, actual_play_in_results = NULL, deterministic = TRUE, log_matchups = TRUE) {
    region_name <- unique(region_teams$Region)[1]
    if (!isTRUE(log_matchups)) {
        logger::log_info("Simulating {region_name} region")
    }
    play_in <- resolve_play_in_games(
        region_teams,
        model_results,
        draws,
        actual_play_in_results = actual_play_in_results,
        deterministic = deterministic,
        log_matchups = log_matchups
    )
    region_teams <- play_in$teams

    if (nrow(region_teams) != 16) {
        stop_with_message(sprintf("Region %s must contain exactly 16 teams after play-in resolution", region_name))
    }

    ordered_teams <- region_teams %>%
        dplyr::arrange(match(Assigned_Seed, standard_bracket_order()))

    round_results <- list()
    if (nrow(play_in$results) > 0) {
        round_results[["First Four"]] <- play_in$results
    }

    rounds <- c("Round of 64", "Round of 32", "Sweet 16", "Elite 8")
    remaining <- ordered_teams

    for (round_name in rounds) {
        if (!isTRUE(log_matchups)) {
            logger::log_info("Simulating {round_name} in {region_name}")
        }
        simulated_round <- simulate_round(remaining, round_name, model_results, draws, deterministic = deterministic, log_matchups = log_matchups)
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
#'
#' @param region_champions A named list of regional champion rows.
#' @param model_results A fitted matchup-model result bundle.
#' @param draws Number of posterior draws to use.
#' @param deterministic Whether national games are resolved deterministically.
#' @param log_matchups Whether to emit per-matchup log lines.
#'
#' @return A list containing semifinal, championship, and champion results.
#' @export
simulate_final_four <- function(region_champions, model_results, draws = 1000, deterministic = TRUE, log_matchups = TRUE) {
    if (!isTRUE(log_matchups)) {
        logger::log_info("Simulating Final Four and championship")
    }
    semifinal1 <- simulate_matchup(
        region_champions$South,
        region_champions$West,
        "Final Four",
        1,
        model_results,
        draws,
        deterministic = deterministic,
        log_matchup = log_matchups
    )
    semifinal2 <- simulate_matchup(
        region_champions$East,
        region_champions$Midwest,
        "Final Four",
        2,
        model_results,
        draws,
        deterministic = deterministic,
        log_matchup = log_matchups
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
        draws,
        deterministic = deterministic,
        log_matchup = log_matchups
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
#'
#' @param all_teams A current-year team feature table.
#' @param model_results A fitted matchup-model result bundle.
#' @param draws Number of posterior draws to use.
#' @param actual_play_in_results Optional normalized current-year First Four
#'   results used to replace simulated duplicate-seed outcomes when available.
#' @param deterministic Whether all matchups are resolved deterministically.
#' @param log_matchups Whether to emit per-matchup log lines.
#'
#' @return A list containing regional results and Final Four results.
#' @export
simulate_full_bracket <- function(all_teams, model_results, draws = 1000, actual_play_in_results = NULL, deterministic = TRUE, log_matchups = TRUE) {
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
                draws = draws,
                actual_play_in_results = actual_play_in_results,
                deterministic = deterministic,
                log_matchups = log_matchups
            )
        }
    )

    champions <- purrr::map(region_results, "champion")
    final_four <- simulate_final_four(champions, model_results, draws, deterministic = deterministic, log_matchups = log_matchups)

    list(
        region_results = region_results,
        final_four = final_four
    )
}
