library(dplyr)
library(purrr)
library(tibble)

# ── Slot-win probability estimation ───────────────────────────────────────────

#' Estimate the probability that each team wins each bracket slot
#'
#' Runs `n_simulations` stochastic bracket simulations and records, for every
#' (region, round, matchup_number) slot, how often each team emerges as the
#' winner.  These empirical frequencies are Monte Carlo estimates of the
#' posterior probability that a given team reaches **and wins** that slot —
#' accounting for the full conditional structure of the bracket (i.e. a team
#' can only appear in later rounds if it has survived earlier ones).
#'
#' This is the core of the Bayesian bracket optimisation: by integrating over
#' the posterior predictive distribution with Monte Carlo rather than making
#' point predictions, we propagate uncertainty from every earlier game into
#' the expected value of each later pick.
#'
#' @param all_teams A current-year team feature table.
#' @param model_results A fitted matchup-model result bundle.
#' @param draws Number of posterior draws per matchup.
#' @param actual_play_in_results Optional normalized current-year First Four
#'   results forwarded to [simulate_full_bracket()].
#' @param n_simulations Number of stochastic bracket simulations to run.
#' @param random_seed Random seed for reproducibility.
#'
#' @return A tibble with columns `region`, `round`, `matchup_number`, `team`,
#'   and `slot_win_prob` (the estimated probability that `team` wins that
#'   slot).
#' @export
compute_slot_win_probabilities <- function(
    all_teams,
    model_results,
    draws = 200L,
    actual_play_in_results = NULL,
    n_simulations = 400L,
    random_seed = 42L
) {
    set.seed(random_seed)

    # Accumulate win counts per (region, round, matchup_number, team) slot
    slot_counts <- list()

    for (sim_idx in seq_len(n_simulations)) {
        bracket <- simulate_full_bracket(
            all_teams = all_teams,
            model_results = model_results,
            draws = draws,
            actual_play_in_results = actual_play_in_results,
            deterministic = FALSE,
            log_matchups = FALSE
        )
        flat <- flatten_matchup_results(bracket)

        for (row_idx in seq_len(nrow(flat))) {
            key <- paste(
                flat$region[row_idx],
                flat$round[row_idx],
                flat$matchup_number[row_idx],
                sep = "\x01"
            )
            winner <- flat$winner[row_idx]
            if (is.null(slot_counts[[key]])) {
                slot_counts[[key]] <- list()
            }
            prev <- slot_counts[[key]][[winner]] %||% 0L
            slot_counts[[key]][[winner]] <- prev + 1L
        }
    }

    # Convert counts to tibble of probabilities
    purrr::imap_dfr(slot_counts, function(team_counts, key) {
        parts <- strsplit(key, "\x01", fixed = TRUE)[[1]]
        purrr::imap_dfr(team_counts, function(count, team_name) {
            tibble::tibble(
                region = parts[[1]],
                round = parts[[2]],
                matchup_number = as.integer(parts[[3]]),
                team = team_name,
                slot_win_prob = count / n_simulations
            )
        })
    })
}

# ── Optimal bracket construction ──────────────────────────────────────────────

#' Find the bracket that maximises expected weighted correct picks
#'
#' Implements a **Bayesian bracket path optimisation** by treating the
#' tournament bracket as a directed graph (a binary elimination tree) and
#' selecting, at every slot, the team that maximises expected bracket score.
#'
#' ## Method
#'
#' A bracket fill requires a pick for every game that *will eventually be
#' played*, not just those already scheduled.  For a pick to earn points it
#' must satisfy two conditions simultaneously:
#' 1. The chosen team must **reach** the round (survive all prior games).
#' 2. The chosen team must **win** the game once there.
#'
#' These two conditions are jointly captured by the slot-win probability
#' \eqn{P(\text{team } t \text{ wins slot } (r, g))}, which is the
#' probability that \eqn{t} both advances to round \eqn{r} **and** defeats
#' whoever it faces.
#'
#' Under an additive expected-score objective the optimal pick for slot
#' \eqn{(r, g)} is simply:
#' \deqn{\hat{t}^*(r,g) = \arg\max_t\; w_r \cdot P(t \text{ wins slot } (r,g))}
#' where \eqn{w_r} is the round scoring weight.  Because \eqn{w_r} is
#' constant for a given slot, this reduces to picking the team with the
#' highest slot-win probability, which is exactly what
#' [compute_slot_win_probabilities()] provides.
#'
#' The slot-win probabilities are estimated by Monte Carlo integration over
#' the full posterior predictive distribution of the Bayesian model (see
#' [compute_slot_win_probabilities()]).
#'
#' @param all_teams A current-year team feature table.
#' @param model_results A fitted matchup-model result bundle.
#' @param draws Number of posterior draws per matchup forwarded to
#'   [compute_slot_win_probabilities()].
#' @param actual_play_in_results Optional normalized current-year First Four
#'   results.
#' @param n_simulations Number of stochastic simulations used to estimate
#'   slot-win probabilities.
#' @param random_seed Random seed for reproducibility.
#' @param round_weights Named round-scoring weight vector (defaults to
#'   [default_round_weights()]).
#'
#' @return A list with elements:
#'   \describe{
#'     \item{`optimal_picks`}{A tibble with columns `region`, `round`,
#'       `matchup_number`, `optimal_pick`, and `slot_win_prob` — one row per
#'       bracket slot, with the team that maximises expected score.}
#'     \item{`expected_bracket_score`}{Scalar numeric: the expected weighted
#'       bracket score under the optimal picks.}
#'     \item{`slot_win_probs`}{The full slot-win probability table returned by
#'       [compute_slot_win_probabilities()].}
#'     \item{`n_simulations`}{Number of simulations used.}
#'   }
#' @export
find_optimal_bracket <- function(
    all_teams,
    model_results,
    draws = 200L,
    actual_play_in_results = NULL,
    n_simulations = 400L,
    random_seed = 42L,
    round_weights = default_round_weights()
) {
    slot_probs <- compute_slot_win_probabilities(
        all_teams = all_teams,
        model_results = model_results,
        draws = draws,
        actual_play_in_results = actual_play_in_results,
        n_simulations = n_simulations,
        random_seed = random_seed
    )

    if (nrow(slot_probs) == 0L) {
        return(list(
            optimal_picks = tibble::tibble(),
            expected_bracket_score = 0,
            slot_win_probs = slot_probs,
            n_simulations = n_simulations
        ))
    }

    # For each slot, pick the team with the highest slot-win probability.
    # Ties are broken by team name (alphabetically) for reproducibility.
    optimal_picks <- slot_probs %>%
        dplyr::group_by(region, round, matchup_number) %>%
        dplyr::arrange(dplyr::desc(slot_win_prob), team) %>%
        dplyr::slice(1L) %>%
        dplyr::ungroup() %>%
        dplyr::rename(optimal_pick = team) %>%
        dplyr::mutate(
            round_weight = unname(round_weights[round]),
            round_weight = dplyr::coalesce(round_weight, 0)
        )

    expected_bracket_score <- sum(
        optimal_picks$slot_win_prob * optimal_picks$round_weight,
        na.rm = TRUE
    )

    list(
        optimal_picks = dplyr::select(optimal_picks, region, round, matchup_number, optimal_pick, slot_win_prob),
        expected_bracket_score = expected_bracket_score,
        slot_win_probs = slot_probs,
        n_simulations = n_simulations
    )
}

#' Compare an optimal bracket against a candidate bracket
#'
#' Joins the optimal-bracket picks returned by [find_optimal_bracket()] with
#' the matchup table of a simulated candidate bracket and flags slots where
#' the two disagree.
#'
#' @param optimal_result The list returned by [find_optimal_bracket()].
#' @param candidate_matchups A flattened candidate matchup table (as stored in
#'   the `matchups` element of a candidate bracket object).
#'
#' @return A tibble with one row per bracket slot and columns indicating the
#'   deterministic pick, the optimal pick, whether they differ, and the
#'   slot-win probability for the optimal pick.
#' @export
compare_optimal_to_candidate <- function(optimal_result, candidate_matchups) {
    optimal_picks <- optimal_result$optimal_picks %>%
        dplyr::mutate(round = as.character(round))

    candidate_picks <- candidate_matchups %>%
        dplyr::transmute(
            region = as.character(region),
            round = as.character(round),
            matchup_number,
            candidate_pick = winner
        )

    dplyr::left_join(
        candidate_picks,
        optimal_picks,
        by = c("region", "round", "matchup_number")
    ) %>%
        dplyr::mutate(
            differs_from_optimal = !is.na(optimal_pick) & candidate_pick != optimal_pick
        ) %>%
        dplyr::arrange(
            factor(region, levels = bracket_region_levels()),
            factor(round, levels = round_levels()),
            matchup_number
        )
}
