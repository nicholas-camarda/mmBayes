library(dplyr)
library(tibble)

# ── Odds conversion helpers ────────────────────────────────────────────────────

#' Convert American moneyline odds to raw implied probability
#'
#' Positive American odds (e.g. +150) are the underdog price; negative odds
#' (e.g. -200) are the favourite price.  The raw probability still contains
#' the bookmaker's vig.
#'
#' @param odds A numeric vector of American-format moneyline odds.
#'
#' @return A numeric vector of raw implied win probabilities in (0, 1).
#' @export
american_odds_to_prob <- function(odds) {
    odds <- as.numeric(odds)
    ifelse(
        odds >= 0,
        100 / (100 + odds),
        abs(odds) / (abs(odds) + 100)
    )
}

#' Convert decimal (European) odds to raw implied probability
#'
#' Decimal odds include the stake, so a £1 bet at 1.50 returns £1.50.
#' The implied probability is simply `1 / decimal_odds`.
#'
#' @param odds A numeric vector of decimal-format odds (must be > 1).
#'
#' @return A numeric vector of raw implied win probabilities in (0, 1).
#' @export
decimal_odds_to_prob <- function(odds) {
    odds <- as.numeric(odds)
    if (any(!is.na(odds) & odds <= 1)) {
        stop_with_message("Decimal odds must be greater than 1.")
    }
    1 / odds
}

#' Remove bookmaker vig from a pair of raw implied probabilities
#'
#' A standard two-outcome market (e.g. team A vs team B) has raw implied
#' probabilities that sum to more than 1 because the book takes a margin.
#' This function normalises a two-element vector so the fair probabilities
#' sum to exactly 1.
#'
#' @param prob_a Raw implied probability for outcome A.
#' @param prob_b Raw implied probability for outcome B (defaults to
#'   `1 - prob_a` when `NULL`).
#'
#' @return A named list with `prob_a` and `prob_b` as fair probabilities.
#' @export
remove_vig <- function(prob_a, prob_b = NULL) {
    prob_a <- as.numeric(prob_a)
    if (is.null(prob_b)) {
        prob_b <- 1 - prob_a
    } else {
        prob_b <- as.numeric(prob_b)
    }

    overround <- prob_a + prob_b
    if (any(!is.na(overround) & overround <= 0)) {
        stop_with_message("Sum of implied probabilities must be positive.")
    }

    list(
        prob_a = prob_a / overround,
        prob_b = prob_b / overround
    )
}

# ── Probability blending ───────────────────────────────────────────────────────

#' Blend a Bayesian posterior win probability with a market-implied probability
#'
#' Combines the model's posterior win probability for team A with the
#' bookmaker's fair-probability estimate using a configurable weight parameter.
#' A weight of 0 returns the pure posterior; a weight of 1 returns the pure
#' market estimate.
#'
#' The blended probability is a simple convex combination:
#' \deqn{p_{\text{blend}} = (1 - w)\,p_{\text{posterior}} + w\,p_{\text{market}}}
#'
#' @param posterior_prob Posterior win probability for team A from the Bayesian
#'   model.  Scalar or vector in \eqn{[0, 1]}.
#' @param market_implied_prob Fair market win probability for team A (after vig
#'   removal).  Same length as `posterior_prob`.
#' @param weight Blending weight for the market signal, in \eqn{[0, 1]}.
#'   Defaults to `0.3` (30 % market, 70 % model).
#'
#' @return A numeric vector of blended win probabilities in \eqn{(0, 1)}.
#' @export
blend_market_odds <- function(posterior_prob, market_implied_prob, weight = 0.3) {
    posterior_prob <- as.numeric(posterior_prob)
    market_implied_prob <- as.numeric(market_implied_prob)
    weight <- as.numeric(weight)

    if (length(weight) != 1L || !is.finite(weight) || weight < 0 || weight > 1) {
        stop_with_message("blend_market_odds: weight must be a single numeric value in [0, 1].")
    }
    if (any(!is.na(posterior_prob) & (posterior_prob < 0 | posterior_prob > 1))) {
        stop_with_message("blend_market_odds: posterior_prob values must be in [0, 1].")
    }
    if (any(!is.na(market_implied_prob) & (market_implied_prob < 0 | market_implied_prob > 1))) {
        stop_with_message("blend_market_odds: market_implied_prob values must be in [0, 1].")
    }

    blended <- (1 - weight) * posterior_prob + weight * market_implied_prob
    pmin(pmax(blended, 1e-6), 1 - 1e-6)
}

# ── Market-odds data attachment ────────────────────────────────────────────────

#' Attach market odds to a flattened matchup table
#'
#' Joins an external market-odds data frame onto the matchup table by team
#' names (both `teamA` and `teamB`), converts the supplied odds to fair
#' implied probabilities, and appends blended win-probability columns.
#'
#' @param matchups A flattened matchup table, as returned by
#'   [flatten_matchup_results()] or [augment_matchup_decisions()].
#' @param market_odds A data frame with at minimum the columns `teamA`,
#'   `teamB`, and one of:
#'   * `odds_a` / `odds_b` — American moneyline odds for each side, **or**
#'   * `decimal_a` / `decimal_b` — decimal odds for each side, **or**
#'   * `implied_prob_a` — pre-computed fair probability for team A (values
#'     in \eqn{(0, 1)}, already vig-adjusted).
#' @param weight Blending weight passed to [blend_market_odds()].
#'
#' @return The matchup table with additional columns:
#'   `market_prob_A` (fair market probability for team A),
#'   `market_prob_B` (fair market probability for team B), and
#'   `blended_prob_A` (convex combination of posterior and market for team A).
#' @export
attach_market_odds <- function(matchups, market_odds, weight = 0.3) {
    if (nrow(matchups) == 0L || nrow(market_odds) == 0L) {
        return(dplyr::mutate(
            matchups,
            market_prob_A = NA_real_,
            market_prob_B = NA_real_,
            blended_prob_A = win_prob_A
        ))
    }

    has_american <- all(c("odds_a", "odds_b") %in% names(market_odds))
    has_decimal  <- all(c("decimal_a", "decimal_b") %in% names(market_odds))
    has_implied  <- "implied_prob_a" %in% names(market_odds)

    if (!has_american && !has_decimal && !has_implied) {
        stop_with_message(
            paste0(
                "attach_market_odds: market_odds must contain one of: ",
                "(odds_a + odds_b), (decimal_a + decimal_b), or implied_prob_a."
            )
        )
    }

    market_processed <- market_odds %>%
        dplyr::mutate(
            teamA = canonicalize_team_name(teamA),
            teamB = canonicalize_team_name(teamB)
        )

    if (has_american) {
        market_processed <- market_processed %>%
            dplyr::mutate(
                raw_a = american_odds_to_prob(odds_a),
                raw_b = american_odds_to_prob(odds_b),
                market_prob_A = raw_a / (raw_a + raw_b),
                market_prob_B = raw_b / (raw_a + raw_b)
            )
    } else if (has_decimal) {
        market_processed <- market_processed %>%
            dplyr::mutate(
                raw_a = decimal_odds_to_prob(decimal_a),
                raw_b = decimal_odds_to_prob(decimal_b),
                market_prob_A = raw_a / (raw_a + raw_b),
                market_prob_B = raw_b / (raw_a + raw_b)
            )
    } else {
        market_processed <- market_processed %>%
            dplyr::mutate(
                market_prob_A = as.numeric(implied_prob_a),
                market_prob_B = 1 - as.numeric(implied_prob_a)
            )
    }

    market_processed <- market_processed %>%
        dplyr::select(teamA, teamB, market_prob_A, market_prob_B)

    matchups %>%
        dplyr::left_join(market_processed, by = c("teamA", "teamB")) %>%
        dplyr::mutate(
            blended_prob_A = dplyr::if_else(
                !is.na(market_prob_A),
                blend_market_odds(win_prob_A, market_prob_A, weight = weight),
                win_prob_A
            )
        )
}
