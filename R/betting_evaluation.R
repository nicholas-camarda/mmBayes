library(dplyr)
library(tibble)

#' Evaluate whether blending betting lines improves prediction metrics
#'
#' This function compares model-only probabilities against a blended probability:
#'
#' `p_blend = (1 - w) * p_model + w * p_line`
#'
#' where `p_line` is a line-implied win probability for team A.
#'
#' @param predictions A data frame containing `predicted_prob`, `actual_outcome`,
#'   `Year`, `round`, `teamA`, and `teamB`.
#' @param closing_lines A data frame containing `Year`, `round`, `teamA`, `teamB`,
#'   and `implied_prob_teamA`.
#' @param blend_weight Blend weight `w` in `[0, 1]`.
#' @param rounds Optional character vector of round labels to include.
#'
#' @return A one-row tibble with baseline and blended log loss / Brier metrics
#'   evaluated on the matched games.
#' @export
evaluate_betting_blend_metrics <- function(predictions, closing_lines, blend_weight = 0.35, rounds = NULL) {
    if (is.null(predictions) || nrow(predictions) == 0) {
        return(tibble::tibble())
    }
    if (is.null(closing_lines) || nrow(closing_lines) == 0) {
        return(tibble::tibble())
    }

    weight <- suppressWarnings(as.numeric(blend_weight))
    weight <- max(0, min(1, weight))

    pred_tbl <- predictions %>%
        dplyr::mutate(
            Year = as.character(Year),
            round = as.character(round),
            teamA = canonicalize_team_name(teamA),
            teamB = canonicalize_team_name(teamB),
            matchup_key = purrr::map2_chr(teamA, teamB, build_matchup_key)
        )

    lines_tbl <- closing_lines %>%
        dplyr::mutate(
            Year = as.character(Year),
            round = as.character(round),
            teamA = canonicalize_team_name(teamA),
            teamB = canonicalize_team_name(teamB),
            matchup_key = purrr::map2_chr(teamA, teamB, build_matchup_key),
            implied_prob_teamA = suppressWarnings(as.numeric(implied_prob_teamA))
        )

    if (!is.null(rounds) && length(rounds) > 0) {
        pred_tbl <- pred_tbl %>% dplyr::filter(round %in% rounds)
        lines_tbl <- lines_tbl %>% dplyr::filter(round %in% rounds)
    }

    joined <- pred_tbl %>%
        dplyr::inner_join(
            lines_tbl %>% dplyr::select(Year, round, matchup_key, implied_prob_teamA),
            by = c("Year", "round", "matchup_key")
        ) %>%
        dplyr::filter(is.finite(predicted_prob), is.finite(actual_outcome), is.finite(implied_prob_teamA))

    if (nrow(joined) == 0) {
        return(tibble::tibble())
    }

    baseline <- compute_binary_metrics(joined$predicted_prob, joined$actual_outcome)
    blended_prob <- (1 - weight) * joined$predicted_prob + weight * joined$implied_prob_teamA
    blended <- compute_binary_metrics(blended_prob, joined$actual_outcome)

    tibble::tibble(
        n_games = nrow(joined),
        blend_weight = weight,
        log_loss_model = baseline$log_loss[[1]],
        log_loss_blend = blended$log_loss[[1]],
        delta_log_loss = blended$log_loss[[1]] - baseline$log_loss[[1]],
        brier_model = baseline$brier[[1]],
        brier_blend = blended$brier[[1]],
        delta_brier = blended$brier[[1]] - baseline$brier[[1]]
    )
}

#' Evaluate a grid of blend weights for betting-line blending
#'
#' @param predictions Backtest predictions table (see [evaluate_betting_blend_metrics()]).
#' @param closing_lines Closing-line table (see [evaluate_betting_blend_metrics()]).
#' @param weights Numeric vector of candidate blend weights.
#' @param rounds Optional character vector of rounds to evaluate.
#'
#' @return A tibble with one row per candidate blend weight.
#' @export
evaluate_betting_blend_weight_grid <- function(predictions,
                                               closing_lines,
                                               weights = c(0, 0.1, 0.25, 0.35, 0.5, 0.75, 1),
                                               rounds = NULL) {
    purrr::map_dfr(weights, function(w) {
        evaluate_betting_blend_metrics(
            predictions = predictions,
            closing_lines = closing_lines,
            blend_weight = w,
            rounds = rounds
        )
    })
}

