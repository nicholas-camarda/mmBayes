library(dplyr)
library(ggplot2)
library(patchwork)

round_levels <- c("Round of 64", "Round of 32", "Sweet 16", "Elite 8", "Final Four", "Championship")

#' Create the main tournament visualization
#' @export
create_tournament_visualization <- function(simulation_results) {
    bracket_plot <- create_bracket_visualization(simulation_results)
    probability_plot <- create_probability_plot(simulation_results)
    uncertainty_plot <- create_uncertainty_plot(simulation_results)

    (bracket_plot | probability_plot) / uncertainty_plot
}

#' Create a bracket-style overview plot
#' @export
create_bracket_visualization <- function(results) {
    matchups <- flatten_matchup_results(results) %>%
        dplyr::mutate(
            round = factor(round, levels = round_levels),
            matchup_label = paste(teamA, "vs", teamB)
        )

    ggplot2::ggplot(matchups, ggplot2::aes(x = round, y = matchup_number, color = region)) +
        ggplot2::geom_point(size = 3) +
        ggplot2::geom_text(ggplot2::aes(label = winner), nudge_y = 0.18, size = 2.8, show.legend = FALSE) +
        ggplot2::theme_minimal() +
        ggplot2::labs(
            title = "Tournament Winners By Round",
            x = NULL,
            y = "Matchup"
        ) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}

#' Create a matchup probability plot
#' @export
create_probability_plot <- function(results) {
    matchups <- flatten_matchup_results(results) %>%
        dplyr::mutate(
            round = factor(round, levels = round_levels),
            matchup_label = sprintf("%s vs %s", teamA, teamB)
        )

    ggplot2::ggplot(matchups, ggplot2::aes(x = win_prob_A, y = stats::reorder(matchup_label, win_prob_A), fill = region)) +
        ggplot2::geom_col(width = 0.7) +
        ggplot2::facet_wrap(~round, scales = "free_y") +
        ggplot2::theme_minimal() +
        ggplot2::labs(
            title = "Win Probability For Team A In Each Matchup",
            x = "Probability",
            y = NULL
        )
}

#' Create a prediction uncertainty plot
#' @export
create_uncertainty_plot <- function(results) {
    matchups <- flatten_matchup_results(results) %>%
        dplyr::mutate(
            round = factor(round, levels = round_levels),
            matchup_label = sprintf("%s vs %s", teamA, teamB)
        )

    ggplot2::ggplot(matchups, ggplot2::aes(x = win_prob_A, y = stats::reorder(matchup_label, win_prob_A), color = region)) +
        ggplot2::geom_point() +
        ggplot2::geom_errorbarh(ggplot2::aes(xmin = ci_lower, xmax = ci_upper), height = 0.2) +
        ggplot2::facet_wrap(~round, scales = "free_y") +
        ggplot2::theme_minimal() +
        ggplot2::labs(
            title = "Prediction Uncertainty",
            x = "Win Probability Interval",
            y = NULL
        )
}
