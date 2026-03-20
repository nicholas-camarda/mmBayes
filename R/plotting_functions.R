library(dplyr)
library(ggplot2)
library(patchwork)

#' Create the main tournament visualization
#'
#' @param simulation_results A full bracket simulation result bundle.
#'
#' @return A patchwork object combining bracket, probability, and uncertainty plots.
#' @export
create_tournament_visualization <- function(simulation_results) {
    bracket_plot <- create_bracket_visualization(simulation_results)
    probability_plot <- create_probability_plot(simulation_results)
    uncertainty_plot <- create_uncertainty_plot(simulation_results)

    (bracket_plot | probability_plot) / uncertainty_plot
}

#' Create a bracket-style overview plot
#'
#' @param results A full bracket simulation result bundle.
#'
#' @return A `ggplot` showing winners by matchup and round.
#' @export
create_bracket_visualization <- function(results) {
    matchups <- flatten_matchup_results(results) %>%
        dplyr::mutate(
            round = factor(round, levels = round_levels()),
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
#'
#' @param results A full bracket simulation result bundle.
#'
#' @return A faceted `ggplot` of matchup win probabilities.
#' @export
create_probability_plot <- function(results) {
    matchups <- flatten_matchup_results(results) %>%
        dplyr::mutate(
            round = factor(round, levels = round_levels()),
            matchup_label = sprintf("%s vs %s", teamA, teamB),
            matchup_label_wrap = gsub(" vs ", "\nvs\n", matchup_label, fixed = TRUE)
        )

    ggplot2::ggplot(matchups, ggplot2::aes(x = win_prob_A, y = stats::reorder(matchup_label_wrap, win_prob_A), fill = region)) +
        ggplot2::geom_col(width = 0.7) +
        ggplot2::facet_wrap(~round, scales = "free_y", ncol = 2) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
            axis.text.y = ggplot2::element_text(size = 8),
            strip.text = ggplot2::element_text(size = 10),
            panel.spacing = grid::unit(1, "lines")
        ) +
        ggplot2::labs(
            title = "Win Probability For Team A In Each Matchup",
            x = "Probability",
            y = NULL
        )
}

#' Create a prediction uncertainty plot
#'
#' @param results A full bracket simulation result bundle.
#'
#' @return A faceted `ggplot` of matchup intervals and posterior means.
#' @export
create_uncertainty_plot <- function(results) {
    matchups <- flatten_matchup_results(results) %>%
        dplyr::mutate(
            round = factor(round, levels = round_levels()),
            matchup_label = sprintf("%s vs %s", teamA, teamB),
            matchup_label_wrap = gsub(" vs ", "\nvs\n", matchup_label, fixed = TRUE)
        )

    ggplot2::ggplot(matchups, ggplot2::aes(x = win_prob_A, y = stats::reorder(matchup_label_wrap, win_prob_A), color = region)) +
        ggplot2::geom_point() +
        ggplot2::geom_errorbarh(ggplot2::aes(xmin = ci_lower, xmax = ci_upper), height = 0.2) +
        ggplot2::facet_wrap(~round, scales = "free_y", ncol = 2) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
            axis.text.y = ggplot2::element_text(size = 8),
            strip.text = ggplot2::element_text(size = 10),
            panel.spacing = grid::unit(1, "lines")
        ) +
        ggplot2::labs(
            title = "Prediction Uncertainty",
            x = "Win Probability Interval",
            y = NULL
        )
}

#' Escape text for safe HTML rendering
#'
#' @param x A character vector.
#'
#' @return An HTML-escaped character vector.
#' @keywords internal
html_escape <- function(x) {
    x <- as.character(x)
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    gsub("\"", "&quot;", x, fixed = TRUE)
}

#' Format probabilities for dashboard display
#'
#' @param x A numeric vector of probabilities.
#'
#' @return A character vector of percentages.
#' @keywords internal
format_probability <- function(x) {
    sprintf("%.1f%%", 100 * safe_numeric(x, default = NA_real_))
}

#' Render a data frame as a simple HTML table
#'
#' @param data A data frame to render.
#' @param max_rows Optional maximum number of rows to include.
#' @param row_classes Optional vector of CSS classes aligned to the displayed
#'   table rows.
#'
#' @return A scalar character string containing an HTML table.
#' @keywords internal
render_html_table <- function(data, max_rows = NULL, row_classes = NULL) {
    if (nrow(data) == 0) {
        return("<p class='empty-state'>None</p>")
    }

    if (!is.null(max_rows)) {
        data <- utils::head(data, max_rows)
        if (!is.null(row_classes)) {
            row_classes <- utils::head(row_classes, max_rows)
        }
    }

    header_html <- paste(
        sprintf("<th>%s</th>", html_escape(names(data))),
        collapse = ""
    )

    row_html <- purrr::map_chr(seq_len(nrow(data)), function(index) {
        row <- data[index, , drop = FALSE]
        row_class <- row_classes[[index]] %||% ""
        cell_html <- paste(
            sprintf("<td>%s</td>", html_escape(unlist(row, use.names = FALSE))),
            collapse = ""
        )
        sprintf("<tr class='%s'>%s</tr>", html_escape(row_class), cell_html)
    })

    body_html <- paste(row_html, collapse = "\n")

    paste0(
        "<table class='dashboard-table'>",
        "<thead><tr>", header_html, "</tr></thead>",
        "<tbody>", body_html, "</tbody>",
        "</table>"
    )
}

#' Format a total-points interval for dashboard display
#'
#' @param lower Lower interval bound.
#' @param upper Upper interval bound.
#'
#' @return A compact character string for the interval.
#' @keywords internal
format_total_interval <- function(lower, upper) {
    sprintf("%s-%s", round(safe_numeric(lower, default = NA_real_)), round(safe_numeric(upper, default = NA_real_)))
}

#' Map inspection levels to dashboard row classes
#'
#' @param inspection_level A character vector of inspection levels.
#'
#' @return A character vector of CSS class names.
#' @keywords internal
inspection_row_class <- function(inspection_level) {
    dplyr::case_when(
        inspection_level == "primary" ~ "inspection-primary",
        inspection_level == "secondary" ~ "inspection-secondary",
        TRUE ~ ""
    )
}

#' Build a usage note for a ranked decision row
#'
#' @param candidate_diff_flag Whether the candidates diverge on this row.
#' @param confidence_tier The confidence tier for the matchup.
#' @param round_name The round label.
#'
#' @return A short plain-language usage note.
#' @keywords internal
build_ranked_decision_note <- function(candidate_diff_flag, confidence_tier, round_name) {
    if (isTRUE(candidate_diff_flag) && round_name %in% c("Elite 8", "Final Four", "Championship")) {
        return("High-stakes pivot. If you want more upside, this is where to separate the brackets.")
    }
    if (confidence_tier == "Toss-up") {
        return("True decision point. Either side is defensible, so choose the path you want to live with.")
    }
    if (confidence_tier == "Volatile") {
        return("Unstable matchup. The favorite still leads, but the interval says the game can swing.")
    }
    if (isTRUE(candidate_diff_flag)) {
        return("Meaningful fork between Candidate 1 and Candidate 2. Review this before locking the bracket.")
    }

    "Shared pick across both candidates. Review it for confidence, not because the candidates disagree."
}

#' Build a usage note for an upset pivot row
#'
#' @param candidate_1_pick Candidate 1's selected winner.
#' @param candidate_2_pick Candidate 2's selected winner.
#' @param underdog The underdog team.
#' @param round_name The round label.
#'
#' @return A short plain-language note for the pivot.
#' @keywords internal
build_upset_pivot_note <- function(candidate_1_pick, candidate_2_pick, underdog, round_name) {
    candidate_one_uses <- identical(as.character(candidate_1_pick), as.character(underdog))
    candidate_two_uses <- identical(as.character(candidate_2_pick), as.character(underdog))

    if (candidate_one_uses && candidate_two_uses) {
        return("Both candidates already take this underdog. It is a core upset, not just a speculative flyer.")
    }
    if (candidate_one_uses && !candidate_two_uses) {
        return("Candidate 1 uses this upset. Candidate 2 stays on the safer path.")
    }
    if (!candidate_one_uses && candidate_two_uses) {
        if (round_name %in% c("Sweet 16", "Elite 8", "Final Four", "Championship")) {
            return("Candidate 2 uses this as a late-round leverage swing with real downstream payoff.")
        }
        return("Candidate 2 uses this as the alternate underdog swing. Candidate 1 leaves it on the board.")
    }

    "Neither candidate currently takes this upset. Use it only if you want an even more aggressive path."
}

#' Build a usage note for a candidate divergence row
#'
#' @param round_name The round label.
#' @param confidence_tier The confidence tier for the matchup.
#'
#' @return A short note explaining why the divergence matters.
#' @keywords internal
build_divergence_note <- function(round_name, confidence_tier) {
    if (round_name %in% c("Elite 8", "Final Four", "Championship")) {
        return("Late-round fork. This changes the bracket's identity and expected pool payoff.")
    }
    if (round_name == "Sweet 16") {
        return("Mid-bracket fork. This is where the two paths start producing different title routes.")
    }
    if (confidence_tier == "Toss-up") {
        return("Early disagreement in a near coin flip. The choice is more about bracket style than certainty.")
    }

    "Early-path disagreement. This mainly matters for the downstream teams each candidate unlocks."
}

#' Render an inline board explainer
#'
#' @param what_this_shows Plain-language description of the board content.
#' @param how_to_use Plain-language workflow instruction.
#' @param why_it_matters Plain-language decision relevance.
#' @param math_text Plain-language explanation of the ranking math.
#'
#' @return A scalar character string containing HTML markup.
#' @keywords internal
render_board_explainer_html <- function(what_this_shows, how_to_use, why_it_matters, math_text) {
    paste0(
        "<div class='explain-grid'>",
        "<div class='explain-card'><div class='explain-label'>What this shows</div><p>", html_escape(what_this_shows), "</p></div>",
        "<div class='explain-card'><div class='explain-label'>How to use it</div><p>", html_escape(how_to_use), "</p></div>",
        "<div class='explain-card'><div class='explain-label'>Why it matters</div><p>", html_escape(why_it_matters), "</p></div>",
        "<div class='explain-card'><div class='explain-label'>Underlying math</div><p>", html_escape(math_text), "</p></div>",
        "</div>"
    )
}

#' Format a probability interval in plain language
#'
#' @param lower Lower probability bound.
#' @param upper Upper probability bound.
#'
#' @return A scalar character string for the interval.
#' @keywords internal
format_probability_interval <- function(lower, upper) {
    paste(format_probability(lower), format_probability(upper), sep = " to ")
}

#' Resolve a stable slot key for matchup lookups
#'
#' @param data A data frame containing matchup-identifying columns.
#'
#' @return A character vector of slot keys aligned to `data`.
#' @keywords internal
resolve_matchup_slot_key <- function(data) {
    if ("slot_key" %in% names(data)) {
        return(as.character(data$slot_key))
    }

    paste(
        as.character(data$round),
        as.character(data$region),
        as.character(data$matchup_number),
        sep = "|"
    )
}

#' Derive the underdog interval from the favorite interval
#'
#' @param favorite_lower Lower bound for the favorite win probability.
#' @param favorite_upper Upper bound for the favorite win probability.
#'
#' @return A named list with `lower` and `upper` underdog bounds.
#' @keywords internal
derive_underdog_interval <- function(favorite_lower, favorite_upper) {
    list(
        lower = pmax(0, 1 - safe_numeric(favorite_upper, default = NA_real_)),
        upper = pmin(1, 1 - safe_numeric(favorite_lower, default = NA_real_))
    )
}

#' Render a compact probability interval track
#'
#' @param mean_probability Posterior mean probability.
#' @param lower_probability Lower credible-interval bound.
#' @param upper_probability Upper credible-interval bound.
#' @param axis_min Minimum probability displayed on the track.
#' @param axis_max Maximum probability displayed on the track.
#' @param color Hex color used for the interval and point.
#' @param value_label Label for the posterior mean.
#' @param interval_label Label for the credible interval.
#'
#' @return A scalar character string containing HTML markup.
#' @keywords internal
render_probability_track_html <- function(mean_probability, lower_probability, upper_probability, axis_min = 0, axis_max = 1, color = "#457b9d", value_label = "Posterior mean", interval_label = "Posterior credible interval") {
    clip_probability <- function(x) {
        pmin(pmax(safe_numeric(x, default = axis_min), axis_min), axis_max)
    }
    to_percent <- function(x) {
        ((clip_probability(x) - axis_min) / (axis_max - axis_min)) * 100
    }

    mean_position <- to_percent(mean_probability)
    lower_position <- to_percent(lower_probability)
    upper_position <- to_percent(upper_probability)
    mid_probability <- axis_min + ((axis_max - axis_min) / 2)

    paste0(
        "<div class='prob-track'>",
        "<div class='prob-track__lane'>",
        "<div class='prob-track__range' style='left:", sprintf("%.2f", lower_position), "%;width:", sprintf("%.2f", max(upper_position - lower_position, 1)), "%;background:", html_escape(color), ";'></div>",
        "<div class='prob-track__point' style='left:", sprintf("%.2f", mean_position), "%;background:", html_escape(color), ";'></div>",
        "</div>",
        "<div class='prob-track__scale'>",
        "<span>", html_escape(format_probability(axis_min)), "</span>",
        "<span>", html_escape(format_probability(mid_probability)), "</span>",
        "<span>", html_escape(format_probability(axis_max)), "</span>",
        "</div>",
        "<div class='prob-track__caption'><strong>", html_escape(value_label), ":</strong> ", html_escape(format_probability(mean_probability)),
        " <span class='muted'>| ", html_escape(interval_label), ": ", html_escape(format_probability_interval(lower_probability, upper_probability)), "</span></div>",
        "</div>"
    )
}

#' Render a labeled probability interval block
#'
#' @param title Short label shown above the track.
#' @param mean_probability Posterior mean probability.
#' @param lower_probability Lower credible-interval bound.
#' @param upper_probability Upper credible-interval bound.
#' @param axis_min Minimum probability displayed on the track.
#' @param axis_max Maximum probability displayed on the track.
#' @param color Hex color used for the interval and point.
#' @param value_label Label for the posterior mean.
#' @param interval_label Label for the credible interval.
#'
#' @return A scalar character string containing HTML markup.
#' @keywords internal
render_labeled_probability_track_html <- function(title, mean_probability, lower_probability, upper_probability, axis_min = 0, axis_max = 1, color = "#457b9d", value_label = "Posterior mean", interval_label = "Posterior credible interval") {
    paste0(
        "<div class='prob-track-block'>",
        "<div class='prob-track-block__title'>", html_escape(title), "</div>",
        render_probability_track_html(
            mean_probability = mean_probability,
            lower_probability = lower_probability,
            upper_probability = upper_probability,
            axis_min = axis_min,
            axis_max = axis_max,
            color = color,
            value_label = value_label,
            interval_label = interval_label
        ),
        "</div>"
    )
}

#' Build a matchup lookup table for comparison boards
#'
#' @param candidate_matchups A candidate matchup table.
#'
#' @return A tibble keyed by slot with matchup and uncertainty fields.
#' @keywords internal
build_candidate_matchup_lookup <- function(candidate_matchups) {
    if (is.null(candidate_matchups) || nrow(candidate_matchups) == 0) {
        return(tibble::tibble())
    }

    candidate_matchups %>%
        dplyr::mutate(
            slot_lookup_key = resolve_matchup_slot_key(candidate_matchups),
            matchup_label = if ("matchup_label" %in% names(candidate_matchups)) {
                as.character(matchup_label)
            } else {
                sprintf("%s vs %s", teamA, teamB)
            }
        ) %>%
        dplyr::transmute(
            slot_lookup_key,
            matchup_label,
            chosen_pick = winner,
            posterior_favorite = posterior_favorite,
            win_prob_favorite = win_prob_favorite,
            ci_lower = ci_lower,
            ci_upper = ci_upper,
            confidence_tier = confidence_tier
        )
}

#' Build a plain-language divergence summary for a row
#'
#' @param candidate_one_matchup Candidate 1 matchup label.
#' @param candidate_two_matchup Candidate 2 matchup label.
#'
#' @return A short plain-language summary of the fork.
#' @keywords internal
build_divergence_matchup_summary <- function(candidate_one_matchup, candidate_two_matchup) {
    if (identical(as.character(candidate_one_matchup), as.character(candidate_two_matchup))) {
        return(paste0("Same game, different winner: ", candidate_one_matchup))
    }

    paste0(
        "Different game created by earlier picks. Candidate 1 sees ",
        candidate_one_matchup,
        "; Candidate 2 sees ",
        candidate_two_matchup,
        "."
    )
}

#' Build a plain-language explanation of what changes in a divergence row
#'
#' @param candidate_one_pick Candidate 1 selected winner.
#' @param candidate_two_pick Candidate 2 selected winner.
#' @param round_name The round label.
#' @param confidence_tier The confidence tier for the row.
#' @param candidate_one_matchup Candidate 1 matchup label.
#' @param candidate_two_matchup Candidate 2 matchup label.
#'
#' @return A short plain-language explanation of the fork.
#' @keywords internal
build_divergence_change_text <- function(candidate_one_pick, candidate_two_pick, round_name, confidence_tier, candidate_one_matchup, candidate_two_matchup) {
    base_text <- if (identical(as.character(candidate_one_matchup), as.character(candidate_two_matchup))) {
        paste0(
            "Both brackets reach the same game here, but Candidate 1 advances ",
            candidate_one_pick,
            " while Candidate 2 advances ",
            candidate_two_pick,
            "."
        )
    } else {
        paste0(
            "Earlier picks already changed the path, so this bracket position is no longer the same game. ",
            "Candidate 1 advances ",
            candidate_one_pick,
            " from ",
            candidate_one_matchup,
            ", while Candidate 2 advances ",
            candidate_two_pick,
            " from ",
            candidate_two_matchup,
            "."
        )
    }

    paste(base_text, build_divergence_note(round_name, confidence_tier))
}

#' Build shared scale settings for championship distribution charts
#'
#' @param distribution A championship-distribution tibble.
#'
#' @return A list with total-point and probability bounds.
#' @keywords internal
build_championship_distribution_scale <- function(distribution) {
    if (is.null(distribution) || nrow(distribution) == 0) {
        return(list(min_total = 100, max_total = 200, max_probability = 0.05))
    }

    min_total <- suppressWarnings(min(distribution$total_points, na.rm = TRUE))
    max_total <- suppressWarnings(max(distribution$total_points, na.rm = TRUE))
    max_probability <- suppressWarnings(max(distribution$probability, na.rm = TRUE))

    if (!is.finite(min_total) || !is.finite(max_total) || min_total == max_total) {
        min_total <- 100
        max_total <- 200
    }
    if (!is.finite(max_probability) || max_probability <= 0) {
        max_probability <- 0.05
    }

    list(
        min_total = as.numeric(min_total),
        max_total = as.numeric(max_total),
        max_probability = as.numeric(max_probability)
    )
}

#' Render a championship total-points distribution chart
#'
#' @param summary_row A one-row candidate championship summary tibble.
#' @param distribution A candidate-level championship distribution tibble.
#' @param scale_settings Shared scale settings from
#'   [build_championship_distribution_scale()].
#'
#' @return A scalar character string containing inline SVG markup.
#' @keywords internal
render_championship_distribution_svg <- function(summary_row, distribution, scale_settings) {
    if (nrow(summary_row) == 0 || nrow(distribution) == 0) {
        return("<p class='empty-state'>Championship tiebreaker unavailable.</p>")
    }

    width <- 460
    height <- 220
    margin_left <- 42
    margin_right <- 24
    margin_top <- 18
    margin_bottom <- 42
    plot_width <- width - margin_left - margin_right
    plot_height <- height - margin_top - margin_bottom

    min_total <- scale_settings$min_total
    max_total <- scale_settings$max_total
    max_probability <- scale_settings$max_probability
    if (!is.finite(max_total - min_total) || (max_total - min_total) <= 0) {
        min_total <- min_total - 1
        max_total <- max_total + 1
    }

    to_x <- function(total_points) {
        margin_left + ((safe_numeric(total_points, default = min_total) - min_total) / (max_total - min_total)) * plot_width
    }
    to_y <- function(probability) {
        margin_top + plot_height - (safe_numeric(probability, default = 0) / max_probability) * plot_height
    }

    bar_width <- max(3, plot_width / max(nrow(distribution), 40))
    bar_html <- paste(
        vapply(seq_len(nrow(distribution)), function(index) {
            row <- distribution[index, , drop = FALSE]
            x <- to_x(row$total_points[[1]]) - (bar_width / 2)
            y <- to_y(row$probability[[1]])
            height_value <- (margin_top + plot_height) - y
            sprintf(
                "<rect x='%.1f' y='%.1f' width='%.1f' height='%.1f' rx='1.5' fill='#7c9bff' fill-opacity='0.75'/>",
                x,
                y,
                bar_width,
                height_value
            )
        }, character(1)),
        collapse = "\n"
    )

    recommended_total <- safe_numeric(summary_row$recommended_tiebreaker_points[[1]], default = NA_real_)
    median_total <- safe_numeric(summary_row$predicted_total_median[[1]], default = NA_real_)
    interval_low <- safe_numeric(summary_row$predicted_total_80_lower[[1]], default = NA_real_)
    interval_high <- safe_numeric(summary_row$predicted_total_80_upper[[1]], default = NA_real_)
    peak_row <- distribution %>%
        dplyr::arrange(dplyr::desc(probability), total_points) %>%
        dplyr::slice_head(n = 1)
    peak_total <- safe_numeric(peak_row$total_points[[1]], default = NA_real_)

    overlay_html <- paste0(
        if (is.finite(interval_low) && is.finite(interval_high)) {
            sprintf(
                "<rect x='%.1f' y='%.1f' width='%.1f' height='%.1f' fill='#fde68a' fill-opacity='0.35' rx='5'/>",
                to_x(interval_low),
                margin_top + 6,
                max(6, to_x(interval_high) - to_x(interval_low)),
                plot_height - 12
            )
        } else {
            ""
        },
        if (is.finite(recommended_total)) {
            sprintf(
                "<line x1='%.1f' y1='%.1f' x2='%.1f' y2='%.1f' stroke='#dc2626' stroke-width='2' stroke-dasharray='6 4'/>",
                to_x(recommended_total),
                margin_top,
                to_x(recommended_total),
                margin_top + plot_height
            )
        } else {
            ""
        },
        if (is.finite(median_total)) {
            sprintf(
                "<line x1='%.1f' y1='%.1f' x2='%.1f' y2='%.1f' stroke='#1d4ed8' stroke-width='2'/>",
                to_x(median_total),
                margin_top,
                to_x(median_total),
                margin_top + plot_height
            )
        } else {
            ""
        },
        if (is.finite(peak_total)) {
            sprintf(
                "<circle cx='%.1f' cy='%.1f' r='4.5' fill='#1d4ed8' stroke='white' stroke-width='1.5'/>",
                to_x(peak_total),
                to_y(peak_row$probability[[1]])
            )
        } else {
            ""
        }
    )

    paste0(
        "<svg viewBox='0 0 ", width, " ", height, "' class='tech-svg tiebreaker-svg' role='img' aria-label='Championship tiebreaker distribution'>",
        "<line x1='", margin_left, "' y1='", margin_top + plot_height, "' x2='", margin_left + plot_width, "' y2='", margin_top + plot_height, "' stroke='#94a3b8' stroke-width='1'/>",
        "<line x1='", margin_left, "' y1='", margin_top, "' x2='", margin_left, "' y2='", margin_top + plot_height, "' stroke='#94a3b8' stroke-width='1'/>",
        overlay_html,
        bar_html,
        "<text x='", margin_left, "' y='", height - 10, "' font-size='11' fill='#6b7280'>", round(min_total), "</text>",
        "<text x='", margin_left + plot_width, "' y='", height - 10, "' text-anchor='end' font-size='11' fill='#6b7280'>", round(max_total), "</text>",
        "<text x='", margin_left + (plot_width / 2), "' y='", height - 10, "' text-anchor='middle' font-size='11' fill='#6b7280'>Championship total points</text>",
        "<text x='12' y='", margin_top + (plot_height / 2), "' font-size='11' fill='#6b7280' transform='rotate(-90 12 ", margin_top + (plot_height / 2), ")'>Probability</text>",
        "<rect x='", width - 168, "' y='14' width='10' height='10' fill='#fde68a' fill-opacity='0.55' rx='2'/>",
        "<text x='", width - 152, "' y='23' font-size='10' fill='#6b7280'>80% interval</text>",
        "<line x1='", width - 168, "' y1='38' x2='", width - 156, "' y2='38' stroke='#1d4ed8' stroke-width='2'/>",
        "<text x='", width - 152, "' y='41' font-size='10' fill='#6b7280'>Median</text>",
        "<line x1='", width - 96, "' y1='38' x2='", width - 84, "' y2='38' stroke='#dc2626' stroke-width='2' stroke-dasharray='6 4'/>",
        "<text x='", width - 80, "' y='41' font-size='10' fill='#6b7280'>Recommended</text>",
        "</svg>"
    )
}

#' Render a championship total-points distribution panel
#'
#' @param summary_row A one-row candidate championship summary tibble.
#' @param distribution A candidate-level championship distribution tibble.
#' @param scale_settings Shared scale settings used across candidate charts.
#'
#' @return A scalar character string containing an HTML panel body.
#' @keywords internal
render_championship_distribution_panel <- function(summary_row, distribution, scale_settings = NULL) {
    if (nrow(summary_row) == 0) {
        return("<p class='empty-state'>Championship tiebreaker unavailable.</p>")
    }

    scale_settings <- scale_settings %||% build_championship_distribution_scale(distribution)
    peak_row <- if (nrow(distribution) > 0) {
        distribution %>%
            dplyr::arrange(dplyr::desc(probability), total_points) %>%
            dplyr::slice_head(n = 1)
    } else {
        tibble::tibble(total_points = NA_real_, probability = NA_real_)
    }

    paste0(
        "<div class='distribution-meta'>",
        "<p><strong>Projected title game:</strong> ", html_escape(summary_row$championship_matchup[[1]]), "</p>",
        "<p><strong>Recommended tiebreaker:</strong> ", html_escape(summary_row$recommended_tiebreaker_points[[1]]), "</p>",
        "<p><strong>Median:</strong> ", sprintf("%.1f", safe_numeric(summary_row$predicted_total_median[[1]], default = NA_real_)),
        " <span class='muted'>| 80% interval ", html_escape(format_total_interval(summary_row$predicted_total_80_lower[[1]], summary_row$predicted_total_80_upper[[1]])), "</span></p>",
        "<p><strong>Most likely total:</strong> ", html_escape(round(safe_numeric(peak_row$total_points[[1]], default = NA_real_))),
        " <span class='muted'>| peak probability ", html_escape(format_probability(peak_row$probability[[1]])), "</span></p>",
        "<p class='distribution-note'>Use the recommended tiebreaker as the default entry. It is the model's rounded median championship total, so it minimizes miss distance more often than chasing an extreme outcome.</p>",
        "</div>",
        render_championship_distribution_svg(summary_row, distribution, scale_settings)
    )
}

#' Render the confidence-tier legend for dashboard views
#'
#' @return A scalar character string containing HTML markup for the legend.
#' @keywords internal
render_confidence_legend_html <- function() {
    palette <- dashboard_tier_palette()
    descriptions <- c(
        "Lock" = "Stable favorite",
        "Lean" = "Clear edge",
        "Toss-up" = "Near coin flip",
        "Volatile" = "Wide interval"
    )

    chips <- paste(
        vapply(names(palette), function(tier) {
            sprintf(
                "<div class='legend-chip'><span class='legend-swatch' style='background:%s'></span><div><strong>%s</strong><div class='legend-copy'>%s</div></div></div>",
                palette[[tier]],
                html_escape(tier),
                html_escape(descriptions[[tier]])
            )
        }, character(1)),
        collapse = "\n"
    )

    paste0("<div class='legend-row'>", chips, "</div>")
}

#' Render a compact guide for reading dashboard rows
#'
#' @return A scalar character string containing HTML markup for the guide.
#' @keywords internal
render_dashboard_reading_guide_html <- function() {
    paste0(
        "<div class='guide-grid'>",
        "<div class='guide-card'>",
        "<div class='guide-label'>How to read a row</div>",
        "<p><strong>Dot:</strong> posterior mean probability.</p>",
        "<p><strong>Bar:</strong> posterior interval around that mean.</p>",
        "<p><strong>Color:</strong> confidence tier from Lock through Volatile.</p>",
        "</div>",
        "<div class='guide-card'>",
        "<div class='guide-label'>Why the rows are ordered this way</div>",
        "<p>The ranked board sorts by decision score so the most consequential calls rise first.</p>",
        "<p>The upset board sorts by leverage so the highest-payoff underdog pivots are obvious.</p>",
        "<p>The divergence board keeps only the slots where Candidate 1 and Candidate 2 actually differ.</p>",
        "</div>",
        "</div>"
    )
}

#' Render an autogenerated interpretation card
#'
#' @param title The card title.
#' @param summary A plain-language summary string.
#' @param note An optional short note or source string.
#'
#' @return A scalar character string containing HTML markup for the card.
#' @keywords internal
render_interpretation_card_html <- function(title, summary, note = NULL) {
    note_html <- if (!is.null(note) && nzchar(note)) {
        sprintf("<p class='interpretation-note'>%s</p>", html_escape(note))
    } else {
        ""
    }

    paste0(
        "<div class='interpretation-card'>",
        "<div class='interpretation-label'>", html_escape(title), "</div>",
        "<p class='interpretation-summary'>", html_escape(summary), "</p>",
        note_html,
        "</div>"
    )
}

#' Render the autogenerated board interpretation grid
#'
#' @param decision_sheet A decision-sheet data frame.
#' @param quality_backtest Optional backtest bundle used for the model-quality card.
#' @param quality_source_label Optional source label for the model-quality card.
#'
#' @return A scalar character string containing HTML markup for the grid.
#' @keywords internal
render_interpretation_grid_html <- function(decision_sheet, quality_backtest = NULL, quality_source_label = NULL) {
    quality_summary <- summarize_model_quality(quality_backtest)
    quality_note <- quality_source_label %||% if (model_quality_has_backtest(quality_backtest)) "Current run backtest" else NULL

    paste0(
        "<div class='interpretation-grid'>",
        render_interpretation_card_html(
            "Ranked Decision Board",
            summarize_ranked_decision_board(decision_sheet),
            "This board is ordered by decision score, so the rows at the top are the most consequential calls."
        ),
        render_interpretation_card_html(
            "Upset Opportunity Board",
            summarize_upset_opportunity_board(decision_sheet),
            "The x-axis is underdog win probability, and the board is sorted by leverage so the highest-payoff pivots appear first."
        ),
        render_interpretation_card_html(
            "Candidate Divergence",
            summarize_candidate_divergence(decision_sheet),
            "Only the slots that change between Candidate 1 and Candidate 2 are shown, which keeps the comparison focused."
        ),
        render_interpretation_card_html(
            "Model Quality",
            quality_summary,
            quality_note
        ),
        "</div>"
    )
}

#' Render a simple leverage scatter plot as inline SVG
#'
#' @param decision_sheet A decision-sheet data frame.
#'
#' @return A scalar character string containing inline SVG markup.
#' @keywords internal
render_leverage_scatter_svg <- function(decision_sheet) {
    if (nrow(decision_sheet) == 0) {
        return("<p class='empty-state'>No decision rows available.</p>")
    }

    width <- 680
    height <- 260
    margin_left <- 40
    margin_bottom <- 28
    plot_width <- width - margin_left - 16
    plot_height <- height - margin_bottom - 16
    leverage_max <- max(decision_sheet$upset_leverage, na.rm = TRUE)
    if (!is.finite(leverage_max) || leverage_max <= 0) {
        leverage_max <- 1
    }
    points <- decision_sheet %>%
        dplyr::arrange(dplyr::desc(decision_score)) %>%
        dplyr::slice_head(n = 20) %>%
        dplyr::mutate(
            x = margin_left + (pmin(pmax(win_prob_favorite, 0.5), 1) - 0.5) / 0.5 * plot_width,
            y = 8 + (1 - (upset_leverage / leverage_max)) * plot_height,
            color = dplyr::case_when(
                confidence_tier == "Lock" ~ "#2a9d8f",
                confidence_tier == "Lean" ~ "#457b9d",
                confidence_tier == "Toss-up" ~ "#e9c46a",
                TRUE ~ "#e76f51"
            ),
            tooltip = sprintf(
                "%s | %s | %s over %s | fav=%s | tier=%s",
                region,
                as.character(round),
                candidate_1_pick,
                ifelse(candidate_1_pick == teamA, teamB, teamA),
                format_probability(win_prob_favorite),
                confidence_tier
            )
        )

    circle_html <- paste(
        sprintf(
            "<circle cx='%.1f' cy='%.1f' r='6' fill='%s'><title>%s</title></circle>",
            points$x,
            points$y,
            points$color,
            html_escape(points$tooltip)
        ),
        collapse = "\n"
    )

    paste0(
        "<svg viewBox='0 0 ", width, " ", height, "' class='scatter-svg' role='img' aria-label='Leverage scatter'>",
        "<line x1='", margin_left, "' y1='", plot_height + 8, "' x2='", width - 8, "' y2='", plot_height + 8, "' stroke='#9aa0a6' stroke-width='1'/>",
        "<line x1='", margin_left, "' y1='8' x2='", margin_left, "' y2='", plot_height + 8, "' stroke='#9aa0a6' stroke-width='1'/>",
        "<text x='", margin_left, "' y='", height - 4, "' font-size='11'>50%</text>",
        "<text x='", width - 40, "' y='", height - 4, "' font-size='11'>100%</text>",
        "<text x='8' y='16' font-size='11'>Leverage</text>",
        "<text x='", width - 130, "' y='", height - 4, "' font-size='11'>Favorite win probability</text>",
        circle_html,
        "</svg>"
    )
}

#' Create the bracket dashboard HTML
#'
#' @param bracket_year The active bracket year.
#' @param decision_sheet The decision-sheet data frame.
#' @param candidates A list of candidate bracket objects.
#' @param backtest Optional backtest result bundle.
#' @param play_in_resolution Optional one-row tibble from
#'   [summarize_play_in_resolution()] describing whether unresolved simulated
#'   First Four slots remain in the active bracket.
#' @param total_points_predictions Optional list returned by
#'   [predict_candidate_total_points()].
#' @param model_quality_context Optional resolved model-quality bundle used for
#'   reading the latest backtest snapshot.
#'
#' @return A complete HTML document as a scalar character string.
#' @export
create_bracket_dashboard_html <- function(bracket_year, decision_sheet, candidates, backtest = NULL, play_in_resolution = NULL, total_points_predictions = NULL, model_quality_context = NULL) {
    primary <- candidates[[1]]
    alternate <- if (length(candidates) >= 2) candidates[[2]] else candidates[[1]]

    top_decisions <- decision_sheet %>%
        dplyr::arrange(dplyr::desc(decision_score), round, region, matchup_number) %>%
        dplyr::transmute(
            round = as.character(round),
            region,
            matchup = matchup_label,
            recommended_pick = candidate_1_pick,
            favorite_prob = format_probability(win_prob_favorite),
            tier = confidence_tier,
            review = dplyr::case_when(
                inspection_level == "primary" ~ "Inspect now",
                inspection_level == "secondary" ~ "Watch closely",
                TRUE ~ ""
            ),
            inspection_level,
            rationale = rationale_short
        )
    top_decision_classes <- inspection_row_class(top_decisions$inspection_level)
    top_decisions <- top_decisions %>%
        dplyr::select(-inspection_level)

    candidate_diff <- decision_sheet %>%
        dplyr::filter(candidate_diff_flag) %>%
        dplyr::transmute(
            round = as.character(round),
            region,
            slot = matchup_number,
            candidate_1 = paste0(candidate_1_pick, " from ", matchup_label),
            candidate_2 = paste0(candidate_2_pick, " from ", candidate_2_matchup),
            tier = confidence_tier,
            review = dplyr::case_when(
                inspection_level == "primary" ~ "Inspect now",
                inspection_level == "secondary" ~ "Watch closely",
                TRUE ~ ""
            ),
            inspection_level,
            why = alternate_rationale
        )
    candidate_diff_classes <- inspection_row_class(candidate_diff$inspection_level)
    candidate_diff <- candidate_diff %>%
        dplyr::select(-inspection_level)

    build_sequence_view <- function(matchup_column, winner_column) {
        sequence_round_levels <- c("First Four", "Round of 64", "Round of 32", "Sweet 16", "Elite 8", "Final Four", "Championship")
        sequence_region_levels <- c("East", "South", "West", "Midwest")

        decision_sheet %>%
            dplyr::mutate(
                round = factor(round, levels = sequence_round_levels),
                region = factor(region, levels = sequence_region_levels)
            ) %>%
            dplyr::arrange(round, region, matchup_number) %>%
            dplyr::transmute(
                round = as.character(round),
                region = as.character(region),
                winner = .data[[winner_column]],
                matchup = .data[[matchup_column]],
                tier = confidence_tier,
                review = dplyr::case_when(
                    inspection_level == "primary" ~ "Inspect now",
                    inspection_level == "secondary" ~ "Watch closely",
                    TRUE ~ ""
                ),
                inspection_level
            )
    }

    safe_bracket_view <- build_sequence_view("matchup_label", "candidate_1_pick")
    alternate_bracket_view <- build_sequence_view("candidate_2_matchup", "candidate_2_pick")
    safe_bracket_classes <- inspection_row_class(safe_bracket_view$inspection_level)
    alternate_bracket_classes <- inspection_row_class(alternate_bracket_view$inspection_level)
    safe_bracket_view <- safe_bracket_view %>% dplyr::select(-inspection_level)
    alternate_bracket_view <- alternate_bracket_view %>% dplyr::select(-inspection_level)

    quality_context <- model_quality_context
    if (is.null(quality_context)) {
        quality_context <- list(
            backtest = backtest,
            source_label = if (model_quality_has_backtest(backtest)) "Current run backtest" else "Backtest unavailable"
        )
    }
    quality_backtest <- quality_context$backtest %||% backtest
    quality_source_label <- quality_context$source_label %||% if (model_quality_has_backtest(quality_backtest)) "Current run backtest" else "Backtest unavailable"

    calibration_summary <- if (model_quality_has_backtest(quality_backtest)) {
        quality_backtest$summary %>%
            dplyr::mutate(
                mean_log_loss = sprintf("%.3f", mean_log_loss),
                mean_brier = sprintf("%.3f", mean_brier),
                mean_accuracy = format_probability(mean_accuracy),
                mean_bracket_score = sprintf("%.1f", mean_bracket_score),
                mean_correct_picks = sprintf("%.1f", mean_correct_picks)
            )
    } else {
        tibble::tibble(status = "Backtest skipped in this run.")
    }

    tiebreaker_summary <- total_points_predictions$candidate_summaries %||% tibble::tibble()
    championship_distribution <- total_points_predictions$championship_distribution %||% tibble::tibble()
    if (!"candidate_id" %in% names(tiebreaker_summary)) {
        tiebreaker_summary <- tibble::tibble()
    }
    if (!"candidate_id" %in% names(championship_distribution)) {
        championship_distribution <- tibble::tibble()
    }
    lookup_candidate_tiebreaker <- function(candidate_id_value) {
        if (!"candidate_id" %in% names(tiebreaker_summary)) {
            return(tibble::tibble())
        }
        tiebreaker_summary %>%
            dplyr::filter(candidate_id == candidate_id_value)
    }
    lookup_candidate_distribution <- function(candidate_id_value) {
        if (!"candidate_id" %in% names(championship_distribution)) {
            return(tibble::tibble())
        }
        championship_distribution %>%
            dplyr::filter(candidate_id == candidate_id_value)
    }
    distribution_scale_settings <- build_championship_distribution_scale(championship_distribution)

    candidate_cards <- paste(
        purrr::map_chr(candidates, function(candidate) {
            tiebreaker_row <- lookup_candidate_tiebreaker(candidate$candidate_id)
            tiebreaker_block <- if (nrow(tiebreaker_row) == 1L) {
                paste0(
                    "<div class='tiebreaker-block'>",
                    "<div class='tiebreaker-label'>Championship Tiebreaker</div>",
                    "<div class='tiebreaker-points'>", html_escape(tiebreaker_row$recommended_tiebreaker_points[[1]]), "</div>",
                    "<p><strong>Projected title game:</strong> ", html_escape(tiebreaker_row$championship_matchup[[1]]), "</p>",
                    "<p><strong>Median total:</strong> ", sprintf("%.1f", safe_numeric(tiebreaker_row$predicted_total_median[[1]], default = NA_real_)),
                    " <span class='muted'>| 80% interval ", html_escape(format_total_interval(tiebreaker_row$predicted_total_80_lower[[1]], tiebreaker_row$predicted_total_80_upper[[1]])), "</span></p>",
                    "</div>"
                )
            } else {
                "<div class='tiebreaker-block'><div class='tiebreaker-label'>Championship Tiebreaker</div><p class='empty-state'>Prediction unavailable.</p></div>"
            }
            paste0(
                "<div class='candidate-card'>",
                "<h3>Candidate ", candidate$candidate_id, " <span class='candidate-type'>", html_escape(candidate$type), "</span></h3>",
                "<p><strong>Champion:</strong> ", html_escape(candidate$champion), "</p>",
                "<p><strong>Final Four:</strong> ", html_escape(candidate$final_four), "</p>",
                "<p><strong>Bracket log probability:</strong> ", sprintf("%.3f", candidate$bracket_log_prob), "</p>",
                "<p><strong>Mean picked-game probability:</strong> ", sprintf("%.3f", candidate$mean_game_prob), "</p>",
                "<p><strong>Guidance:</strong> ", html_escape(candidate$diff_summary %||% ""), "</p>",
                tiebreaker_block,
                "</div>"
            )
        }),
        collapse = "\n"
    )

    championship_distribution_panels <- if (nrow(tiebreaker_summary) > 0) {
        paste(
            purrr::map_chr(candidates, function(candidate) {
                summary_row <- lookup_candidate_tiebreaker(candidate$candidate_id)
                distribution_row <- lookup_candidate_distribution(candidate$candidate_id)

                paste0(
                    "<div class='panel'>",
                    "<h3>Candidate ", candidate$candidate_id, " Championship Distribution</h3>",
                    render_championship_distribution_panel(summary_row, distribution_row, scale_settings = distribution_scale_settings),
                    "</div>"
                )
            }),
            collapse = "\n"
        )
    } else {
        "<div class='panel'><p class='empty-state'>Championship total-points distributions were not supplied for this run.</p></div>"
    }

    status_panel <- ""
    if (!is.null(play_in_resolution) && nrow(play_in_resolution) > 0 && isTRUE(play_in_resolution$has_unresolved_slots[[1]])) {
        status_panel <- paste0(
            "<div class='panel status-panel status-simulated'><strong>Status: Simulated bracket path.</strong>",
            " ",
            html_escape(sprintf(
                "%s of %s play-in slots are still unresolved, so the generated brackets assume simulated First Four winners. Any downstream Round of 64 and later matchups can shift until those games are final.",
                play_in_resolution$unresolved_slots[[1]],
                play_in_resolution$expected_slots[[1]]
            )),
            "</div>"
        )
    } else if (!is.null(play_in_resolution) && nrow(play_in_resolution) > 0) {
        status_panel <- paste0(
            "<div class='panel status-panel status-final'><strong>Status: Final result.</strong>",
            " First Four slots are resolved, so the displayed bracket path reflects finalized play-in outcomes.",
            "</div>"
        )
    } else {
        status_panel <- paste0(
            "<div class='panel status-panel status-unknown'><strong>Status unavailable.</strong>",
            " No play-in summary was supplied, so the review panel cannot tell whether these slots are simulated or final.",
            "</div>"
        )
    }

    paste0(
        "<!DOCTYPE html><html><head><meta charset='utf-8'>",
        "<title>mmBayes Bracket Dashboard</title>",
        "<style>",
        "body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;background:#f7f4ee;color:#1b1b1b;margin:0;padding:24px;line-height:1.4;}",
        "h1,h2,h3{margin:0 0 8px 0;} h1{font-size:30px;} h2{margin-top:28px;font-size:20px;} h3{font-size:17px;}",
        ".lede{max-width:920px;color:#3f3f46;margin:8px 0 20px 0;}",
        ".card-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(260px,1fr));gap:16px;}",
        ".candidate-card,.panel{background:white;border:1px solid #d6d3d1;border-radius:14px;padding:16px;box-shadow:0 1px 2px rgba(0,0,0,0.03);}",
        ".muted{color:#6b7280;}",
        ".status-panel{font-size:15px;}",
        ".status-panel strong{display:block;margin-bottom:6px;font-size:17px;text-transform:uppercase;letter-spacing:0.03em;}",
        ".status-simulated{background:#fff3cd;border:2px solid #d97706;color:#7c2d12;}",
        ".status-final{background:#ecfdf3;border:2px solid #16a34a;color:#14532d;}",
        ".status-unknown{background:#eef2ff;border:2px solid #6366f1;color:#312e81;}",
        ".candidate-type{text-transform:uppercase;font-size:11px;letter-spacing:0.08em;color:#57534e;}",
        ".tiebreaker-block{margin-top:14px;padding:14px;border-radius:12px;background:#102542;color:#f8fafc;}",
        ".tiebreaker-label{text-transform:uppercase;font-size:11px;letter-spacing:0.08em;color:#bfdbfe;margin-bottom:4px;}",
        ".tiebreaker-points{font-size:34px;font-weight:700;line-height:1;margin-bottom:8px;}",
        ".dashboard-table{width:100%;border-collapse:collapse;font-size:13px;background:white;}",
        ".dashboard-table th,.dashboard-table td{border:1px solid #e7e5e4;padding:8px 10px;vertical-align:top;text-align:left;}",
        ".dashboard-table th{background:#f5f5f4;}",
        ".dashboard-table tr.inspection-primary td{background:#fff7cc;}",
        ".dashboard-table tr.inspection-primary td:first-child{border-left:4px solid #d97706;}",
        ".dashboard-table tr.inspection-secondary td{background:#fff1eb;}",
        ".dashboard-table tr.inspection-secondary td:first-child{border-left:4px solid #ea580c;}",
        ".section-grid{display:grid;grid-template-columns:1.2fr 1fr;gap:18px;align-items:start;}",
        ".distribution-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(280px,1fr));gap:16px;}",
        ".distribution-meta p{margin:0 0 8px 0;}",
        ".distribution-note{font-size:13px;color:#475569;max-width:54ch;}",
        ".tier-pill{display:inline-block;padding:2px 8px;border-radius:999px;font-size:12px;background:#f3f4f6;}",
        ".legend-row{display:flex;gap:12px;flex-wrap:wrap;margin:12px 0 18px 0;}",
        ".legend-chip{display:flex;align-items:center;gap:8px;padding:8px 10px;border-radius:999px;background:white;border:1px solid #d6d3d1;font-size:13px;}",
        ".legend-swatch{width:12px;height:12px;border-radius:999px;display:inline-block;}",
        ".legend-copy{font-size:12px;color:#6b7280;}",
        ".legend-primary{background:#fbbf24;}",
        ".legend-secondary{background:#fb923c;}",
        ".empty-state{color:#6b7280;font-style:italic;}",
        ".scatter-svg{width:100%;height:auto;background:white;border:1px solid #d6d3d1;border-radius:14px;padding:8px;}",
        "</style></head><body>",
        "<h1>mmBayes Bracket Decision Console</h1>",
        "<p class='lede'>Bracket year ", html_escape(bracket_year),
        ". Use Candidate 1 as the safest expected-value bracket and Candidate 2 as the bounded-risk alternate. The tables below surface the hardest calls first, make the championship tiebreaker explicit for each bracket, and highlight the rows that deserve closer inspection.</p>",
        status_panel,
        "<div class='legend-row'>",
        "<div class='legend-chip'><span class='legend-swatch legend-primary'></span><strong>Inspect now</strong> Toss-up rows worth a closer user review.</div>",
        "<div class='legend-chip'><span class='legend-swatch legend-secondary'></span><strong>Watch closely</strong> Volatile rows with wider uncertainty.</div>",
        "</div>",
        render_confidence_legend_html(),
        "<div class='card-grid'>", candidate_cards, "</div>",
        "<h2>Hardest Decisions</h2>",
        "<div class='section-grid'><div class='panel'>",
        render_html_table(top_decisions, max_rows = 16, row_classes = top_decision_classes),
        "</div><div>",
        render_leverage_scatter_svg(decision_sheet),
        "</div></div>",
        "<h2>Candidate Differences</h2><div class='panel'>",
        render_html_table(candidate_diff, max_rows = 20, row_classes = candidate_diff_classes),
        "</div>",
        "<h2>Championship Tiebreaker Distribution</h2>",
        "<div class='distribution-grid'>", championship_distribution_panels, "</div>",
        "<h2>Bracket-Ordered Review</h2>",
        "<div class='panel'><h3>Safe Bracket #1 Sequence</h3>",
        render_html_table(safe_bracket_view, row_classes = safe_bracket_classes),
        "</div>",
        "<div class='panel'><h3>Bracket #2 Sequence</h3>",
        render_html_table(alternate_bracket_view, row_classes = alternate_bracket_classes),
        "</div>",
        "<h2>Calibration Snapshot</h2><div class='panel'>",
        if (model_quality_has_backtest(quality_backtest)) {
            paste0(
                "<p class='panel-caption'><strong>Source:</strong> ", html_escape(quality_source_label), "</p>",
                render_html_table(calibration_summary)
            )
        } else {
            render_html_table(calibration_summary)
        },
        "</div>",
        "<h2>How To Use This</h2><div class='panel'>",
        "<p><strong>Pick rule:</strong> start with Candidate 1, then only override with Candidate 2 on games that appear in the diff table and still feel plausible to you.</p>",
        "<p><strong>Confidence tiers:</strong> Locks are stable favorite spots. Leans still favor one side. Toss-ups are real decisions. Volatile games carry wide uncertainty even if one team is favored.</p>",
        "<p><strong>Inspection workflow:</strong> use the highlighted Toss-up and Volatile rows as the short list for manual re-checks. Full matchup total-points detail is exported to CSVs if you want a deeper scoring review.</p>",
        "<p><strong>Best pool behavior:</strong> late-round volatility matters more than noisy Round of 64 upset hunting under standard scoring.</p>",
        "</div>",
        "</body></html>"
    )
}

#' Return the technical dashboard confidence-tier palette
#'
#' @return A named character vector mapping confidence tiers to hex colors.
#' @keywords internal
dashboard_tier_palette <- function() {
    c(
        "Lock" = "#2a9d8f",
        "Lean" = "#457b9d",
        "Toss-up" = "#e9c46a",
        "Volatile" = "#e76f51"
    )
}

#' Map confidence tiers to technical dashboard colors
#'
#' @param tier A character vector of confidence tiers.
#'
#' @return A character vector of hex colors aligned to `tier`.
#' @keywords internal
dashboard_tier_color <- function(tier) {
    palette <- dashboard_tier_palette()
    colors <- unname(palette[as.character(tier)])
    colors[is.na(colors)] <- "#6b7280"
    colors
}

#' Truncate long labels for compact dashboard plots
#'
#' @param x A character vector of labels.
#' @param width Maximum character width before truncation.
#'
#' @return A character vector truncated to the requested width.
#' @keywords internal
truncate_dashboard_label <- function(x, width = 36L) {
    stringr::str_trunc(as.character(x), width = width, side = "right")
}

#' Build a bracket-ordered sequence view for a candidate
#'
#' @param decision_sheet The decision-sheet data frame.
#' @param matchup_column Column name containing the matchup label to display.
#' @param winner_column Column name containing the chosen winner to display.
#'
#' @return A data frame ordered by round, region, and matchup slot.
#' @keywords internal
build_candidate_sequence_view <- function(decision_sheet, matchup_column, winner_column) {
    sequence_round_levels <- c("First Four", "Round of 64", "Round of 32", "Sweet 16", "Elite 8", "Final Four", "Championship")
    sequence_region_levels <- c("East", "South", "West", "Midwest", "National")

    decision_sheet %>%
        dplyr::mutate(
            round = factor(round, levels = sequence_round_levels),
            region = factor(region, levels = sequence_region_levels)
        ) %>%
        dplyr::arrange(round, region, matchup_number) %>%
        dplyr::transmute(
            round = as.character(round),
            region = as.character(region),
            winner = .data[[winner_column]],
            matchup = .data[[matchup_column]],
            tier = confidence_tier,
            review = dplyr::case_when(
                inspection_level == "primary" ~ "Inspect now",
                inspection_level == "secondary" ~ "Watch closely",
                TRUE ~ ""
            ),
            inspection_level
        )
}

#' Render the ranked decision board as a wrapped HTML comparison board
#'
#' @param decision_sheet The decision-sheet data frame.
#' @param top_n Maximum number of rows to render.
#'
#' @return A scalar character string containing HTML markup.
#' @keywords internal
render_ranked_decision_svg <- function(decision_sheet, top_n = 12L) {
    if (nrow(decision_sheet) == 0) {
        return("<p class='empty-state'>No decision rows available.</p>")
    }

    plot_data <- decision_sheet %>%
        dplyr::arrange(dplyr::desc(decision_score), round, region, matchup_number) %>%
        dplyr::slice_head(n = top_n) %>%
        dplyr::mutate(
            preferred_pick = paste0("Preferred: ", candidate_1_pick),
            alternate_pick = ifelse(
                candidate_diff_flag,
                paste0("Alternate: ", candidate_2_pick),
                "Alternate: same pick in both candidates"
            ),
            detail_summary = purrr::pmap_chr(
                list(candidate_diff_flag, confidence_tier, as.character(round)),
                build_ranked_decision_note
            ),
            tier_color = dashboard_tier_color(confidence_tier)
        )

    row_html <- paste(
        purrr::map_chr(seq_len(nrow(plot_data)), function(index) {
            row <- plot_data[index, , drop = FALSE]
            paste0(
                "<div class='board-row board-row--ranked'>",
                "<div class='board-cell board-cell--game' data-label='Game to revisit'><div class='board-value'>", html_escape(row$matchup_label[[1]]), "</div><div class='board-note'>", html_escape(as.character(row$round[[1]])), " | ", html_escape(as.character(row$confidence_tier[[1]])), "</div></div>",
                "<div class='board-cell' data-label='Preferred pick'><div class='board-value'>", html_escape(row$candidate_1_pick[[1]]), "</div><div class='board-note'>Posterior favorite: ", html_escape(row$posterior_favorite[[1]]), "</div></div>",
                "<div class='board-cell' data-label='Alternate path'><div class='board-value'>", html_escape(if (isTRUE(row$candidate_diff_flag[[1]])) row$candidate_2_pick[[1]] else "Same as preferred"), "</div><div class='board-note'>", html_escape(row$alternate_pick[[1]]), "</div></div>",
                "<div class='board-cell board-cell--plot' data-label='Posterior uncertainty'>",
                render_probability_track_html(
                    mean_probability = row$win_prob_favorite[[1]],
                    lower_probability = row$ci_lower[[1]],
                    upper_probability = row$ci_upper[[1]],
                    axis_min = 0.5,
                    axis_max = 1,
                    color = row$tier_color[[1]],
                    value_label = "Posterior mean",
                    interval_label = "Posterior credible interval"
                ),
                "</div>",
                "<div class='board-cell' data-label='How to use this'><div class='board-value'>How to use this</div><div class='board-note'>", html_escape(row$detail_summary[[1]]), "</div></div>",
                "</div>"
            )
        }),
        collapse = "\n"
    )

    paste0(
        "<div class='comparison-board comparison-board--ranked'>",
        "<div class='comparison-board__header'>",
        "<div>Game to revisit</div>",
        "<div>Preferred pick</div>",
        "<div>Alternate path</div>",
        "<div>Posterior uncertainty</div>",
        "<div>How to use this</div>",
        "</div>",
        row_html,
        "</div>"
    )
}

#' Render a round-by-region risk heatmap as inline SVG
#'
#' @param decision_sheet The decision-sheet data frame.
#'
#' @return A scalar character string containing inline SVG markup.
#' @keywords internal
render_round_risk_heatmap_svg <- function(decision_sheet) {
    if (nrow(decision_sheet) == 0) {
        return("<p class='empty-state'>No decision rows available.</p>")
    }

    round_order <- round_levels()
    region_order <- c(bracket_region_levels(), "National")
    summary_tbl <- decision_sheet %>%
        dplyr::mutate(round = as.character(round), region = as.character(region)) %>%
        dplyr::group_by(region, round) %>%
        dplyr::summarise(
            risk_score = max(decision_score, na.rm = TRUE),
            inspect_count = sum(inspection_flag, na.rm = TRUE),
            .groups = "drop"
        )

    heatmap_grid <- tidyr::expand_grid(
        region = region_order,
        round = round_order
    ) %>%
        dplyr::left_join(summary_tbl, by = c("region", "round")) %>%
        dplyr::mutate(
            risk_score = dplyr::coalesce(risk_score, 0),
            inspect_count = dplyr::coalesce(inspect_count, 0L)
        )

    max_risk <- max(heatmap_grid$risk_score, na.rm = TRUE)
    if (!is.finite(max_risk) || max_risk <= 0) {
        max_risk <- 1
    }

    color_fn <- grDevices::colorRamp(c("#f8fafc", "#fdba74", "#dc2626"))
    to_fill <- function(score) {
        rgb_matrix <- color_fn(pmin(pmax(score / max_risk, 0), 1))
        grDevices::rgb(rgb_matrix[1, 1], rgb_matrix[1, 2], rgb_matrix[1, 3], maxColorValue = 255)
    }

    cell_width <- 92
    cell_height <- 42
    margin_left <- 88
    margin_top <- 34
    width <- margin_left + (length(round_order) * cell_width) + 20
    height <- margin_top + (length(region_order) * cell_height) + 24

    cell_html <- paste(
        vapply(seq_len(nrow(heatmap_grid)), function(index) {
            row <- heatmap_grid[index, , drop = FALSE]
            round_index <- match(row$round[[1]], round_order)
            region_index <- match(row$region[[1]], region_order)
            x <- margin_left + ((round_index - 1) * cell_width)
            y <- margin_top + ((region_index - 1) * cell_height)
            sprintf(
                paste0(
                    "<g>",
                    "<rect x='%.1f' y='%.1f' width='%s' height='%s' rx='8' fill='%s' stroke='white' stroke-width='1.5'/>",
                    "<text x='%.1f' y='%.1f' text-anchor='middle' font-size='12' font-weight='600' fill='#111827'>%s</text>",
                    "<text x='%.1f' y='%.1f' text-anchor='middle' font-size='10' fill='#374151'>%s inspect</text>",
                    "</g>"
                ),
                x,
                y,
                cell_width - 6,
                cell_height - 6,
                to_fill(row$risk_score[[1]]),
                x + ((cell_width - 6) / 2),
                y + 16,
                sprintf("%.1f", safe_numeric(row$risk_score[[1]], default = 0)),
                x + ((cell_width - 6) / 2),
                y + 30,
                row$inspect_count[[1]]
            )
        }, character(1)),
        collapse = "\n"
    )
    column_labels <- paste(
        vapply(seq_along(round_order), function(index) {
            x <- margin_left + ((index - 1) * cell_width) + ((cell_width - 6) / 2)
            sprintf(
                "<text x='%.1f' y='18' text-anchor='middle' font-size='11' fill='#6b7280'>%s</text>",
                x,
                html_escape(truncate_dashboard_label(round_order[[index]], width = 12L))
            )
        }, character(1)),
        collapse = "\n"
    )
    row_labels <- paste(
        vapply(seq_along(region_order), function(index) {
            y <- margin_top + ((index - 1) * cell_height) + ((cell_height - 6) / 2) + 4
            sprintf(
                "<text x='10' y='%.1f' font-size='11' fill='#6b7280'>%s</text>",
                y,
                html_escape(region_order[[index]])
            )
        }, character(1)),
        collapse = "\n"
    )

    paste0(
        "<svg viewBox='0 0 ", width, " ", height, "' class='tech-svg' role='img' aria-label='Round by region risk map'>",
        column_labels,
        row_labels,
        cell_html,
        "</svg>"
    )
}

#' Render the candidate divergence board as wrapped HTML rows
#'
#' @param decision_sheet The decision-sheet data frame.
#'
#' @return A scalar character string containing HTML markup.
#' @keywords internal
render_candidate_divergence_svg <- function(decision_sheet) {
    diff_rows <- decision_sheet %>%
        dplyr::filter(candidate_diff_flag) %>%
        dplyr::mutate(round = factor(round, levels = round_levels())) %>%
        dplyr::arrange(dplyr::desc(round_weight), round, region, matchup_number)

    if (nrow(diff_rows) == 0) {
        return("<p class='empty-state'>Candidate 1 and Candidate 2 match on every slot in this run.</p>")
    }

    candidate_one_lookup <- build_candidate_matchup_lookup(diff_rows %>%
        dplyr::transmute(
            slot_key = slot_key,
            matchup_label = matchup_label,
            winner = candidate_1_pick,
            posterior_favorite = posterior_favorite,
            win_prob_favorite = win_prob_favorite,
            ci_lower = ci_lower,
            ci_upper = ci_upper,
            confidence_tier = confidence_tier
        ))
    candidate_two_lookup <- if ("candidate_2_matchup" %in% names(diff_rows)) {
        diff_rows %>%
            dplyr::transmute(
                slot_lookup_key = slot_key,
                matchup_label = candidate_2_matchup,
                chosen_pick = candidate_2_pick,
                posterior_favorite = posterior_favorite,
                win_prob_favorite = win_prob_favorite,
                ci_lower = ci_lower,
                ci_upper = ci_upper,
                confidence_tier = confidence_tier
            )
    } else {
        candidate_one_lookup
    }

    row_html <- paste(
        purrr::map_chr(seq_len(nrow(diff_rows)), function(index) {
            row <- diff_rows[index, , drop = FALSE]
            slot_key <- as.character(row$slot_key[[1]])
            candidate_one_row <- candidate_one_lookup %>% dplyr::filter(slot_lookup_key == slot_key) %>% dplyr::slice_head(n = 1)
            candidate_two_row <- candidate_two_lookup %>% dplyr::filter(slot_lookup_key == slot_key) %>% dplyr::slice_head(n = 1)
            tier_color <- dashboard_tier_color(row$confidence_tier[[1]])
            candidate_one_matchup <- if (nrow(candidate_one_row) == 1L) candidate_one_row$matchup_label[[1]] else row$matchup_label[[1]]
            candidate_two_matchup <- if (nrow(candidate_two_row) == 1L) candidate_two_row$matchup_label[[1]] else row$candidate_2_matchup[[1]]
            candidate_one_mean <- if (nrow(candidate_one_row) == 1L) candidate_one_row$win_prob_favorite[[1]] else row$win_prob_favorite[[1]]
            candidate_one_lower <- if (nrow(candidate_one_row) == 1L) candidate_one_row$ci_lower[[1]] else row$ci_lower[[1]]
            candidate_one_upper <- if (nrow(candidate_one_row) == 1L) candidate_one_row$ci_upper[[1]] else row$ci_upper[[1]]
            candidate_two_mean <- if (nrow(candidate_two_row) == 1L) candidate_two_row$win_prob_favorite[[1]] else row$win_prob_favorite[[1]]
            candidate_two_lower <- if (nrow(candidate_two_row) == 1L) candidate_two_row$ci_lower[[1]] else row$ci_lower[[1]]
            candidate_two_upper <- if (nrow(candidate_two_row) == 1L) candidate_two_row$ci_upper[[1]] else row$ci_upper[[1]]
            change_text <- build_divergence_change_text(
                candidate_one_pick = row$candidate_1_pick[[1]],
                candidate_two_pick = row$candidate_2_pick[[1]],
                round_name = as.character(row$round[[1]]),
                confidence_tier = as.character(row$confidence_tier[[1]]),
                candidate_one_matchup = candidate_one_matchup,
                candidate_two_matchup = candidate_two_matchup
            )
            matchup_summary <- build_divergence_matchup_summary(
                candidate_one_matchup = candidate_one_matchup,
                candidate_two_matchup = candidate_two_matchup
            )

            paste0(
                "<div class='board-row board-row--divergence'>",
                "<div class='board-cell board-cell--game' data-label='Bracket position where the paths split'><div class='board-value'>", html_escape(matchup_summary), "</div><div class='board-note'>Round: ", html_escape(as.character(row$round[[1]])), " | Tier: ", html_escape(as.character(row$confidence_tier[[1]])), "</div></div>",
                "<div class='board-cell' data-label='Candidate 1 choice'><div class='board-value'>", html_escape(row$candidate_1_pick[[1]]), "</div><div class='board-note'>Candidate 1 game: ", html_escape(candidate_one_matchup), "</div></div>",
                "<div class='board-cell' data-label='Candidate 2 choice'><div class='board-value'>", html_escape(row$candidate_2_pick[[1]]), "</div><div class='board-note'>Candidate 2 game: ", html_escape(candidate_two_matchup), "</div></div>",
                "<div class='board-cell board-cell--plot' data-label='Posterior uncertainty'><div class='prob-track-stack'>",
                render_labeled_probability_track_html(
                    title = "Candidate 1 matchup uncertainty",
                    mean_probability = candidate_one_mean,
                    lower_probability = candidate_one_lower,
                    upper_probability = candidate_one_upper,
                    axis_min = 0.5,
                    axis_max = 1,
                    color = tier_color,
                    value_label = "Posterior mean",
                    interval_label = "Posterior credible interval"
                ),
                render_labeled_probability_track_html(
                    title = "Candidate 2 matchup uncertainty",
                    mean_probability = candidate_two_mean,
                    lower_probability = candidate_two_lower,
                    upper_probability = candidate_two_upper,
                    axis_min = 0.5,
                    axis_max = 1,
                    color = tier_color,
                    value_label = "Posterior mean",
                    interval_label = "Posterior credible interval"
                ),
                "</div></div>",
                "<div class='board-cell' data-label='What changes'><div class='board-value'>What changes</div><div class='board-note'>", html_escape(change_text), "</div></div>",
                "</div>"
            )
        }),
        collapse = "\n"
    )

    paste0(
        "<div class='comparison-board comparison-board--divergence'>",
        "<div class='comparison-board__header'>",
        "<div>Bracket position where the paths split</div>",
        "<div>Candidate 1 choice</div>",
        "<div>Candidate 2 choice</div>",
        "<div>Posterior uncertainty</div>",
        "<div>What changes</div>",
        "</div>",
        row_html,
        "</div>"
    )
}

#' Render the upset opportunity board as a wrapped HTML comparison board
#'
#' @param decision_sheet The decision-sheet data frame.
#' @param top_n Maximum number of rows to render.
#'
#' @return A scalar character string containing HTML markup.
#' @keywords internal
render_upset_opportunity_svg <- function(decision_sheet, top_n = 10L) {
    if (nrow(decision_sheet) == 0) {
        return("<p class='empty-state'>No decision rows available.</p>")
    }

    plot_data <- decision_sheet %>%
        dplyr::arrange(dplyr::desc(upset_leverage), dplyr::desc(decision_score), round, matchup_number) %>%
        dplyr::slice_head(n = top_n) %>%
        dplyr::mutate(
            underdog_summary = paste0(underdog, " over ", posterior_favorite),
            pivot_note = purrr::pmap_chr(
                list(candidate_1_pick, candidate_2_pick, underdog, as.character(round)),
                build_upset_pivot_note
            ),
            underdog_interval = purrr::map2(ci_lower, ci_upper, derive_underdog_interval),
            underdog_lower = purrr::map_dbl(underdog_interval, "lower"),
            underdog_upper = purrr::map_dbl(underdog_interval, "upper"),
            tier_color = dashboard_tier_color(confidence_tier)
        )

    row_html <- paste(
        purrr::map_chr(seq_len(nrow(plot_data)), function(index) {
            row <- plot_data[index, , drop = FALSE]
            pick_source <- if (row$candidate_diff_flag[[1]]) {
                paste0("Candidate 1 takes ", row$candidate_1_pick[[1]], "; Candidate 2 takes ", row$candidate_2_pick[[1]], ".")
            } else {
                paste0("Both candidates currently take ", row$candidate_1_pick[[1]], ".")
            }

            paste0(
                "<div class='board-row board-row--upset'>",
                "<div class='board-cell board-cell--game' data-label='Underdog pivot'><div class='board-value'>", html_escape(row$underdog_summary[[1]]), "</div><div class='board-note'>Round: ", html_escape(as.character(row$round[[1]])), " | Tier: ", html_escape(as.character(row$confidence_tier[[1]])), "</div></div>",
                "<div class='board-cell board-cell--plot' data-label='Underdog posterior uncertainty'>",
                render_probability_track_html(
                    mean_probability = row$win_prob_underdog[[1]],
                    lower_probability = row$underdog_lower[[1]],
                    upper_probability = row$underdog_upper[[1]],
                    axis_min = 0,
                    axis_max = 0.5,
                    color = row$tier_color[[1]],
                    value_label = "Underdog posterior mean",
                    interval_label = "Derived posterior credible interval"
                ),
                "</div>",
                "<div class='board-cell' data-label='Candidate usage'><div class='board-value'>Candidate usage</div><div class='board-note'>", html_escape(pick_source), "</div></div>",
                "<div class='board-cell' data-label='Why to consider it'><div class='board-value'>Why to consider it</div><div class='board-note'>", html_escape(row$pivot_note[[1]]), "</div></div>",
                "</div>"
            )
        }),
        collapse = "\n"
    )

    paste0(
        "<div class='comparison-board comparison-board--upset'>",
        "<div class='comparison-board__header comparison-board__header--four'>",
        "<div>Underdog pivot</div>",
        "<div>Underdog posterior uncertainty</div>",
        "<div>Candidate usage</div>",
        "<div>Why to consider it</div>",
        "</div>",
        row_html,
        "</div>"
    )
}

#' Render a candidate fragility chart as inline SVG
#'
#' @param candidate_matchups A candidate matchup table with decision metadata.
#' @param top_n Maximum number of rows to render.
#'
#' @return A scalar character string containing inline SVG markup.
#' @keywords internal
render_candidate_fragility_svg <- function(candidate_matchups, top_n = 8L) {
    if (nrow(candidate_matchups) == 0) {
        return("<p class='empty-state'>No candidate matchups available.</p>")
    }

    plot_data <- candidate_matchups %>%
        dplyr::mutate(
            chosen_prob = ifelse(winner == teamA, win_prob_A, 1 - win_prob_A),
            chosen_ci_lower = ifelse(winner == teamA, ci_lower, 1 - ci_upper),
            chosen_ci_upper = ifelse(winner == teamA, ci_upper, 1 - ci_lower),
            matchup_label = sprintf("%s vs %s", teamA, teamB)
        ) %>%
        dplyr::arrange(dplyr::desc(decision_score), dplyr::desc(interval_width), round, matchup_number) %>%
        dplyr::slice_head(n = top_n) %>%
        dplyr::mutate(
            row_id = dplyr::row_number(),
            tier_color = dashboard_tier_color(confidence_tier)
        )

    width <- 860
    margin_left <- 250
    margin_right <- 170
    margin_top <- 34
    row_height <- 34
    plot_width <- width - margin_left - margin_right
    height <- margin_top + (nrow(plot_data) * row_height) + 34

    to_x <- function(probability) {
        clipped <- pmin(pmax(safe_numeric(probability, default = 0), 0), 1)
        margin_left + (clipped * plot_width)
    }

    grid_lines <- paste(
        vapply(seq(0, 1, by = 0.2), function(probability) {
            x <- to_x(probability)
            sprintf(
                "<line x1='%.1f' y1='16' x2='%.1f' y2='%.1f' stroke='#e7e5e4' stroke-dasharray='4 4' stroke-width='1'/>",
                x,
                x,
                height - 18
            )
        }, character(1)),
        collapse = "\n"
    )
    row_html <- paste(
        vapply(seq_len(nrow(plot_data)), function(index) {
            row <- plot_data[index, , drop = FALSE]
            y <- margin_top + ((index - 1) * row_height)
            sprintf(
                paste0(
                    "<g>",
                    "<text x='16' y='%.1f' font-size='12' fill='#1f2937' dominant-baseline='middle'>%s</text>",
                    "<line x1='%.1f' y1='%.1f' x2='%.1f' y2='%.1f' stroke='%s' stroke-width='4' stroke-linecap='round'/>",
                    "<circle cx='%.1f' cy='%.1f' r='6' fill='%s' stroke='white' stroke-width='2'/>",
                    "<text x='%.1f' y='%.1f' font-size='11' fill='#6b7280' dominant-baseline='middle'>%s</text>",
                    "</g>"
                ),
                y,
                html_escape(truncate_dashboard_label(row$matchup_label[[1]], width = 34L)),
                to_x(row$chosen_ci_lower[[1]]),
                y,
                to_x(row$chosen_ci_upper[[1]]),
                y,
                row$tier_color[[1]],
                to_x(row$chosen_prob[[1]]),
                y,
                row$tier_color[[1]],
                width - margin_right + 8,
                y,
                html_escape(truncate_dashboard_label(paste(row$winner[[1]], format_probability(row$chosen_prob[[1]]), sep = " | "), width = 24L))
            )
        }, character(1)),
        collapse = "\n"
    )
    axis_labels <- paste(
        vapply(seq(0, 1, by = 0.2), function(probability) {
            x <- to_x(probability)
            sprintf(
                "<text x='%.1f' y='%s' text-anchor='middle' font-size='11' fill='#57534e'>%s</text>",
                x,
                height - 2,
                format_probability(probability)
            )
        }, character(1)),
        collapse = "\n"
    )

    paste0(
        "<svg viewBox='0 0 ", width, " ", height, "' class='tech-svg' role='img' aria-label='Candidate fragility board'>",
        grid_lines,
        row_html,
        axis_labels,
        "<text x='16' y='18' font-size='11' fill='#6b7280'>Selected matchup</text>",
        "</svg>"
    )
}

#' Render a backtest calibration chart as inline SVG
#'
#' @param calibration_tbl A calibration summary tibble from
#'   [summarize_calibration()].
#'
#' @return A scalar character string containing inline SVG markup.
#' @keywords internal
render_calibration_svg <- function(calibration_tbl) {
    if (is.null(calibration_tbl) || nrow(calibration_tbl) == 0) {
        return("<p class='empty-state'>Backtest calibration was not available for this run.</p>")
    }

    width <- 520
    height <- 340
    margin_left <- 62
    margin_right <- 36
    margin_bottom <- 58
    plot_width <- width - margin_left - margin_right
    plot_height <- height - margin_bottom - 50
    to_x <- function(probability) margin_left + (safe_numeric(probability, default = 0) * plot_width)
    to_y <- function(probability) 30 + ((1 - safe_numeric(probability, default = 0)) * plot_height)

    calibration_tbl <- calibration_tbl %>%
        dplyr::arrange(mean_predicted)
    polyline_points <- paste(
        sprintf(
            "%.1f,%.1f",
            to_x(calibration_tbl$mean_predicted),
            to_y(calibration_tbl$empirical_rate)
        ),
        collapse = " "
    )
    points_html <- paste(
        sprintf(
            "<circle cx='%.1f' cy='%.1f' r='5' fill='#457b9d' stroke='white' stroke-width='1.5'><title>%s games</title></circle>",
            to_x(calibration_tbl$mean_predicted),
            to_y(calibration_tbl$empirical_rate),
            calibration_tbl$n_games
        ),
        collapse = "\n"
    )

    paste0(
        "<svg viewBox='0 0 ", width, " ", height, "' class='tech-svg tech-svg-compact' role='img' aria-label='Backtest calibration chart'>",
        "<text x='", margin_left, "' y='16' font-size='12' font-weight='700' fill='#334155'>Calibration curve</text>",
        "<line x1='", margin_left, "' y1='", 30 + plot_height, "' x2='", margin_left + plot_width, "' y2='30' stroke='#d6d3d1' stroke-dasharray='4 4' stroke-width='1.5'/>",
        "<line x1='", margin_left, "' y1='30' x2='", margin_left, "' y2='", 30 + plot_height, "' stroke='#a8a29e' stroke-width='1'/>",
        "<line x1='", margin_left, "' y1='", 30 + plot_height, "' x2='", margin_left + plot_width, "' y2='", 30 + plot_height, "' stroke='#a8a29e' stroke-width='1'/>",
        "<polyline points='", polyline_points, "' fill='none' stroke='#1d4ed8' stroke-width='3' stroke-linecap='round' stroke-linejoin='round'/>",
        points_html,
        "<text x='", margin_left, "' y='", height - 8, "' font-size='11' fill='#6b7280'>0%</text>",
        "<text x='", margin_left + plot_width, "' y='", height - 8, "' text-anchor='end' font-size='11' fill='#6b7280'>100%</text>",
        "<text x='", margin_left + (plot_width / 2), "' y='", height - 8, "' text-anchor='middle' font-size='11' fill='#6b7280'>Predicted probability</text>",
        "<text x='18' y='", 36 + (plot_height / 2), "' font-size='11' fill='#6b7280' transform='rotate(-90 18 ", 36 + (plot_height / 2), ")'>Observed win rate</text>",
        "<circle cx='", width - 194, "' cy='16' r='5' fill='#1d4ed8'/>",
        "<text x='", width - 182, "' y='20' font-size='11' fill='#6b7280'>Observed by bin</text>",
        "<line x1='", width - 92, "' y1='16' x2='", width - 64, "' y2='16' stroke='#d6d3d1' stroke-dasharray='4 4' stroke-width='1.5'/>",
        "<text x='", width - 58, "' y='20' font-size='11' fill='#6b7280'>Perfect line</text>",
        "</svg>"
    )
}

#' Create the technical bracket dashboard HTML
#'
#' @param bracket_year The active bracket year.
#' @param decision_sheet The decision-sheet data frame.
#' @param candidates A list of candidate bracket objects.
#' @param backtest Optional backtest result bundle.
#' @param total_points_predictions Optional list returned by
#'   [predict_candidate_total_points()].
#' @param play_in_resolution Optional one-row tibble from
#'   [summarize_play_in_resolution()] describing whether unresolved simulated
#'   First Four slots remain in the active bracket.
#' @param model_quality_context Optional resolved model-quality bundle used for
#'   the quality section and fallback dashboard copy.
#'
#' @return A complete HTML document as a scalar character string.
#' @export
create_technical_dashboard_html <- function(bracket_year, decision_sheet, candidates, backtest = NULL, total_points_predictions = NULL, play_in_resolution = NULL, model_quality_context = NULL) {
    top_decisions <- decision_sheet %>%
        dplyr::arrange(dplyr::desc(decision_score), round, region, matchup_number) %>%
        dplyr::mutate(
            inspection_level = dplyr::coalesce(inspection_level, "none"),
            round = as.character(round),
            region = as.character(region)
        ) %>%
        dplyr::transmute(
            rank = decision_rank,
            round,
            region,
            matchup = matchup_label,
            favorite = posterior_favorite,
            favorite_prob = format_probability(win_prob_favorite),
            interval = sprintf("%s-%s", format_probability(ci_lower), format_probability(ci_upper)),
            tier = confidence_tier,
            recommended_pick = candidate_1_pick,
            alternate_pick = candidate_2_pick,
            rationale = rationale_short,
            inspection_level
        )
    top_decision_classes <- inspection_row_class(top_decisions$inspection_level)
    top_decisions <- top_decisions %>%
        dplyr::select(-inspection_level)

    candidate_diff <- decision_sheet %>%
        dplyr::filter(candidate_diff_flag) %>%
        dplyr::mutate(
            inspection_level = dplyr::coalesce(inspection_level, "none"),
            round = as.character(round),
            region = as.character(region)
        ) %>%
        dplyr::transmute(
            round,
            region,
            slot = matchup_number,
            matchup = matchup_label,
            candidate_1 = candidate_1_pick,
            candidate_2 = candidate_2_pick,
            tier = confidence_tier,
            leverage = sprintf("%.2f", safe_numeric(upset_leverage, default = 0)),
            why = alternate_rationale,
            inspection_level
        )
    candidate_diff_classes <- inspection_row_class(candidate_diff$inspection_level)
    candidate_diff <- candidate_diff %>%
        dplyr::select(-inspection_level)

    candidate_one_path <- build_candidate_sequence_view(decision_sheet, "matchup_label", "candidate_1_pick")
    candidate_two_path <- build_candidate_sequence_view(decision_sheet, "candidate_2_matchup", "candidate_2_pick")
    candidate_one_classes <- inspection_row_class(candidate_one_path$inspection_level)
    candidate_two_classes <- inspection_row_class(candidate_two_path$inspection_level)
    candidate_one_path <- candidate_one_path %>% dplyr::select(-inspection_level)
    candidate_two_path <- candidate_two_path %>% dplyr::select(-inspection_level)

    quality_context <- model_quality_context
    if (is.null(quality_context)) {
        quality_context <- list(
            backtest = backtest,
            source_label = if (model_quality_has_backtest(backtest)) "Current run backtest" else "Backtest unavailable"
        )
    }
    quality_backtest <- quality_context$backtest %||% backtest
    quality_source_label <- quality_context$source_label %||% if (model_quality_has_backtest(quality_backtest)) "Current run backtest" else "Backtest unavailable"

    tier_counts <- decision_sheet %>%
        dplyr::mutate(confidence_tier = as.character(confidence_tier)) %>%
        dplyr::count(confidence_tier, .drop = FALSE)
    get_tier_count <- function(tier_name) {
        value <- tier_counts$n[tier_counts$confidence_tier == tier_name]
        if (length(value) == 0) {
            return(0L)
        }
        value[[1]]
    }

    top_leverage <- decision_sheet %>%
        dplyr::arrange(dplyr::desc(upset_leverage), dplyr::desc(decision_score)) %>%
        dplyr::slice_head(n = 1)
    leverage_text <- if (nrow(top_leverage) == 1L) {
        paste0(
            top_leverage$underdog[[1]],
            " over ",
            top_leverage$posterior_favorite[[1]],
            " | ",
            as.character(top_leverage$round[[1]])
        )
    } else {
        "No leverage slot available"
    }

    tiebreaker_summary <- total_points_predictions$candidate_summaries %||% tibble::tibble()
    championship_distribution <- total_points_predictions$championship_distribution %||% tibble::tibble()
    if (!"candidate_id" %in% names(tiebreaker_summary)) {
        tiebreaker_summary <- tibble::tibble()
    }
    if (!"candidate_id" %in% names(championship_distribution)) {
        championship_distribution <- tibble::tibble()
    }

    lookup_candidate_tiebreaker <- function(candidate_id_value) {
        if (nrow(tiebreaker_summary) == 0) {
            return(tibble::tibble())
        }
        tiebreaker_summary %>%
            dplyr::filter(candidate_id == candidate_id_value)
    }
    lookup_candidate_distribution <- function(candidate_id_value) {
        if (nrow(championship_distribution) == 0) {
            return(tibble::tibble())
        }
        championship_distribution %>%
            dplyr::filter(candidate_id == candidate_id_value)
    }
    distribution_scale_settings <- build_championship_distribution_scale(championship_distribution)

    candidate_summary_cards <- paste(
        purrr::map_chr(candidates, function(candidate) {
            paste0(
                "<div class='summary-card'>",
                "<div class='summary-label'>Candidate ", candidate$candidate_id, "</div>",
                "<div class='summary-value'>", html_escape(candidate$champion), "</div>",
                "<p class='summary-note'>", html_escape(candidate$type), " bracket | Final Four: ", html_escape(candidate$final_four), "</p>",
                "</div>"
            )
        }),
        collapse = "\n"
    )

    status_panel <- if (!is.null(play_in_resolution) && nrow(play_in_resolution) > 0 && isTRUE(play_in_resolution$has_unresolved_slots[[1]])) {
        paste0(
            "<div class='panel status-panel status-simulated'><strong>Status: Simulated bracket path.</strong> ",
            html_escape(sprintf(
                "%s of %s play-in slots are unresolved, so downstream paths can still move.",
                play_in_resolution$unresolved_slots[[1]],
                play_in_resolution$expected_slots[[1]]
            )),
            "</div>"
        )
    } else if (!is.null(play_in_resolution) && nrow(play_in_resolution) > 0) {
        "<div class='panel status-panel status-final'><strong>Status: Final result.</strong> First Four slots are resolved, so the path is stable.</div>"
    } else {
        "<div class='panel status-panel status-unknown'><strong>Status unavailable.</strong> Play-in resolution was not supplied for this run.</div>"
    }

    calibration_summary <- if (model_quality_has_backtest(quality_backtest)) {
        quality_backtest$summary %>%
            dplyr::transmute(
                `Log loss` = sprintf("%.3f", mean_log_loss),
                `Brier score` = sprintf("%.3f", mean_brier),
                Accuracy = format_probability(mean_accuracy),
                `Bracket score` = sprintf("%.1f", mean_bracket_score),
                `Correct picks` = sprintf("%.1f", mean_correct_picks)
            )
    } else {
        tibble::tibble(status = "Backtest skipped in this run.")
    }

    totals_section <- if (nrow(tiebreaker_summary) > 0) {
        panels <- paste(
            purrr::map_chr(candidates, function(candidate) {
                summary_row <- lookup_candidate_tiebreaker(candidate$candidate_id)
                distribution_row <- lookup_candidate_distribution(candidate$candidate_id)
                paste0(
                    "<div class='panel'>",
                    "<h3>Candidate ", candidate$candidate_id, " Championship Distribution</h3>",
                    render_championship_distribution_panel(summary_row, distribution_row, scale_settings = distribution_scale_settings),
                    "</div>"
                )
            }),
            collapse = "\n"
        )
        paste0(
            "<div class='panel'><h2>Championship Tiebreaker Comparison</h2>",
            render_board_explainer_html(
                what_this_shows = "These charts compare the full title-game total distribution for each candidate on a shared x-axis.",
                how_to_use = "Use the recommended tiebreaker as your default entry. Only move off it if you intentionally want a different miss-distance profile from the field.",
                why_it_matters = "If two brackets look similar, the tiebreaker can still decide the pool. Shared scales make it obvious when one path pulls totals materially higher or lower.",
                math_text = "The recommended tiebreaker is the rounded median of the championship total-points predictive distribution. The shaded band is the 80% interval and the bars show the probability mass over exact totals."
            ),
            "<div class='distribution-grid'>", panels, "</div></div>"
        )
    } else {
        paste0(
            "<div class='panel'><h2>Championship Tiebreaker Comparison</h2><p class='empty-state'>Championship total-points distributions were not supplied for this run.</p></div>"
        )
    }

    candidate_view_panel <- function(candidate, candidate_key, sequence_view, row_classes) {
        tiebreaker_row <- lookup_candidate_tiebreaker(candidate$candidate_id)
        tiebreaker_html <- if (nrow(tiebreaker_row) == 1L) {
            paste0(
                "<p><strong>Championship tiebreaker:</strong> ", html_escape(tiebreaker_row$recommended_tiebreaker_points[[1]]), "</p>",
                "<p><strong>Projected title game:</strong> ", html_escape(tiebreaker_row$championship_matchup[[1]]), "</p>",
                "<p><strong>Median total:</strong> ", sprintf("%.1f", safe_numeric(tiebreaker_row$predicted_total_median[[1]], default = NA_real_)),
                " <span class='muted'>| 80% interval ", html_escape(format_total_interval(tiebreaker_row$predicted_total_80_lower[[1]], tiebreaker_row$predicted_total_80_upper[[1]])), "</span></p>",
                "<p class='muted'>Use the recommended tiebreaker as the default entry because it is the rounded median championship total for this bracket path.</p>"
            )
        } else {
            "<p class='empty-state'>Championship tiebreaker unavailable.</p>"
        }

        paste0(
            "<div class='toggle-panel' data-view-panel='", candidate_key, "'>",
            "<div class='two-column'>",
            "<div class='panel'>",
            "<h2>Candidate ", candidate$candidate_id, " Overview</h2>",
            "<p><strong>Champion:</strong> ", html_escape(candidate$champion), "</p>",
            "<p><strong>Final Four:</strong> ", html_escape(candidate$final_four), "</p>",
            "<p><strong>Bracket log probability:</strong> ", sprintf("%.3f", candidate$bracket_log_prob), "</p>",
            "<p><strong>Mean picked-game probability:</strong> ", sprintf("%.3f", candidate$mean_game_prob), "</p>",
            "<p><strong>How this bracket differs:</strong> ", html_escape(candidate$diff_summary %||% "Matches the primary bracket."), "</p>",
            tiebreaker_html,
            "</div>",
            "<div class='panel'>",
            "<h2>Candidate ", candidate$candidate_id, " Most Fragile Picks</h2>",
            "<p class='panel-caption'>These are the slots where this bracket is leaning on the smallest edge or widest uncertainty band.</p>",
            render_candidate_fragility_svg(candidate$matchups),
            "</div>",
            "</div>",
            "<div class='panel'>",
            "<h2>Candidate ", candidate$candidate_id, " Path Review</h2>",
            "<p class='panel-caption'>Read this after Compare when you want the full single-bracket path without the side-by-side comparison noise.</p>",
            render_html_table(sequence_view, row_classes = row_classes),
            "</div>",
            "</div>"
        )
    }

    paste0(
        "<!DOCTYPE html><html><head><meta charset='utf-8'>",
        "<title>mmBayes Technical Dashboard</title>",
        "<style>",
        "body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;background:#f5f1e8;color:#111827;margin:0;padding:24px;line-height:1.5;}",
        "h1,h2,h3{margin:0 0 8px 0;} h1{font-size:32px;} h2{margin-top:26px;font-size:21px;} h3{font-size:17px;}",
        ".lede{max-width:980px;color:#374151;margin:8px 0 18px 0;}",
        ".quality-intro{margin:10px 0 14px 0;color:#374151;max-width:760px;}",
        ".quality-grid{display:grid;grid-template-columns:minmax(360px,1.15fr) minmax(320px,0.85fr);gap:18px;align-items:start;}",
        ".quality-card{background:#fff;border:1px solid #e7e5e4;border-radius:14px;padding:14px;box-shadow:0 1px 2px rgba(0,0,0,0.03);}",
        ".quality-card h3{margin-bottom:10px;}",
        ".quality-note{margin:10px 0 0 0;font-size:13px;color:#6b7280;}",
        ".panel{background:white;border:1px solid #d6d3d1;border-radius:16px;padding:18px;box-shadow:0 2px 10px rgba(15,23,42,0.04);margin-bottom:18px;}",
        ".panel-caption{color:#6b7280;margin:0 0 12px 0;}",
        ".overview-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(180px,1fr));gap:14px;margin:18px 0;}",
        ".summary-card{background:white;border:1px solid #d6d3d1;border-radius:14px;padding:16px;box-shadow:0 2px 8px rgba(15,23,42,0.04);}",
        ".summary-label{text-transform:uppercase;letter-spacing:0.08em;font-size:11px;color:#6b7280;margin-bottom:8px;}",
        ".summary-value{font-size:24px;font-weight:700;line-height:1.1;color:#111827;}",
        ".summary-note{font-size:13px;color:#4b5563;margin:8px 0 0 0;}",
        ".two-column{display:grid;grid-template-columns:repeat(auto-fit,minmax(320px,1fr));gap:18px;align-items:start;}",
        ".guide-grid,.explain-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(220px,1fr));gap:14px;}",
        ".guide-card,.explain-card{background:#fff;border:1px solid #d6d3d1;border-radius:14px;padding:14px;}",
        ".guide-label,.explain-label{text-transform:uppercase;letter-spacing:0.08em;font-size:11px;color:#6b7280;margin-bottom:8px;}",
        ".guide-card p,.explain-card p{margin:0;color:#111827;}",
        ".distribution-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(280px,1fr));gap:16px;}",
        ".distribution-meta p{margin:0 0 8px 0;}",
        ".distribution-note{font-size:13px;color:#475569;max-width:54ch;}",
        ".status-panel{font-size:15px;}",
        ".status-panel strong{display:inline-block;margin-right:6px;text-transform:uppercase;letter-spacing:0.03em;}",
        ".status-simulated{background:#fff3cd;border-color:#d97706;color:#7c2d12;}",
        ".status-final{background:#ecfdf3;border-color:#16a34a;color:#14532d;}",
        ".status-unknown{background:#eef2ff;border-color:#6366f1;color:#312e81;}",
        ".toggle-bar{display:flex;gap:10px;flex-wrap:wrap;margin:18px 0 16px 0;}",
        ".toggle-button{appearance:none;border:1px solid #cbd5e1;background:white;border-radius:999px;padding:10px 16px;font-size:14px;font-weight:600;color:#334155;cursor:pointer;}",
        ".toggle-button.is-active{background:#0f172a;color:white;border-color:#0f172a;}",
        ".toggle-panel{display:none;}",
        ".toggle-panel.is-active{display:block;}",
        ".dashboard-table{width:100%;border-collapse:collapse;font-size:13px;background:white;}",
        ".dashboard-table th,.dashboard-table td{border:1px solid #e7e5e4;padding:8px 10px;vertical-align:top;text-align:left;}",
        ".dashboard-table th{background:#f8fafc;}",
        ".dashboard-table tr.inspection-primary td{background:#fff7cc;}",
        ".dashboard-table tr.inspection-primary td:first-child{border-left:4px solid #d97706;}",
        ".dashboard-table tr.inspection-secondary td{background:#fff1eb;}",
        ".dashboard-table tr.inspection-secondary td:first-child{border-left:4px solid #ea580c;}",
        ".tech-svg{width:100%;height:auto;background:#fcfbf7;border:1px solid #e7e5e4;border-radius:14px;padding:8px;}",
        ".tech-svg-compact{max-width:520px;}",
        ".comparison-board{display:flex;flex-direction:column;border:1px solid #e7e5e4;border-radius:16px;background:#fff;overflow:hidden;}",
        ".comparison-board__header{display:grid;grid-template-columns:minmax(220px,1.25fr) minmax(150px,0.8fr) minmax(150px,0.8fr) minmax(320px,1.4fr) minmax(260px,1.25fr);gap:16px;padding:16px 18px;background:#fcfbf7;border-bottom:1px solid #e7e5e4;font-size:12px;font-weight:700;letter-spacing:0.03em;color:#64748b;text-transform:uppercase;}",
        ".comparison-board__header--four{grid-template-columns:minmax(220px,1.2fr) minmax(320px,1.35fr) minmax(220px,1fr) minmax(260px,1.1fr);}",
        ".board-row{display:grid;gap:16px;padding:18px;align-items:start;border-top:1px solid #f1f5f9;}",
        ".board-row:first-of-type{border-top:none;}",
        ".board-row--ranked,.board-row--divergence{grid-template-columns:minmax(220px,1.25fr) minmax(150px,0.8fr) minmax(150px,0.8fr) minmax(320px,1.4fr) minmax(260px,1.25fr);}",
        ".board-row--upset{grid-template-columns:minmax(220px,1.2fr) minmax(320px,1.35fr) minmax(220px,1fr) minmax(260px,1.1fr);}",
        ".board-cell{min-width:0;overflow-wrap:anywhere;}",
        ".board-cell::before{display:none;content:attr(data-label);margin-bottom:6px;font-size:11px;font-weight:700;letter-spacing:0.08em;text-transform:uppercase;color:#6b7280;}",
        ".board-cell--plot{min-width:0;}",
        ".board-value{font-size:15px;font-weight:700;line-height:1.35;color:#0f172a;overflow-wrap:anywhere;}",
        ".board-note{margin-top:4px;font-size:14px;line-height:1.45;color:#475569;overflow-wrap:anywhere;}",
        ".prob-track{display:flex;flex-direction:column;gap:8px;min-width:0;}",
        ".prob-track__lane{position:relative;height:22px;border-radius:999px;border:1px solid #dbe4ef;background:linear-gradient(90deg,#f8fafc 0%,#f1f5f9 100%);overflow:visible;}",
        ".prob-track__range{position:absolute;top:50%;height:6px;transform:translateY(-50%);border-radius:999px;opacity:0.95;}",
        ".prob-track__point{position:absolute;top:50%;width:16px;height:16px;transform:translate(-50%,-50%);border-radius:999px;border:3px solid #fff;box-shadow:0 1px 4px rgba(15,23,42,0.2);}",
        ".prob-track__scale{display:flex;justify-content:space-between;gap:8px;font-size:11px;color:#64748b;}",
        ".prob-track__caption{font-size:13px;line-height:1.45;color:#334155;overflow-wrap:anywhere;}",
        ".prob-track__caption strong{color:#111827;}",
        ".prob-track-stack{display:flex;flex-direction:column;gap:12px;}",
        ".prob-track-block{padding:12px;border:1px solid #e7e5e4;border-radius:12px;background:#fafaf9;}",
        ".prob-track-block__title{font-size:11px;font-weight:700;letter-spacing:0.08em;text-transform:uppercase;color:#6b7280;margin-bottom:8px;}",
        ".muted{color:#6b7280;}",
        ".empty-state{color:#6b7280;font-style:italic;}",
        ".legend-row{display:flex;gap:12px;flex-wrap:wrap;margin:12px 0 18px 0;}",
        ".legend-chip{display:flex;align-items:center;gap:8px;padding:8px 10px;border-radius:999px;background:white;border:1px solid #d6d3d1;font-size:13px;}",
        ".legend-swatch{width:12px;height:12px;border-radius:999px;display:inline-block;}",
        ".legend-copy{font-size:12px;color:#6b7280;}",
        ".quality-metric-list p{margin:0 0 10px 0;color:#334155;}",
        ".quality-metric-list strong{color:#111827;}",
        "@media (max-width: 1240px){.comparison-board__header{display:none;}.board-row--ranked,.board-row--divergence,.board-row--upset{grid-template-columns:1fr;}.board-cell::before{display:block;}}",
        "@media (max-width: 900px){.quality-grid{grid-template-columns:1fr;}.tech-svg-compact{max-width:none;}}",
        "</style></head><body>",
        "<h1>mmBayes Technical Bracket Dashboard</h1>",
        "<p class='lede'>Bracket year ", html_escape(bracket_year),
        ". Start in Compare to decide which games actually deserve your attention, then switch to Candidate 1 or Candidate 2 only after you want the full path for one bracket. This view is built to explain not just the recommendation, but also the math driving it.</p>",
        status_panel,
        "<div class='panel'>",
        "<h2>How To Use This Dashboard</h2>",
        "<div class='guide-grid'>",
        "<div class='guide-card'><div class='guide-label'>Workflow</div><p>Use Compare first. It shows the review-priority queue, the underdog pivots worth considering, where the candidates actually diverge, and the tiebreaker implications.</p></div>",
        "<div class='guide-card'><div class='guide-label'>Confidence tiers</div><p>Locks are stable favorites. Leans still favor one side. Toss-ups are true decision points. Volatile games have wider uncertainty even when a favorite exists.</p></div>",
        "<div class='guide-card'><div class='guide-label'>Interpret the dots and bars</div><p>Dots are posterior mean win probabilities and bars are posterior intervals. The more important the round and the wider the uncertainty, the higher a game rises in the review queue.</p></div>",
        "</div>",
        render_confidence_legend_html(),
        "</div>",
        "<div class='overview-grid'>",
        candidate_summary_cards,
        "<div class='summary-card'><div class='summary-label'>Toss-up games</div><div class='summary-value'>", get_tier_count("Toss-up"), "</div><p class='summary-note'>Primary manual-review slots.</p></div>",
        "<div class='summary-card'><div class='summary-label'>Volatile games</div><div class='summary-value'>", get_tier_count("Volatile"), "</div><p class='summary-note'>Wide intervals with unstable outcomes.</p></div>",
        "<div class='summary-card'><div class='summary-label'>Candidate differences</div><div class='summary-value'>", sum(decision_sheet$candidate_diff_flag, na.rm = TRUE), "</div><p class='summary-note'>Slots where the alternate path diverges.</p></div>",
        "<div class='summary-card'><div class='summary-label'>Top leverage upset</div><div class='summary-value'>", html_escape(truncate_dashboard_label(leverage_text, width = 22L)), "</div><p class='summary-note'>Highest underdog payoff under current scoring.</p></div>",
        "</div>",
        "<div class='toggle-bar' role='group' aria-label='Candidate dashboard view'>",
        "<button type='button' class='toggle-button is-active' data-view-target='compare' aria-pressed='true'>Compare</button>",
        "<button type='button' class='toggle-button' data-view-target='candidate-1' aria-pressed='false'>Candidate 1</button>",
        "<button type='button' class='toggle-button' data-view-target='candidate-2' aria-pressed='false'>Candidate 2</button>",
        "</div>",
        "<div class='toggle-panel is-active' data-view-panel='compare'>",
        "<div class='panel'><h2>Ranked Decision Board</h2>",
        render_board_explainer_html(
            what_this_shows = "The highest-priority games to revisit, ordered by how important and uncertain they are. Each row now shows the full posterior credible interval for the favorite.",
            how_to_use = "Start here. Read from top to bottom and only spend manual review time on the rows that genuinely change your bracket choices. Use the interval to see whether the edge is narrow or actually stable.",
            why_it_matters = "This is the fastest way to separate cosmetic uncertainty from decisions that can meaningfully move your pool outcome.",
            math_text = "Rows are sorted by review priority = round weight x (underdog win probability + interval width). A game rises when it is both later in the bracket and less settled."
        ),
        render_ranked_decision_svg(decision_sheet),
        "</div>",
        "<div class='panel'><h2>Upset Opportunity Board</h2>",
        render_board_explainer_html(
            what_this_shows = "The underdog picks with the best mix of plausible win probability and downstream payoff. Each row shows the underdog posterior mean and the derived credible interval for that upset path.",
            how_to_use = "Use this after the ranked board when you want to decide whether a risky upset is worth taking or leaving as a favorite. The uncertainty track shows whether the underdog has a real chance or just a thin tail.",
            why_it_matters = "Not every upset helps. This board surfaces the swings that can actually change the shape of the bracket instead of rewarding random chaos.",
            math_text = "Rows are sorted by leverage = round weight x underdog win probability x (1 + interval width). A pivot ranks higher when the underdog has a real shot and the round payoff is larger."
        ),
        render_upset_opportunity_svg(decision_sheet),
        "</div>",
        "<div class='panel'><h2>Candidate Divergence</h2>",
        render_board_explainer_html(
            what_this_shows = "Each row is one bracket position where the two candidates stop matching. Sometimes they still reach the same game and choose different winners; other times an earlier pick changed the path, so the candidates are now in different games entirely.",
            how_to_use = "Use this to understand what the alternate bracket is actually buying you. Focus first on late-round disagreements because they do the most work, then read the What changes column to see whether the fork is just a different winner or a fully different route.",
            why_it_matters = "A second bracket candidate is only useful if you can explain how and where it differs from the safer path.",
            math_text = "This board filters to rows where the chosen winner or matchup path changes between candidates. Late-round rows are shown first because they create larger downstream payoff differences."
        ),
        render_candidate_divergence_svg(decision_sheet),
        "</div>",
        totals_section,
        "<div class='panel'><h2>Model Quality</h2>",
        render_board_explainer_html(
            what_this_shows = "Backtest metrics for probability quality and pool-scoring quality, plus the calibration curve.",
            how_to_use = "Use this as a trust check. If calibration is poor or score metrics are weak, treat close dashboard recommendations with more skepticism.",
            why_it_matters = "Better probability calibration helps every downstream board. Bracket score matters most for pool decisions because it respects round weighting, not just raw pick count.",
            math_text = "Log loss and Brier score reward accurate probabilities. Bracket score is the average pool points earned under standard round scoring, while correct picks is the average number of games chosen correctly."
        ),
        "<p class='quality-intro'>The chart on the right is the calibration curve. The blue points are the observed win rates inside each predicted-probability bin. If that blue path stays near the dotted diagonal, the model's probabilities are lining up well with reality.</p>",
        if (model_quality_has_backtest(quality_backtest)) {
            paste0(
                "<p class='panel-caption'><strong>Source:</strong> ", html_escape(quality_source_label), "</p>",
                "<div class='quality-grid'><div class='quality-card'>",
                render_html_table(calibration_summary),
                "<div class='quality-metric-list'>",
                "<p><strong>Bracket score</strong> means expected pool points under standard round scoring. It matters more than raw pick count because late-round misses cost much more.</p>",
                "<p><strong>Correct picks</strong> means the average number of games chosen correctly. It is intuitive, but it treats every game equally and is therefore less useful than bracket score for pool strategy.</p>",
                "<p><strong>Log loss</strong> and <strong>Brier score</strong> both measure probability quality. Lower is better because the model is assigning more honest probabilities.</p>",
                "</div></div><div class='quality-card'>",
                render_calibration_svg(quality_backtest$calibration %||% tibble::tibble()),
                "<p class='quality-note'>Empirical rate means the observed win rate within each probability bin. Points above the diagonal indicate the model was under-confident in that region; points below it indicate over-confidence.</p>",
                "</div></div>"
            )
        } else {
            "<p class='empty-state'>Backtest skipped in this run.</p>"
        },
        "</div>",
        "</div>",
        candidate_view_panel(candidates[[1]], "candidate-1", candidate_one_path, candidate_one_classes),
        candidate_view_panel(if (length(candidates) >= 2) candidates[[2]] else candidates[[1]], "candidate-2", candidate_two_path, candidate_two_classes),
        "<h2>Ranked Decision Table</h2><div class='panel'>",
        render_html_table(top_decisions, row_classes = top_decision_classes),
        "</div>",
        "<h2>Candidate Difference Table</h2><div class='panel'>",
        render_html_table(candidate_diff, row_classes = candidate_diff_classes),
        "</div>",
        "<script>",
        "(function(){",
        "var buttons=document.querySelectorAll('[data-view-target]');",
        "var panels=document.querySelectorAll('[data-view-panel]');",
        "function setView(view){",
        "panels.forEach(function(panel){panel.classList.toggle('is-active', panel.getAttribute('data-view-panel')===view);});",
        "buttons.forEach(function(button){var active=button.getAttribute('data-view-target')===view;button.classList.toggle('is-active', active);button.setAttribute('aria-pressed', active ? 'true' : 'false');});",
        "}",
        "buttons.forEach(function(button){button.addEventListener('click', function(){setView(button.getAttribute('data-view-target'));});});",
        "setView('compare');",
        "})();",
        "</script>",
        "</body></html>"
    )
}
