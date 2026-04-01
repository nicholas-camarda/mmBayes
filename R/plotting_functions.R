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
    x <- gsub("\"", "&quot;", x, fixed = TRUE)
    gsub("'", "&#39;", x, fixed = TRUE)
}

#' Build a dashboard href for local or hosted use
#'
#' @param filename Dashboard filename inside `output/`.
#'
#' @return A relative file path by default, or an absolute URL when
#'   `MMBAYES_DASHBOARD_BASE_URL` is set.
#' @keywords internal
dashboard_preview_url <- function(filename) {
    base_url <- Sys.getenv("MMBAYES_DASHBOARD_BASE_URL", unset = "")
    base_url <- sub("/+$", "", base_url)
    if (nzchar(base_url)) {
        return(sprintf("%s/%s", base_url, filename))
    }
    filename
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
        return("<p class='empty-state'>Championship tiebreaker not included for this run.</p>")
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
        return("<p class='empty-state'>Championship tiebreaker not included for this run.</p>")
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

#' Render a compact glossary for the dashboard variables
#'
#' @return A scalar character string containing HTML markup for the glossary.
#' @keywords internal
render_dashboard_term_legend_html <- function() {
    term_rows <- tibble::tibble(
        term = c(
            "Barthag",
            "AdjOE",
            "AdjDE",
            "WAB",
            "TOR",
            "TORD",
            "ORB",
            "DRB",
            "3P%",
            "3P%D",
            "Adj T.",
            "decision_score",
            "upset_leverage",
            "confidence_tier",
            "inspection_level"
        ),
        definition = c(
            "Overall strength rating",
            "Adjusted offense",
            "Adjusted defense",
            "Wins above bubble",
            "Turnover rate",
            "Opponent turnover rate",
            "Offensive rebounding",
            "Defensive rebounding",
            "Three-point make rate",
            "Opponent three-point make rate",
            "Adjusted tempo",
            "Review priority score",
            "Upset payoff if it hits",
            "Lock / Lean / Toss-up / Volatile",
            "Primary, secondary, or watchlist"
        )
    )

    chips <- paste(
        vapply(seq_len(nrow(term_rows)), function(index) {
            row <- term_rows[index, , drop = FALSE]
            sprintf(
                "<div class='term-chip'><strong>%s</strong><span>%s</span></div>",
                html_escape(row$term[[1]]),
                html_escape(row$definition[[1]])
            )
        }, character(1)),
        collapse = "\n"
    )

    paste0(
        "<section class='term-legend' aria-label='Key terms'>",
        "<div class='term-legend__label'>Key terms</div>",
        "<div class='term-legend__grid'>",
        chips,
        "</div>",
        "<p class='term-legend__note'>These are the short labels used in the dashboard cards and evidence panels. Seeds still appear beside every matchup.</p>",
        "</section>"
    )
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

#' Render a model-overview panel or strip
#'
#' @param model_overview A model-overview bundle returned by
#'   [summarize_model_overview()].
#' @param compact Whether to render a compact strip instead of a full panel.
#'
#' @return A scalar character string containing HTML markup.
#' @keywords internal
render_model_overview_html <- function(model_overview, compact = FALSE) {
    if (is.null(model_overview) || length(model_overview) == 0) {
        empty_html <- "<p class='empty-state'>Model overview was not supplied for this run.</p>"
        return(if (compact) empty_html else paste0("<div class='panel'><h2>Model Overview</h2>", empty_html, "</div>"))
    }

    if (!is.null(model_overview$matchup) || !is.null(model_overview$totals)) {
        render_model_card <- function(title, overview) {
            if (is.null(overview) || length(overview) == 0) {
                return("")
            }

            bart_config <- overview$bart_config %||% list()
            bart_rows <- if (identical(overview$engine, "bart")) {
                tibble::tibble(
                    Setting = c("n_trees", "n_burn", "n_post", "k", "power"),
                    Value = c(
                        bart_config$n_trees %||% NA_integer_,
                        bart_config$n_burn %||% NA_integer_,
                        bart_config$n_post %||% NA_integer_,
                        bart_config$k %||% NA_real_,
                        bart_config$power %||% NA_real_
                    )
                )
            } else {
                tibble::tibble(
                    Setting = c("Engine", "Prior type"),
                    Value = c(overview$engine_label %||% overview$engine %||% "Unknown", overview$prior_type %||% "default")
                )
            }

            paste0(
                "<div class='quality-card'>",
                "<h3>", html_escape(title), "</h3>",
                "<div class='overview-grid'>",
                "<div class='summary-card'><div class='summary-label'>Engine</div><div class='summary-value'>", html_escape(overview$engine_label %||% overview$engine %||% "Unknown"), "</div><p class='summary-note'>", html_escape(if (identical(overview$engine, "bart")) "Nonlinear posterior tree model" else "Bayesian logistic regression"), "</p></div>",
                "<div class='summary-card'><div class='summary-label'>Draw budget</div><div class='summary-value'>", html_escape(if (is.finite(safe_numeric(overview$draw_budget, default = NA_real_))) format(safe_numeric(overview$draw_budget, default = NA_real_), scientific = FALSE) else "n/a"), "</div><p class='summary-note'>Posterior draws used for scoring and simulation.</p></div>",
                "<div class='summary-card'><div class='summary-label'>Predictors</div><div class='summary-value'>", html_escape(as.character(overview$predictor_count %||% 0L)), "</div><p class='summary-note'>", html_escape(overview$predictor_summary %||% "No predictors were recorded."), "</p></div>",
                "<div class='summary-card'><div class='summary-label'>Feature mix</div><div class='summary-value'>", html_escape(if (length(overview$interaction_terms %||% character(0)) > 0) "Interactions on" else "No interactions"), "</div><p class='summary-note'>", html_escape(if (length(overview$interaction_terms %||% character(0)) > 0) paste("Interaction terms:", paste(overview$interaction_terms, collapse = ", ")) else "No explicit interaction terms were supplied."), "</p></div>",
                "</div>",
                if (nrow(bart_rows) > 0) paste0(
                    "<h4>", html_escape(if (identical(overview$engine, "bart")) "BART settings" else "Model settings"), "</h4>",
                    render_html_table(bart_rows)
                ) else "",
                "</div>"
            )
        }

        panels <- c(
            if (!is.null(model_overview$matchup)) render_model_card("Matchup Model", model_overview$matchup) else "",
            if (!is.null(model_overview$totals)) render_model_card("Total Points Model", model_overview$totals) else ""
        )
        panels <- panels[nzchar(panels)]
        if (length(panels) == 0) {
            return("")
        }

        return(
            paste0(
                "<div class='panel'><h2>Model Overview</h2>",
                if (compact) paste0("<p class='panel-caption'>Model metadata for the current run.</p>") else "",
                "<div class='quality-grid'>",
                paste(panels, collapse = "\n"),
                "</div>",
                "</div>"
            )
        )
    }

    engine_label <- model_overview$engine_label %||% model_overview$engine %||% "Unknown"
    draw_budget <- model_overview$draw_budget %||% NA_real_
    predictor_summary <- model_overview$predictor_summary %||% "No predictors were recorded."
    bart_config <- model_overview$bart_config %||% list()
    prior_type <- model_overview$prior_type %||% NULL
    interaction_terms <- model_overview$interaction_terms %||% character(0)

    bart_rows <- if (identical(model_overview$engine, "bart")) {
        tibble::tibble(
            Setting = c("n_trees", "n_burn", "n_post", "k", "power"),
            Value = c(
                bart_config$n_trees %||% NA_integer_,
                bart_config$n_burn %||% NA_integer_,
                bart_config$n_post %||% NA_integer_,
                bart_config$k %||% NA_real_,
                bart_config$power %||% NA_real_
            )
        )
    } else {
        tibble::tibble(
            Setting = c("Engine", "Prior type"),
            Value = c(engine_label, prior_type %||% "default")
        )
    }

    summary_cards <- paste0(
        "<div class='overview-grid'>",
        "<div class='summary-card'><div class='summary-label'>Engine</div><div class='summary-value'>", html_escape(engine_label), "</div><p class='summary-note'>", html_escape(if (identical(model_overview$engine, "bart")) "Nonlinear posterior tree model" else "Bayesian logistic regression"), "</p></div>",
        "<div class='summary-card'><div class='summary-label'>Draw budget</div><div class='summary-value'>", html_escape(if (is.finite(draw_budget)) format(draw_budget, scientific = FALSE) else "n/a"), "</div><p class='summary-note'>Posterior draws used for scoring and simulation.</p></div>",
        "<div class='summary-card'><div class='summary-label'>Predictors</div><div class='summary-value'>", html_escape(as.character(model_overview$predictor_count %||% 0L)), "</div><p class='summary-note'>", html_escape(predictor_summary), "</p></div>",
        "<div class='summary-card'><div class='summary-label'>Feature mix</div><div class='summary-value'>", html_escape(if (length(interaction_terms) > 0) "Interactions on" else "No interactions"), "</div><p class='summary-note'>", html_escape(if (length(interaction_terms) > 0) paste("Interaction terms:", paste(interaction_terms, collapse = ", ")) else "No explicit interaction terms were supplied."), "</p></div>",
        "</div>"
    )

    bart_table <- if (nrow(bart_rows) > 0) {
        paste0(
            "<div class='quality-card'><h3>", html_escape(if (identical(model_overview$engine, "bart")) "BART settings" else "Model settings"), "</h3>",
            render_html_table(bart_rows),
            "</div>"
        )
    } else {
        ""
    }

    if (compact) {
        paste0(
            "<div class='model-overview-strip'>",
            "<strong>Engine:</strong> ", html_escape(engine_label),
            " | <strong>Draw budget:</strong> ", html_escape(if (is.finite(draw_budget)) format(draw_budget, scientific = FALSE) else "n/a"),
            " | <strong>Predictors:</strong> ", html_escape(as.character(model_overview$predictor_count %||% 0L)),
            if (identical(model_overview$engine, "bart")) paste0(" | <strong>BART:</strong> ", html_escape(paste0("trees=", bart_config$n_trees %||% "?", ", post=", bart_config$n_post %||% "?"))) else "",
            "</div>"
        )
    } else {
        paste0(
            "<div class='panel'><h2>Model Overview</h2>",
            summary_cards,
            "<div class='quality-grid'>",
            bart_table,
            "<div class='quality-card'><h3>Feature summary</h3><p>", html_escape(predictor_summary), "</p>",
            if (length(interaction_terms) > 0) paste0("<p><strong>Interactions:</strong> ", html_escape(paste(interaction_terms, collapse = ", ")), "</p>") else "",
            "</div>",
            "</div>",
            "</div>"
        )
    }
}

#' Render a model diagnostics panel
#'
#' @param quality_backtest A backtest result bundle.
#' @param quality_source_label Optional label describing the source of the metrics.
#'
#' @return A scalar character string containing HTML markup.
#' @keywords internal
render_model_diagnostics_html <- function(quality_backtest, quality_source_label = NULL) {
    if (!model_quality_has_backtest(quality_backtest)) {
        return(
            paste0(
                if (!is.null(quality_source_label)) paste0("<p class='panel-caption'><strong>Source:</strong> ", html_escape(quality_source_label), "</p>") else "",
                "<div class='quality-grid'>",
                "<div class='quality-card'><h3>Backtest Summary</h3><p class='empty-state'>Backtest not computed in this run.</p></div>",
                "<div class='quality-card'><h3>What this means</h3>",
                "<div class='diagnostic-callout'><strong>Doing well</strong><ul><li>No backtest snapshot was supplied for this run.</li></ul></div>",
                "<div class='diagnostic-callout'><strong>Needs attention</strong><ul><li>Run the full simulation pipeline if you want calibration and round-level diagnostics here.</li></ul></div>",
                "</div>",
                "</div>",
                "<div class='quality-grid'>",
                "<div class='quality-card'><h3>Backtest Calibration Curve</h3><p class='empty-state'>Backtest calibration was not available for this run.</p></div>",
                "<div class='quality-card'><h3>Backtest By Round</h3><p class='empty-state'>Round-level diagnostics were not available for this run.</p></div>",
                "</div>"
            )
        )
    }

    diagnostics <- summarize_backtest_diagnostics(quality_backtest)
    summary_tbl <- quality_backtest$summary %>% dplyr::slice_head(n = 1)
    backtest_years_note <- if (!is.null(diagnostics$backtest_years) && nzchar(diagnostics$backtest_years)) {
        paste0(
            "<p class='summary-note backtest-window-note'><strong>Rolling holdout years:</strong> ",
            html_escape(diagnostics$backtest_years),
            "</p>"
        )
    } else {
        ""
    }
    calibration_summary <- summary_tbl %>%
        dplyr::transmute(
            `Log loss` = sprintf("%.3f", mean_log_loss),
            `Brier score` = sprintf("%.3f", mean_brier),
            Accuracy = format_probability(mean_accuracy),
            `Bracket score` = sprintf("%.1f", mean_bracket_score),
            `Correct picks` = sprintf("%.1f", mean_correct_picks)
        )

    round_tbl <- diagnostics$round_summary %||% tibble::tibble()
    round_rows <- if (nrow(round_tbl) > 0) {
        paste(
            purrr::map_chr(seq_len(nrow(round_tbl)), function(index) {
                row <- round_tbl[index, , drop = FALSE]
                paste0(
                    "<tr>",
                    "<td>", html_escape(as.character(row$round[[1]])), "</td>",
                    "<td>", html_escape(as.character(row$games[[1]])), "</td>",
                    "<td>", html_escape(format_probability(row$accuracy[[1]])), "</td>",
                    "<td>", html_escape(sprintf("%.3f", row$log_loss[[1]])), "</td>",
                    "<td>", html_escape(sprintf("%.3f", row$brier[[1]])), "</td>",
                    "<td>", html_escape(format_probability(row$empirical_rate[[1]])), "</td>",
                    "</tr>"
                )
            }),
            collapse = "\n"
        )
    } else {
        ""
    }

    strength_lines <- paste(
        purrr::map_chr(diagnostics$strengths, function(item) paste0("<li>", html_escape(item), "</li>")),
        collapse = "\n"
    )
    weakness_lines <- paste(
        purrr::map_chr(diagnostics$weaknesses, function(item) paste0("<li>", html_escape(item), "</li>")),
        collapse = "\n"
    )

    paste0(
        if (!is.null(quality_source_label)) paste0("<p class='panel-caption'><strong>Source:</strong> ", html_escape(quality_source_label), "</p>") else "",
        "<p class='quality-intro'>The backtest is the historical baseline, so you can judge the live performance panel that follows against seasons the model has already seen.</p>",
        "<div class='quality-grid'>",
        "<div class='quality-card'><h3>Backtest Summary</h3>", backtest_years_note, render_html_table(calibration_summary), "</div>",
        "<div class='quality-card'><h3>What this means</h3>",
        "<div class='diagnostic-callout'><strong>Doing well</strong><ul>", if (nzchar(strength_lines)) strength_lines else "<li>No strengths identified.</li>", "</ul></div>",
        "<div class='diagnostic-callout'><strong>Needs attention</strong><ul>", if (nzchar(weakness_lines)) weakness_lines else "<li>No weaknesses identified.</li>", "</ul></div>",
        "</div>",
        "</div>",
        "<div class='quality-grid'>",
        "<div class='quality-card'><h3>Backtest Calibration Curve</h3>", render_calibration_svg(quality_backtest$calibration %||% tibble::tibble()), render_calibration_help_html(), "</div>",
        "<div class='quality-card'><h3>Backtest By Round</h3>",
        "<table class='dashboard-table'><thead><tr><th>Round</th><th>Games</th><th>Accuracy</th><th>Log loss</th><th>Brier</th><th>Empirical rate</th></tr></thead><tbody>", round_rows, "</tbody></table>",
        "</div>",
        "</div>",
        ""
    )
}

#' Render a compact link panel for the model comparison dashboard
#'
#' @param model_comparison A comparison bundle returned by the main pipeline.
#'
#' @return A scalar character string containing HTML markup.
#' @keywords internal
render_model_comparison_link_html <- function(model_comparison) {
    if (is.null(model_comparison) || length(model_comparison) == 0) {
        return("")
    }

    if (!isTRUE(model_comparison$available %||% FALSE)) {
        note <- model_comparison$status %||% "Model comparison was requested, but the alternate engine could not be fit."
        return(
            paste0(
                "<div class='panel comparison-link-panel'>",
                "<h2>Model Comparison</h2>",
                "<p class='panel-caption'>", html_escape(note), "</p>",
                "</div>"
            )
        )
    }

    current_label <- model_comparison$current_label %||% "Current model"
    alternate_label <- model_comparison$alternate_label %||% "Alternate model"
    summary_text <- model_comparison$summary$text %||% "A comparison summary is available."

    paste0(
        "<div class='panel comparison-link-panel'>",
        "<h2>Model Comparison</h2>",
        "<p class='panel-caption'>Compare ", html_escape(current_label), " against ", html_escape(alternate_label), " on the same data. The dedicated comparison page keeps the scorecards together so the main dashboard stays readable.</p>",
        "<div class='overview-grid'>",
        "<div class='summary-card'><div class='summary-label'>Current engine</div><div class='summary-value'>", html_escape(current_label), "</div><p class='summary-note'>Primary run used for the bracket and live dashboard.</p></div>",
        "<div class='summary-card'><div class='summary-label'>Alternate engine</div><div class='summary-value'>", html_escape(alternate_label), "</div><p class='summary-note'>The compare page shows the alternate fit alongside the current run.</p></div>",
        "<div class='summary-card'><div class='summary-label'>Backtest takeaway</div><div class='summary-value summary-value--wrap summary-value--tight'>", html_escape(if (is.character(summary_text)) summary_text else "See comparison"), "</div><p class='summary-note'>Backtest winner summary from the comparison bundle.</p></div>",
        "</div>",
        "<p class='comparison-link-copy'><a href='", dashboard_preview_url("model_comparison_dashboard.html"), "'>Open the full model comparison dashboard</a></p>",
        "</div>"
    )
}

#' Build a compact summary for a model-comparison dashboard
#'
#' @param model_comparison A comparison bundle returned by the main pipeline.
#'
#' @return A scalar character string containing HTML markup.
#' @keywords internal
render_model_comparison_summary_html <- function(model_comparison) {
    if (is.null(model_comparison) || length(model_comparison) == 0) {
        return("<p class='empty-state'>Model comparison was not supplied for this run.</p>")
    }

    if (!isTRUE(model_comparison$available %||% FALSE)) {
        return(
            paste0(
                "<div class='quality-card'>",
                "<h3>Comparison unavailable</h3>",
                "<p class='empty-state'>", html_escape(model_comparison$status %||% "The alternate engine could not be fit."), "</p>",
                "</div>"
            )
        )
    }

    current_label <- model_comparison$current_label %||% "Current model"
    alternate_label <- model_comparison$alternate_label %||% "Alternate model"
    backtest_table <- model_comparison$backtest_comparison %||% tibble::tibble()
    live_table <- model_comparison$live_comparison %||% tibble::tibble()
    modeling_notes <- model_comparison$notes %||% character()
    current_live_model <- normalize_model_overview(model_comparison$current$model_overview %||% list())
    alternate_live_model <- normalize_model_overview(model_comparison$alternate$model_overview %||% list())
    current_live_model_label <- current_live_model$engine_label %||% current_live_model$engine %||% current_label
    alternate_live_model_label <- alternate_live_model$engine_label %||% alternate_live_model$engine %||% alternate_label
    backtest_summary <- summarize_model_metric_comparison(backtest_table, current_label, alternate_label)
    live_summary <- summarize_model_metric_comparison(live_table, current_label, alternate_label)
    notes_html <- if (length(modeling_notes) > 0L) {
        paste(
            purrr::map_chr(modeling_notes, function(note) paste0("<li>", html_escape(note), "</li>")),
            collapse = "\n"
        )
    } else {
        ""
    }

    paste0(
        "<div class='panel'>",
        "<h2>Model Comparison</h2>",
        "<p class='panel-caption'>This page compares ", html_escape(current_label), " and ", html_escape(alternate_label), " on the same tournament data. Start in Compare, then move into an engine tab only if you want the full diagnostics behind a winner.</p>",
        if (nzchar(notes_html)) paste0(
            "<div class='quality-card comparison-note-card'>",
            "<h3>Modeling note</h3>",
            "<ul class='note-list'>", notes_html, "</ul>",
            "</div>"
        ) else "",
        "<div class='overview-grid'>",
        "<div class='summary-card'><div class='summary-label'>Backtest takeaway</div><div class='summary-value summary-value--wrap summary-value--tight'>", html_escape(backtest_summary$text), "</div><p class='summary-note'>Cross-validation style metrics from completed historical games.</p></div>",
        "<div class='summary-card'><div class='summary-label'>Live takeaway</div><div class='summary-value summary-value--wrap summary-value--tight'>", html_escape(live_summary$text), "</div><p class='summary-note'>Current-year games already completed in the live dashboard.</p></div>",
        "<div class='summary-card'><div class='summary-label'>Engine tabs</div><div class='summary-value'>3</div><p class='summary-note'>Compare, current engine, and alternate engine.</p></div>",
        "</div>",
        "<div class='comparison-tab-bar' role='group' aria-label='Model comparison view'>",
        "<button type='button' class='toggle-button is-active' data-model-view-target='compare' aria-pressed='true'>Compare</button>",
        "<button type='button' class='toggle-button' data-model-view-target='current' aria-pressed='false'>", html_escape(current_label), "</button>",
        "<button type='button' class='toggle-button' data-model-view-target='alternate' aria-pressed='false'>", html_escape(alternate_label), "</button>",
        "</div>",
        "<div class='toggle-panel is-active' data-model-view-panel='compare'>",
        "<div class='quality-grid'>",
        "<div class='quality-card'><h3>Historical Backtest Metrics</h3><p class='panel-caption'>Held-out tournament summaries across completed historical seasons.</p>", render_html_table(backtest_table), "</div>",
        "<div class='quality-card'><h3>Current-Year Live Metrics</h3><p class='panel-caption'>Columns are labeled by engine. These metrics are from completed current-year games only.</p>", render_html_table(live_table), "</div>",
        "</div>",
        "</div>",
        "<div class='toggle-panel' data-model-view-panel='current'>",
        render_model_overview_html(model_comparison$current$model_overview %||% list()),
        render_model_diagnostics_html(model_comparison$current$backtest %||% NULL, quality_source_label = paste(current_label, "backtest")),
        render_live_performance_html(model_comparison$current$live_performance %||% NULL, model_label = current_live_model_label),
        "</div>",
        "<div class='toggle-panel' data-model-view-panel='alternate'>",
        render_model_overview_html(model_comparison$alternate$model_overview %||% list()),
        render_model_diagnostics_html(model_comparison$alternate$backtest %||% NULL, quality_source_label = paste(alternate_label, "backtest")),
        render_live_performance_html(model_comparison$alternate$live_performance %||% NULL, model_label = alternate_live_model_label),
        "</div>",
        "<script>",
        "(function(){",
        "var buttons=document.querySelectorAll('[data-model-view-target]');",
        "var panels=document.querySelectorAll('[data-model-view-panel]');",
        "function setView(view){",
        "panels.forEach(function(panel){panel.classList.toggle('is-active', panel.getAttribute('data-model-view-panel')===view);});",
        "buttons.forEach(function(button){var active=button.getAttribute('data-model-view-target')===view;button.classList.toggle('is-active', active);button.setAttribute('aria-pressed', active ? 'true' : 'false');});",
        "}",
        "buttons.forEach(function(button){button.addEventListener('click', function(){setView(button.getAttribute('data-model-view-target'));});});",
        "setView('compare');",
        "})();",
        "</script>",
        "</div>"
    )
}

#' Create the model comparison dashboard HTML
#'
#' @param bracket_year The active bracket year.
#' @param model_comparison A comparison bundle returned by the main pipeline.
#'
#' @return A complete HTML document as a scalar character string.
#' @export
create_model_comparison_dashboard_html <- function(bracket_year, model_comparison) {
    if (is.null(model_comparison) || length(model_comparison) == 0) {
        return(paste0(
            "<!DOCTYPE html><html><head><meta charset='utf-8'><title>mmBayes Model Comparison</title></head><body>",
            "<h1>mmBayes Model Comparison</h1>",
            "<p class='empty-state'>Model comparison was not supplied for this run.</p>",
            "<p><a href='", dashboard_preview_url("technical_dashboard.html"), "'>Back to the technical dashboard</a></p>",
            "</body></html>"
        ))
    }

    current_label <- model_comparison$current_label %||% "Current model"
    alternate_label <- model_comparison$alternate_label %||% "Alternate model"
    comparison_status <- if (isTRUE(model_comparison$available %||% FALSE)) {
        model_comparison$status %||% "Model comparison completed."
    } else {
        model_comparison$status %||% "Model comparison could not be completed."
    }

    paste0(
        "<!DOCTYPE html><html><head><meta charset='utf-8'>",
        "<title>mmBayes Model Comparison</title>",
        "<style>",
        "body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;background:#f5f1e8;color:#111827;margin:0;padding:24px;line-height:1.5;}",
        "h1,h2,h3{margin:0 0 8px 0;} h1{font-size:32px;} h2{margin-top:26px;font-size:21px;} h3{font-size:17px;}",
        ".lede{max-width:980px;color:#374151;margin:8px 0 18px 0;}",
        ".panel{background:white;border:1px solid #d6d3d1;border-radius:16px;padding:18px;box-shadow:0 2px 10px rgba(15,23,42,0.04);margin-bottom:18px;}",
        ".quality-card{background:#fff;border:1px solid #e7e5e4;border-radius:14px;padding:14px;box-shadow:0 1px 2px rgba(0,0,0,0.03);}",
        ".quality-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(320px,1fr));gap:16px;}",
        ".summary-card{background:white;border:1px solid #d6d3d1;border-radius:14px;padding:16px;box-shadow:0 2px 8px rgba(15,23,42,0.04);}",
        ".summary-label{text-transform:uppercase;letter-spacing:0.08em;font-size:11px;color:#6b7280;margin-bottom:8px;}",
        ".summary-value{font-size:24px;font-weight:700;line-height:1.1;color:#111827;overflow-wrap:anywhere;}",
        ".summary-value--wrap{font-size:18px;line-height:1.2;}",
        ".summary-value--wrap.summary-value--tight{font-size:16px;line-height:1.15;}",
        ".summary-note{font-size:13px;color:#4b5563;margin:8px 0 0 0;}",
        ".comparison-note-card{margin-bottom:16px;}",
        ".note-list{margin:0;padding-left:18px;color:#374151;}",
        ".note-list li + li{margin-top:8px;}",
        ".overview-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(180px,1fr));gap:14px;margin:18px 0;}",
        ".toggle-button{appearance:none;border:1px solid #cbd5e1;background:white;border-radius:999px;padding:10px 16px;font-size:14px;font-weight:600;color:#334155;cursor:pointer;}",
        ".toggle-button.is-active{background:#0f172a;color:white;border-color:#0f172a;}",
        ".toggle-panel{display:none;}",
        ".toggle-panel.is-active{display:block;}",
        ".comparison-tab-bar{display:flex;gap:10px;flex-wrap:wrap;margin:18px 0 16px 0;}",
        ".comparison-link-copy{margin:14px 0 0 0;font-size:14px;}",
        ".dashboard-table{width:100%;border-collapse:collapse;font-size:13px;background:white;}",
        ".dashboard-table th,.dashboard-table td{border:1px solid #e7e5e4;padding:8px 10px;vertical-align:top;text-align:left;}",
        ".dashboard-table th{background:#f8fafc;}",
        ".empty-state{color:#6b7280;font-style:italic;}",
        ".panel-caption{color:#6b7280;margin:0 0 12px 0;}",
        "@media (max-width: 900px){.quality-grid{grid-template-columns:1fr;}}",
        "</style></head><body>",
        "<h1>mmBayes Model Comparison</h1>",
        "<p class='lede'>Bracket year ", html_escape(bracket_year), ". This page compares ", html_escape(current_label), " and ", html_escape(alternate_label), " so you can judge calibration, accuracy, and live behavior without mixing the comparison into the main bracket workflow.</p>",
        "<div class='panel'><strong>Status:</strong> ", html_escape(comparison_status), "</div>",
        "<p class='comparison-link-copy'><a href='", dashboard_preview_url("technical_dashboard.html"), "'>Back to the technical dashboard</a></p>",
        render_model_comparison_summary_html(model_comparison),
        "</body></html>"
    )
}

#' Create the bracket dashboard HTML
#'
#' Legacy renderer retained only as an internal historical reference while the
#' workflow dashboard lives below. The new renderer is the active implementation.
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
#' @keywords internal
.deprecated_create_bracket_dashboard_html <- function(bracket_year, decision_sheet, candidates, backtest = NULL, play_in_resolution = NULL, total_points_predictions = NULL, model_quality_context = NULL, model_overview = NULL, model_comparison = NULL) {
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
        quality_context <- list(backtest = backtest, source_label = "Current run backtest")
    }
    quality_backtest <- NULL
    if (!is.null(quality_context) && is.list(quality_context) && model_quality_has_backtest(quality_context$backtest %||% NULL)) {
        quality_backtest <- quality_context$backtest
    }
    if (!model_quality_has_backtest(quality_backtest) && model_quality_has_backtest(backtest)) {
        quality_backtest <- backtest
    }
    quality_source_label <- quality_context$source_label %||% "Current run backtest"

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
        tibble::tibble()
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
                "<div class='tiebreaker-block'><div class='tiebreaker-label'>Championship Tiebreaker</div><p class='empty-state'>Prediction not included for this run.</p></div>"
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
            "<div class='panel status-panel status-unknown'><strong>Status not supplied.</strong>",
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
        ".term-legend{margin:14px 0 18px 0;padding:14px 16px;border:1px solid #e2e8f0;border-radius:18px;background:linear-gradient(180deg,#ffffff 0%,#f8fafc 100%);}",
        ".term-legend__label{font-size:11px;font-weight:700;letter-spacing:0.1em;text-transform:uppercase;color:#64748b;margin-bottom:10px;}",
        ".term-legend__grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(180px,1fr));gap:8px 10px;}",
        ".term-chip{padding:8px 10px;border-radius:14px;background:#fff;border:1px solid #dbe4ef;}",
        ".term-chip strong{display:block;font-size:13px;color:#0f172a;line-height:1.2;}",
        ".term-chip span{display:block;font-size:12px;color:#475569;line-height:1.35;margin-top:2px;}",
        ".term-legend__note{margin:10px 0 0 0;font-size:12px;color:#64748b;max-width:88ch;}",
        ".empty-state{color:#6b7280;font-style:italic;}",
        ".model-overview-strip{margin:10px 0 18px 0;padding:10px 12px;border-radius:999px;background:#eef2ff;color:#1e293b;font-size:13px;border:1px solid #c7d2fe;}",
        ".comparison-link-panel{border-left:5px solid #0f172a;}",
        ".comparison-link-copy{margin:14px 0 0 0;font-size:14px;}",
        ".diagnostic-callout{margin:10px 0 0 0;padding:12px 14px;border-radius:12px;background:#f8fafc;border:1px solid #e2e8f0;}",
        ".diagnostic-callout strong{display:block;margin-bottom:8px;}",
        ".scatter-svg{width:100%;height:auto;background:white;border:1px solid #d6d3d1;border-radius:14px;padding:8px;}",
        "</style></head><body>",
        "<h1>mmBayes Bracket Decision Console</h1>",
        "<p class='lede'>Bracket year ", html_escape(bracket_year),
        ". Use Candidate 1 as the safest expected-value bracket and Candidate 2 as the bounded-risk alternate. The tables below surface the hardest calls first, make the championship tiebreaker explicit for each bracket, and highlight the rows that deserve closer inspection.</p>",
        render_model_overview_html(model_overview, compact = TRUE),
        render_model_comparison_link_html(model_comparison),
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
        if (model_quality_has_backtest(quality_backtest)) {
            paste0(
                "<h2>Calibration Snapshot</h2><div class='panel'>",
                "<p class='panel-caption'><strong>Source:</strong> ", html_escape(quality_source_label), "</p>",
                render_html_table(calibration_summary),
                "</div>"
            )
        } else {
            ""
        },
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
            alternate_pick = dplyr::case_when(
                candidate_diff_flag ~ as.character(candidate_2_pick),
                TRUE ~ "Same pick in both candidates"
            ),
            alternate_note = dplyr::case_when(
                candidate_diff_flag & candidate_1_pick == candidate_2_pick ~ "Same winner, different path",
                candidate_diff_flag ~ "Different winner on the alternate path",
                TRUE ~ "No alternate path divergence"
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
                "<div class='board-cell' data-label='Alternate path'><div class='board-value'>", html_escape(row$alternate_pick[[1]]), "</div><div class='board-note'>", html_escape(row$alternate_note[[1]]), "</div></div>",
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

#' Render the interactive bracket tree SVG section
#'
#' Converts the output of [build_bracket_tree_data()] into a self-contained
#' HTML snippet containing one SVG bracket tree per candidate, candidate tab
#' controls, a tier legend, and a floating tooltip element driven by vanilla JS
#' in the parent dashboard.
#'
#' @param tree_data The list returned by [build_bracket_tree_data()], with a
#'   `trees` entry containing candidate-specific tree objects.
#'
#' @return A scalar character string containing HTML markup ready to embed in
#'   the bracket dashboard.
#' @keywords internal
render_bracket_tree_svg <- function(tree_data) {
    tree_entries <- tree_data$trees %||% list()
    if (length(tree_entries) == 0L) {
        return("<p class='empty-state'>Bracket tree data not available.</p>")
    }

    SVG_WIDTH     <- 1400L
    SVG_HEIGHT    <- 1070L
    TOP_MARGIN    <- 55L
    REGION_HEIGHT <- 240L
    NODE_W        <- 120L
    NODE_H        <- 28L

    round_x <- c(
        "Round of 64"  = 170L,
        "Round of 32"  = 350L,
        "Sweet 16"     = 530L,
        "Elite 8"      = 710L,
        "Final Four"   = 890L,
        "Championship" = 1070L
    )
    round_labels_short <- c(
        "Round of 64"  = "ROUND OF 64",
        "Round of 32"  = "ROUND OF 32",
        "Sweet 16"     = "SWEET 16",
        "Elite 8"      = "ELITE 8",
        "Final Four"   = "FINAL FOUR",
        "Championship" = "CHAMP"
    )

    region_row_index <- c("East" = 0L, "South" = 1L, "Midwest" = 2L, "West" = 3L)

    # --- Round column labels ---
    col_labels_html <- paste(
        vapply(names(round_x), function(rnd) {
            sprintf(
                "<text x='%d' y='22' text-anchor='middle' font-size='10' font-weight='700' fill='#64748b' font-family='system-ui,sans-serif'>%s</text>",
                round_x[[rnd]], round_labels_short[[rnd]]
            )
        }, character(1L)),
        collapse = "\n"
    )

    # --- Region band backgrounds and labels ---
    region_names <- c("East", "South", "Midwest", "West")
    band_colors  <- c("rgba(248,250,252,0.6)", "rgba(241,245,249,0.6)", "rgba(248,250,252,0.6)", "rgba(241,245,249,0.6)")

    region_bands_html <- paste(
        vapply(region_names, function(r) {
            row_idx  <- region_row_index[[r]]
            y_top    <- TOP_MARGIN + row_idx * REGION_HEIGHT
            y_mid    <- y_top + REGION_HEIGHT / 2L
            bg_color <- band_colors[[row_idx + 1L]]
            sep_line <- if (row_idx < 3L) {
                sprintf("<line x1='60' y1='%d' x2='%d' y2='%d' stroke='#e2e8f0' stroke-width='1'/>",
                    y_top + REGION_HEIGHT, SVG_WIDTH, y_top + REGION_HEIGHT)
            } else ""
            paste0(
                sprintf("<rect x='0' y='%d' width='%d' height='%d' fill='%s'/>",
                    y_top, SVG_WIDTH, REGION_HEIGHT, bg_color),
                sprintf("<text x='56' y='%d' text-anchor='end' font-size='13' font-weight='700' fill='#334155' dominant-baseline='middle' font-family='system-ui,sans-serif'>%s</text>",
                    y_mid, r),
                sep_line
            )
        }, character(1L)),
        collapse = "\n"
    )

    clean_attr <- function(value) {
        value <- as.character(value %||% "")
        value <- gsub("[\r\n\t]+", " ", value)
        html_escape(value)
    }

    render_tree_panel <- function(tree, is_active = FALSE) {
        nodes <- tree$nodes %||% tibble::tibble()
        edges <- tree$edges %||% tibble::tibble()
        panel_key <- sprintf("candidate-%s", tree$candidate_id %||% 1L)

        if (nrow(nodes) == 0L) {
            return("")
        }

        nodes$tier_color <- dashboard_tier_color(as.character(nodes$confidence_tier))
        nodes$label_a <- paste0(
            ifelse(!is.na(nodes$teamA_seed), paste0("(", nodes$teamA_seed, ") "), ""),
            truncate_dashboard_label(nodes$teamA, width = 9L)
        )
        nodes$label_b <- paste0(
            ifelse(!is.na(nodes$teamB_seed), paste0("(", nodes$teamB_seed, ") "), ""),
            truncate_dashboard_label(nodes$teamB, width = 9L)
        )

        edges_html <- if (nrow(edges) > 0L) {
            paste(
                vapply(seq_len(nrow(edges)), function(i) {
                    edge_row <- edges[i, ]
                    x1 <- as.integer(round(edge_row$x1[[1L]]))
                    y1 <- as.integer(round(edge_row$y1[[1L]]))
                    x2 <- as.integer(round(edge_row$x2[[1L]]))
                    y2 <- as.integer(round(edge_row$y2[[1L]]))
                    sx <- x1 + NODE_W %/% 2L
                    tx <- x2 - NODE_W %/% 2L
                    mid <- (sx + tx) %/% 2L

                    sprintf(
                        "<path d='M%d,%d L%d,%d L%d,%d L%d,%d' fill='none' stroke='#94a3b8' stroke-width='1.5' class='btree-edge'/>",
                        sx, y1, mid, y1, mid, y2, tx, y2
                    )
                }, character(1L)),
                collapse = "\n"
            )
        } else {
            ""
        }

        nodes_html <- paste(
            vapply(seq_len(nrow(nodes)), function(i) {
                node_row <- nodes[i, ]
                x <- node_row$node_x[[1L]]
                y <- node_row$node_y[[1L]]
                if (is.na(x) || is.na(y)) {
                    return("")
                }

                x <- as.integer(round(x))
                y <- as.integer(round(y))
                tier <- as.character(node_row$confidence_tier[[1L]] %||% "")
                text_color <- if (tier %in% c("Toss-up", "Volatile")) "#0f172a" else "#ffffff"
                rx <- x - NODE_W %/% 2L
                ry <- y - NODE_H %/% 2L
                pick_value <- as.character(node_row$candidate_pick[[1L]] %||% node_row$winner[[1L]] %||% "")
                prob_val <- node_row$win_prob_favorite[[1L]]
                prob_str <- if (!is.null(prob_val) && length(prob_val) == 1L && !is.na(prob_val)) {
                    sprintf("%.0f%%", 100 * prob_val)
                } else {
                    "n/a"
                }

                paste0(
                    sprintf(paste0(
                        "<g class=\"btree-node\"",
                        " data-slot=\"%s\"",
                        " data-open-evidence=\"%s\"",
                        " data-tip-candidate=\"%s\"",
                        " data-tip-matchup=\"%s\"",
                        " data-tip-round=\"%s\"",
                        " data-tip-region=\"%s\"",
                        " data-tip-pick=\"%s\"",
                        " data-tip-fav=\"%s\"",
                        " data-tip-prob=\"%s\"",
                        " data-tip-tier=\"%s\"",
                        " data-tip-rationale=\"%s\"",
                        ">"
                    ),
                    clean_attr(as.character(node_row$slot_key[[1L]])),
                    clean_attr(as.character(node_row$evidence_id[[1L]])),
                    clean_attr(as.character(tree$candidate_label %||% paste("Candidate", tree$candidate_id %||% 1L))),
                    clean_attr(paste0(as.character(node_row$teamA[[1L]] %||% ""), " vs ", as.character(node_row$teamB[[1L]] %||% ""))),
                    clean_attr(as.character(node_row$round[[1L]] %||% "")),
                    clean_attr(as.character(node_row$region[[1L]] %||% "")),
                    clean_attr(pick_value),
                    clean_attr(as.character(node_row$posterior_favorite[[1L]] %||% "")),
                    clean_attr(prob_str),
                    clean_attr(tier),
                    clean_attr(as.character(node_row$rationale_short[[1L]] %||% ""))
                    ),
                    sprintf(
                        "<rect x='%d' y='%d' width='%d' height='%d' rx='4' fill='%s'/>",
                        rx, ry, NODE_W, NODE_H, as.character(node_row$tier_color[[1L]])
                    ),
                    sprintf(
                        "<text x='%d' y='%d' text-anchor='middle' font-size='9' fill='%s' dominant-baseline='middle' font-family='system-ui,sans-serif'>%s</text>",
                        x, y - 7L, text_color, html_escape(as.character(node_row$label_a[[1L]]))
                    ),
                    sprintf(
                        "<text x='%d' y='%d' text-anchor='middle' font-size='9' fill='%s' dominant-baseline='middle' font-family='system-ui,sans-serif'>%s</text>",
                        x, y + 7L, text_color, html_escape(as.character(node_row$label_b[[1L]]))
                    ),
                    if (isTRUE(node_row$upset[[1L]])) sprintf(
                        "<text x='%d' y='%d' font-size='8' font-weight='800' fill='#e76f51' font-family='system-ui,sans-serif' class='btree-upset-badge'>U</text>",
                        rx + NODE_W - 10L, ry + 2L
                    ) else "",
                    "</g>"
                )
            }, character(1L)),
            collapse = "\n"
        )

        paste0(
            "<div class='btree-panel", if (is_active) " is-active" else "", "' data-btree-panel='", panel_key, "'",
            if (!is_active) " hidden" else "", ">",
            "<div class='bracket-tree-container'>",
            sprintf(
                "<svg id='btree-svg-%s' class='btree-svg' viewBox='0 0 %d %d' width='100%%' style='min-width:%dpx;display:block;'>",
                tree$candidate_id %||% 1L, SVG_WIDTH, SVG_HEIGHT, SVG_WIDTH
            ),
            "<g class='btree-bg'>", region_bands_html, "</g>",
            "<g class='btree-col-labels'>", col_labels_html, "</g>",
            "<g class='btree-edges'>", edges_html, "</g>",
            "<g class='btree-nodes'>", nodes_html, "</g>",
            "</svg>",
            "</div>",
            "</div>"
        )
    }

    controls_html <- if (length(tree_entries) > 1L) {
        paste0(
            "<div class='bracket-tree-controls'>",
            paste(
                vapply(seq_along(tree_entries), function(index) {
                    tree <- tree_entries[[index]]
                    sprintf(
                        "<button class='btree-toggle%s' data-btree-target='candidate-%s'>%s</button>",
                        if (index == 1L) " is-active" else "",
                        tree$candidate_id %||% index,
                        html_escape(as.character(tree$candidate_label %||% sprintf("Candidate %d", index)))
                    )
                }, character(1L)),
                collapse = ""
            ),
            "</div>"
        )
    } else {
        ""
    }

    # --- Legend ---
    tier_palette <- dashboard_tier_palette()
    legend_items <- paste(
        vapply(names(tier_palette), function(tier_name) {
            sprintf("<span class='btree-legend-item'><span class='btree-legend-swatch' style='background:%s'></span>%s</span>",
                tier_palette[[tier_name]], html_escape(tier_name))
        }, character(1L)),
        collapse = ""
    )
    legend_html <- paste0(
        "<div class='btree-legend'>",
        legend_items,
        "<span class='btree-legend-item'><span style='font-weight:800;color:#e76f51;font-size:11px;'>U</span> Upset pick</span>",
        "</div>"
    )

    paste0(
        controls_html,
        legend_html,
        paste(
            vapply(seq_along(tree_entries), function(index) {
                render_tree_panel(tree_entries[[index]], is_active = index == 1L)
            }, character(1L)),
            collapse = ""
        ),
        "<div id='btree-tooltip'></div>"
    )
}

#' Render a live tournament performance panel
#'
#' @param live_performance A live performance summary returned by
#'   [summarize_live_tournament_performance()].
#' @param model_label Optional human-readable model label for the panel.
#'
#' @return A scalar character string containing HTML markup.
#' @keywords internal
render_live_performance_html <- function(live_performance, model_label = NULL) {
    format_live_round_label <- function(round_name) {
        if (identical(round_name, "Championship")) {
            return("National Championship")
        }
        round_name
    }
    model_label <- if (!is.null(model_label) && nzchar(as.character(model_label)[[1]])) {
        as.character(model_label)[[1]]
    } else {
        NULL
    }
    panel_title <- if (!is.null(model_label)) {
        paste0("Live Tournament Performance - ", model_label)
    } else {
        "Live Tournament Performance"
    }

    if (is.null(live_performance)) {
        return(
            paste0("<div class='panel'><h2>", html_escape(panel_title), "</h2><p class='empty-state'>No completed current-year games have been recorded yet.</p></div>")
        )
    }

    summary_tbl <- live_performance$summary %||% tibble::tibble()
    if (nrow(summary_tbl) == 0) {
        return(
            paste0(
                "<div class='panel'><h2>", html_escape(panel_title), "</h2>",
                "<p class='empty-state'>", html_escape(live_performance$status %||% "No completed current-year games have been recorded yet."), "</p>",
                "</div>"
            )
        )
    }

    main_bracket_tbl <- live_performance$main_bracket_summary %||% tibble::tibble()
    round_tbl <- live_performance$round_summary %||% tibble::tibble()
    recent_games <- live_performance$games %||% tibble::tibble()
    recent_games_title <- live_performance$recent_games_title %||% "Recent Games"
    recent_games_note <- live_performance$recent_games_note %||% "Ordered by the best available monitoring metadata."
    monitoring_note <- live_performance$monitoring_note %||% "Monitoring only: current-year results are for evaluation and commentary."
    interpretive_status <- live_performance$interpretive_status %||% "early read"
    main_bracket_games_played <- safe_numeric(live_performance$main_bracket_games_played %||% 0, default = 0)

    format_metric_value <- function(value, digits = 3L, percent = FALSE) {
        value <- safe_numeric(value, default = NA_real_)
        if (!is.finite(value)) {
            return("n/a")
        }
        if (isTRUE(percent)) {
            return(format_probability(value))
        }
        sprintf(paste0("%.", digits, "f"), value)
    }

    render_summary_card <- function(title, summary_row, note_text) {
        if (nrow(summary_row) == 0) {
            return(
                paste0(
                    "<div class='quality-card'>",
                    "<h3>", html_escape(title), "</h3>",
                    "<p class='empty-state'>No live summary is available for this view yet.</p>",
                    "</div>"
                )
            )
        }

        paste0(
            "<div class='quality-card'>",
            "<h3>", html_escape(title), "</h3>",
            "<div class='overview-grid' style='margin-top:0;'>",
            "<div class='summary-card'><div class='summary-label'>Games played</div><div class='summary-value'>", html_escape(as.character(safe_numeric(summary_row$games_played[[1]], default = 0))), "</div><p class='summary-note'>", html_escape(note_text), "</p></div>",
            "<div class='summary-card'><div class='summary-label'>Accuracy</div><div class='summary-value'>", html_escape(format_metric_value(summary_row$accuracy[[1]], percent = TRUE)), "</div><p class='summary-note'>Share of scored games the model picked correctly.</p></div>",
            "<div class='summary-card'><div class='summary-label'>Log loss</div><div class='summary-value'>", html_escape(format_metric_value(summary_row$log_loss[[1]], digits = 3L)), "</div><p class='summary-note'>Lower means the probabilities are more honest.</p></div>",
            "<div class='summary-card'><div class='summary-label'>Brier score</div><div class='summary-value'>", html_escape(format_metric_value(summary_row$brier[[1]], digits = 3L)), "</div><p class='summary-note'>Lower means tighter probability estimates.</p></div>",
            "</div>",
            "</div>"
        )
    }

    overview_cards <- paste0(
        "<div class='quality-grid'>",
        render_summary_card(
            "Overall live performance",
            summary_tbl,
            "All completed current-year games, including First Four."
        ),
        render_summary_card(
            "Main-bracket live performance",
            main_bracket_tbl,
            if (main_bracket_games_played == 0) {
                "No Round of 64+ games have completed yet."
            } else {
                "Excludes First Four so play-in games do not dominate the signal."
            }
        ),
        "</div>"
    )

    round_rows <- if (nrow(round_tbl) > 0) {
        paste(
            purrr::map_chr(seq_len(nrow(round_tbl)), function(index) {
                row <- round_tbl[index, , drop = FALSE]
                paste0(
                    "<tr>",
                    "<td>", html_escape(format_live_round_label(row$round[[1]])), "</td>",
                    "<td>", html_escape(as.character(row$games[[1]])), "</td>",
                    "<td>", html_escape(format_probability(row$accuracy[[1]])), "</td>",
                    "<td>", html_escape(format_probability(row$mean_predicted_prob[[1]])), "</td>",
                    "</tr>"
                )
            }),
            collapse = "\n"
        )
    } else {
        ""
    }

    recent_rows <- if (nrow(recent_games) > 0) {
        paste(
            purrr::map_chr(seq_len(nrow(recent_games)), function(index) {
                row <- recent_games[index, , drop = FALSE]
                paste0(
                    "<tr>",
                    "<td>", html_escape(format_live_round_label(as.character(row$round[[1]]))), "</td>",
                    "<td>", html_escape(row$teamA[[1]]), " vs ", html_escape(row$teamB[[1]]), "</td>",
                    "<td>", html_escape(row$actual_winner[[1]]), "</td>",
                    "<td>", html_escape(row$model_pick[[1]]), "</td>",
                    "<td>", html_escape(format_probability(row$predicted_prob[[1]])), "</td>",
                    "<td>", html_escape(row$model_pick_note[[1]]), "</td>",
                    "</tr>"
                )
            }),
            collapse = "\n"
        )
    } else {
        ""
    }

    paste0(
        "<div class='panel'>",
        "<h2>", html_escape(panel_title), "</h2>",
        if (!is.null(model_label)) {
            paste0("<p class='panel-caption'><strong>Model:</strong> ", html_escape(model_label), "</p>")
        } else {
            ""
        },
        "<p class='panel-caption'>", html_escape(live_performance$status %||% "Current-year results update here when the refresh job runs again."), "</p>",
        "<p class='quality-note'><strong>Interpretive status:</strong> ", html_escape(interpretive_status), "</p>",
        "<p class='quality-note'>", html_escape(monitoring_note), "</p>",
        overview_cards,
        "<div class='two-column'>",
        "<div class='quality-card'><h3>Live By Round</h3><p class='panel-caption'>Current-year completed games grouped by tournament round.</p><table class='dashboard-table'><thead><tr><th>Round</th><th>Games</th><th>Accuracy</th><th>Mean predicted</th></tr></thead><tbody>", round_rows, "</tbody></table></div>",
        "<div class='quality-card'><h3>", html_escape(recent_games_title), "</h3><p class='panel-caption'>", html_escape(recent_games_note), "</p><table class='dashboard-table'><thead><tr><th>Round</th><th>Matchup</th><th>Winner</th><th>Model pick</th><th>Model P(win)</th><th>Status</th></tr></thead><tbody>", recent_rows, "</tbody></table></div>",
        "</div>",
        "</div>"
    )
}

#' Render a model overview panel
#'
#' @param model_overview A list of model summary structures.
#'
#' @return A scalar character string containing HTML markup.
#' @keywords internal
render_model_overview_html_legacy <- function(model_overview) {
    if (is.null(model_overview) || length(model_overview) == 0) {
        return("")
    }

    render_model_card <- function(title, overview) {
        if (is.null(overview) || length(overview) == 0) {
            return("")
        }

        bart_config <- overview$bart_config %||% list()
        bart_rows <- if (identical(overview$engine, "bart")) {
            paste0(
                "<tr><th>n_trees</th><td>", html_escape(as.character(bart_config$n_trees %||% NA_integer_)), "</td></tr>",
                "<tr><th>n_burn</th><td>", html_escape(as.character(bart_config$n_burn %||% NA_integer_)), "</td></tr>",
                "<tr><th>n_post</th><td>", html_escape(as.character(bart_config$n_post %||% NA_integer_)), "</td></tr>",
                "<tr><th>k</th><td>", html_escape(as.character(bart_config$k %||% NA_real_)), "</td></tr>",
                "<tr><th>power</th><td>", html_escape(as.character(bart_config$power %||% NA_real_)), "</td></tr>"
            )
        } else {
            paste0(
                "<tr><th>Prior</th><td>", html_escape(overview$prior_type %||% "normal"), "</td></tr>",
                "<tr><th>Interactions</th><td>", html_escape(if (length(overview$interaction_terms %||% character(0)) == 0) "None" else paste(overview$interaction_terms, collapse = ", ")), "</td></tr>"
            )
        }

        paste0(
            "<div class='quality-card'>",
            "<h3>", html_escape(title), "</h3>",
            "<div class='overview-grid'>",
            "<div class='summary-card'><div class='summary-label'>Engine</div><div class='summary-value'>", html_escape(overview$engine_label %||% overview$engine %||% "Unknown"), "</div><p class='summary-note'>", html_escape(overview$predictor_summary %||% "Model summary not recorded."), "</p></div>",
            "<div class='summary-card'><div class='summary-label'>Posterior draw budget</div><div class='summary-value'>", html_escape(formatC(safe_numeric(overview$draw_budget, default = NA_real_), format = "f", digits = 0)), "</div><p class='summary-note'>Used for bracket simulation and live scoring.</p></div>",
            "<div class='summary-card'><div class='summary-label'>Predictors</div><div class='summary-value'>", html_escape(formatC(safe_numeric(overview$predictor_count, default = 0), format = "f", digits = 0)), "</div><p class='summary-note'>Feature count after constant predictors are dropped.</p></div>",
            "</div>",
            "<table class='dashboard-table'><tbody>", bart_rows, "</tbody></table>",
            "</div>"
        )
    }

    panels <- c(
        if (!is.null(model_overview$matchup)) render_model_card("Matchup Model", model_overview$matchup) else "",
        if (!is.null(model_overview$totals)) render_model_card("Total Points Model", model_overview$totals) else ""
    )
    panels <- panels[nzchar(panels)]
    if (length(panels) == 0) {
        return("")
    }

    paste0(
        "<div class='panel'>",
        "<h2>Model Overview</h2>",
        "<div class='diagnostic-grid'>",
        paste(panels, collapse = "\n"),
        "</div>",
        "</div>"
    )
}

#' Render a simple round-performance bar chart
#'
#' @param round_summary Round-level backtest summary.
#'
#' @return A scalar character string containing inline SVG markup.
#' @keywords internal
render_round_performance_svg <- function(round_summary) {
    if (is.null(round_summary) || !inherits(round_summary, "data.frame") || nrow(round_summary) == 0) {
        return("<p class='empty-state'>Round-level diagnostics were not available for this snapshot.</p>")
    }

    plot_data <- round_summary %>%
        dplyr::mutate(round = factor(as.character(round), levels = round_levels())) %>%
        dplyr::arrange(round)
    n_rows <- nrow(plot_data)
    width <- 760
    left <- 150
    top <- 26
    row_h <- 36
    bar_w <- 470
    height <- top + (n_rows * row_h) + 34

    bars <- paste(
        purrr::map_chr(seq_len(n_rows), function(index) {
            row <- plot_data[index, , drop = FALSE]
            accuracy <- safe_numeric(row$accuracy[[1]], default = 0)
            acc_width <- round(bar_w * max(min(accuracy, 1), 0))
            y <- top + ((index - 1L) * row_h)
            paste0(
                "<text x='18' y='", y + 17, "' font-size='12' fill='#111827'>", html_escape(as.character(row$round[[1]])), "</text>",
                "<rect x='", left, "' y='", y, "' width='", bar_w, "' height='16' rx='8' fill='#e7e5e4'/>",
                "<rect x='", left, "' y='", y, "' width='", acc_width, "' height='16' rx='8' fill='#0f766e'/>",
                "<text x='", left + bar_w + 12, "' y='", y + 13, "' font-size='12' fill='#111827'>", sprintf("%.1f%%", 100 * accuracy), " | ", row$games[[1]], " games</text>"
            )
        }),
        collapse = "\n"
    )

    paste0(
        "<svg viewBox='0 0 ", width, " ", height, "' class='tech-svg tech-svg-compact' role='img' aria-label='Round performance by accuracy'>",
        "<text x='18' y='16' font-size='12' fill='#6b7280'>Round</text>",
        "<text x='", left, "' y='16' font-size='12' fill='#6b7280'>Accuracy</text>",
        bars,
        "</svg>"
    )
}

#' Render a model diagnostics panel
#'
#' @param quality_backtest A backtest bundle used for diagnostics.
#' @param quality_source_label Human-readable source label for the metrics.
#'
#' @return A scalar character string containing HTML markup.
#' @keywords internal
render_model_diagnostics_html_legacy <- function(quality_backtest, quality_source_label = NULL) {
    diagnostics <- summarize_backtest_diagnostics(quality_backtest)
    if (is.null(quality_backtest) || !model_quality_has_backtest(quality_backtest)) {
        return("")
    }

    summary_tbl <- quality_backtest$summary %||% tibble::tibble()
    if (nrow(summary_tbl) == 0) {
        return("")
    }

    metric_cards <- paste0(
        "<div class='overview-grid'>",
        "<div class='summary-card'><div class='summary-label'>Log loss</div><div class='summary-value'>", sprintf("%.3f", summary_tbl$mean_log_loss[[1]]), "</div><p class='summary-note'>Lower means the probabilities are more honest.</p></div>",
        "<div class='summary-card'><div class='summary-label'>Brier score</div><div class='summary-value'>", sprintf("%.3f", summary_tbl$mean_brier[[1]]), "</div><p class='summary-note'>Lower means tighter probability estimates.</p></div>",
        "<div class='summary-card'><div class='summary-label'>Accuracy</div><div class='summary-value'>", format_probability(summary_tbl$mean_accuracy[[1]]), "</div><p class='summary-note'>How often the model picked the right winner.</p></div>",
        "<div class='summary-card'><div class='summary-label'>Bracket score</div><div class='summary-value'>", sprintf("%.1f", summary_tbl$mean_bracket_score[[1]]), "</div><p class='summary-note'>Pool-style points earned under the scoring rules.</p></div>",
        "</div>"
    )

    round_summary <- diagnostics$round_summary %||% tibble::tibble()
    strengths <- diagnostics$strengths %||% character()
    weaknesses <- diagnostics$weaknesses %||% character()
    calibration_notes <- diagnostics$calibration_notes %||% character()
    backtest_years_note <- if (!is.null(diagnostics$backtest_years) && nzchar(diagnostics$backtest_years)) {
        paste0(
            "<p class='summary-note backtest-window-note'><strong>Rolling holdout years:</strong> ",
            html_escape(diagnostics$backtest_years),
            "</p>"
        )
    } else {
        ""
    }
    notes_html <- paste(
        c(
            if (length(strengths) > 0) sprintf("<li>%s</li>", html_escape(strengths)) else NULL,
            if (length(weaknesses) > 0) sprintf("<li>%s</li>", html_escape(weaknesses)) else NULL,
            if (length(calibration_notes) > 0) sprintf("<li>%s</li>", html_escape(calibration_notes)) else NULL
        ),
        collapse = "\n"
    )
    if (!nzchar(notes_html)) {
        notes_html <- "<li>No additional diagnostic notes were available.</li>"
    }

    round_rows <- if (nrow(round_summary) > 0) {
        paste(
            purrr::map_chr(seq_len(nrow(round_summary)), function(index) {
                row <- round_summary[index, , drop = FALSE]
                paste0(
                    "<tr>",
                    "<td>", html_escape(as.character(row$round[[1]])), "</td>",
                    "<td>", html_escape(as.character(row$games[[1]])), "</td>",
                    "<td>", html_escape(format_probability(row$accuracy[[1]])), "</td>",
                    "<td>", html_escape(sprintf("%.3f", row$log_loss[[1]])), "</td>",
                    "<td>", html_escape(sprintf("%.3f", row$brier[[1]])), "</td>",
                    "<td>", html_escape(format_probability(row$mean_predicted[[1]])), "</td>",
                    "<td>", html_escape(format_probability(row$empirical_rate[[1]])), "</td>",
                    "</tr>"
                )
            }),
            collapse = "\n"
        )
    } else {
        ""
    }

    calibration_svg <- render_calibration_svg(quality_backtest$calibration %||% tibble::tibble())
    calibration_help <- render_calibration_help_html()
    round_svg <- render_round_performance_svg(round_summary)
    source_text <- if (!is.null(quality_source_label) && nzchar(quality_source_label)) {
        paste0("<p class='panel-caption'><strong>Source:</strong> ", html_escape(quality_source_label), "</p>")
    } else {
        ""
    }

    paste0(
        "<div class='panel'>",
        "<h2>Model Diagnostics</h2>",
        source_text,
        "<p class='quality-intro'>The backtest is the historical baseline, so you can judge the live performance panel that follows against seasons the model has already seen.</p>",
        metric_cards,
        "<div class='quality-grid'>",
        "<div class='quality-card'><h3>Why it looks good or bad</h3><ul class='diagnostic-bullets'>", notes_html, "</ul></div>",
        "<div class='quality-card'><h3>Round Performance</h3>", round_svg, "</div>",
        "</div>",
        "<div class='quality-grid'>",
        "<div class='quality-card'><h3>Calibration Curve</h3>", backtest_years_note, calibration_svg, calibration_help, "</div>",
        "<div class='quality-card'><h3>Round Breakdown</h3><table class='dashboard-table'><thead><tr><th>Round</th><th>Games</th><th>Accuracy</th><th>Log loss</th><th>Brier</th><th>Mean predicted</th><th>Empirical rate</th></tr></thead><tbody>", round_rows, "</tbody></table></div>",
        "</div>",
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
        "</svg>"
    )
}

#' Render a calibration explainer block for the dashboard
#'
#' @return A scalar character string containing HTML markup.
#' @keywords internal
render_calibration_help_html <- function() {
    paste0(
        "<div class='legend-row'>",
        "<div class='legend-chip'><span class='legend-swatch' style='background:#457b9d;'></span>Observed by bin (win rate)</div>",
        "<div class='legend-chip'><span class='legend-swatch' style='background:transparent;border:1px dashed #d6d3d1;box-sizing:border-box;'></span>Perfect line</div>",
        "</div>",
        "<p class='legend-copy calibration-note'><strong>How to read this chart:</strong> the x-axis is the model's average predicted win rate in each probability bin, and the y-axis is the observed win rate in that bin. Points close to the dashed line are well calibrated. Points below the line mean the model was too optimistic; points above the line mean the model was too pessimistic.</p>"
    )
}

#' Render a compact model-overview panel
#'
#' @param model_overview A model overview list from [summarize_model_overview()].
#'
#' @return A scalar character string containing HTML markup.
#' @keywords internal
render_model_overview_html_legacy <- function(model_overview) {
    if (is.null(model_overview) || length(model_overview) == 0) {
        return(
            "<div class='panel'><h2>Model Overview</h2><p class='empty-state'>Model metadata was not recorded for this run.</p></div>"
        )
    }

    bart_config <- model_overview$bart_config %||% list()
    engine_label <- model_overview$engine_label %||% as.character(model_overview$engine %||% "unknown")
    draws_text <- if (is.finite(safe_numeric(model_overview$draw_budget, default = NA_real_))) {
        sprintf("%.0f posterior draws", safe_numeric(model_overview$draw_budget, default = NA_real_))
    } else {
        "Draw budget not recorded"
    }
    predictor_text <- if (safe_numeric(model_overview$predictor_count, default = 0) > 0) {
        model_overview$predictor_summary %||% sprintf("%s predictors were recorded.", safe_numeric(model_overview$predictor_count, default = 0))
    } else {
        "No predictors were recorded."
    }
    interaction_text <- if (length(model_overview$interaction_terms %||% character(0)) > 0) {
        paste(model_overview$interaction_terms, collapse = ", ")
    } else {
        "None"
    }
    betting_text <- if (safe_numeric(model_overview$betting_predictor_count, default = 0) > 0) {
        sprintf("%s betting-derived predictors", safe_numeric(model_overview$betting_predictor_count, default = 0))
    } else {
        "No betting-derived predictors"
    }
    engine_detail <- if (identical(model_overview$engine, "bart")) {
        paste0(
            "Trees ", safe_numeric(bart_config$n_trees, default = NA_real_),
            " | Burn ", safe_numeric(bart_config$n_burn, default = NA_real_),
            " | Post ", safe_numeric(bart_config$n_post, default = NA_real_),
            " | k ", safe_numeric(bart_config$k, default = NA_real_),
            " | power ", safe_numeric(bart_config$power, default = NA_real_)
        )
    } else {
        paste0("Prior type: ", model_overview$prior_type %||% "normal")
    }

    paste0(
        "<div class='panel'>",
        "<h2>Model Overview</h2>",
        "<p class='panel-caption'>This block shows the model that produced the current dashboard and how it was configured.</p>",
        "<div class='overview-grid'>",
        "<div class='summary-card'><div class='summary-label'>Engine</div><div class='summary-value'>", html_escape(engine_label), "</div><p class='summary-note'>", html_escape(model_overview$engine %||% "unknown"), "</p></div>",
        "<div class='summary-card'><div class='summary-label'>Draw budget</div><div class='summary-value'>", html_escape(draws_text), "</div><p class='summary-note'>Posterior samples used for scoring and simulation.</p></div>",
        "<div class='summary-card'><div class='summary-label'>Predictors</div><div class='summary-value'>", html_escape(as.character(safe_numeric(model_overview$predictor_count, default = 0))), "</div><p class='summary-note'>", html_escape(predictor_text), "</p></div>",
        "<div class='summary-card'><div class='summary-label'>Betting features</div><div class='summary-value'>", html_escape(betting_text), "</div><p class='summary-note'>Whether market-derived predictors are in play.</p></div>",
        "<div class='summary-card'><div class='summary-label'>Extra structure</div><div class='summary-value'>", html_escape(interaction_text), "</div><p class='summary-note'>Interactions or other model terms beyond the base predictors.</p></div>",
        "<div class='summary-card'><div class='summary-label'>Engine settings</div><div class='summary-value'>", html_escape(engine_label), "</div><p class='summary-note'>", html_escape(engine_detail), "</p></div>",
        "</div>",
        "</div>"
    )
}

#' Render a model-diagnostics panel
#'
#' @param backtest A backtest result bundle.
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
create_technical_dashboard_html <- function(bracket_year, decision_sheet, candidates, backtest = NULL, total_points_predictions = NULL, play_in_resolution = NULL, model_quality_context = NULL, live_performance = NULL, model_overview = NULL, model_comparison = NULL) {
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
            source_label = "Current run backtest"
        )
    }
    quality_backtest <- quality_context$backtest %||% backtest
    quality_source_label <- quality_context$source_label %||% "Current run backtest"
    live_model <- normalize_model_overview(model_overview)
    live_model_label <- live_model$engine_label %||% live_model$engine %||% if (!is.null(live_performance)) "Stan GLM" else NULL
    overview_panel <- render_model_overview_html(model_overview)
    comparison_panel <- render_model_comparison_link_html(model_comparison)
    diagnostics_panel <- render_model_diagnostics_html(quality_backtest, quality_source_label = quality_source_label)

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
        "<div class='panel status-panel status-unknown'><strong>Status not supplied.</strong> Play-in resolution was not supplied for this run.</div>"
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
        tibble::tibble(status = "Backtest not computed in this run.")
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
            "<p class='empty-state'>Championship tiebreaker not included for this run.</p>"
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
        ".comparison-link-panel{border-left:5px solid #0f172a;}",
        ".comparison-link-copy{margin:14px 0 0 0;font-size:14px;}",
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
        ".diagnostic-callout{margin:10px 0 0 0;padding:12px 14px;border-radius:12px;background:#f8fafc;border:1px solid #e2e8f0;}",
        ".diagnostic-callout strong{display:block;margin-bottom:8px;}",
        ".diagnostic-bullets{margin:0;padding-left:18px;}",
        ".diagnostic-bullets li{margin-bottom:6px;}",
        "@media (max-width: 1240px){.comparison-board__header{display:none;}.board-row--ranked,.board-row--divergence,.board-row--upset{grid-template-columns:1fr;}.board-cell::before{display:block;}}",
        "@media (max-width: 900px){.quality-grid{grid-template-columns:1fr;}.tech-svg-compact{max-width:none;}}",
        "</style></head><body>",
        "<h1>mmBayes Technical Bracket Dashboard</h1>",
        "<p class='lede'>Bracket year ", html_escape(bracket_year),
        ". Start in Compare to decide which games actually deserve your attention, then switch to Candidate 1 or Candidate 2 only after you want the full path for one bracket. This view is built to explain not just the recommendation, but also the math driving it.</p>",
        status_panel,
        overview_panel,
        comparison_panel,
        diagnostics_panel,
        render_live_performance_html(
            live_performance,
            model_label = live_model_label
        ),
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
        "<div class='summary-card'><div class='summary-label'>Top leverage upset</div><div class='summary-value summary-value--wrap summary-value--tight'>", html_escape(leverage_text), "</div><p class='summary-note'>Highest underdog payoff under current scoring.</p></div>",
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

#' Create the bracket dashboard HTML
#'
#' @param bracket_year The active bracket year.
#' @param decision_sheet The decision-sheet data frame.
#' @param candidates A list of candidate bracket objects.
#' @param current_teams Optional current-year team feature table.
#' @param dashboard_context Optional prebuilt dashboard context from
#'   [build_bracket_dashboard_context()].
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
create_bracket_dashboard_html <- function(bracket_year, decision_sheet, candidates, current_teams = NULL, dashboard_context = NULL, backtest = NULL, play_in_resolution = NULL, total_points_predictions = NULL, model_quality_context = NULL, model_overview = NULL, model_comparison = NULL) {
    dashboard_context <- dashboard_context %||% build_bracket_dashboard_context(
        current_teams = current_teams,
        decision_sheet = decision_sheet,
        candidates = candidates,
        total_points_predictions = total_points_predictions,
        play_in_resolution = play_in_resolution
    )

    candidate_summary_rows <- dashboard_context$candidate_summary_rows %||% tibble::tibble()
    candidate_delta_rows <- dashboard_context$candidate_delta_rows %||% tibble::tibble()
    watchlist_rows <- dashboard_context$watchlist_rows %||% tibble::tibble()
    matchup_context_rows <- dashboard_context$matchup_context_rows %||% tibble::tibble()
    bracket_tree_data <- dashboard_context$bracket_tree_data

    primary_candidate <- if (length(candidates) >= 1L) candidates[[1]] else list(candidate_id = 1L, type = "safe", champion = NA_character_, final_four = NA_character_, bracket_log_prob = NA_real_, mean_game_prob = NA_real_, diff_summary = "Primary bracket.")
    alternate_candidate <- if (length(candidates) >= 2L) candidates[[2]] else primary_candidate

    quality_context <- model_quality_context
    if (is.null(quality_context)) {
        quality_context <- list(backtest = backtest, source_label = "Current run backtest")
    }
    quality_backtest <- NULL
    if (!is.null(quality_context) && is.list(quality_context) && model_quality_has_backtest(quality_context$backtest %||% NULL)) {
        quality_backtest <- quality_context$backtest
    }
    if (!model_quality_has_backtest(quality_backtest) && model_quality_has_backtest(backtest)) {
        quality_backtest <- backtest
    }
    quality_source_label <- quality_context$source_label %||% "Current run backtest"

    status_panel <- ""
    if (!is.null(play_in_resolution) && nrow(play_in_resolution) > 0 && isTRUE(play_in_resolution$has_unresolved_slots[[1]])) {
        status_panel <- paste0(
            "<div class='status-panel status-simulated'><strong>Status: Simulated bracket path.</strong> ",
            html_escape(sprintf(
                "%s of %s play-in slots are still unresolved, so the generated brackets assume simulated First Four winners. Any downstream Round of 64 and later matchups can shift until those games are final.",
                play_in_resolution$unresolved_slots[[1]],
                play_in_resolution$expected_slots[[1]]
            )),
            "</div>"
        )
    } else if (!is.null(play_in_resolution) && nrow(play_in_resolution) > 0) {
        status_panel <- "<div class='status-panel status-final'><strong>Status: Final result.</strong> First Four slots are resolved, so the displayed bracket path reflects finalized play-in outcomes.</div>"
    } else {
        status_panel <- "<div class='status-panel status-unknown'><strong>Status not supplied.</strong> No play-in summary was supplied, so the review panel cannot tell whether these slots are simulated or final.</div>"
    }

    round_order <- round_levels()
    region_order <- bracket_region_levels()

    row_value <- function(row, name, default = NA_character_) {
        if (!name %in% names(row) || length(row[[name]]) == 0) {
            return(default)
        }
        value <- row[[name]][[1]]
        if (is.null(value) || (length(value) == 1L && is.na(value))) default else value
    }

    display_value <- function(value, digits = 1L, scale_percent = FALSE) {
        if (length(value) == 0 || all(is.na(value))) {
            return("n/a")
        }
        value <- safe_numeric(value, default = NA_real_)
        if (all(is.na(value))) {
            return("n/a")
        }
        if (scale_percent) {
            return(sprintf("%.1f%%", 100 * value))
        }
        sprintf(paste0("%.", digits, "f"), value)
    }

    surface_class <- function(surface) {
        paste0("surface-", gsub("[^a-z0-9]+", "-", tolower(surface)))
    }

    render_value_table <- function(title, data) {
        paste0(
            "<div class='mini-table-card'>",
            "<div class='mini-table-title'>", html_escape(title), "</div>",
            render_html_table(data),
            "</div>"
        )
    }

    render_metric_grid <- function(metrics) {
        paste0(
            "<div class='metric-grid'>",
            paste(
                purrr::map_chr(seq_len(nrow(metrics)), function(index) {
                    paste0(
                        "<div class='metric-pill'><span>", html_escape(metrics$Metric[[index]]), "</span><strong>",
                        html_escape(metrics$Value[[index]]),
                        "</strong></div>"
                    )
                }),
                collapse = ""
            ),
            "</div>"
        )
    }

    render_team_card <- function(row, prefix, fallback_name) {
        team_name <- row_value(row, paste0(prefix, "_Team"), fallback_name)
        seed <- row_value(row, paste0(prefix, "_Seed"), row_value(row, paste0(prefix, "_seed"), NA_integer_))
        conf <- row_value(row, paste0(prefix, "_Conf"), row_value(row, paste0(prefix, "_conf"), "n/a"))
        core_metrics <- tibble::tibble(
            Metric = c("Barthag", "AdjOE", "AdjDE", "WAB"),
            Value = c(
                display_value(row_value(row, paste0(prefix, "_Barthag"), NA_real_), digits = 1L, scale_percent = TRUE),
                display_value(row_value(row, paste0(prefix, "_AdjOE"), NA_real_), digits = 1L),
                display_value(row_value(row, paste0(prefix, "_AdjDE"), NA_real_), digits = 1L),
                display_value(row_value(row, paste0(prefix, "_WAB"), NA_real_), digits = 1L)
            )
        )
        style_metrics <- tibble::tibble(
            Metric = c("TOR", "TORD", "ORB", "DRB", "3P%", "3P%D", "Adj T."),
            Value = c(
                display_value(row_value(row, paste0(prefix, "_TOR"), NA_real_), digits = 3L),
                display_value(row_value(row, paste0(prefix, "_TORD"), NA_real_), digits = 3L),
                display_value(row_value(row, paste0(prefix, "_ORB"), NA_real_), digits = 3L),
                display_value(row_value(row, paste0(prefix, "_DRB"), NA_real_), digits = 3L),
                display_value(row_value(row, paste0(prefix, "_3P%"), NA_real_), digits = 3L),
                display_value(row_value(row, paste0(prefix, "_3P%D"), NA_real_), digits = 3L),
                display_value(row_value(row, paste0(prefix, "_Adj T."), NA_real_), digits = 1L)
            )
        )

        paste0(
            "<div class='team-card'>",
            "<div class='team-card__head'>",
            "<div>",
            "<div class='team-card__eyebrow'>", html_escape(fallback_name), "</div>",
            "<h4>", html_escape(team_name), "</h4>",
            "</div>",
            "<div class='team-card__seed'>Seed ", html_escape(display_value(seed, digits = 0L)), "</div>",
            "</div>",
            "<p class='team-card__meta'><strong>Conference:</strong> ", html_escape(conf), "</p>",
            render_value_table("Core metrics", core_metrics),
            render_value_table("Style metrics", style_metrics),
            "</div>"
        )
    }

    render_diff_table <- function(row) {
        metric_rows <- tibble::tibble(
            Feature = c("same_conf", "Seed", "Barthag logit", "AdjOE", "AdjDE", "WAB", "TOR", "TORD", "ORB", "DRB", "3P%", "3P%D", "Adj T."),
            `Team A` = c(
                display_value(row_value(row, "same_conf", NA_integer_), digits = 0L),
                display_value(row_value(row, "teamA_Seed", NA_real_), digits = 0L),
                display_value(row_value(row, "teamA_barthag_logit", NA_real_), digits = 3L),
                display_value(row_value(row, "teamA_AdjOE", NA_real_), digits = 1L),
                display_value(row_value(row, "teamA_AdjDE", NA_real_), digits = 1L),
                display_value(row_value(row, "teamA_WAB", NA_real_), digits = 1L),
                display_value(row_value(row, "teamA_TOR", NA_real_), digits = 3L),
                display_value(row_value(row, "teamA_TORD", NA_real_), digits = 3L),
                display_value(row_value(row, "teamA_ORB", NA_real_), digits = 3L),
                display_value(row_value(row, "teamA_DRB", NA_real_), digits = 3L),
                display_value(row_value(row, "teamA_3P%", NA_real_), digits = 3L),
                display_value(row_value(row, "teamA_3P%D", NA_real_), digits = 3L),
                display_value(row_value(row, "teamA_Adj T.", NA_real_), digits = 1L)
            ),
            `Team B` = c(
                display_value(row_value(row, "same_conf", NA_integer_), digits = 0L),
                display_value(row_value(row, "teamB_Seed", NA_real_), digits = 0L),
                display_value(row_value(row, "teamB_barthag_logit", NA_real_), digits = 3L),
                display_value(row_value(row, "teamB_AdjOE", NA_real_), digits = 1L),
                display_value(row_value(row, "teamB_AdjDE", NA_real_), digits = 1L),
                display_value(row_value(row, "teamB_WAB", NA_real_), digits = 1L),
                display_value(row_value(row, "teamB_TOR", NA_real_), digits = 3L),
                display_value(row_value(row, "teamB_TORD", NA_real_), digits = 3L),
                display_value(row_value(row, "teamB_ORB", NA_real_), digits = 3L),
                display_value(row_value(row, "teamB_DRB", NA_real_), digits = 3L),
                display_value(row_value(row, "teamB_3P%", NA_real_), digits = 3L),
                display_value(row_value(row, "teamB_3P%D", NA_real_), digits = 3L),
                display_value(row_value(row, "teamB_Adj T.", NA_real_), digits = 1L)
            ),
            Diff = c(
                display_value(row_value(row, "same_conf", NA_integer_), digits = 0L),
                display_value(row_value(row, "seed_diff", NA_real_), digits = 0L),
                display_value(row_value(row, "barthag_logit_diff", NA_real_), digits = 3L),
                display_value(row_value(row, "AdjOE_diff", NA_real_), digits = 1L),
                display_value(row_value(row, "AdjDE_diff", NA_real_), digits = 1L),
                display_value(row_value(row, "WAB_diff", NA_real_), digits = 1L),
                display_value(row_value(row, "TOR_diff", NA_real_), digits = 3L),
                display_value(row_value(row, "TORD_diff", NA_real_), digits = 3L),
                display_value(row_value(row, "ORB_diff", NA_real_), digits = 3L),
                display_value(row_value(row, "DRB_diff", NA_real_), digits = 3L),
                display_value(row_value(row, "3P%_diff", NA_real_), digits = 3L),
                display_value(row_value(row, "3P%D_diff", NA_real_), digits = 3L),
                display_value(row_value(row, "Adj T._diff", NA_real_), digits = 1L)
            )
        )

        render_value_table("Model-facing matchup diffs", metric_rows)
    }

    render_candidate_card <- function(summary_row) {
        candidate_id <- row_value(summary_row, "candidate_id", 1L)
        tiebreaker_points <- row_value(summary_row, "recommended_tiebreaker_points", NA_integer_)
        tiebreaker_matchup <- row_value(summary_row, "championship_matchup", NA_character_)
        tiebreaker_interval <- format_total_interval(
            row_value(summary_row, "predicted_total_80_lower", NA_real_),
            row_value(summary_row, "predicted_total_80_upper", NA_real_)
        )
        tiebreaker_median <- display_value(row_value(summary_row, "predicted_total_median", NA_real_), digits = 1L)
        summary_note <- row_value(summary_row, "identity_text", "No summary available.")
        change_copy <- sprintf(
            "%s changed slot%s | late-round changes: %s",
            display_value(row_value(summary_row, "diff_count", NA_real_), digits = 0L),
            ifelse(safe_numeric(row_value(summary_row, "diff_count", 0L), default = 0) == 1, "", "s"),
            display_value(row_value(summary_row, "late_round_diff_count", NA_real_), digits = 0L)
        )

        paste0(
            "<article class='candidate-card' id='candidate-summary-", candidate_id, "'>",
            "<div class='candidate-card__head'>",
            "<div>",
            "<div class='candidate-card__eyebrow'>Candidate ", candidate_id, " ", html_escape(row_value(summary_row, "candidate_type", "unknown")), "</div>",
            "<h3>", html_escape(row_value(summary_row, "champion", "n/a")), "</h3>",
            "</div>",
            "<div class='candidate-card__mini'>", html_escape(display_value(row_value(summary_row, "bracket_log_probability", NA_real_), digits = 3L)), "</div>",
            "</div>",
            "<p class='candidate-card__subline'><strong>Final Four:</strong> ", html_escape(row_value(summary_row, "final_four", "n/a")), "</p>",
            "<p class='candidate-card__subline'><strong>Identity:</strong> ", html_escape(summary_note), "</p>",
            "<div class='candidate-card__facts'>",
            "<div><span>Bracket log probability</span><strong>", html_escape(display_value(row_value(summary_row, "bracket_log_probability", NA_real_), digits = 3L)), "</strong></div>",
            "<div><span>Mean picked-game probability</span><strong>", html_escape(display_value(row_value(summary_row, "mean_picked_game_probability", NA_real_), digits = 3L)), "</strong></div>",
            "<div><span>Championship tiebreaker</span><strong>", html_escape(display_value(tiebreaker_points, digits = 0L)), "</strong></div>",
            "<div><span>Title-game total</span><strong>", html_escape(tiebreaker_median), " <span class='muted'>(", html_escape(tiebreaker_interval), ")</span></strong></div>",
            "<div><span>Change count</span><strong>", html_escape(change_copy), "</strong></div>",
            "</div>",
            if (!is.na(tiebreaker_points)) {
                paste0(
                    "<div class='candidate-card__tiebreaker'>",
                    "<p><strong>Projected title game:</strong> ", html_escape(tiebreaker_matchup), "</p>",
                    "</div>"
                )
            } else {
                "<div class='candidate-card__tiebreaker'><p class='empty-state'>Championship total-points prediction was not supplied for this run.</p></div>"
            },
            "<div class='candidate-card__links'>",
            "<a href='#candidate-path-", candidate_id, "'>Full path</a>",
            "<a href='#evidence'>Evidence</a>",
            "<a href='#delta'>Delta</a>",
            "</div>",
            "</article>"
        )
    }

    render_delta_round <- function(round_name, round_rows) {
        if (nrow(round_rows) == 0) {
            return("")
        }

        headers <- c("Round", "Region", "Slot", "Candidate 1 pick", "Candidate 2 pick", "Posterior favorite", "Favorite win probability", "Confidence tier", "Why this swap exists", "Evidence")
        header_html <- paste(sprintf("<th>%s</th>", html_escape(headers)), collapse = "")
        row_html <- purrr::map_chr(seq_len(nrow(round_rows)), function(index) {
            row <- round_rows[index, , drop = FALSE]
            evidence_button <- paste0(
                "<button type='button' class='jump-button' data-open-evidence='", html_escape(row$evidence_id[[1]]), "'>Open evidence</button>"
            )
            cells <- c(
                round_name,
                row$region[[1]],
                row$matchup_slot[[1]],
                row$candidate_1_pick[[1]],
                row$candidate_2_pick[[1]],
                row$posterior_favorite[[1]],
                sprintf("%.1f%%", 100 * safe_numeric(row$win_prob_favorite[[1]], default = NA_real_)),
                row$confidence_tier[[1]],
                row$why_swap_exists[[1]],
                evidence_button
            )
            escaped_cells <- html_escape(cells[-length(cells)])
            cell_html <- paste(
                c(
                    sprintf("<td>%s</td>", escaped_cells),
                    sprintf("<td>%s</td>", evidence_button)
                ),
                collapse = ""
            )
            sprintf("<tr>%s</tr>", cell_html)
        })

        paste0(
            "<section class='delta-round'>",
            "<h3>", html_escape(round_name), "</h3>",
            "<div class='table-shell'>",
            "<table class='dashboard-table'>",
            "<thead><tr>", header_html, "</tr></thead>",
            "<tbody>", paste(row_html, collapse = "\n"), "</tbody>",
            "</table>",
            "</div>",
            "</section>"
        )
    }

    render_watchlist_card <- function(row, hidden = FALSE) {
        surface <- row_value(row, "reason_surface", "Bracket-changing toss-ups")
        surface_attr <- html_escape(surface)
        hidden_class <- if (hidden) " collapsed-row" else ""
        round_name <- row_value(row, "round", "n/a")
        matchup_label <- row_value(row, "matchup_label", sprintf("%s vs %s", row_value(row, "teamA", "n/a"), row_value(row, "teamB", "n/a")))
        seeds_label <- sprintf("%s vs %s", display_value(row_value(row, "teamA_seed", NA_real_), digits = 0L), display_value(row_value(row, "teamB_seed", NA_real_), digits = 0L))
        why <- row_value(row, "why_this_matters", "No summary available.")
        usage <- row_value(row, "candidate_usage", "n/a")
        late_round <- isTRUE(row_value(row, "late_round_only", FALSE))

        paste0(
            "<article class='watchlist-card", hidden_class, "' data-surface='", surface_attr, "' data-late-round='", ifelse(late_round, "true", "false"), "' data-evidence-id='", html_escape(row_value(row, "evidence_id", "")), "'>",
            "<div class='watchlist-card__head'>",
            "<div>",
            "<div class='surface-pill'>", surface_attr, "</div>",
            "<h3>", html_escape(matchup_label), "</h3>",
            "<p class='watchlist-card__meta'>", html_escape(round_name), " | ", html_escape(row_value(row, "region", "n/a")), " | Seeds ", html_escape(seeds_label), "</p>",
            "</div>",
            "<button type='button' class='jump-button' data-open-evidence='", html_escape(row_value(row, "evidence_id", "")), "'>Open evidence</button>",
            "</div>",
            "<p><strong>Why this matters:</strong> ", html_escape(why), "</p>",
            "<p><strong>Candidate usage:</strong> ", html_escape(usage), "</p>",
            if (late_round) "<p class='watchlist-card__note'>Late-round only row.</p>" else "",
            "</article>"
        )
    }

    render_evidence_detail <- function(row) {
        surface <- row_value(row, "reason_surface", "Bracket-changing toss-ups")
        latest_note <- row_value(row, "downstream_implication_text", NA_character_)
        detail_id <- row_value(row, "evidence_id", "")
        round_name <- row_value(row, "round", "n/a")
        matchup_label <- row_value(row, "matchup_label", sprintf("%s vs %s", row_value(row, "teamA", "n/a"), row_value(row, "teamB", "n/a")))
        team_a_name <- row_value(row, "teamA", row_value(row, "teamA_Team", "n/a"))
        team_b_name <- row_value(row, "teamB", row_value(row, "teamB_Team", "n/a"))
        row_context <- matchup_context_rows %>%
            dplyr::filter(slot_key == row_value(row, "slot_key", ""))
        if (nrow(row_context) == 0) {
            row_context <- row
        } else {
            row_context <- row_context[1, , drop = FALSE]
        }

        usage_table <- tibble::tibble(
            Candidate = c("Candidate 1", "Candidate 2"),
            Pick = c(row_value(row, "candidate_1_pick", "n/a"), row_value(row, "candidate_2_pick", "n/a")),
            Usage = c(
                if (isTRUE(row_value(row, "candidate_1_upset", FALSE))) "Underdog" else "Favorite",
                if (isTRUE(row_value(row, "candidate_2_upset", FALSE))) "Underdog" else "Favorite"
            )
        )

        favorite_prob <- display_value(row_value(row, "win_prob_favorite", NA_real_), digits = 1L, scale_percent = TRUE)
        favorite_interval <- format_probability_interval(row_value(row, "ci_lower", NA_real_), row_value(row, "ci_upper", NA_real_))
        underdog_prob <- display_value(row_value(row, "win_prob_underdog", NA_real_), digits = 1L, scale_percent = TRUE)
        leverage <- display_value(row_value(row, "upset_leverage", NA_real_), digits = 3L)

        paste0(
            "<details class='evidence-panel", "' id='", html_escape(detail_id), "' data-surface='", html_escape(surface), "' data-late-round='", ifelse(isTRUE(row_value(row, "late_round_only", FALSE)), "true", "false"), "'>",
            "<summary>",
            "<div class='evidence-panel__summary'>",
            "<span class='surface-pill'>", html_escape(surface), "</span>",
            "<strong>", html_escape(matchup_label), "</strong>",
            "<span>", html_escape(round_name), " | ", html_escape(row_value(row, "region", "n/a")), " | Seeds ", html_escape(sprintf("%s vs %s", display_value(row_value(row, "teamA_seed", NA_real_), digits = 0L), display_value(row_value(row, "teamB_seed", NA_real_), digits = 0L))), "</span>",
            "</div>",
            "</summary>",
            "<div class='evidence-panel__body'>",
            "<p class='evidence-panel__lede'><strong>Why this surfaced:</strong> ", html_escape(row_value(row, "why_this_matters", "No summary available.")), "</p>",
            if (!is.na(latest_note)) {
                paste0("<p class='evidence-panel__implication'><strong>Downstream implication:</strong> ", html_escape(latest_note), "</p>")
            } else {
                "<p class='evidence-panel__implication muted'>No downstream implication text is attached because this row does not change the path.</p>"
            },
            "<div class='evidence-summary-grid'>",
            "<div class='summary-chip'><span>Favorite probability</span><strong>", html_escape(favorite_prob), "</strong><small>", html_escape(favorite_interval), "</small></div>",
            "<div class='summary-chip'><span>Underdog probability</span><strong>", html_escape(underdog_prob), "</strong><small>Leverage ", html_escape(leverage), "</small></div>",
            "<div class='summary-chip'><span>Candidate usage</span><strong>", html_escape(row_value(row, "candidate_usage", "n/a")), "</strong><small>", html_escape(row_value(row, "difference_mode", "n/a")), "</small></div>",
            "</div>",
            "<div class='team-grid'>",
            render_team_card(row_context, "teamA", team_a_name),
            render_team_card(row_context, "teamB", team_b_name),
            "</div>",
            render_diff_table(row_context),
            render_value_table("Candidate usage", usage_table),
            "<p class='evidence-panel__note'>This is the matchup evidence the model saw, not a decomposition of why the model caused the pick.</p>",
            "</div>",
            "</details>"
        )
    }

    round_candidates <- lapply(round_order, function(round_name) {
        if (nrow(candidate_delta_rows) == 0) {
            return(NULL)
        }
        candidate_delta_rows %>% dplyr::filter(round == round_name)
    })
    names(round_candidates) <- round_order

    candidate_delta_html <- if (nrow(candidate_delta_rows) == 0) {
        "<p class='empty-state'>Candidate 1 and Candidate 2 currently match on every slot.</p>"
    } else {
        paste(
            purrr::map_chr(round_order, function(round_name) {
                round_rows <- candidate_delta_rows %>% dplyr::filter(round == round_name)
                render_delta_round(round_name, round_rows)
            })[vapply(round_order, function(round_name) nrow(candidate_delta_rows %>% dplyr::filter(round == round_name)) > 0, logical(1))],
            collapse = "\n"
        )
    }

    watchlist_summary_cards <- watchlist_rows %>%
        dplyr::count(reason_surface, name = "n") %>%
        dplyr::right_join(
            tibble::tibble(reason_surface = c("Bracket-changing toss-ups", "Upset pivots", "Fragile favorites")),
            by = "reason_surface"
        ) %>%
        dplyr::mutate(n = dplyr::coalesce(n, 0L))
    get_watchlist_count <- function(surface_name) {
        value <- watchlist_summary_cards$n[watchlist_summary_cards$reason_surface == surface_name]
        if (length(value) == 0) 0L else value[[1]]
    }

    watchlist_cards <- if (nrow(watchlist_rows) == 0) {
        "<p class='empty-state'>No rows met the current watchlist rules.</p>"
    } else {
        paste(
            purrr::map_chr(seq_len(nrow(watchlist_rows)), function(index) {
                render_watchlist_card(watchlist_rows[index, , drop = FALSE], hidden = index > 5L)
            }),
            collapse = "\n"
        )
    }

    evidence_seed <- decision_sheet %||% tibble::tibble()

    watchlist_lookup <- watchlist_rows %||% tibble::tibble()
    required_watchlist_cols <- c("slot_key", "reason_surface", "why_this_matters", "downstream_implication_text")
    if (!all(required_watchlist_cols %in% names(watchlist_lookup))) {
        watchlist_lookup <- tibble::tibble(
            slot_key = character(),
            reason_surface = character(),
            why_this_matters = character(),
            downstream_implication_text = character()
        )
    }

    watchlist_lookup <- watchlist_lookup %>%
        dplyr::transmute(
            slot_key = as.character(slot_key),
            watch_surface = as.character(reason_surface),
            watch_why = as.character(why_this_matters),
            watch_downstream = as.character(downstream_implication_text)
        )

    evidence_seed <- evidence_seed %>%
        dplyr::mutate(
            slot_key = as.character(slot_key),
            evidence_id = paste0("evidence-", slot_key),
            late_round_only = as.character(round) %in% c("Sweet 16", "Elite 8", "Final Four", "Championship")
        ) %>%
        dplyr::left_join(watchlist_lookup, by = "slot_key") %>%
        dplyr::mutate(
            reason_surface = dplyr::coalesce(watch_surface, "All matchups"),
            why_this_matters = dplyr::coalesce(
                watch_why,
                "Reference matchup. Use this drawer when you want the evidence inputs behind a specific node."
            ),
            downstream_implication_text = watch_downstream
        ) %>%
        dplyr::select(-watch_surface, -watch_why, -watch_downstream)

    evidence_panels <- if (nrow(evidence_seed) == 0) {
        "<p class='empty-state'>No evidence drawers to show yet.</p>"
    } else {
        paste(
            purrr::map_chr(seq_len(nrow(evidence_seed)), function(index) {
                render_evidence_detail(evidence_seed[index, , drop = FALSE])
            }),
            collapse = "\n"
        )
    }

    candidate_paths_html <- paste(
        purrr::map_chr(candidates, function(candidate) {
            candidate_id_value <- candidate$candidate_id %||% 1L
            summary_row <- candidate_summary_rows %>% dplyr::filter(candidate_id == candidate_id_value)
            if (nrow(summary_row) == 0) {
                summary_row <- tibble::tibble(
                    candidate_id = candidate_id_value,
                    candidate_type = candidate$type %||% "unknown",
                    champion = candidate$champion %||% "n/a",
                    final_four = candidate$final_four %||% "n/a"
                )
            }
            path_view <- if (candidate_id_value == 1L) {
                build_candidate_sequence_view(decision_sheet, "matchup_label", "candidate_1_pick")
            } else {
                build_candidate_sequence_view(decision_sheet, "candidate_2_matchup", "candidate_2_pick")
            }
            row_classes <- inspection_row_class(path_view$inspection_level)
            path_view <- path_view %>% dplyr::select(-inspection_level)
            paste0(
                "<details class='path-panel' id='candidate-path-", candidate_id_value, "'>",
                "<summary><strong>Candidate ", candidate_id_value, " full path</strong></summary>",
                "<p class='path-panel__lede'>Reference material only. Open this when you want the bracket-ordered sequence after you have decided what to enter.</p>",
                render_html_table(path_view, row_classes = row_classes),
                "</details>"
            )
        }),
        collapse = "\n"
    )

    appendix_links <- paste0(
        "<div class='appendix-links'>",
        "<a href='", dashboard_preview_url("technical_dashboard.html"), "'>Open technical_dashboard.html</a>",
        "<a href='", dashboard_preview_url("model_comparison_dashboard.html"), "'>Open model_comparison_dashboard.html</a>",
        "</div>"
    )

    appendix_panel <- paste0(
        "<section class='section' id='technical-appendix'>",
        "<h2>Technical Appendix</h2>",
        "<p class='section-note'>Use this only after you have the bracket entries and matchup evidence in hand. The technical dashboard stays available for calibration, diagnostics, and engine comparison.</p>",
        appendix_links,
        if (!is.null(model_comparison) && length(model_comparison) > 0) render_model_comparison_link_html(model_comparison) else "",
        "</section>"
    )

    candidate_cards_html <- if (nrow(candidate_summary_rows) == 0) {
        paste(
            purrr::map_chr(seq_along(candidates), function(index) {
                candidate <- candidates[[index]]
                candidate_summary <- tibble::tibble(
                    candidate_id = candidate$candidate_id %||% index,
                    candidate_type = candidate$type %||% "unknown",
                    champion = candidate$champion %||% "n/a",
                    final_four = candidate$final_four %||% "n/a",
                    bracket_log_probability = candidate$bracket_log_prob %||% NA_real_,
                    mean_picked_game_probability = candidate$mean_game_prob %||% NA_real_,
                    diff_count = if (nrow(candidate_delta_rows) > 0) nrow(candidate_delta_rows) else 0L,
                    late_round_diff_count = if (nrow(candidate_delta_rows) > 0) sum(candidate_delta_rows$round %in% c("Sweet 16", "Elite 8", "Final Four", "Championship"), na.rm = TRUE) else 0L,
                    identity_text = candidate$diff_summary %||% "Bracket summary unavailable."
                )
                render_candidate_card(candidate_summary)
            }),
            collapse = "\n"
        )
    } else {
        paste(
            purrr::map_chr(seq_len(nrow(candidate_summary_rows)), function(index) {
                render_candidate_card(candidate_summary_rows[index, , drop = FALSE])
            }),
            collapse = "\n"
        )
    }

    watchlist_filter_toolbar <- paste0(
        "<div class='filter-toolbar' role='group' aria-label='Watchlist filters'>",
        "<button type='button' class='filter-chip is-active' data-surface-filter='All'>All</button>",
        "<button type='button' class='filter-chip' data-surface-filter='Bracket-changing toss-ups'>Bracket-changing</button>",
        "<button type='button' class='filter-chip' data-surface-filter='Upset pivots'>Upset pivots</button>",
        "<button type='button' class='filter-chip' data-surface-filter='Fragile favorites'>Fragile favorites</button>",
        "<button type='button' class='filter-chip' data-surface-filter='Late-round only'>Late-round only</button>",
        "</div>"
    )

    watchlist_shell <- paste0(
        "<div class='watchlist-shell' data-shell='watchlist'>",
        watchlist_cards,
        "</div>",
        "<div class='show-more-row'>",
        "<button type='button' class='show-more-button' data-shell-toggle='watchlist'>Show more</button>",
        "</div>"
    )

    evidence_shell <- paste0(
        "<div class='evidence-shell'>",
        evidence_panels,
        "</div>"
    )

    build_note <- if (!is.null(total_points_predictions) && nrow(candidate_summary_rows) > 0) {
        "Championship tiebreakers are included in the candidate cards. If you want the exact total-point distributions, open the technical appendix."
    } else {
        "Championship tiebreakers are not available in this run, so the candidate cards show only bracket-path context."
    }

    top_links <- paste0(
        "<nav class='jump-nav' aria-label='Dashboard sections'>",
        "<a href='#build'>Build</a>",
        "<a href='#bracket-tree'>Tree</a>",
        "<a href='#delta'>Delta</a>",
        "<a href='#watchlist'>Watchlist</a>",
        "<a href='#evidence'>Evidence</a>",
        "<a href='#paths'>Paths</a>",
        "</nav>"
    )

    appendix_callout <- paste0(
        "<div class='diagnostic-callout'>",
        "<strong>Need more diagnostics?</strong>",
        "<p>The technical dashboard keeps calibration, backtest, and engine diagnostics separate from the bracket workflow.</p>",
        "<p><a href='", dashboard_preview_url("technical_dashboard.html"), "'>Open technical_dashboard.html</a> <span class='muted'>|</span> <a href='", dashboard_preview_url("model_comparison_dashboard.html"), "'>Open model_comparison_dashboard.html</a></p>",
        "</div>"
    )

    page_style <- paste0(
        "body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;background:linear-gradient(180deg,#f7f4ee 0%,#faf8f2 40%,#f5f7fb 100%);color:#111827;margin:0;line-height:1.45;}",
        "a{color:#1d4ed8;text-decoration:none;} a:hover{text-decoration:underline;}",
        ".page{max-width:1320px;margin:0 auto;padding:24px 22px 48px;}",
        ".hero{background:rgba(255,255,255,0.78);backdrop-filter:blur(8px);border:1px solid #e7e5e4;border-radius:24px;padding:22px 22px 18px;box-shadow:0 18px 40px rgba(15,23,42,0.06);margin-bottom:18px;}",
        ".eyebrow{text-transform:uppercase;letter-spacing:0.12em;font-size:11px;color:#6b7280;margin-bottom:8px;}",
        "h1,h2,h3,h4{margin:0 0 8px 0;line-height:1.1;}",
        "h1{font-size:34px;} h2{font-size:24px;margin-top:4px;} h3{font-size:18px;} h4{font-size:16px;}",
        ".lede{max-width:940px;color:#374151;margin:0 0 14px 0;font-size:16px;}",
        ".jump-nav{display:flex;flex-wrap:wrap;gap:10px;margin-top:12px;}",
        ".jump-nav a,.filter-chip,.show-more-button,.jump-button{border:1px solid #d6d3d1;background:#fff;border-radius:999px;padding:9px 14px;font-weight:600;font-size:13px;cursor:pointer;display:inline-flex;align-items:center;gap:8px;}",
        ".jump-nav a{background:#101827;color:#fff;border-color:#101827;}",
        ".section{background:rgba(255,255,255,0.84);border:1px solid #e7e5e4;border-radius:22px;padding:20px;box-shadow:0 12px 30px rgba(15,23,42,0.05);margin:18px 0;}",
        ".section-note{color:#5b6472;max-width:900px;margin:0 0 14px 0;}",
        ".section-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(300px,1fr));gap:16px;align-items:start;}",
        ".candidate-card,.watchlist-card,.evidence-panel,.path-panel,.mini-table-card,.status-panel,.diagnostic-callout{background:#fff;border:1px solid #e7e5e4;border-radius:18px;padding:16px;box-shadow:0 2px 10px rgba(15,23,42,0.04);}",
        ".candidate-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(300px,1fr));gap:16px;}",
        ".candidate-card__head,.watchlist-card__head,.evidence-panel__summary{display:flex;justify-content:space-between;align-items:flex-start;gap:14px;}",
        ".candidate-card__eyebrow,.team-card__eyebrow,.surface-pill{text-transform:uppercase;letter-spacing:0.08em;font-size:11px;color:#64748b;font-weight:700;}",
        ".candidate-card__mini{font-size:20px;font-weight:800;color:#0f172a;}",
        ".candidate-card__subline{margin:6px 0;color:#334155;}",
        ".candidate-card__facts{display:grid;grid-template-columns:repeat(auto-fit,minmax(150px,1fr));gap:10px;margin:12px 0;}",
        ".candidate-card__facts div,.summary-chip,.metric-pill{border:1px solid #e2e8f0;border-radius:14px;padding:10px 12px;background:#f8fafc;}",
        ".candidate-card__facts span,.summary-chip span,.metric-pill span{display:block;font-size:11px;text-transform:uppercase;letter-spacing:0.08em;color:#64748b;margin-bottom:4px;}",
        ".candidate-card__facts strong,.summary-chip strong,.metric-pill strong{display:block;font-size:14px;color:#0f172a;}",
        ".candidate-card__links{display:flex;gap:10px;flex-wrap:wrap;margin-top:12px;}",
        ".candidate-card__tiebreaker{margin-top:12px;padding-top:12px;border-top:1px dashed #e2e8f0;}",
        ".summary-strip{display:grid;grid-template-columns:repeat(auto-fit,minmax(150px,1fr));gap:12px;margin:12px 0 0;}",
        ".summary-strip .summary-chip{box-shadow:none;}",
        ".delta-round{margin-top:16px;}",
        ".table-shell{overflow:auto;border:1px solid #e7e5e4;border-radius:16px;background:#fff;}",
        ".dashboard-table{width:100%;border-collapse:collapse;font-size:13px;min-width:960px;}",
        ".dashboard-table th,.dashboard-table td{border-bottom:1px solid #eef2f7;padding:10px 12px;vertical-align:top;text-align:left;}",
        ".dashboard-table th{background:#f8fafc;position:sticky;top:0;z-index:1;}",
        ".dashboard-table tr:last-child td{border-bottom:none;}",
        ".watchlist-toolbar,.filter-toolbar{display:flex;gap:10px;flex-wrap:wrap;margin:8px 0 14px;}",
        ".filter-chip.is-active{background:#101827;color:#fff;border-color:#101827;}",
        ".watchlist-shell{display:grid;grid-template-columns:repeat(auto-fit,minmax(300px,1fr));gap:14px;align-items:start;}",
        ".watchlist-card p{margin:8px 0 0;}",
        ".watchlist-card__meta{color:#64748b;font-size:13px;}",
        ".collapsed-row{display:none;}",
        ".watchlist-shell:not(.is-expanded) .collapsed-row{display:none;}",
        ".show-more-row{margin-top:12px;}",
        ".show-more-button{background:#f8fafc;}",
        ".watchlist-card.is-hidden-by-filter,.evidence-panel.is-hidden-by-filter{display:none !important;}",
        ".evidence-shell{display:grid;gap:14px;}",
        ".evidence-panel{padding:0;overflow:hidden;}",
        ".evidence-panel > summary{list-style:none;cursor:pointer;padding:16px;}",
        ".evidence-panel > summary::-webkit-details-marker{display:none;}",
        ".evidence-panel__body{padding:0 16px 16px;}",
        ".evidence-panel__lede,.evidence-panel__implication,.evidence-panel__note{margin:12px 0 0;color:#334155;}",
        ".evidence-summary-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(180px,1fr));gap:10px;margin:14px 0;}",
        ".summary-chip small{display:block;color:#64748b;margin-top:4px;}",
        ".team-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(300px,1fr));gap:14px;margin:14px 0;}",
        ".team-card{border:1px solid #e7e5e4;border-radius:16px;padding:14px;background:#fffaf4;}",
        ".team-card__head{display:flex;justify-content:space-between;gap:10px;align-items:flex-start;}",
        ".team-card__seed{font-weight:800;color:#0f172a;background:#fff;border:1px solid #e7e5e4;border-radius:999px;padding:6px 10px;}",
        ".team-card__meta{margin:8px 0 12px;color:#374151;}",
        ".mini-table-title{text-transform:uppercase;letter-spacing:0.08em;font-size:11px;color:#64748b;font-weight:700;margin-bottom:8px;}",
        ".mini-table-card{margin-top:12px;}",
        ".mini-table-card .dashboard-table{min-width:0;}",
        ".metric-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(140px,1fr));gap:10px;}",
        ".metric-pill{display:flex;flex-direction:column;gap:2px;}",
        ".path-panel{margin-top:14px;}",
        ".path-panel > summary{cursor:pointer;font-weight:700;}",
        ".path-panel__lede{color:#64748b;margin:8px 0 12px;}",
        ".appendix-links{display:flex;gap:12px;flex-wrap:wrap;}",
        ".diagnostic-callout{margin-top:16px;}",
        ".diagnostic-callout p{margin:6px 0 0;}",
        ".muted{color:#64748b;}",
        ".empty-state{color:#64748b;font-style:italic;}",
        ".status-panel strong{display:block;margin-bottom:6px;text-transform:uppercase;letter-spacing:0.04em;}",
        ".status-simulated{background:#fff6d8;border-color:#d97706;color:#7c2d12;}",
        ".status-final{background:#ecfdf3;border-color:#16a34a;color:#14532d;}",
        ".status-unknown{background:#eef2ff;border-color:#6366f1;color:#312e81;}",
        ".surface-pill{display:inline-flex;align-items:center;gap:8px;padding:4px 9px;border-radius:999px;background:#f8fafc;border:1px solid #e2e8f0;}",
        ".jump-button{background:#101827;color:#fff;border-color:#101827;}",
        ".bracket-tree-container{overflow-x:auto;margin:0;}",
        ".bracket-tree-controls{display:flex;gap:10px;margin-bottom:10px;flex-wrap:wrap;}",
        ".btree-toggle{border:1px solid #d6d3d1;background:#fff;border-radius:999px;padding:7px 14px;font-weight:600;font-size:13px;cursor:pointer;}",
        ".btree-toggle.is-active{background:#101827;color:#fff;border-color:#101827;}",
        ".btree-panel[hidden]{display:none !important;}",
        ".btree-node{cursor:pointer;}",
        ".btree-node:hover rect:first-of-type{filter:brightness(0.85);}",
        ".btree-upset-badge{pointer-events:none;}",
        "#btree-tooltip{position:fixed;background:#0f172a;color:#f8fafc;border-radius:12px;padding:12px 14px;font-size:13px;max-width:280px;z-index:999;pointer-events:none;opacity:0;transition:opacity 0.12s;line-height:1.6;box-shadow:0 8px 24px rgba(0,0,0,0.3);}",
        "#btree-tooltip.is-visible{opacity:1;}",
        ".btree-legend{display:flex;flex-wrap:wrap;gap:12px;margin-bottom:14px;align-items:center;}",
        ".btree-legend-item{display:inline-flex;align-items:center;gap:6px;font-size:12px;color:#334155;}",
        ".btree-legend-swatch{display:inline-block;width:14px;height:14px;border-radius:3px;flex-shrink:0;}",
        "@media (max-width: 880px){.page{padding:16px 14px 36px;}.hero,.section{padding:16px;}.dashboard-table{min-width:820px;}.candidate-grid,.watchlist-shell,.team-grid,.evidence-summary-grid{grid-template-columns:1fr;}}"
    )

    script <- paste0(
        "<script>",
        "(function(){",
        "var filterButtons=Array.from(document.querySelectorAll('[data-surface-filter]'));",
        "var filterTargets=Array.from(document.querySelectorAll('[data-surface]'));",
        "var shells=Array.from(document.querySelectorAll('[data-shell-toggle]'));",
        "function setFilter(value){",
        "filterButtons.forEach(function(button){var active=button.getAttribute('data-surface-filter')===value;button.classList.toggle('is-active', active);});",
        "filterTargets.forEach(function(node){",
        "var surface=node.getAttribute('data-surface')||'';",
        "var lateRound=node.getAttribute('data-late-round')==='true';",
        "var show=value==='All' || (value==='Late-round only' && lateRound) || surface===value;",
        "node.classList.toggle('is-hidden-by-filter', !show);",
        "node.hidden=!show;",
        "if(show){node.hidden=false;}",
        "});",
        "}",
        "filterButtons.forEach(function(button){button.addEventListener('click', function(){setFilter(button.getAttribute('data-surface-filter'));});});",
        "shells.forEach(function(button){button.addEventListener('click', function(){var shell=document.querySelector('[data-shell=\"'+button.getAttribute('data-shell-toggle')+'\"]');if(shell){shell.classList.toggle('is-expanded');button.textContent=shell.classList.contains('is-expanded') ? 'Show less' : 'Show more';}});});",
        "document.addEventListener('click', function(event){",
        "var target=event.target.closest('[data-open-evidence]');",
        "if(!target){return;}",
        "var id=target.getAttribute('data-open-evidence');",
        "if(!id){return;}",
        "setFilter('All');",
        "var panel=document.getElementById(id);",
        "if(panel){panel.open=true;panel.scrollIntoView({behavior:'smooth', block:'start'});}",
        "});",
        "setFilter('All');",
        "var btreeTip=document.getElementById('btree-tooltip');",
        "if(btreeTip){",
        "var setTip=function(node){",
        "btreeTip.textContent='';",
        "var candidate=node.getAttribute('data-tip-candidate')||'';",
        "var matchup=node.getAttribute('data-tip-matchup')||'';",
        "var round=node.getAttribute('data-tip-round')||'';",
        "var region=node.getAttribute('data-tip-region')||'';",
        "var pick=node.getAttribute('data-tip-pick')||'';",
        "var fav=node.getAttribute('data-tip-fav')||'';",
        "var prob=node.getAttribute('data-tip-prob')||'';",
        "var tier=node.getAttribute('data-tip-tier')||'';",
        "var rationale=node.getAttribute('data-tip-rationale')||'';",
        "var strong=document.createElement('strong');",
        "strong.textContent=matchup;",
        "btreeTip.appendChild(strong);",
        "btreeTip.appendChild(document.createElement('br'));",
        "btreeTip.appendChild(document.createTextNode(round+' · '+region));",
        "if(candidate || pick){",
        "btreeTip.appendChild(document.createElement('br'));",
        "btreeTip.appendChild(document.createTextNode(candidate+': '+pick));",
        "}",
        "btreeTip.appendChild(document.createElement('br'));",
        "btreeTip.appendChild(document.createTextNode('Fav: '));",
        "var favStrong=document.createElement('strong');",
        "favStrong.textContent=fav;",
        "btreeTip.appendChild(favStrong);",
        "btreeTip.appendChild(document.createTextNode(' ('+prob+')'));",
        "btreeTip.appendChild(document.createElement('br'));",
        "btreeTip.appendChild(document.createTextNode('Tier: '+tier));",
        "if(rationale){",
        "btreeTip.appendChild(document.createElement('br'));",
        "var em=document.createElement('em');",
        "em.textContent=rationale;",
        "btreeTip.appendChild(em);",
        "}",
        "};",
        "Array.from(document.querySelectorAll('.btree-node')).forEach(function(node){",
        "node.addEventListener('mouseenter',function(){setTip(node);btreeTip.classList.add('is-visible');});",
        "node.addEventListener('mousemove',function(e){",
        "btreeTip.style.left=Math.min(e.clientX+14,window.innerWidth-295)+'px';",
        "btreeTip.style.top=Math.min(e.clientY-10,window.innerHeight-185)+'px';",
        "});",
        "node.addEventListener('mouseleave',function(){btreeTip.classList.remove('is-visible');});",
        "});",
        "}",
        "var btreeButtons=Array.from(document.querySelectorAll('.btree-toggle[data-btree-target]'));",
        "var btreePanels=Array.from(document.querySelectorAll('[data-btree-panel]'));",
        "if(btreeButtons.length>0 && btreePanels.length>0){",
        "var setTree=function(target){",
        "btreeButtons.forEach(function(button){button.classList.toggle('is-active', button.getAttribute('data-btree-target')===target);});",
        "btreePanels.forEach(function(panel){var active=panel.getAttribute('data-btree-panel')===target;panel.hidden=!active;panel.classList.toggle('is-active', active);});",
        "if(btreeTip){btreeTip.classList.remove('is-visible');}",
        "};",
        "btreeButtons.forEach(function(button){button.addEventListener('click',function(){setTree(button.getAttribute('data-btree-target'));});});",
        "setTree(btreeButtons[0].getAttribute('data-btree-target'));",
        "}",
        "})();",
        "</script>"
    )

    candidate_path_blocks <- if (nchar(candidate_paths_html) > 0) candidate_paths_html else "<p class='empty-state'>Candidate paths were not supplied.</p>"

    paste0(
        "<!DOCTYPE html><html><head><meta charset='utf-8'>",
        "<title>mmBayes Bracket Dashboard</title>",
        "<style>", page_style, "</style>",
        "</head><body>",
        "<div class='page'>",
        "<header class='hero'>",
        "<div class='eyebrow'>mmBayes Bracket Workspace</div>",
        "<h1>Bracket-building decision workspace</h1>",
        "<p class='lede'>Bracket year ", html_escape(bracket_year), ". Start with Candidate 1 and Candidate 2, then use the delta rows and evidence drawers to decide what actually goes on paper. The technical dashboard stays available as secondary diagnostics.</p>",
        top_links,
        render_dashboard_term_legend_html(),
        "</header>",
        status_panel,
        "<section class='section' id='build'>",
        "<h2>Build Your Entries</h2>",
        "<p class='section-note'>Candidate 1 is the baseline entry. Candidate 2 is the alternate bracket. Use the jump links on each card to move straight to the path reference or matchup evidence.</p>",
        "<div class='candidate-grid'>",
        candidate_cards_html,
        "</div>",
        "<div class='summary-strip'>",
        "<div class='summary-chip'><span>Candidate 2 delta rows</span><strong>", html_escape(display_value(nrow(candidate_delta_rows), digits = 0L)), "</strong></div>",
        "<div class='summary-chip'><span>Bracket-changing rows</span><strong>", html_escape(display_value(get_watchlist_count("Bracket-changing toss-ups"), digits = 0L)), "</strong></div>",
        "<div class='summary-chip'><span>Upset pivots</span><strong>", html_escape(display_value(get_watchlist_count("Upset pivots"), digits = 0L)), "</strong></div>",
        "<div class='summary-chip'><span>Fragile favorites</span><strong>", html_escape(display_value(get_watchlist_count("Fragile favorites"), digits = 0L)), "</strong></div>",
        "</div>",
        "</section>",
        "<section class='section' id='bracket-tree'>",
        "<h2>Bracket Tree</h2>",
        "<p class='section-note'>Each node is a game in the selected candidate's actual bracket path. Color shows confidence tier. Click any game to open its evidence drawer. Hover for the active candidate's matchup, pick, and probabilities.</p>",
        render_bracket_tree_svg(bracket_tree_data),
        "</section>",
        "<section class='section' id='delta'>",
        "<h2>Candidate 2 Delta From Candidate 1</h2>",
        "<p class='section-note'>Candidate 2 is the same bracket as Candidate 1 except for the rows below. The rows are ordered by bracket fill order, not by score.</p>",
        candidate_delta_html,
        "</section>",
        "<section class='section' id='watchlist'>",
        "<h2>Think Harder About These Matchups</h2>",
        "<p class='section-note'>This watchlist only surfaces rows that matter for human review: bracket-changing toss-ups, high-payoff upset pivots, and fragile favorites with enough score to deserve another look.</p>",
        watchlist_filter_toolbar,
        render_confidence_legend_html(),
        watchlist_shell,
        appendix_callout,
        "</section>",
        "<section class='section' id='evidence'>",
        "<h2>Matchup Evidence</h2>",
        "<p class='section-note'>Each drawer shows the matchup evidence the model saw. It is not a causal decomposition. Use the jump buttons in the delta and watchlist sections to open a specific drawer.</p>",
        evidence_shell,
        "</section>",
        "<section class='section' id='paths'>",
        "<h2>Full Candidate Paths</h2>",
        "<p class='section-note'>Reference material only. These panels keep the full bracket-ordered sequence for each candidate behind collapsible details so the landing page stays focused on the entry workflow.</p>",
        candidate_path_blocks,
        "</section>",
        appendix_panel,
        if (!is.null(model_comparison) && length(model_comparison) > 0) render_model_comparison_link_html(model_comparison) else "",
        if (!is.null(quality_backtest) && model_quality_has_backtest(quality_backtest)) paste0("<div class='section'><p class='section-note'>Calibration and backtest details live on the technical dashboard.</p><p class='muted'>", html_escape(quality_source_label), "</p></div>") else "",
        "</div>",
        script,
        "</body></html>"
    )
}
