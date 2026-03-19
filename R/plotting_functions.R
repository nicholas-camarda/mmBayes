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
#'
#' @param results A full bracket simulation result bundle.
#'
#' @return A faceted `ggplot` of matchup intervals and posterior means.
#' @export
create_uncertainty_plot <- function(results) {
    matchups <- flatten_matchup_results(results) %>%
        dplyr::mutate(
            round = factor(round, levels = round_levels()),
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

#' Render a championship total-points distribution panel
#'
#' @param summary_row A one-row candidate championship summary tibble.
#' @param distribution A candidate-level championship distribution tibble.
#'
#' @return A scalar character string containing an HTML panel body.
#' @keywords internal
render_championship_distribution_panel <- function(summary_row, distribution) {
    if (nrow(summary_row) == 0) {
        return("<p class='empty-state'>Championship tiebreaker unavailable.</p>")
    }

    top_distribution <- distribution %>%
        dplyr::arrange(dplyr::desc(probability), total_points) %>%
        dplyr::slice_head(n = 8) %>%
        dplyr::mutate(
            total_points = as.character(total_points),
            probability = format_probability(probability)
        )

    paste0(
        "<div class='distribution-meta'>",
        "<p><strong>Projected title game:</strong> ", html_escape(summary_row$championship_matchup[[1]]), "</p>",
        "<p><strong>Recommended tiebreaker:</strong> ", html_escape(summary_row$recommended_tiebreaker_points[[1]]), "</p>",
        "<p><strong>Median:</strong> ", sprintf("%.1f", safe_numeric(summary_row$predicted_total_median[[1]], default = NA_real_)),
        " <span class='muted'>| 80% interval ", html_escape(format_total_interval(summary_row$predicted_total_80_lower[[1]], summary_row$predicted_total_80_upper[[1]])), "</span></p>",
        "</div>",
        render_html_table(top_distribution %>% dplyr::rename(`Most likely total` = total_points, Probability = probability))
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
                    render_championship_distribution_panel(summary_row, distribution_row),
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

#' Render the ranked decision board as inline SVG
#'
#' @param decision_sheet The decision-sheet data frame.
#' @param top_n Maximum number of rows to render.
#'
#' @return A scalar character string containing inline SVG markup.
#' @keywords internal
render_ranked_decision_svg <- function(decision_sheet, top_n = 12L) {
    if (nrow(decision_sheet) == 0) {
        return("<p class='empty-state'>No decision rows available.</p>")
    }

    width <- 920
    margin_left <- 240
    margin_right <- 250
    margin_top <- 34
    row_height <- 34
    plot_width <- width - margin_left - margin_right

    plot_data <- decision_sheet %>%
        dplyr::arrange(dplyr::desc(decision_score), round, region, matchup_number) %>%
        dplyr::slice_head(n = top_n) %>%
        dplyr::mutate(
            row_id = dplyr::row_number(),
            y = margin_top + ((row_id - 1) * row_height),
            matchup_short = truncate_dashboard_label(matchup_label, width = 34L),
            pick_summary = ifelse(
                candidate_diff_flag,
                paste0(candidate_1_pick, " | alt ", candidate_2_pick),
                candidate_1_pick
            ),
            detail_summary = sprintf(
                "%s | %s",
                as.character(round),
                truncate_dashboard_label(rationale_short, width = 26L)
            ),
            tier_color = dashboard_tier_color(confidence_tier)
        )

    height <- margin_top + (nrow(plot_data) * row_height) + 40
    to_x <- function(probability) {
        clipped <- pmin(pmax(safe_numeric(probability, default = 0.5), 0.5), 1)
        margin_left + ((clipped - 0.5) / 0.5) * plot_width
    }

    grid_lines <- paste(
        vapply(seq(0.5, 1, by = 0.1), function(probability) {
            x <- to_x(probability)
            sprintf(
                "<line x1='%.1f' y1='16' x2='%.1f' y2='%.1f' stroke='#d6d3d1' stroke-dasharray='4 4' stroke-width='1'/>",
                x,
                x,
                height - 18
            )
        }, character(1)),
        collapse = "\n"
    )
    axis_labels <- paste(
        vapply(seq(0.5, 1, by = 0.1), function(probability) {
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
    row_html <- paste(
        vapply(seq_len(nrow(plot_data)), function(index) {
            row <- plot_data[index, , drop = FALSE]
            sprintf(
                paste0(
                    "<g>",
                    "<text x='16' y='%.1f' font-size='12' fill='#1f2937' dominant-baseline='middle'>%s</text>",
                    "<line x1='%.1f' y1='%.1f' x2='%.1f' y2='%.1f' stroke='%s' stroke-width='4' stroke-linecap='round'/>",
                    "<circle cx='%.1f' cy='%.1f' r='6' fill='%s' stroke='white' stroke-width='2'/>",
                    "<text x='%.1f' y='%.1f' font-size='12' fill='#111827' dominant-baseline='middle'>%s</text>",
                    "<text x='%.1f' y='%.1f' font-size='11' fill='#6b7280' dominant-baseline='middle'>%s</text>",
                    "</g>"
                ),
                row$y[[1]],
                html_escape(row$matchup_short[[1]]),
                to_x(row$ci_lower[[1]]),
                row$y[[1]],
                to_x(row$ci_upper[[1]]),
                row$y[[1]],
                row$tier_color[[1]],
                to_x(row$win_prob_favorite[[1]]),
                row$y[[1]],
                row$tier_color[[1]],
                width - margin_right + 14,
                row$y[[1]] - 7,
                html_escape(row$pick_summary[[1]]),
                width - margin_right + 14,
                row$y[[1]] + 9,
                html_escape(row$detail_summary[[1]])
            )
        }, character(1)),
        collapse = "\n"
    )

    paste0(
        "<svg viewBox='0 0 ", width, " ", height, "' class='tech-svg' role='img' aria-label='Ranked decision board'>",
        grid_lines,
        "<line x1='", margin_left, "' y1='16' x2='", margin_left, "' y2='", height - 18, "' stroke='#a8a29e' stroke-width='1'/>",
        "<line x1='", width - margin_right, "' y1='16' x2='", width - margin_right, "' y2='", height - 18, "' stroke='#e7e5e4' stroke-width='1'/>",
        "<text x='16' y='18' font-size='11' fill='#6b7280'>Matchup</text>",
        "<text x='", width - margin_right + 14, "' y='18' font-size='11' fill='#6b7280'>Recommended pick</text>",
        row_html,
        axis_labels,
        "</svg>"
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

#' Render the candidate divergence view as inline SVG
#'
#' @param decision_sheet The decision-sheet data frame.
#'
#' @return A scalar character string containing inline SVG markup.
#' @keywords internal
render_candidate_divergence_svg <- function(decision_sheet) {
    diff_rows <- decision_sheet %>%
        dplyr::filter(candidate_diff_flag) %>%
        dplyr::mutate(
            round = factor(round, levels = round_levels()),
            region = factor(region, levels = c(bracket_region_levels(), "National"))
        ) %>%
        dplyr::arrange(round, region, matchup_number)

    if (nrow(diff_rows) == 0) {
        return("<p class='empty-state'>Candidate 1 and Candidate 2 match on every slot in this run.</p>")
    }

    width <- 940
    margin_left <- 260
    candidate_one_x <- 470
    candidate_two_x <- 700
    margin_top <- 42
    row_height <- 34
    height <- margin_top + (nrow(diff_rows) * row_height) + 28

    row_html <- paste(
        vapply(seq_len(nrow(diff_rows)), function(index) {
            row <- diff_rows[index, , drop = FALSE]
            y <- margin_top + ((index - 1) * row_height)
            tier_color <- dashboard_tier_color(row$confidence_tier[[1]])
            sprintf(
                paste0(
                    "<g>",
                    "<text x='16' y='%.1f' font-size='12' fill='#1f2937' dominant-baseline='middle'>%s</text>",
                    "<line x1='%.1f' y1='%.1f' x2='%.1f' y2='%.1f' stroke='#d6d3d1' stroke-width='2'/>",
                    "<circle cx='%.1f' cy='%.1f' r='6' fill='%s'/>",
                    "<circle cx='%.1f' cy='%.1f' r='6' fill='%s'/>",
                    "<text x='%.1f' y='%.1f' text-anchor='middle' font-size='12' fill='#111827' dominant-baseline='middle'>%s</text>",
                    "<text x='%.1f' y='%.1f' text-anchor='middle' font-size='12' fill='#111827' dominant-baseline='middle'>%s</text>",
                    "<text x='%.1f' y='%.1f' font-size='11' fill='#6b7280' dominant-baseline='middle'>%s | %s</text>",
                    "</g>"
                ),
                y,
                html_escape(truncate_dashboard_label(row$matchup_label[[1]], width = 34L)),
                candidate_one_x,
                y,
                candidate_two_x,
                y,
                candidate_one_x,
                y,
                tier_color,
                candidate_two_x,
                y,
                tier_color,
                candidate_one_x,
                y - 10,
                html_escape(truncate_dashboard_label(row$candidate_1_pick[[1]], width = 16L)),
                candidate_two_x,
                y - 10,
                html_escape(truncate_dashboard_label(row$candidate_2_pick[[1]], width = 16L)),
                candidate_two_x + 34,
                y + 7,
                html_escape(as.character(row$round[[1]])),
                html_escape(as.character(row$confidence_tier[[1]]))
            )
        }, character(1)),
        collapse = "\n"
    )

    paste0(
        "<svg viewBox='0 0 ", width, " ", height, "' class='tech-svg' role='img' aria-label='Candidate divergence view'>",
        "<text x='16' y='18' font-size='11' fill='#6b7280'>Differing slot</text>",
        "<text x='", candidate_one_x, "' y='18' text-anchor='middle' font-size='11' fill='#6b7280'>Candidate 1</text>",
        "<text x='", candidate_two_x, "' y='18' text-anchor='middle' font-size='11' fill='#6b7280'>Candidate 2</text>",
        row_html,
        "</svg>"
    )
}

#' Render the upset opportunity board as inline SVG
#'
#' @param decision_sheet The decision-sheet data frame.
#' @param top_n Maximum number of rows to render.
#'
#' @return A scalar character string containing inline SVG markup.
#' @keywords internal
render_upset_opportunity_svg <- function(decision_sheet, top_n = 10L) {
    if (nrow(decision_sheet) == 0) {
        return("<p class='empty-state'>No decision rows available.</p>")
    }

    plot_data <- decision_sheet %>%
        dplyr::arrange(dplyr::desc(upset_leverage), dplyr::desc(decision_score), round, matchup_number) %>%
        dplyr::slice_head(n = top_n) %>%
        dplyr::mutate(
            row_id = dplyr::row_number(),
            underdog_summary = paste0(underdog, " over ", posterior_favorite),
            tier_color = dashboard_tier_color(confidence_tier)
        )

    width <- 820
    margin_left <- 250
    margin_right <- 150
    margin_top <- 34
    row_height <- 34
    plot_width <- width - margin_left - margin_right
    height <- margin_top + (nrow(plot_data) * row_height) + 34

    to_x <- function(probability) {
        clipped <- pmin(pmax(safe_numeric(probability, default = 0), 0), 0.5)
        margin_left + (clipped / 0.5) * plot_width
    }

    grid_lines <- paste(
        vapply(seq(0, 0.5, by = 0.1), function(probability) {
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
    axis_labels <- paste(
        vapply(seq(0, 0.5, by = 0.1), function(probability) {
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
    row_html <- paste(
        vapply(seq_len(nrow(plot_data)), function(index) {
            row <- plot_data[index, , drop = FALSE]
            y <- margin_top + ((index - 1) * row_height)
            pick_source <- if (row$candidate_diff_flag[[1]]) {
                paste0("C1 ", row$candidate_1_pick[[1]], " | C2 ", row$candidate_2_pick[[1]])
            } else {
                paste0("Both candidates: ", row$candidate_1_pick[[1]])
            }
            x <- to_x(row$win_prob_underdog[[1]])
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
                html_escape(truncate_dashboard_label(row$underdog_summary[[1]], width = 34L)),
                margin_left,
                y,
                x,
                y,
                row$tier_color[[1]],
                x,
                y,
                row$tier_color[[1]],
                width - margin_right + 10,
                y,
                html_escape(truncate_dashboard_label(paste(as.character(row$round[[1]]), pick_source, sep = " | "), width = 30L))
            )
        }, character(1)),
        collapse = "\n"
    )

    paste0(
        "<svg viewBox='0 0 ", width, " ", height, "' class='tech-svg' role='img' aria-label='Upset opportunity board'>",
        grid_lines,
        row_html,
        axis_labels,
        "<text x='16' y='18' font-size='11' fill='#6b7280'>Underdog angle</text>",
        "</svg>"
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

    width <- 420
    height <- 320
    margin_left <- 56
    margin_right <- 26
    margin_bottom <- 52
    plot_width <- width - margin_left - margin_right
    plot_height <- height - margin_bottom - 34
    to_x <- function(probability) margin_left + (safe_numeric(probability, default = 0) * plot_width)
    to_y <- function(probability) 18 + ((1 - safe_numeric(probability, default = 0)) * plot_height)

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
        "<text x='", margin_left, "' y='14' font-size='12' font-weight='700' fill='#334155'>Calibration curve</text>",
        "<line x1='", margin_left, "' y1='", 18 + plot_height, "' x2='", margin_left + plot_width, "' y2='18' stroke='#d6d3d1' stroke-dasharray='4 4' stroke-width='1.5'/>",
        "<line x1='", margin_left, "' y1='18' x2='", margin_left, "' y2='", 18 + plot_height, "' stroke='#a8a29e' stroke-width='1'/>",
        "<line x1='", margin_left, "' y1='", 18 + plot_height, "' x2='", margin_left + plot_width, "' y2='", 18 + plot_height, "' stroke='#a8a29e' stroke-width='1'/>",
        "<polyline points='", polyline_points, "' fill='none' stroke='#1d4ed8' stroke-width='3' stroke-linecap='round' stroke-linejoin='round'/>",
        points_html,
        "<text x='", margin_left, "' y='", height - 8, "' font-size='11' fill='#6b7280'>0%</text>",
        "<text x='", margin_left + plot_width, "' y='", height - 8, "' text-anchor='end' font-size='11' fill='#6b7280'>100%</text>",
        "<text x='", margin_left + 8, "' y='", height - 8, "' font-size='11' fill='#6b7280'>Predicted probability</text>",
        "<text x='12' y='", 26 + (plot_height / 2), "' font-size='11' fill='#6b7280' transform='rotate(-90 12 ", 26 + (plot_height / 2), ")'>Observed win rate</text>",
        "<circle cx='", width - 134, "' cy='", 18, "' r='5' fill='#1d4ed8'/>",
        "<line x1='", width - 151, "' y1='18' x2='", width - 124, "' y2='18' stroke='#d6d3d1' stroke-dasharray='4 4' stroke-width='1.5'/>",
        "<text x='", width - 116, "' y='22' font-size='11' fill='#6b7280'>Observed by bin</text>",
        "<text x='", width - 31, "' y='22' font-size='11' fill='#6b7280'>Perfect calibration</text>",
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

    interpretation_grid <- render_interpretation_grid_html(
        decision_sheet = decision_sheet,
        quality_backtest = quality_backtest,
        quality_source_label = quality_source_label
    )
    reading_guide <- render_dashboard_reading_guide_html()

    totals_section <- if (nrow(tiebreaker_summary) > 0) {
        panels <- paste(
            purrr::map_chr(candidates, function(candidate) {
                summary_row <- lookup_candidate_tiebreaker(candidate$candidate_id)
                distribution_row <- lookup_candidate_distribution(candidate$candidate_id)
                paste0(
                    "<div class='panel'>",
                    "<h3>Candidate ", candidate$candidate_id, " Championship Distribution</h3>",
                    render_championship_distribution_panel(summary_row, distribution_row),
                    "</div>"
                )
            }),
            collapse = "\n"
        )
        paste0(
            "<h2>Championship Tiebreaker Distribution</h2>",
            "<div class='distribution-grid'>", panels, "</div>"
        )
    } else {
        paste0(
            "<h2>Championship Tiebreaker Distribution</h2>",
            "<div class='panel'><p class='empty-state'>Championship total-points distributions were not supplied for this run.</p></div>"
        )
    }

    candidate_view_panel <- function(candidate, candidate_key, sequence_view, row_classes) {
        tiebreaker_row <- lookup_candidate_tiebreaker(candidate$candidate_id)
        tiebreaker_html <- if (nrow(tiebreaker_row) == 1L) {
            paste0(
                "<p><strong>Championship tiebreaker:</strong> ", html_escape(tiebreaker_row$recommended_tiebreaker_points[[1]]), "</p>",
                "<p><strong>Projected title game:</strong> ", html_escape(tiebreaker_row$championship_matchup[[1]]), "</p>",
                "<p><strong>Median total:</strong> ", sprintf("%.1f", safe_numeric(tiebreaker_row$predicted_total_median[[1]], default = NA_real_)),
                " <span class='muted'>| 80% interval ", html_escape(format_total_interval(tiebreaker_row$predicted_total_80_lower[[1]], tiebreaker_row$predicted_total_80_upper[[1]])), "</span></p>"
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
            tiebreaker_html,
            "</div>",
            "<div class='panel'>",
            "<h2>Candidate ", candidate$candidate_id, " Most Fragile Picks</h2>",
            "<p class='panel-caption'>These are the slots where this candidate is leaning on the thinnest margin or widest interval.</p>",
            render_candidate_fragility_svg(candidate$matchups),
            "</div>",
            "</div>",
            "<div class='panel'>",
            "<h2>Candidate ", candidate$candidate_id, " Path Review</h2>",
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
        ".guide-grid,.interpretation-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(240px,1fr));gap:14px;}",
        ".guide-card,.interpretation-card{background:#fff;border:1px solid #d6d3d1;border-radius:14px;padding:14px;}",
        ".guide-label,.interpretation-label{text-transform:uppercase;letter-spacing:0.08em;font-size:11px;color:#6b7280;margin-bottom:8px;}",
        ".interpretation-summary{margin:0;color:#111827;}",
        ".interpretation-note{margin:10px 0 0 0;color:#6b7280;font-size:13px;}",
        ".distribution-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(280px,1fr));gap:16px;}",
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
        ".tech-svg-compact{max-width:420px;}",
        ".muted{color:#6b7280;}",
        ".empty-state{color:#6b7280;font-style:italic;}",
        ".legend-row{display:flex;gap:12px;flex-wrap:wrap;margin:12px 0 18px 0;}",
        ".legend-chip{display:flex;align-items:center;gap:8px;padding:8px 10px;border-radius:999px;background:white;border:1px solid #d6d3d1;font-size:13px;}",
        ".legend-swatch{width:12px;height:12px;border-radius:999px;display:inline-block;}",
        ".legend-copy{font-size:12px;color:#6b7280;}",
        "</style></head><body>",
        "<h1>mmBayes Technical Bracket Dashboard</h1>",
        "<p class='lede'>Bracket year ", html_escape(bracket_year),
        ". This view is built for deeper inspection rather than quick entry. Use Compare to see the highest-value review spots, then flip to Candidate 1 or Candidate 2 to inspect the full bracket path and fragile picks for that bracket only.</p>",
        status_panel,
        "<div class='panel'>",
        "<h2>How To Read This</h2>",
        reading_guide,
        render_confidence_legend_html(),
        "</div>",
        "<div class='overview-grid'>",
        candidate_summary_cards,
        "<div class='summary-card'><div class='summary-label'>Toss-up games</div><div class='summary-value'>", get_tier_count("Toss-up"), "</div><p class='summary-note'>Primary manual-review slots.</p></div>",
        "<div class='summary-card'><div class='summary-label'>Volatile games</div><div class='summary-value'>", get_tier_count("Volatile"), "</div><p class='summary-note'>Wide intervals with unstable outcomes.</p></div>",
        "<div class='summary-card'><div class='summary-label'>Candidate differences</div><div class='summary-value'>", sum(decision_sheet$candidate_diff_flag, na.rm = TRUE), "</div><p class='summary-note'>Slots where the alternate path diverges.</p></div>",
        "<div class='summary-card'><div class='summary-label'>Top leverage upset</div><div class='summary-value'>", html_escape(truncate_dashboard_label(leverage_text, width = 22L)), "</div><p class='summary-note'>Highest underdog payoff under current scoring.</p></div>",
        "</div>",
        "<div class='panel'>",
        "<h2>Why These Boards Are Ordered This Way</h2>",
        interpretation_grid,
        "</div>",
        "<div class='panel'><h2>How To Read This</h2>",
        "<p>Start in Compare. The ranked board answers which games deserve attention first, the risk map shows where bracket fragility clusters, and the upset board surfaces the few underdog swings with real leverage.</p>",
        "<p>Switch to Candidate 1 or Candidate 2 when you want the full path review for a single bracket without the comparison noise.</p>",
        "</div>",
        "<div class='toggle-bar' role='group' aria-label='Candidate dashboard view'>",
        "<button type='button' class='toggle-button is-active' data-view-target='compare' aria-pressed='true'>Compare</button>",
        "<button type='button' class='toggle-button' data-view-target='candidate-1' aria-pressed='false'>Candidate 1</button>",
        "<button type='button' class='toggle-button' data-view-target='candidate-2' aria-pressed='false'>Candidate 2</button>",
        "</div>",
        "<div class='toggle-panel is-active' data-view-panel='compare'>",
        "<div class='panel'><h2>Ranked Decision Board</h2><p class='panel-caption'>Top review spots sorted by decision score. Dots are posterior means, bars are posterior intervals, and colors map to the confidence-tier legend above.</p>",
        render_ranked_decision_svg(decision_sheet),
        "</div>",
        "<div class='two-column'>",
        "<div class='panel'><h2>Round-by-Region Risk Map</h2><p class='panel-caption'>Rows are regions and columns are rounds. Each cell shows the highest review pressure in that region-round slice, and the small count in the cell shows how many games are flagged there for review.</p>",
        render_round_risk_heatmap_svg(decision_sheet),
        "</div>",
        "<div class='panel'><h2>Upset Opportunity Board</h2><p class='panel-caption'>Ordered by upset leverage so the highest-payoff underdog pivots are obvious.</p>",
        render_upset_opportunity_svg(decision_sheet),
        "</div>",
        "</div>",
        "<div class='panel'><h2>Candidate Divergence</h2><p class='panel-caption'>Only the slots where the alternate bracket changes the path, which keeps the comparison focused on the actual decision points.</p>",
        render_candidate_divergence_svg(decision_sheet),
        "</div>",
        "</div>",
        candidate_view_panel(candidates[[1]], "candidate-1", candidate_one_path, candidate_one_classes),
        candidate_view_panel(if (length(candidates) >= 2) candidates[[2]] else candidates[[1]], "candidate-2", candidate_two_path, candidate_two_classes),
        "<div class='panel'><h2>Model Quality</h2>",
        "<p class='quality-intro'>The chart on the right is a calibration plot. The blue points show the observed win rate for each probability bin, which is what the dashboard calls the empirical rate. If the blue line hugs the dotted diagonal, the model’s probabilities are lining up well with reality.</p>",
        if (model_quality_has_backtest(quality_backtest)) {
            paste0(
                "<p class='panel-caption'><strong>Source:</strong> ", html_escape(quality_source_label), "</p>",
                "<div class='quality-grid'><div class='quality-card'>",
                render_html_table(calibration_summary),
                "</div><div class='quality-card'>",
                render_calibration_svg(quality_backtest$calibration %||% tibble::tibble()),
                "<p class='quality-note'>Empirical means the observed win rate within each probability bin. The diagonal is perfect calibration; departures from it show where the model is over- or under-confident.</p>",
                "</div></div>"
            )
        } else {
            "<p class='empty-state'>Backtest skipped in this run.</p>"
        },
        "</div>",
        "<h2>Ranked Decision Table</h2><div class='panel'>",
        render_html_table(top_decisions, row_classes = top_decision_classes),
        "</div>",
        "<h2>Candidate Difference Table</h2><div class='panel'>",
        render_html_table(candidate_diff, row_classes = candidate_diff_classes),
        "</div>",
        totals_section,
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
