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
#'
#' @return A scalar character string containing an HTML table.
#' @keywords internal
render_html_table <- function(data, max_rows = NULL) {
    if (nrow(data) == 0) {
        return("<p class='empty-state'>None</p>")
    }

    if (!is.null(max_rows)) {
        data <- utils::head(data, max_rows)
    }

    header_html <- paste(
        sprintf("<th>%s</th>", html_escape(names(data))),
        collapse = ""
    )

    row_html <- apply(data, 1, function(row) {
        paste(
            sprintf("<td>%s</td>", html_escape(row)),
            collapse = ""
        )
    })

    body_html <- paste(sprintf("<tr>%s</tr>", row_html), collapse = "\n")

    paste0(
        "<table class='dashboard-table'>",
        "<thead><tr>", header_html, "</tr></thead>",
        "<tbody>", body_html, "</tbody>",
        "</table>"
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
#'
#' @return A complete HTML document as a scalar character string.
#' @export
create_bracket_dashboard_html <- function(bracket_year, decision_sheet, candidates, backtest = NULL) {
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
            rationale = rationale_short
        )

    candidate_diff <- decision_sheet %>%
        dplyr::filter(candidate_diff_flag) %>%
        dplyr::transmute(
            round = as.character(round),
            region,
            slot = matchup_number,
            candidate_1 = paste0(candidate_1_pick, " from ", matchup_label),
            candidate_2 = paste0(candidate_2_pick, " from ", candidate_2_matchup),
            tier = confidence_tier,
            why = alternate_rationale
        )

    build_sequence_view <- function(matchup_column, winner_column) {
        sequence_round_levels <- c("Round of 64", "Round of 32", "Sweet 16", "Elite 8", "Final Four", "Championship", "First Four")
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
                tier = confidence_tier
            )
    }

    safe_bracket_view <- build_sequence_view("matchup_label", "candidate_1_pick")
    alternate_bracket_view <- build_sequence_view("candidate_2_matchup", "candidate_2_pick")

    calibration_summary <- if (!is.null(backtest) && !is.null(backtest$summary) && nrow(backtest$summary) > 0) {
        backtest$summary %>%
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

    candidate_cards <- paste(
        purrr::map_chr(candidates, function(candidate) {
            paste0(
                "<div class='candidate-card'>",
                "<h3>Candidate ", candidate$candidate_id, " <span class='candidate-type'>", html_escape(candidate$type), "</span></h3>",
                "<p><strong>Champion:</strong> ", html_escape(candidate$champion), "</p>",
                "<p><strong>Final Four:</strong> ", html_escape(candidate$final_four), "</p>",
                "<p><strong>Bracket log probability:</strong> ", sprintf("%.3f", candidate$bracket_log_prob), "</p>",
                "<p><strong>Mean picked-game probability:</strong> ", sprintf("%.3f", candidate$mean_game_prob), "</p>",
                "<p><strong>Guidance:</strong> ", html_escape(candidate$diff_summary %||% ""), "</p>",
                "</div>"
            )
        }),
        collapse = "\n"
    )

    paste0(
        "<!DOCTYPE html><html><head><meta charset='utf-8'>",
        "<title>mmBayes Bracket Dashboard</title>",
        "<style>",
        "body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;background:#f7f4ee;color:#1b1b1b;margin:0;padding:24px;line-height:1.4;}",
        "h1,h2,h3{margin:0 0 8px 0;} h1{font-size:30px;} h2{margin-top:28px;font-size:20px;} h3{font-size:17px;}",
        ".lede{max-width:920px;color:#3f3f46;margin:8px 0 20px 0;}",
        ".card-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(260px,1fr));gap:16px;}",
        ".candidate-card,.panel{background:white;border:1px solid #d6d3d1;border-radius:14px;padding:16px;box-shadow:0 1px 2px rgba(0,0,0,0.03);}",
        ".warning-panel{background:#fff7e6;border:1px solid #f2c66d;color:#6b4f12;}",
        ".warning-panel strong{display:block;margin-bottom:4px;}",
        ".candidate-type{text-transform:uppercase;font-size:11px;letter-spacing:0.08em;color:#57534e;}",
        ".dashboard-table{width:100%;border-collapse:collapse;font-size:13px;background:white;}",
        ".dashboard-table th,.dashboard-table td{border:1px solid #e7e5e4;padding:8px 10px;vertical-align:top;text-align:left;}",
        ".dashboard-table th{background:#f5f5f4;}",
        ".section-grid{display:grid;grid-template-columns:1.2fr 1fr;gap:18px;align-items:start;}",
        ".tier-pill{display:inline-block;padding:2px 8px;border-radius:999px;font-size:12px;background:#f3f4f6;}",
        ".empty-state{color:#6b7280;font-style:italic;}",
        ".scatter-svg{width:100%;height:auto;background:white;border:1px solid #d6d3d1;border-radius:14px;padding:8px;}",
        "</style></head><body>",
        "<h1>mmBayes Bracket Decision Console</h1>",
        "<p class='lede'>Bracket year ", html_escape(bracket_year),
        ". Use Candidate 1 as the safest expected-value bracket and Candidate 2 as the bounded-risk alternate. The tables below surface the hardest calls first and show where the alternate bracket meaningfully diverges.</p>",
        "<div class='panel warning-panel'><strong>Important:</strong> this review is simulation-based. Play-in results can change later-round matchups, so the bracket path may not match ESPN's pre-game bracket view until tournament start day. Rerun the dashboard on or after the first games for exact placements.</div>",
        "<div class='card-grid'>", candidate_cards, "</div>",
        "<h2>Hardest Decisions</h2>",
        "<div class='section-grid'><div class='panel'>",
        render_html_table(top_decisions, max_rows = 16),
        "</div><div>",
        render_leverage_scatter_svg(decision_sheet),
        "</div></div>",
        "<h2>Candidate Differences</h2><div class='panel'>",
        render_html_table(candidate_diff, max_rows = 20),
        "</div>",
        "<h2>Bracket-Ordered Review</h2>",
        "<div class='panel'><h3>Safe Bracket #1 Sequence</h3>",
        render_html_table(safe_bracket_view),
        "</div>",
        "<div class='panel'><h3>Bracket #2 Sequence</h3>",
        render_html_table(alternate_bracket_view),
        "</div>",
        "<h2>Calibration Snapshot</h2><div class='panel'>",
        render_html_table(calibration_summary),
        "</div>",
        "<h2>How To Use This</h2><div class='panel'>",
        "<p><strong>Pick rule:</strong> start with Candidate 1, then only override with Candidate 2 on games that appear in the diff table and still feel plausible to you.</p>",
        "<p><strong>Confidence tiers:</strong> Locks are stable favorite spots. Leans still favor one side. Toss-ups are real decisions. Volatile games carry wide uncertainty even if one team is favored.</p>",
        "<p><strong>Best pool behavior:</strong> late-round volatility matters more than noisy Round of 64 upset hunting under standard scoring.</p>",
        "</div>",
        "</body></html>"
    )
}
