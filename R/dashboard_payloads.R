#' Drop NULL entries from a payload list
#'
#' @param payload A named list.
#'
#' @return The list with NULL members removed.
#' @keywords internal
compact_payload <- function(payload) {
    payload[!vapply(payload, is.null, logical(1))]
}

#' Select bracket-tree node columns for the dashboard payload
#'
#' @param nodes Candidate tree node table from [build_bracket_tree_data()].
#'
#' @return A data frame with frontend-facing node fields only.
#' @keywords internal
payload_tree_node_columns <- function(nodes) {
    if (!is.data.frame(nodes) || nrow(nodes) == 0) {
        return(nodes)
    }
    keep <- c(
        "slot_key", "teamA", "teamB", "teamA_seed", "teamB_seed",
        "round", "region", "matchup_number", "node_x", "node_y",
        "confidence_tier", "winner", "candidate_pick", "posterior_favorite",
        "win_prob_favorite", "upset", "route_diff", "evidence_id", "rationale_short"
    )
    keep <- intersect(keep, names(nodes))
    as.data.frame(nodes[, keep, drop = FALSE])
}

#' Serialize bracket tree data for the dashboard payload
#'
#' @param bracket_tree_data Output from [build_bracket_tree_data()].
#'
#' @return A list suitable for JSON serialization, or NULL when empty.
#' @keywords internal
serialize_bracket_tree_payload <- function(bracket_tree_data) {
    trees <- bracket_tree_data$trees %||% list()
    if (length(trees) == 0) {
        return(NULL)
    }

    serialized_trees <- lapply(trees, function(tree) {
        edges <- tree$edges %||% tibble::tibble()
        list(
            candidate_id = as.integer(tree$candidate_id),
            candidate_label = as.character(tree$candidate_label),
            nodes = payload_tree_node_columns(tree$nodes),
            edges = if (is.data.frame(edges) && nrow(edges) > 0) as.data.frame(edges) else edges
        )
    })

    list(trees = serialized_trees)
}

#' Build the versioned bracket dashboard payload
#'
#' @param bracket_year The active bracket year.
#' @param candidates A list of candidate bracket objects.
#' @param decision_sheet Decision sheet from [build_decision_sheet()].
#' @param dashboard_context Optional bundle from [build_bracket_dashboard_context()].
#' @param total_points_predictions Optional tiebreaker prediction bundle.
#' @param play_in_resolution Optional one-row play-in resolution tibble.
#'
#' @return A named list satisfying inst/schemas/bracket_dashboard_payload.schema.json.
#' @keywords internal
build_bracket_dashboard_payload <- function(bracket_year,
                                            candidates,
                                            decision_sheet,
                                            dashboard_context = NULL,
                                            total_points_predictions = NULL,
                                            play_in_resolution = NULL) {
    candidate_entries <- lapply(candidates, function(candidate) {
        compact_payload(list(
            candidate_id = as.integer(candidate$candidate_id),
            type = as.character(candidate$type),
            champion = as.character(candidate$champion),
            final_four = as.list(trimws(strsplit(
                as.character(candidate$final_four %||% ""), ","
            )[[1]])),
            bracket_log_prob = candidate$bracket_log_prob %||% NULL,
            mean_game_prob = candidate$mean_game_prob %||% NULL,
            title_path_mean_prob = candidate$title_path_mean_prob %||% NULL,
            path_support_label = candidate$path_support_label %||% NULL,
            matchups = as.data.frame(candidate$matchups)
        ))
    })

    matchup_context <- dashboard_context$matchup_context_rows
    candidate_summaries <- total_points_predictions$candidate_summaries
    bracket_tree <- serialize_bracket_tree_payload(dashboard_context$bracket_tree_data)
    divergence_map <- dashboard_context$divergence_map_rows
    watchlist <- dashboard_context$watchlist_rows

    compact_payload(list(
        dashboard_schema_version = dashboard_payload_schema_version("bracket"),
        dashboard = "bracket",
        bracket_year = as.integer(bracket_year),
        generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
        build_metadata = dashboard_context$build_metadata %||% list(),
        candidates = candidate_entries,
        decision_sheet = as.data.frame(decision_sheet),
        matchup_context = if (is.data.frame(matchup_context) && nrow(matchup_context) > 0) as.data.frame(matchup_context) else NULL,
        candidate_summaries = if (is.data.frame(candidate_summaries) && nrow(candidate_summaries) > 0) as.data.frame(candidate_summaries) else NULL,
        play_in_resolution = if (is.data.frame(play_in_resolution) && nrow(play_in_resolution) > 0) as.data.frame(play_in_resolution) else NULL,
        bracket_tree = bracket_tree,
        divergence_map = if (is.data.frame(divergence_map) && nrow(divergence_map) > 0) as.data.frame(divergence_map) else NULL,
        watchlist = if (is.data.frame(watchlist) && nrow(watchlist) > 0) as.data.frame(watchlist) else NULL
    ))
}

#' Resolve the backtest bundle used for technical dashboard payloads
#'
#' @param model_quality_context Optional resolved model-quality context.
#' @param backtest Optional direct backtest bundle from the active run.
#'
#' @return A backtest bundle or NULL when unavailable.
#' @keywords internal
resolve_technical_backtest <- function(model_quality_context = NULL, backtest = NULL) {
    quality_backtest <- NULL
    if (!is.null(model_quality_context) && is.list(model_quality_context) &&
        model_quality_has_backtest(model_quality_context$backtest %||% NULL)) {
        quality_backtest <- model_quality_context$backtest
    }
    if (!model_quality_has_backtest(quality_backtest) && model_quality_has_backtest(backtest)) {
        quality_backtest <- backtest
    }
    quality_backtest
}

#' Serialize backtest diagnostics for the technical dashboard payload
#'
#' @param quality_backtest A backtest bundle with summary and calibration tables.
#' @param quality_source_label Optional source label for provenance display.
#'
#' @return A list suitable for JSON serialization, or NULL when empty.
#' @keywords internal
serialize_backtest_payload <- function(quality_backtest, quality_source_label = NULL) {
    if (!model_quality_has_backtest(quality_backtest)) {
        return(NULL)
    }

    diagnostics <- summarize_backtest_diagnostics(quality_backtest)
    summary_row <- quality_backtest$summary %>% dplyr::slice_head(n = 1)
    calibration <- quality_backtest$calibration %||% tibble::tibble()
    round_summary <- diagnostics$round_summary %||% tibble::tibble()
    if (nrow(round_summary) == 0) {
        stored_round_summary <- quality_backtest$round_summary %||% tibble::tibble()
        if (nrow(stored_round_summary) > 0) {
            round_summary <- stored_round_summary
        }
    }
    summary_payload <- if (nrow(summary_row) > 0) {
        as.list(summary_row[1, , drop = TRUE])
    } else {
        NULL
    }

    compact_payload(list(
        source_label = quality_source_label,
        summary = summary_payload,
        calibration = if (nrow(calibration) > 0) as.data.frame(calibration) else NULL,
        round_performance = if (nrow(round_summary) > 0) as.data.frame(round_summary) else NULL,
        strengths = as.list(diagnostics$strengths %||% character()),
        weaknesses = as.list(diagnostics$weaknesses %||% character()),
        backtest_years = diagnostics$backtest_years
    ))
}

#' Serialize model overview metadata for the technical dashboard payload
#'
#' @param model_overview A normalized or bundled model overview object.
#'
#' @return A compact list for JSON serialization, or NULL when empty.
#' @keywords internal
serialize_model_overview_payload <- function(model_overview) {
    overview <- normalize_model_overview(model_overview)
    if (length(overview) == 0) {
        return(NULL)
    }

    compact_payload(list(
        engine = overview$engine %||% NULL,
        engine_label = overview$engine_label %||% overview$engine %||% NULL,
        predictor_count = overview$predictor_count %||% length(overview$predictor_columns %||% character()),
        predictor_columns = as.list(overview$predictor_columns %||% character()),
        prior_type = overview$prior_type %||% NULL,
        interaction_terms = as.list(overview$interaction_terms %||% character()),
        configured_history_window = overview$configured_history_window %||% NULL,
        effective_historical_years = overview$effective_historical_years %||% NULL
    ))
}

#' Serialize ensemble diagnostics for the technical dashboard payload
#'
#' @param model_overview A normalized or bundled model overview object.
#'
#' @return A compact list for JSON serialization, or NULL when not an ensemble.
#' @keywords internal
serialize_ensemble_diagnostics_payload <- function(model_overview) {
    overview <- normalize_model_overview(model_overview)
    if (length(overview) == 0 || !identical(overview$engine %||% NULL, "ensemble")) {
        return(NULL)
    }

    validation <- overview$ensemble_validation %||% list()
    validation_summary <- validation$summary %||% tibble::tibble()
    gate_conditions <- validation$gate_conditions %||% tibble::tibble()
    combiner <- overview$ensemble_combiner %||% list()

    compact_payload(list(
        weight_stan_glm = combiner$weight_stan_glm %||% NULL,
        weight_bart = combiner$weight_bart %||% NULL,
        intercept = combiner$intercept %||% NULL,
        gate_passed = validation$gate_passed %||% NULL,
        validation_summary = if (nrow(validation_summary) > 0) as.data.frame(validation_summary) else NULL,
        gate_conditions = if (nrow(gate_conditions) > 0) as.data.frame(gate_conditions) else NULL
    ))
}

#' Serialize technical decision tables from the decision sheet
#'
#' @param decision_sheet Decision sheet from [build_decision_sheet()].
#'
#' @return A list with ranked decision and candidate-difference tables.
#' @keywords internal
serialize_technical_decision_tables <- function(decision_sheet) {
    if (!is.data.frame(decision_sheet) || nrow(decision_sheet) == 0) {
        return(list(ranked_decisions = NULL, candidate_differences = NULL))
    }

    arrange_cols <- list()
    if ("decision_score" %in% names(decision_sheet)) {
        arrange_cols <- c(arrange_cols, list(dplyr::desc(decision_sheet$decision_score)))
    }
    if ("round" %in% names(decision_sheet)) {
        arrange_cols <- c(arrange_cols, list(decision_sheet$round))
    }
    if ("region" %in% names(decision_sheet)) {
        arrange_cols <- c(arrange_cols, list(decision_sheet$region))
    }
    if ("matchup_number" %in% names(decision_sheet)) {
        arrange_cols <- c(arrange_cols, list(decision_sheet$matchup_number))
    }
    ranked_decisions <- if (length(arrange_cols) > 0) {
        decision_sheet %>% dplyr::arrange(!!!arrange_cols)
    } else {
        decision_sheet
    }
    ranked_decisions <- ranked_decisions %>%
        dplyr::transmute(
            rank = if ("decision_rank" %in% names(.)) decision_rank else dplyr::row_number(),
            round = if ("round" %in% names(.)) as.character(round) else NA_character_,
            region = if ("region" %in% names(.)) as.character(region) else NA_character_,
            matchup = if ("matchup_label" %in% names(.)) matchup_label else NA_character_,
            favorite = if ("posterior_favorite" %in% names(.)) posterior_favorite else NA_character_,
            favorite_prob = if ("win_prob_favorite" %in% names(.)) win_prob_favorite else NA_real_,
            ci_lower = if ("ci_lower" %in% names(.)) ci_lower else NA_real_,
            ci_upper = if ("ci_upper" %in% names(.)) ci_upper else NA_real_,
            tier = if ("confidence_tier" %in% names(.)) confidence_tier else NA_character_,
            recommended_pick = if ("candidate_1_pick" %in% names(.)) candidate_1_pick else NA_character_,
            alternate_pick = if ("candidate_2_pick" %in% names(.)) candidate_2_pick else NA_character_,
            inspection_level = if ("inspection_level" %in% names(.)) {
                dplyr::coalesce(inspection_level, "none")
            } else {
                "none"
            },
            rationale = if ("rationale_short" %in% names(.)) rationale_short else NA_character_,
            candidate_diff_flag = if ("candidate_diff_flag" %in% names(.)) {
                candidate_diff_flag
            } else {
                FALSE
            },
            alternate_note = if (all(c("candidate_diff_flag", "candidate_1_pick", "candidate_2_pick") %in% names(.))) {
                dplyr::case_when(
                    candidate_diff_flag & candidate_1_pick == candidate_2_pick ~ "Same winner, different path",
                    candidate_diff_flag ~ "Different winner on the alternate path",
                    TRUE ~ "No alternate path divergence"
                )
            } else {
                NA_character_
            },
            usage_note = if (all(c("candidate_diff_flag", "confidence_tier", "round") %in% names(.))) {
                purrr::pmap_chr(
                    list(candidate_diff_flag, confidence_tier, as.character(round)),
                    build_ranked_decision_note
                )
            } else {
                NA_character_
            }
        ) %>%
        dplyr::slice_head(n = 25L)

    candidate_differences <- if ("candidate_diff_flag" %in% names(decision_sheet)) {
        decision_sheet %>%
            dplyr::filter(candidate_diff_flag) %>%
            dplyr::transmute(
                round = if ("round" %in% names(.)) as.character(round) else NA_character_,
                region = if ("region" %in% names(.)) as.character(region) else NA_character_,
                slot = if ("matchup_number" %in% names(.)) matchup_number else NA_integer_,
                matchup = if ("matchup_label" %in% names(.)) matchup_label else NA_character_,
                candidate_1 = if ("candidate_1_pick" %in% names(.)) candidate_1_pick else NA_character_,
                candidate_2 = if ("candidate_2_pick" %in% names(.)) candidate_2_pick else NA_character_,
                tier = if ("confidence_tier" %in% names(.)) confidence_tier else NA_character_,
                leverage = if ("upset_leverage" %in% names(.)) upset_leverage else NA_real_,
                why = if ("alternate_rationale" %in% names(.)) alternate_rationale else NA_character_
            )
    } else {
        tibble::tibble()
    }

    list(
        ranked_decisions = if (nrow(ranked_decisions) > 0) as.data.frame(ranked_decisions) else NULL,
        candidate_differences = if (nrow(candidate_differences) > 0) as.data.frame(candidate_differences) else NULL
    )
}

#' Build the technical action-summary strip payload
#'
#' @param decision_sheet Decision sheet from [build_decision_sheet()].
#' @param candidates Candidate bracket objects.
#'
#' @return A named list for the action summary cards.
#' @keywords internal
build_action_summary_payload <- function(decision_sheet, candidates = list()) {
    tier_counts <- if (is.data.frame(decision_sheet) && "confidence_tier" %in% names(decision_sheet)) {
        as.list(table(as.character(decision_sheet$confidence_tier)))
    } else {
        list()
    }

    top_leverage <- if (is.data.frame(decision_sheet) && nrow(decision_sheet) > 0) {
        leverage_sheet <- decision_sheet
        if ("upset_leverage" %in% names(leverage_sheet)) {
            leverage_sheet <- leverage_sheet %>% dplyr::arrange(dplyr::desc(upset_leverage))
        }
        if ("decision_score" %in% names(leverage_sheet)) {
            leverage_sheet <- leverage_sheet %>% dplyr::arrange(dplyr::desc(decision_score))
        }
        leverage_sheet %>% dplyr::slice_head(n = 1)
    } else {
        tibble::tibble()
    }
    top_leverage_label <- if (nrow(top_leverage) == 1L) {
        paste0(
            if ("underdog" %in% names(top_leverage)) top_leverage$underdog[[1]] else "n/a",
            " over ",
            if ("posterior_favorite" %in% names(top_leverage)) top_leverage$posterior_favorite[[1]] else "n/a",
            " | ",
            if ("round" %in% names(top_leverage)) as.character(top_leverage$round[[1]]) else "n/a"
        )
    } else {
        "No leverage slot available"
    }

    candidate_cards <- lapply(candidates, function(candidate) {
        compact_payload(list(
            candidate_id = as.integer(candidate$candidate_id),
            type = as.character(candidate$type),
            champion = as.character(candidate$champion),
            final_four = as.character(candidate$final_four %||% "")
        ))
    })

    list(
        candidates = candidate_cards,
        tier_counts = tier_counts,
        candidate_diff_count = if (is.data.frame(decision_sheet) && "candidate_diff_flag" %in% names(decision_sheet)) {
            sum(decision_sheet$candidate_diff_flag %in% TRUE)
        } else {
            0L
        },
        top_leverage_label = top_leverage_label
    )
}

#' Build key warning lines for the technical dashboard payload
#'
#' @param decision_sheet Decision sheet from [build_decision_sheet()].
#' @param play_in_resolution Optional play-in resolution tibble.
#' @param diagnostics_summary Output from [summarize_backtest_diagnostics()].
#'
#' @return A character vector of warning lines.
#' @keywords internal
build_technical_key_warnings <- function(decision_sheet,
                                         play_in_resolution = NULL,
                                         diagnostics_summary = NULL) {
    warnings <- character()
    top_review_row <- if (is.data.frame(decision_sheet) && nrow(decision_sheet) > 0) {
        review_sheet <- decision_sheet
        if ("decision_score" %in% names(review_sheet)) {
            review_sheet <- review_sheet %>% dplyr::arrange(dplyr::desc(decision_score))
        }
        if (all(c("round", "region", "matchup_number") %in% names(review_sheet))) {
            review_sheet <- review_sheet %>% dplyr::arrange(round, region, matchup_number)
        }
        review_sheet %>% dplyr::slice_head(n = 1)
    } else {
        tibble::tibble()
    }

    if (nrow(top_review_row) == 1L && all(c("matchup_label", "round", "posterior_favorite", "win_prob_favorite") %in% names(top_review_row))) {
        warnings <- c(
            warnings,
            sprintf(
                "Top review spot: %s in %s. The current favorite is %s at %.1f%%, so this is still one of the most consequential manual-review slots.",
                top_review_row$matchup_label[[1]],
                as.character(top_review_row$round[[1]]),
                top_review_row$posterior_favorite[[1]],
                100 * safe_numeric(top_review_row$win_prob_favorite[[1]], default = NA_real_)
            )
        )
    }
    if (!is.null(diagnostics_summary) && length(diagnostics_summary$weaknesses %||% character()) > 0L) {
        warnings <- c(warnings, diagnostics_summary$weaknesses)
    }
    if (!is.null(play_in_resolution) && nrow(play_in_resolution) > 0 &&
        isTRUE(play_in_resolution$has_unresolved_slots[[1]])) {
        warnings <- c(
            warnings,
            sprintf(
                "%s First Four slots are still unresolved, so some downstream bracket paths can still move before the field is final.",
                play_in_resolution$unresolved_slots[[1]]
            )
        )
    }
    if (length(warnings) == 0L) {
        warnings <- "No high-signal warnings were recorded for this run."
    }
    warnings
}

#' Serialize championship tiebreaker distributions for the technical payload
#'
#' @param total_points_predictions Optional bundle from [predict_candidate_total_points()].
#'
#' @return A list suitable for JSON serialization, or NULL when empty.
#' @keywords internal
serialize_championship_totals_payload <- function(total_points_predictions) {
    if (is.null(total_points_predictions)) {
        return(NULL)
    }

    summaries <- total_points_predictions$candidate_summaries %||% tibble::tibble()
    distribution <- total_points_predictions$championship_distribution %||% tibble::tibble()
    if ((!is.data.frame(summaries) || nrow(summaries) == 0) &&
        (!is.data.frame(distribution) || nrow(distribution) == 0)) {
        return(NULL)
    }

    compact_payload(list(
        candidate_summaries = if (nrow(summaries) > 0) as.data.frame(summaries) else NULL,
        championship_distribution = if (nrow(distribution) > 0) as.data.frame(distribution) else NULL,
        scale = build_championship_distribution_scale(distribution)
    ))
}

#' Serialize upset opportunity rows for the technical payload
#'
#' @param decision_sheet Decision sheet from [build_decision_sheet()].
#' @param top_n Maximum number of upset rows to include.
#'
#' @return A data frame suitable for JSON serialization, or NULL when empty.
#' @keywords internal
serialize_upset_opportunities_payload <- function(decision_sheet, top_n = 10L) {
    if (!is.data.frame(decision_sheet) || nrow(decision_sheet) == 0 ||
        !all(c("upset_leverage", "underdog", "posterior_favorite", "win_prob_underdog") %in% names(decision_sheet))) {
        return(NULL)
    }

    upset_rows <- decision_sheet %>%
        dplyr::arrange(dplyr::desc(upset_leverage), dplyr::desc(decision_score), round, matchup_number) %>%
        dplyr::slice_head(n = top_n) %>%
        dplyr::mutate(
            underdog_interval = purrr::map2(ci_lower, ci_upper, derive_underdog_interval),
            underdog_ci_lower = purrr::map_dbl(underdog_interval, "lower"),
            underdog_ci_upper = purrr::map_dbl(underdog_interval, "upper"),
            pivot_note = purrr::pmap_chr(
                list(candidate_1_pick, candidate_2_pick, underdog, as.character(round)),
                build_upset_pivot_note
            )
        ) %>%
        dplyr::transmute(
            matchup = if ("matchup_label" %in% names(.)) matchup_label else NA_character_,
            round = if ("round" %in% names(.)) as.character(round) else NA_character_,
            region = if ("region" %in% names(.)) as.character(region) else NA_character_,
            tier = if ("confidence_tier" %in% names(.)) confidence_tier else NA_character_,
            underdog = underdog,
            favorite = posterior_favorite,
            underdog_prob = win_prob_underdog,
            underdog_ci_lower = underdog_ci_lower,
            underdog_ci_upper = underdog_ci_upper,
            leverage = upset_leverage,
            candidate_1_pick = if ("candidate_1_pick" %in% names(.)) candidate_1_pick else NA_character_,
            candidate_2_pick = if ("candidate_2_pick" %in% names(.)) candidate_2_pick else NA_character_,
            candidate_diff_flag = if ("candidate_diff_flag" %in% names(.)) candidate_diff_flag else FALSE,
            pivot_note = pivot_note
        )

    if (nrow(upset_rows) == 0) {
        return(NULL)
    }

    as.data.frame(upset_rows)
}

#' Serialize candidate profile and fragility rows for the technical payload
#'
#' @param candidates Candidate bracket objects from the active run.
#'
#' @return A list of candidate profile objects, or NULL when empty.
#' @keywords internal
serialize_candidate_profiles_payload <- function(candidates = list()) {
    if (length(candidates) == 0) {
        return(NULL)
    }

    profiles <- lapply(candidates, function(candidate) {
        matchups <- candidate$matchups %||% tibble::tibble()
        fragile_picks <- tibble::tibble()
        if (is.data.frame(matchups) && nrow(matchups) > 0 &&
            all(c("winner", "teamA", "teamB", "win_prob_A", "decision_score") %in% names(matchups))) {
            fragile_picks <- matchups %>%
                dplyr::mutate(
                    chosen_prob = ifelse(winner == teamA, win_prob_A, 1 - win_prob_A),
                    chosen_ci_lower = ifelse(
                        winner == teamA,
                        if ("ci_lower" %in% names(.)) ci_lower else NA_real_,
                        if ("ci_upper" %in% names(.)) 1 - ci_upper else NA_real_
                    ),
                    chosen_ci_upper = ifelse(
                        winner == teamA,
                        if ("ci_upper" %in% names(.)) ci_upper else NA_real_,
                        if ("ci_lower" %in% names(.)) 1 - ci_lower else NA_real_
                    ),
                    matchup_label = sprintf("%s vs %s", teamA, teamB)
                ) %>%
                {
                    arranged <- .
                    if ("interval_width" %in% names(arranged)) {
                        arranged <- arranged %>%
                            dplyr::arrange(
                                dplyr::desc(decision_score),
                                dplyr::desc(interval_width),
                                round,
                                matchup_number
                            )
                    } else {
                        arranged <- arranged %>%
                            dplyr::arrange(dplyr::desc(decision_score), round, matchup_number)
                    }
                    arranged
                } %>%
                dplyr::slice_head(n = 8L) %>%
                dplyr::transmute(
                    round = if ("round" %in% names(.)) as.character(round) else NA_character_,
                    matchup_label = matchup_label,
                    winner = winner,
                    chosen_prob = chosen_prob,
                    chosen_ci_lower = chosen_ci_lower,
                    chosen_ci_upper = chosen_ci_upper,
                    confidence_tier = if ("confidence_tier" %in% names(.)) confidence_tier else NA_character_,
                    decision_score = decision_score
                )
        }

        compact_payload(list(
            candidate_id = as.integer(candidate$candidate_id),
            type = as.character(candidate$type %||% ""),
            champion = as.character(candidate$champion %||% ""),
            final_four = as.character(candidate$final_four %||% ""),
            bracket_log_prob = candidate$bracket_log_prob %||% NULL,
            mean_game_prob = candidate$mean_game_prob %||% NULL,
            diff_summary = as.character(candidate$diff_summary %||% ""),
            fragile_picks = if (nrow(fragile_picks) > 0) as.data.frame(fragile_picks) else NULL
        ))
    })

    compact_payload(profiles)
}

#' Serialize live tournament performance for the technical payload
#'
#' @param live_performance Live performance bundle from
#'   [summarize_live_tournament_performance()].
#' @param model_label Optional model label for the monitoring panel.
#'
#' @return A named list suitable for JSON serialization, or NULL when empty.
#' @keywords internal
serialize_live_performance_payload <- function(live_performance, model_label = NULL) {
    if (is.null(live_performance) || length(live_performance) == 0) {
        return(NULL)
    }

    summary_tbl <- live_performance$summary %||% tibble::tibble()
    main_bracket_tbl <- live_performance$main_bracket_summary %||% tibble::tibble()
    round_tbl <- live_performance$round_summary %||% tibble::tibble()
    recent_games <- live_performance$games %||% tibble::tibble()

    compact_payload(list(
        model_label = model_label,
        status = live_performance$status %||% NULL,
        monitoring_note = live_performance$monitoring_note %||% NULL,
        interpretive_status = live_performance$interpretive_status %||% NULL,
        summary = if (nrow(summary_tbl) > 0) as.list(summary_tbl[1, , drop = FALSE]) else NULL,
        main_bracket_summary = if (nrow(main_bracket_tbl) > 0) {
            as.list(main_bracket_tbl[1, , drop = FALSE])
        } else {
            NULL
        },
        round_summary = if (nrow(round_tbl) > 0) as.data.frame(round_tbl) else NULL,
        recent_games = if (nrow(recent_games) > 0) as.data.frame(recent_games) else NULL,
        recent_games_title = live_performance$recent_games_title %||% NULL,
        recent_games_note = live_performance$recent_games_note %||% NULL,
        games_played = live_performance$games_played %||% NULL,
        main_bracket_games_played = live_performance$main_bracket_games_played %||% NULL
    ))
}

#' Build the versioned technical dashboard payload
#'
#' @param bracket_year The active bracket year.
#' @param decision_sheet Optional decision sheet used for summary counts.
#' @param candidates Optional list of candidate bracket objects.
#' @param model_quality_context Optional resolved model-quality context.
#' @param build_metadata Optional build metadata list.
#' @param model_overview Optional model overview bundle.
#' @param total_points_predictions Optional championship totals bundle.
#' @param play_in_resolution Optional play-in resolution tibble.
#' @param backtest Optional direct backtest bundle from the active run.
#' @param live_performance Optional live tournament performance bundle.
#'
#' @return A named list satisfying inst/schemas/technical_dashboard_payload.schema.json.
#' @keywords internal
build_technical_dashboard_payload <- function(bracket_year,
                                              decision_sheet = NULL,
                                              candidates = list(),
                                              model_quality_context = NULL,
                                              build_metadata = NULL,
                                              model_overview = NULL,
                                              total_points_predictions = NULL,
                                              play_in_resolution = NULL,
                                              backtest = NULL,
                                              live_performance = NULL) {
    decision_summary <- NULL
    if (is.data.frame(decision_sheet) && nrow(decision_sheet) > 0) {
        decision_summary <- list(
            n_decisions = nrow(decision_sheet),
            n_divergent = if ("candidate_diff_flag" %in% names(decision_sheet)) {
                sum(decision_sheet$candidate_diff_flag %in% TRUE)
            } else {
                0L
            },
            confidence_tiers = if ("confidence_tier" %in% names(decision_sheet)) {
                as.list(table(as.character(decision_sheet$confidence_tier)))
            } else {
                list()
            }
        )
    }

    quality_source_label <- model_quality_context$source_label %||% "Current run backtest"
    quality_backtest <- resolve_technical_backtest(model_quality_context, backtest)
    diagnostics_summary <- summarize_backtest_diagnostics(quality_backtest)
    decision_tables <- serialize_technical_decision_tables(decision_sheet)
    overview_bundle <- normalize_model_overview(model_overview)
    live_model_label <- overview_bundle$engine_label %||% overview_bundle$engine %||% NULL

    model_quality <- NULL
    if (!is.null(model_quality_context)) {
        model_quality <- compact_payload(list(
            source_label = model_quality_context$source_label %||% NULL,
            used_cached_quality = isTRUE(
                model_quality_context$used_cached_quality %||% model_quality_context$used_fallback %||% FALSE
            )
        ))
    }

    compact_payload(list(
        dashboard_schema_version = dashboard_payload_schema_version("technical"),
        dashboard = "technical",
        bracket_year = as.integer(bracket_year),
        generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
        build_metadata = build_metadata %||% list(),
        model_quality = model_quality,
        decision_summary = decision_summary,
        candidate_count = length(candidates),
        action_summary = if (!is.null(decision_sheet)) {
            build_action_summary_payload(decision_sheet, candidates)
        } else {
            NULL
        },
        key_warnings = as.list(build_technical_key_warnings(
            decision_sheet = decision_sheet,
            play_in_resolution = play_in_resolution,
            diagnostics_summary = diagnostics_summary
        )),
        ranked_decisions = decision_tables$ranked_decisions,
        candidate_differences = decision_tables$candidate_differences,
        upset_opportunities = serialize_upset_opportunities_payload(decision_sheet),
        candidate_profiles = serialize_candidate_profiles_payload(candidates),
        live_performance = serialize_live_performance_payload(
            live_performance,
            model_label = live_model_label
        ),
        backtest = serialize_backtest_payload(quality_backtest, quality_source_label),
        model_overview = serialize_model_overview_payload(model_overview),
        ensemble_diagnostics = serialize_ensemble_diagnostics_payload(model_overview),
        championship_totals = serialize_championship_totals_payload(total_points_predictions),
        play_in_resolution = if (is.data.frame(play_in_resolution) && nrow(play_in_resolution) > 0) {
            as.data.frame(play_in_resolution)
        } else {
            NULL
        }
    ))
}

#' Serialize a dashboard payload list to JSON
#'
#' @param payload A payload list from the dashboard payload builders.
#'
#' @return A length-one JSON character string with rows-oriented data frames.
#' @keywords internal
dashboard_payload_json <- function(payload) {
    as.character(jsonlite::toJSON(
        payload,
        dataframe = "rows",
        auto_unbox = TRUE,
        na = "null",
        digits = 6,
        null = "null"
    ))
}

#' Validate and write dashboard payload artifacts
#'
#' Writes the bracket and technical payload JSON files plus a
#' dashboard_payloads.js window-global shim used by the static frontend
#' over file:// where fetch() is unavailable.
#'
#' @param bracket_payload Payload from [build_bracket_dashboard_payload()].
#' @param technical_payload Payload from [build_technical_dashboard_payload()].
#' @param output_dir Directory receiving the payload artifacts.
#'
#' @return A list with bracket, technical, and js artifact paths.
#' @keywords internal
write_dashboard_payloads <- function(bracket_payload, technical_payload, output_dir) {
    validate_dashboard_payload(bracket_payload, "bracket")
    validate_dashboard_payload(technical_payload, "technical")
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

    bracket_json <- dashboard_payload_json(bracket_payload)
    technical_json <- dashboard_payload_json(technical_payload)

    bracket_path <- file.path(output_dir, "bracket_dashboard_payload.json")
    technical_path <- file.path(output_dir, "technical_dashboard_payload.json")
    js_path <- file.path(output_dir, "dashboard_payloads.js")

    writeLines(bracket_json, bracket_path, useBytes = TRUE)
    writeLines(technical_json, technical_path, useBytes = TRUE)
    writeLines(sprintf(
        "window.__MMBAYES_PAYLOADS__ = {\"bracket\": %s, \"technical\": %s};",
        bracket_json, technical_json
    ), js_path, useBytes = TRUE)

    list(bracket = bracket_path, technical = technical_path, js = js_path)
}
