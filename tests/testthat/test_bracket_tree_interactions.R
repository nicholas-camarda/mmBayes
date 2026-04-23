#' Override a single candidate matchup row for bracket-tree fixtures
#'
#' @param matchups Candidate matchup table.
#' @param slot_key Slot identifier to rewrite.
#' @param values Named list of replacement field values.
#'
#' @return The matchup table with one row updated in place.
#' @keywords internal
override_candidate_matchup <- function(matchups, slot_key, values) {
    row_index <- which(as.character(matchups$slot_key) == slot_key)
    if (length(row_index) != 1L) {
        stop(sprintf("Expected exactly one matchup row for slot %s", slot_key))
    }

    for (field_name in names(values)) {
        matchups[[field_name]][[row_index]] <- values[[field_name]]
    }

    win_prob_a <- safe_numeric(matchups$win_prob_A[[row_index]], default = NA_real_)
    if (is.na(win_prob_a)) {
        win_prob_a <- 0.5
        matchups$win_prob_A[[row_index]] <- win_prob_a
    }

    team_a <- as.character(matchups$teamA[[row_index]])
    team_b <- as.character(matchups$teamB[[row_index]])
    team_a_seed <- matchups$teamA_seed[[row_index]]
    team_b_seed <- matchups$teamB_seed[[row_index]]

    if (win_prob_a >= 0.5) {
        matchups$posterior_favorite[[row_index]] <- team_a
        matchups$favorite_seed[[row_index]] <- team_a_seed
        matchups$underdog[[row_index]] <- team_b
        matchups$underdog_seed[[row_index]] <- team_b_seed
        matchups$win_prob_favorite[[row_index]] <- win_prob_a
        matchups$win_prob_underdog[[row_index]] <- 1 - win_prob_a
    } else {
        matchups$posterior_favorite[[row_index]] <- team_b
        matchups$favorite_seed[[row_index]] <- team_b_seed
        matchups$underdog[[row_index]] <- team_a
        matchups$underdog_seed[[row_index]] <- team_a_seed
        matchups$win_prob_favorite[[row_index]] <- 1 - win_prob_a
        matchups$win_prob_underdog[[row_index]] <- win_prob_a
    }

    matchups$upset[[row_index]] <- identical(
        as.character(matchups$winner[[row_index]]),
        as.character(matchups$underdog[[row_index]])
    )

    matchups
}

#' Prepare deterministic bracket-tree test candidates
#'
#' @param candidates Candidate list returned by [generate_bracket_candidates()].
#'
#' @return The candidate list with deterministic East-region divergence applied.
#' @keywords internal
prepare_bracket_tree_test_candidates <- function(candidates) {
    if (length(candidates) < 2L) {
        stop("Expected two candidates for bracket tree tests")
    }

    for (candidate_index in seq_len(length(candidates))) {
        matchups <- candidates[[candidate_index]]$matchups
        first_main_game <- which(as.character(matchups$round) != "First Four")[[1]]
        matchups <- override_candidate_matchup(matchups, as.character(matchups$slot_key[[first_main_game]]), list(
            teamA = "St. John's",
            teamB = "Texas A&M",
            teamA_seed = 5L,
            teamB_seed = 12L,
            winner = "St. John's",
            win_prob_A = 0.62,
            rationale_short = "Don't break HTML attributes."
        ))
        candidates[[candidate_index]]$matchups <- matchups
    }

    candidates[[1]]$matchups <- override_candidate_matchup(candidates[[1]]$matchups, "East|Round of 64|5", list(
        teamA = "Louisville",
        teamB = "South Florida",
        teamA_seed = 6L,
        teamB_seed = 11L,
        winner = "Louisville",
        win_prob_A = 0.757,
        confidence_tier = "Lean",
        rationale_short = "Lean favorite. The safer side is clear, but it is not a lock."
    ))
    candidates[[2]]$matchups <- override_candidate_matchup(candidates[[2]]$matchups, "East|Round of 64|5", list(
        teamA = "Louisville",
        teamB = "South Florida",
        teamA_seed = 6L,
        teamB_seed = 11L,
        winner = "South Florida",
        win_prob_A = 0.757,
        confidence_tier = "Lean",
        rationale_short = "Lean favorite. The safer side is clear, but it is not a lock."
    ))

    for (candidate_index in seq_len(2L)) {
        candidates[[candidate_index]]$matchups <- override_candidate_matchup(candidates[[candidate_index]]$matchups, "East|Round of 64|6", list(
            teamA = "Michigan State",
            teamB = "North Dakota State",
            teamA_seed = 3L,
            teamB_seed = 14L,
            winner = "Michigan State",
            win_prob_A = 0.945,
            confidence_tier = "Lock"
        ))
        candidates[[candidate_index]]$matchups <- override_candidate_matchup(candidates[[candidate_index]]$matchups, "East|Round of 64|7", list(
            teamA = "UCLA",
            teamB = "UCF",
            teamA_seed = 7L,
            teamB_seed = 10L,
            winner = "UCLA",
            win_prob_A = 0.88,
            confidence_tier = "Lock"
        ))
        candidates[[candidate_index]]$matchups <- override_candidate_matchup(candidates[[candidate_index]]$matchups, "East|Round of 64|8", list(
            teamA = "Connecticut",
            teamB = "Furman",
            teamA_seed = 2L,
            teamB_seed = 15L,
            winner = "Connecticut",
            win_prob_A = 0.98,
            confidence_tier = "Lock"
        ))
        candidates[[candidate_index]]$matchups <- override_candidate_matchup(candidates[[candidate_index]]$matchups, "East|Round of 32|4", list(
            teamA = "UCLA",
            teamB = "Connecticut",
            teamA_seed = 7L,
            teamB_seed = 2L,
            winner = "Connecticut",
            win_prob_A = 0.29,
            confidence_tier = "Lean"
        ))
    }

    candidates[[1]]$matchups <- override_candidate_matchup(candidates[[1]]$matchups, "East|Round of 32|3", list(
        teamA = "Louisville",
        teamB = "Michigan State",
        teamA_seed = 6L,
        teamB_seed = 3L,
        winner = "Louisville",
        win_prob_A = 0.56,
        confidence_tier = "Toss-up",
        rationale_short = "Near coin flip. Either side is defensible in pool play."
    ))
    candidates[[2]]$matchups <- override_candidate_matchup(candidates[[2]]$matchups, "East|Round of 32|3", list(
        teamA = "South Florida",
        teamB = "Michigan State",
        teamA_seed = 11L,
        teamB_seed = 3L,
        winner = "Michigan State",
        win_prob_A = 0.44,
        confidence_tier = "Toss-up",
        rationale_short = "Near coin flip. Either side is defensible in pool play."
    ))

    candidates[[1]]$matchups <- override_candidate_matchup(candidates[[1]]$matchups, "East|Sweet 16|2", list(
        teamA = "Louisville",
        teamB = "Connecticut",
        teamA_seed = 6L,
        teamB_seed = 2L,
        winner = "Connecticut",
        win_prob_A = 0.38,
        confidence_tier = "Toss-up"
    ))
    candidates[[2]]$matchups <- override_candidate_matchup(candidates[[2]]$matchups, "East|Sweet 16|2", list(
        teamA = "Michigan State",
        teamB = "Connecticut",
        teamA_seed = 3L,
        teamB_seed = 2L,
        winner = "Connecticut",
        win_prob_A = 0.38,
        confidence_tier = "Toss-up"
    ))

    candidates
}

#' Build the shared bracket-tree test context
#'
#' @return A list containing prepared candidates, a decision sheet, and
#'   play-in resolution data for bracket-tree tests.
#' @keywords internal
build_bracket_tree_test_context <- function() {
    team_file <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")
    fixture_paths <- write_fixture_data_files(team_file, results_file)

    config <- default_project_config()
    config$data$team_features_path <- fixture_paths$team_path
    config$data$game_results_path <- fixture_paths$results_path
    config$model$history_window <- 3L

    old_options <- options(
        mmBayes.stan_chains = 1L,
        mmBayes.stan_iter = 60L,
        mmBayes.stan_refresh = 0L
    )
    on.exit(options(old_options), add = TRUE)

    loaded <- load_tournament_data(config)
    model_results <- fit_tournament_model(
        loaded$historical_matchups,
        config$model$required_predictors,
        random_seed = 123,
        include_diagnostics = FALSE
    )

    candidates <- generate_bracket_candidates(
        all_teams = loaded$current_teams,
        model_results = model_results,
        draws = 25L,
        n_candidates = 2L,
        n_simulations = 8L,
        random_seed = 123
    )
    candidates <- prepare_bracket_tree_test_candidates(candidates)

    list(
        candidates = candidates,
        decision_sheet = build_decision_sheet(candidates),
        play_in_resolution = summarize_play_in_resolution(
            current_teams = loaded$current_teams,
            actual_play_in_results = loaded$current_play_in_results
        )
    )
}

test_that("bracket tree renders candidate-specific trees and maps evidence drawers", {
    context <- build_bracket_tree_test_context()

    dashboard_html <- create_bracket_dashboard_html(
        bracket_year = 2026L,
        decision_sheet = context$decision_sheet,
        candidates = context$candidates,
        current_teams = NULL,
        backtest = NULL,
        play_in_resolution = context$play_in_resolution
    )

    expect_match(dashboard_html, "id='btree-svg-1'")
    expect_match(dashboard_html, "id='btree-svg-2'")
    expect_match(dashboard_html, "<meta name='viewport' content='width=device-width, initial-scale=1'>")
    expect_no_match(dashboard_html, "style='min-width:[0-9]+px;display:block;'")
    expect_match(dashboard_html, "St\\. John&#39;s")
    expect_match(dashboard_html, "Texas A&amp;M")

    tree_data <- build_bracket_tree_data(context$candidates)
    expect_length(tree_data$trees, 2L)

    ff_nodes <- tree_data$trees[[1]]$nodes %>%
        dplyr::filter(round == "Final Four")
    e8_nodes <- tree_data$trees[[1]]$nodes %>%
        dplyr::filter(round == "Elite 8")
    expect_equal(nrow(ff_nodes), 2L)
    expect_equal(nrow(e8_nodes), 4L)

    # Resolve which region nodes feed each Final Four slot in the rendered tree.
    feeder_regions <- function(tree, ff_matchup_number) {
        ff_slot <- tree$nodes$slot_key[tree$nodes$round == "Final Four" & tree$nodes$matchup_number == ff_matchup_number][[1]]
        feeders <- tree$edges %>%
            dplyr::filter(to_slot == ff_slot) %>%
            dplyr::pull(from_slot)
        tree$nodes$region[match(feeders, tree$nodes$slot_key)]
    }

    expect_setequal(feeder_regions(tree_data$trees[[1]], 1L), c("South", "West"))
    expect_setequal(feeder_regions(tree_data$trees[[1]], 2L), c("East", "Midwest"))

    doc <- xml2::read_html(dashboard_html)

    btree <- rvest::html_elements(doc, "svg.btree-svg")
    expect_equal(length(btree), 2L)
    expect_true(all(is.na(rvest::html_attr(btree, "style")) | !grepl("min-width", rvest::html_attr(btree, "style"), fixed = TRUE)))

    expect_equal(length(rvest::html_elements(doc, ".btree-toggle[data-btree-target='candidate-1']")), 1L)
    expect_equal(length(rvest::html_elements(doc, ".btree-toggle[data-btree-target='candidate-2']")), 1L)
    expect_equal(length(rvest::html_elements(doc, ".btree-toggle[data-btree-target='both']")), 0L)

    evidence_panels <- rvest::html_elements(doc, "details.evidence-panel")
    evidence_ids <- rvest::html_attr(evidence_panels, "id")

    for (panel_key in c("candidate-1", "candidate-2")) {
        panel_nodes <- rvest::html_elements(doc, sprintf("[data-btree-panel='%s'] g.btree-node", panel_key))
        expect_equal(length(panel_nodes), 63L)
        expect_true(all(nzchar(rvest::html_attr(panel_nodes, "data-open-evidence"))))
        expect_true(all(nzchar(rvest::html_attr(panel_nodes, "data-tip-matchup"))))
        expect_true(all(rvest::html_attr(panel_nodes, "data-open-evidence") %in% evidence_ids))
    }
})

test_that("candidate-specific trees preserve the East divergence path correctly", {
    context <- build_bracket_tree_test_context()
    tree_data <- build_bracket_tree_data(context$candidates)

    candidate_one_nodes <- tree_data$trees[[1]]$nodes
    candidate_two_nodes <- tree_data$trees[[2]]$nodes

    c1_round32 <- candidate_one_nodes %>%
        dplyr::filter(slot_key == "East|Round of 32|3")
    c2_round32 <- candidate_two_nodes %>%
        dplyr::filter(slot_key == "East|Round of 32|3")
    c1_sweet16 <- candidate_one_nodes %>%
        dplyr::filter(slot_key == "East|Sweet 16|2")
    c2_sweet16 <- candidate_two_nodes %>%
        dplyr::filter(slot_key == "East|Sweet 16|2")

    expect_equal(c1_round32$teamA[[1]], "Louisville")
    expect_equal(c1_round32$teamB[[1]], "Michigan State")
    expect_equal(c1_round32$winner[[1]], "Louisville")
    expect_equal(c1_sweet16$teamA[[1]], "Louisville")
    expect_equal(c1_sweet16$teamB[[1]], "Connecticut")

    expect_equal(c2_round32$teamA[[1]], "South Florida")
    expect_equal(c2_round32$teamB[[1]], "Michigan State")
    expect_equal(c2_round32$winner[[1]], "Michigan State")
    expect_equal(c2_sweet16$teamA[[1]], "Michigan State")
    expect_equal(c2_sweet16$teamB[[1]], "Connecticut")
    expect_false("Louisville" %in% c(c2_round32$teamA[[1]], c2_round32$teamB[[1]], c2_sweet16$teamA[[1]], c2_sweet16$teamB[[1]]))
})
