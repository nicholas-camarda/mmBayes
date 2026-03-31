test_that("bracket tree nodes map to evidence drawers and escape special characters", {
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
    decision_sheet <- build_decision_sheet(candidates)
    play_in_resolution <- summarize_play_in_resolution(
        current_teams = loaded$current_teams,
        actual_play_in_results = loaded$current_play_in_results
    )

    decision_sheet_with_specials <- decision_sheet
    if (nrow(decision_sheet_with_specials) > 0) {
        main_bracket_indices <- which(as.character(decision_sheet_with_specials$round) != "First Four")
        if (length(main_bracket_indices) > 0) {
            main_bracket_index <- main_bracket_indices[[1]]
            decision_sheet_with_specials$teamA[[main_bracket_index]] <- "St. John's"
            decision_sheet_with_specials$teamB[[main_bracket_index]] <- "Texas A&M"
            decision_sheet_with_specials$rationale_short[[main_bracket_index]] <- "Don't break HTML attributes."
        }
    }

    dashboard_html <- create_bracket_dashboard_html(
        bracket_year = 2026L,
        decision_sheet = decision_sheet_with_specials,
        candidates = candidates,
        current_teams = NULL,
        backtest = NULL,
        play_in_resolution = play_in_resolution
    )

    expect_match(dashboard_html, "id='btree-svg'")
    expect_match(dashboard_html, "St\\. John&#39;s")
    expect_match(dashboard_html, "Texas A&amp;M")

    tree_data <- build_bracket_tree_data(decision_sheet, candidates)
    ff_nodes <- tree_data$nodes %>%
        dplyr::filter(round == "Final Four")
    e8_nodes <- tree_data$nodes %>%
        dplyr::filter(round == "Elite 8")
    expect_equal(nrow(ff_nodes), 2L)
    expect_equal(nrow(e8_nodes), 4L)

    feeder_regions <- function(ff_matchup_number) {
        ff_slot <- ff_nodes$slot_key[ff_nodes$matchup_number == ff_matchup_number][[1]]
        feeders <- tree_data$edges %>%
            dplyr::filter(to_slot == ff_slot) %>%
            dplyr::pull(from_slot)
        e8_nodes$region[match(feeders, e8_nodes$slot_key)]
    }

    expect_setequal(feeder_regions(1L), c("South", "West"))
    expect_setequal(feeder_regions(2L), c("East", "Midwest"))

    doc <- xml2::read_html(dashboard_html)

    btree <- rvest::html_elements(doc, "svg#btree-svg")
    expect_equal(length(btree), 1L)

    nodes <- rvest::html_elements(doc, "g.btree-node")
    expect_equal(length(nodes), 63L)
    expect_true(all(nzchar(rvest::html_attr(nodes, "data-open-evidence"))))
    expect_true(all(nzchar(rvest::html_attr(nodes, "data-tip-matchup"))))

    evidence_panels <- rvest::html_elements(doc, "details.evidence-panel")
    evidence_ids <- rvest::html_attr(evidence_panels, "id")
    node_targets <- rvest::html_attr(nodes, "data-open-evidence")
    expect_true(all(node_targets %in% evidence_ids))
    expect_equal(length(unique(node_targets)), nrow(tree_data$nodes))
    expect_equal(length(intersect(unique(node_targets), evidence_ids)), nrow(tree_data$nodes))
})
