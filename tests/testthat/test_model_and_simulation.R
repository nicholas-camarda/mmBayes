test_that("fit_tournament_model returns a matchup-model object", {
    team_file <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")
    fixture_paths <- write_fixture_data_files(team_file, results_file)

    config <- default_project_config()
    config$data$team_features_path <- fixture_paths$team_path
    config$data$game_results_path <- fixture_paths$results_path
    config$model$history_window <- 3L

    loaded <- load_tournament_data(config)

    old_options <- options(
        mmBayes.stan_chains = 1L,
        mmBayes.stan_iter = 60L,
        mmBayes.stan_refresh = 0L
    )
    on.exit(options(old_options), add = TRUE)

    model_results <- fit_tournament_model(
        loaded$historical_matchups,
        config$model$required_predictors,
        random_seed = 123
    )

    expect_true(is.list(model_results))
    expect_true(all(c("engine", "model", "diagnostics", "scaling_reference", "predictor_columns") %in% names(model_results)))
    expect_equal(model_results$engine, "stan_glm")
})

test_that("fit_total_points_model returns a score-total model object", {
    team_file <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")
    fixture_paths <- write_fixture_data_files(team_file, results_file)

    config <- default_project_config()
    config$data$team_features_path <- fixture_paths$team_path
    config$data$game_results_path <- fixture_paths$results_path
    config$model$history_window <- 3L

    loaded <- load_tournament_data(config)

    old_options <- options(
        mmBayes.stan_chains = 1L,
        mmBayes.stan_iter = 60L,
        mmBayes.stan_refresh = 0L
    )
    on.exit(options(old_options), add = TRUE)

    total_model <- fit_total_points_model(
        historical_total_points = build_total_points_training_rows(loaded$historical_actual_results),
        random_seed = 123
    )

    expect_true(is.list(total_model))
    expect_equal(total_model$outcome, "total_points")
    expect_true(all(c("engine", "model", "scaling_reference", "predictor_columns") %in% names(total_model)))
})

test_that("fit_tournament_model reuses a cached fit when the inputs are unchanged", {
    team_file <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")
    fixture_paths <- write_fixture_data_files(team_file, results_file)

    config <- default_project_config()
    config$data$team_features_path <- fixture_paths$team_path
    config$data$game_results_path <- fixture_paths$results_path
    config$model$history_window <- 3L

    loaded <- load_tournament_data(config)
    cache_dir <- tempfile(pattern = "mmBayes-model-cache-")
    dir.create(cache_dir, recursive = TRUE)

    old_options <- options(
        mmBayes.stan_chains = 1L,
        mmBayes.stan_iter = 60L,
        mmBayes.stan_refresh = 0L
    )
    on.exit(options(old_options), add = TRUE)

    first_fit <- fit_tournament_model(
        loaded$historical_matchups,
        config$model$required_predictors,
        random_seed = 123,
        include_diagnostics = FALSE,
        cache_dir = cache_dir
    )

    expect_true(file.exists(first_fit$cache_path))

    cached_result <- readRDS(first_fit$cache_path)
    cached_result$cache_marker <- "cached"
    saveRDS(cached_result, first_fit$cache_path)

    second_fit <- fit_tournament_model(
        loaded$historical_matchups,
        config$model$required_predictors,
        random_seed = 123,
        include_diagnostics = FALSE,
        cache_dir = cache_dir
    )

    expect_equal(second_fit$cache_marker, "cached")
    expect_equal(second_fit$cache_path, first_fit$cache_path)
    expect_equal(second_fit$predictor_columns, first_fit$predictor_columns)
})

test_that("matchup predictions are approximately antisymmetric", {
    team_file <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")
    fixture_paths <- write_fixture_data_files(team_file, results_file)

    config <- default_project_config()
    config$data$team_features_path <- fixture_paths$team_path
    config$data$game_results_path <- fixture_paths$results_path
    config$model$history_window <- 3L
    config$model$n_draws <- 25L

    loaded <- load_tournament_data(config)

    old_options <- options(
        mmBayes.stan_chains = 1L,
        mmBayes.stan_iter = 60L,
        mmBayes.stan_refresh = 0L
    )
    on.exit(options(old_options), add = TRUE)

    model_results <- fit_tournament_model(
        loaded$historical_matchups,
        config$model$required_predictors,
        random_seed = 123
    )

    east_teams <- loaded$current_teams %>%
        dplyr::filter(Region == "East") %>%
        dplyr::arrange(Seed, Team)
    team_a <- east_teams[1, , drop = FALSE]
    team_b <- east_teams[2, , drop = FALSE]

    p_ab <- mean(predict_matchup_probabilities(team_a, team_b, "Round of 64", model_results, draws = 25))
    p_ba <- mean(predict_matchup_probabilities(team_b, team_a, "Round of 64", model_results, draws = 25))

    expect_lt(abs((p_ab + p_ba) - 1), 0.5)
})

test_that("simulate_full_bracket returns region and final four structure", {
    team_file <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")
    fixture_paths <- write_fixture_data_files(team_file, results_file)

    config <- default_project_config()
    config$data$team_features_path <- fixture_paths$team_path
    config$data$game_results_path <- fixture_paths$results_path
    config$model$history_window <- 3L
    config$model$n_draws <- 25L

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
        random_seed = 123
    )
    simulation <- simulate_full_bracket(loaded$current_teams, model_results, draws = 25)
    flattened <- flatten_matchup_results(simulation)

    expect_true(all(c("region_results", "final_four") %in% names(simulation)))
    expect_equal(sort(names(simulation$region_results)), sort(c("East", "West", "South", "Midwest")))
    expect_true(any(flattened$round == "First Four"))
    expect_true(all(c("semifinalists", "semifinals", "championship", "champion") %in% names(simulation$final_four)))
})

test_that("candidate generation adds decision metadata and an alternate bracket", {
    team_file <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")
    fixture_paths <- write_fixture_data_files(team_file, results_file)

    config <- default_project_config()
    config$data$team_features_path <- fixture_paths$team_path
    config$data$game_results_path <- fixture_paths$results_path
    config$model$history_window <- 3L
    config$model$n_draws <- 25L

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
    total_model <- fit_total_points_model(
        historical_total_points = build_total_points_training_rows(loaded$historical_actual_results),
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
    total_predictions <- predict_candidate_total_points(
        candidates = candidates,
        current_teams = loaded$current_teams,
        total_points_model = total_model,
        draws = 25L
    )
    play_in_resolution <- summarize_play_in_resolution(
        current_teams = loaded$current_teams,
        actual_play_in_results = loaded$current_play_in_results
    )

    expect_true(length(candidates) >= 1)
    expect_true(all(c("decision_rank", "confidence_tier", "upset_leverage", "inspection_flag", "inspection_level") %in% names(candidates[[1]]$matchups)))
    expect_true(all(c("candidate_1_pick", "candidate_2_pick", "candidate_diff_flag", "decision_score", "inspection_flag", "inspection_level") %in% names(decision_sheet)))
    expect_equal(
        as.character(unique(decision_sheet$region[!is.na(decision_sheet$region)])),
        c("East", "South", "Midwest", "West")
    )
    expect_true(all(decision_sheet$confidence_tier %in% c("Lock", "Lean", "Toss-up", "Volatile")))
    expect_true(nrow(total_predictions$candidate_summaries) >= 1)
    expect_true(nrow(total_predictions$championship_distribution) >= 1)
    expect_true(nrow(total_predictions$matchup_summaries) >= nrow(candidates[[1]]$matchups))
    expect_equal(
        total_predictions$candidate_summaries$recommended_tiebreaker_points,
        as.integer(round(total_predictions$candidate_summaries$predicted_total_median))
    )
    expect_equal(
        total_predictions$championship_distribution %>%
            dplyr::group_by(candidate_id) %>%
            dplyr::summarise(probability = sum(probability), .groups = "drop") %>%
            dplyr::pull(probability),
        rep(1, nrow(total_predictions$candidate_summaries)),
        tolerance = 1e-8
    )

    output_dir <- tempfile(pattern = "mmBayes-decision-output-")
    dir.create(output_dir, recursive = TRUE)
    quality_signature <- build_model_quality_signature(list(
        bracket_year = 2026L,
        draws_budget = 25L,
        model = model_results,
        data = list(
            historical_teams = loaded$historical_teams,
            historical_actual_results = loaded$historical_actual_results
        )
    ))
    model_overview <- list(
        matchup = summarize_model_overview(model_results, draws = 25L),
        totals = summarize_model_overview(total_model, draws = 25L)
    )
    fake_quality_backtest <- list(
        summary = tibble::tibble(
            mean_log_loss = 0.401,
            mean_brier = 0.188,
            mean_accuracy = 0.713,
            mean_bracket_score = 85.4,
            mean_correct_picks = 42.7
        ),
        calibration = tibble::tibble(
            mean_predicted = c(0.35, 0.55, 0.75),
            empirical_rate = c(0.30, 0.58, 0.78),
            n_games = c(12L, 16L, 10L)
        )
    )
    quality_artifact <- save_model_quality_artifact(
        fake_quality_backtest,
        output_dir = output_dir,
        quality_signature = quality_signature
    )
    expect_true(file.exists(quality_artifact$latest_path))
    expect_identical(quality_artifact$archive_path, quality_artifact$latest_path)
    expect_equal(length(list.files(file.path(output_dir, "model_quality"), pattern = "^model_quality_.*_pid.*\\.rds$")), 0L)
    resolved_quality <- resolve_model_quality_context(
        backtest = NULL,
        output_dir = output_dir,
        quality_signature = quality_signature
    )
    expect_true(isTRUE(resolved_quality$used_cached_quality))
    expect_match(resolved_quality$source_label, "Cached identical validation snapshot")
    resolved_quality$backtest$yearly_metrics <- tibble::tibble(year = c(2018L, 2019L, 2020L))

    live_team_data <- make_fixture_team_features(current_year = 2025, history_years = 2022:2024)
    live_current_results <- make_fixture_current_year_completed_results(live_team_data, current_year = 2025)
    live_performance <- summarize_live_tournament_performance(
        data = list(
            bracket_year = 2025L,
            current_completed_results = live_current_results,
            current_teams = live_team_data %>% dplyr::filter(Year == "2025")
        ),
        model_results = model_results,
        draws = 25L
    )

    decision_outputs <- save_decision_outputs(
        bracket_year = 2026L,
        candidates = candidates,
        output_dir = output_dir,
        current_teams = loaded$current_teams,
        backtest = NULL,
        model_overview = model_overview,
        quality_signature = quality_signature,
        play_in_resolution = play_in_resolution,
        total_points_predictions = total_predictions,
        live_performance = live_performance
    )
    saved_matchup_totals <- utils::read.csv(decision_outputs$matchup_total_points)

    expect_true(file.exists(decision_outputs$championship_tiebreaker_summary))
    expect_true(file.exists(decision_outputs$championship_tiebreaker_distribution))
    expect_true(file.exists(decision_outputs$matchup_total_points))
    expect_true(file.exists(decision_outputs$matchup_context))
    expect_true(file.exists(decision_outputs$technical_dashboard))
    expect_match(paste(readLines(decision_outputs$technical_dashboard, warn = FALSE), collapse = "\n"), "Live Tournament Performance")
    expect_match(live_performance$status, "Monitoring only")
    expect_match(live_performance$status, "Live tournament performance through")
    expect_true(nrow(live_performance$summary) == 1)
    expect_true(nrow(live_performance$main_bracket_summary) == 1)
    expect_true(live_performance$main_bracket_games_played > 0)
    expect_identical(as.character(live_performance$games$round[[1]]), "Round of 32")
    expect_match(live_performance$recent_games_title, "Recent Games")
    expect_match(render_live_performance_html(live_performance, model_label = "Stan GLM"), "Live Tournament Performance - Stan GLM")
    expect_match(render_live_performance_html(live_performance, model_label = "Stan GLM"), "Main-bracket live performance")
    expect_match(render_live_performance_html(live_performance, model_label = "Stan GLM"), "Monitoring only")
    expect_match(render_live_performance_html(live_performance, model_label = "Stan GLM"), "Live By Round")
    expect_match(render_live_performance_html(live_performance, model_label = "Stan GLM"), "Mean predicted is the model's average win probability")
    expect_match(render_live_performance_html(live_performance, model_label = "Stan GLM"), "Ordered by recorded completion time")
    expect_true(file.exists(file.path(output_dir, "model_quality", "latest_model_quality.rds")))
    expect_equal(length(list.files(file.path(output_dir, "model_quality"), pattern = "^model_quality_.*_pid.*\\.rds$")), 0L)
    expect_true(isTRUE(decision_outputs$model_quality_used_cached_quality))
    expect_match(decision_outputs$model_quality_source_label, "Cached identical validation snapshot")
    expect_true(all(c("inspection_flag", "inspection_level", "predicted_total_median") %in% names(saved_matchup_totals)))

    first_four_only_results <- live_current_results %>%
        dplyr::filter(round == "First Four")
    first_four_only_performance <- summarize_live_tournament_performance(
        data = list(
            bracket_year = 2025L,
            current_completed_results = first_four_only_results,
            current_teams = live_team_data %>% dplyr::filter(Year == "2025")
        ),
        model_results = model_results,
        draws = 25L
    )
    expect_match(first_four_only_performance$status, "Only First Four games have completed so far")
    expect_equal(first_four_only_performance$main_bracket_games_played, 0L)
    expect_match(render_live_performance_html(first_four_only_performance), "early read")
    expect_match(render_live_performance_html(first_four_only_performance), "No Round of 64\\+ games have completed yet")

    live_without_timestamps <- live_current_results %>%
        dplyr::select(-completed_at)
    no_timestamp_performance <- summarize_live_tournament_performance(
        data = list(
            bracket_year = 2025L,
            current_completed_results = live_without_timestamps,
            current_teams = live_team_data %>% dplyr::filter(Year == "2025")
        ),
        model_results = model_results,
        draws = 25L
    )
    expect_match(no_timestamp_performance$recent_games_title, "Latest Available Games")
    expect_match(render_live_performance_html(no_timestamp_performance), "Latest Available Games")

    no_game_performance <- summarize_live_tournament_performance(
        data = list(
            bracket_year = 2025L,
            current_completed_results = tibble::tibble(),
            current_teams = live_team_data %>% dplyr::filter(Year == "2025")
        ),
        model_results = model_results,
        draws = 25L
    )
    expect_equal(nrow(no_game_performance$summary), 0L)
    expect_match(render_live_performance_html(no_game_performance), "No completed current-year games have been recorded yet")

    dashboard_html <- create_bracket_dashboard_html(
        bracket_year = 2026L,
        decision_sheet = decision_sheet,
        candidates = candidates,
        current_teams = loaded$current_teams,
        backtest = NULL,
        play_in_resolution = play_in_resolution,
        total_points_predictions = total_predictions,
        model_quality_context = resolved_quality,
        model_overview = model_overview
    )
    expect_match(dashboard_html, "Review Queue")
    expect_match(dashboard_html, "Divergence Map")
    expect_match(dashboard_html, "Candidate Recommendations")
    expect_match(dashboard_html, "How to read this workspace")
    expect_match(dashboard_html, "<meta name='viewport' content='width=device-width, initial-scale=1'>")
    expect_match(dashboard_html, "Overall strength rating")
    expect_match(dashboard_html, "Review priority score")
    expect_match(dashboard_html, "Candidate 2 changes from the baseline")
    expect_match(dashboard_html, "Matchup Evidence")
    expect_match(dashboard_html, "Reference matchups")
    expect_match(dashboard_html, "Full candidate paths")
    expect_match(dashboard_html, "Technical appendix")
    expect_match(dashboard_html, "Need more diagnostics?")
    expect_match(dashboard_html, "Open technical_dashboard.html")
    expect_match(dashboard_html, "href='technical_dashboard.html'")
    expect_match(dashboard_html, "href='model_comparison_dashboard.html'")
    expect_equal(sum(gregexpr("Open the full model comparison dashboard", dashboard_html, fixed = TRUE)[[1]] > 0), 0L)
    if (isTRUE(play_in_resolution$has_unresolved_slots[[1]])) {
        expect_match(dashboard_html, "Status: Simulated bracket path")
        expect_match(dashboard_html, "generated brackets assume simulated First Four winners")
    } else {
        expect_match(dashboard_html, "Status: Final result")
        expect_match(dashboard_html, "First Four slots are resolved")
    }
    review_pos <- regexpr("Review Queue", dashboard_html, fixed = TRUE)[[1]]
    build_pos <- regexpr("Candidate Recommendations", dashboard_html, fixed = TRUE)[[1]]
    helper_pos <- regexpr("How to read this workspace", dashboard_html, fixed = TRUE)[[1]]
    evidence_pos <- regexpr("Matchup Evidence", dashboard_html, fixed = TRUE)[[1]]
    paths_pos <- regexpr("Full candidate paths", dashboard_html, fixed = TRUE)[[1]]
    appendix_pos <- regexpr("Technical appendix", dashboard_html, fixed = TRUE)[[1]]
    expect_true(build_pos > 0)
    expect_true(helper_pos > 0)
    expect_true(review_pos > 0)
    expect_true(helper_pos < review_pos)
    expect_true(review_pos < build_pos)
    expect_true(build_pos < evidence_pos)
    expect_true(evidence_pos < paths_pos)
    expect_true(paths_pos < appendix_pos)
    expect_match(dashboard_html, "Bracket-changing toss-ups")
    expect_match(dashboard_html, "Upset pivots")
    expect_match(dashboard_html, "Fragile favorites")
    expect_match(dashboard_html, "data-open-evidence")
    expect_match(dashboard_html, "data-divergence-target")
    expect_match(dashboard_html, "data-divergence-round")
    expect_match(dashboard_html, "candidate-divergence-reference")
    expect_match(dashboard_html, "Baseline reference")
    expect_match(dashboard_html, "Changed slots from baseline")
    expect_match(dashboard_html, "Pairwise diff:")
    expect_no_match(dashboard_html, "Change count")
    expect_match(dashboard_html, "Candidate 1 full path")
    expect_match(dashboard_html, "Candidate 2 full path")
    expect_match(dashboard_html, "Need more diagnostics?")
    expect_match(dashboard_html, "Core metrics")
    expect_match(dashboard_html, "Model-facing matchup comparison")
    expect_match(dashboard_html, "Raw model inputs")
    expect_match(dashboard_html, "Favorite:")
    expect_match(dashboard_html, "Underdog:")
    expect_match(dashboard_html, "advantage-row")
    expect_match(dashboard_html, "Diff favors")
    expect_no_match(dashboard_html, "Favorite probability")
    expect_no_match(dashboard_html, "Underdog probability")
    expect_match(dashboard_html, "Candidate usage")
    divergence_map_pos <- regexpr("Divergence Map", dashboard_html, fixed = TRUE)[[1]]
    diff_reference_pos <- regexpr("Candidate 2 changes from the baseline", dashboard_html, fixed = TRUE)[[1]]
    expect_true(divergence_map_pos > 0)
    expect_true(divergence_map_pos < diff_reference_pos)

    dashboard_doc <- xml2::read_html(dashboard_html)
    reference_panels <- rvest::html_elements(dashboard_doc, "details.evidence-panel[data-surface='All matchups']")
    expect_true(length(reference_panels) > 0)
    reference_panel_text <- rvest::html_text2(reference_panels[[1]])
    expect_match(reference_panel_text, "Reference matchup\\. Use this drawer when you want the evidence inputs behind a specific node\\.")
    expect_match(reference_panel_text, "Candidate usage")
    expect_match(reference_panel_text, "C1:")
    expect_match(reference_panel_text, "C2:")
    expect_match(reference_panel_text, "Favorite:")
    expect_match(reference_panel_text, "Underdog:")
    expect_match(reference_panel_text, "Raw model inputs")
    expect_match(reference_panel_text, "Diff favors")

    reference_roles <- rvest::html_text2(rvest::html_elements(reference_panels[[1]], ".team-card__role"))
    expect_equal(reference_roles, c("Favorite", "Underdog"))
    expect_true(length(rvest::html_elements(reference_panels[[1]], ".advantage-row")) > 0)
    expect_equal(length(rvest::html_elements(reference_panels[[1]], "details.comparison-details")), 1L)

    technical_html <- create_technical_dashboard_html(
        bracket_year = 2026L,
        decision_sheet = decision_sheet,
        candidates = candidates,
        backtest = list(summary = tibble::tibble(
            mean_log_loss = 0.401,
            mean_brier = 0.188,
            mean_accuracy = 0.713,
            mean_bracket_score = 85.4,
            mean_correct_picks = 42.7
        ), calibration = tibble::tibble(
            mean_predicted = c(0.35, 0.55, 0.75),
            empirical_rate = c(0.30, 0.58, 0.78),
            n_games = c(12L, 16L, 10L)
        ), yearly_metrics = tibble::tibble(year = c(2018L, 2019L, 2020L))),
        total_points_predictions = total_predictions,
        play_in_resolution = play_in_resolution,
        model_quality_context = resolved_quality,
        live_performance = live_performance,
        model_overview = model_overview
    )
    expect_match(technical_html, "mmBayes Technical Bracket Dashboard")
    expect_match(technical_html, "<meta name='viewport' content='width=device-width, initial-scale=1'>")
    expect_match(technical_html, "How To Use This Dashboard")
    expect_match(technical_html, "Action Summary")
    expect_match(technical_html, "Key Warnings")
    expect_match(technical_html, "Compare Workspace")
    expect_match(technical_html, "Model Overview")
    expect_match(technical_html, "What this means")
    expect_match(technical_html, "Backtest Calibration Curve")
    expect_match(technical_html, "Backtest By Round")
    expect_match(technical_html, "diagnostics-overview-grid")
    expect_match(technical_html, "diagnostics-detail-grid")
    expect_match(technical_html, "calibration-layout")
    expect_match(technical_html, "tech-svg--calibration")
    expect_match(technical_html, "Live Tournament Performance - Stan GLM")
    expect_match(technical_html, "Recent Games")
    expect_match(technical_html, "review-priority queue")
    expect_match(technical_html, "Cached identical validation snapshot")
    expect_match(technical_html, "Log loss")
    expect_match(technical_html, "Brier score")
    expect_match(technical_html, "Accuracy")
    expect_match(technical_html, "Doing well")
    expect_match(technical_html, "Needs attention")
    expect_match(technical_html, "The backtest is the historical baseline")
    expect_match(technical_html, "Rolling holdout years")
    expect_match(technical_html, "2018, 2019, 2020")
    expect_match(technical_html, "How to read this chart")
    expect_match(technical_html, "each point groups held-out games into a probability range")
    expect_match(technical_html, "Additional model detail")
    expect_false(grepl("Engine settings and feature detail", technical_html, fixed = TRUE))
    expect_match(technical_html, "This is about long-run frequency matching")
    expect_match(technical_html, "Observed win rate")
    expect_match(technical_html, "Observed win rate in that probability range")
    expect_match(technical_html, "Perfect line")
    expect_match(technical_html, "too optimistic")
    expect_match(technical_html, "too pessimistic")
    expect_match(technical_html, "quality-grid")
    expect_match(technical_html, "What this means")
    expect_match(technical_html, "Backtest Summary")
    expect_false(grepl("Backtest unavailable", technical_html, fixed = TRUE))
    expect_match(technical_html, "Ranked Decision Board")
    expect_match(technical_html, "review priority = round weight x \\(underdog win probability \\+ interval width\\)")
    expect_match(technical_html, "Upset Opportunity Board")
    expect_match(technical_html, "leverage = round weight x underdog win probability x \\(1 \\+ interval width\\)")
    expect_match(technical_html, "Underdog posterior uncertainty")
    expect_match(technical_html, "Derived posterior credible interval")
    expect_match(technical_html, "Candidate Divergence")
    expect_match(technical_html, "Bracket position where the paths split")
    expect_match(technical_html, "Candidate 1 matchup uncertainty")
    expect_match(technical_html, "Posterior credible interval")
    expect_match(technical_html, "Championship Tiebreaker Comparison")
    expect_match(technical_html, "data-view-target='compare'")
    expect_match(technical_html, "data-view-target='candidate-1'")
    expect_match(technical_html, "data-view-target='candidate-2'")
    expect_match(technical_html, "Candidate 1 Most Fragile Picks")
    expect_match(technical_html, "Candidate 2 Most Fragile Picks")
    expect_match(technical_html, "Candidate 1 Championship Distribution")
    expect_match(technical_html, "Backtest and calibration evidence")
    expect_match(technical_html, "Live performance evidence")
    expect_match(technical_html, "Model setup and engine comparison")
    expect_match(technical_html, "Reference tables")
    expect_false(grepl("Why These Boards Are Ordered This Way", technical_html, fixed = TRUE))
    expect_false(grepl("Differing slot", technical_html, fixed = TRUE))
    expect_false(grepl("Preferred path, alternate path, and usage note", technical_html, fixed = TRUE))
    expect_false(grepl("Round, candidate usage, and why to consider it", technical_html, fixed = TRUE))

    bart_model_overview <- list(
        matchup = list(
            engine = "bart",
            engine_label = "BART",
            prior_type = "normal",
            draw_budget = 1000L,
            predictor_count = 7L,
            betting_predictor_count = 3L,
            predictor_summary = "7 predictors: round, same_conf, seed_gap, barthag_logit_gap, betting_abs_prob_edge, betting_abs_spread, betting_bookmakers",
            bart_config = list(
                n_trees = 200L,
                n_burn = 500L,
                n_post = 1000L,
                k = 2,
                power = 2
            ),
            interaction_terms = character(0)
        ),
        totals = list(
            engine = "bart",
            engine_label = "BART",
            prior_type = "normal",
            draw_budget = 1000L,
            predictor_count = 4L,
            betting_predictor_count = 2L,
            predictor_summary = "4 predictors: round, seed_gap, betting_abs_prob_edge, betting_minutes_before_commence",
            bart_config = list(
                n_trees = 200L,
                n_burn = 500L,
                n_post = 1000L,
                k = 2,
                power = 2
            ),
            interaction_terms = character(0)
        )
    )
    bart_dashboard_html <- create_technical_dashboard_html(
        bracket_year = 2026L,
        decision_sheet = decision_sheet,
        candidates = candidates,
        backtest = NULL,
        total_points_predictions = total_predictions,
        play_in_resolution = play_in_resolution,
        model_quality_context = resolved_quality,
        model_overview = bart_model_overview
    )
    expect_match(bart_dashboard_html, "BART")
    expect_match(bart_dashboard_html, "Nonlinear posterior tree model")
    expect_match(bart_dashboard_html, "n_trees")
    expect_match(bart_dashboard_html, "n_post")
    expect_match(bart_dashboard_html, "Model Overview")
    expect_match(bart_dashboard_html, "BART settings")
    expect_match(bart_dashboard_html, "Cached identical validation snapshot")

    alt_backtest <- list(
        summary = tibble::tibble(
            mean_log_loss = 0.362,
            mean_brier = 0.171,
            mean_accuracy = 0.742,
            mean_bracket_score = 87.1,
            mean_correct_picks = 44.9
        ),
        calibration = tibble::tibble(
            mean_predicted = c(0.32, 0.54, 0.76),
            empirical_rate = c(0.29, 0.57, 0.79),
            n_games = c(12L, 16L, 10L)
        )
    )
    alt_live_performance <- live_performance
    alt_live_performance$summary <- live_performance$summary %>%
        dplyr::mutate(
            log_loss = log_loss + 0.018,
            brier = brier + 0.011,
            accuracy = pmin(1, accuracy + 0.05)
        )
    alt_live_performance$status <- "BART comparison snapshot."
    comparison_bundle <- list(
        available = TRUE,
        attempted = TRUE,
        status = "Comparison completed for Stan GLM and BART.",
        current_label = "Stan GLM",
        alternate_label = "BART",
        current = list(
            model_overview = model_overview,
            backtest = list(summary = tibble::tibble(
                mean_log_loss = 0.401,
                mean_brier = 0.188,
                mean_accuracy = 0.713,
                mean_bracket_score = 85.4,
                mean_correct_picks = 42.7
            )),
            live_performance = live_performance
        ),
        alternate = list(
            model_overview = bart_model_overview,
            backtest = alt_backtest,
            live_performance = alt_live_performance
        ),
        backtest_comparison = build_model_metric_comparison_table(
            current_summary = tibble::tibble(
                mean_log_loss = 0.401,
                mean_brier = 0.188,
                mean_accuracy = 0.713,
                mean_bracket_score = 85.4,
                mean_correct_picks = 42.7
            ),
            alternate_summary = alt_backtest$summary,
            current_label = "Stan GLM",
            alternate_label = "BART",
            kind = "backtest"
        ),
        live_comparison = build_model_metric_comparison_table(
            current_summary = live_performance$summary,
            alternate_summary = alt_live_performance$summary,
            current_label = "Stan GLM",
            alternate_label = "BART",
            kind = "live"
        ),
        summary = summarize_model_metric_comparison(
            comparison_table = build_model_metric_comparison_table(
                current_summary = tibble::tibble(
                    mean_log_loss = 0.401,
                    mean_brier = 0.188,
                    mean_accuracy = 0.713,
                    mean_bracket_score = 85.4,
                    mean_correct_picks = 42.7
                ),
                alternate_summary = alt_backtest$summary,
                current_label = "Stan GLM",
                alternate_label = "BART",
                kind = "backtest"
            ),
            current_label = "Stan GLM",
            alternate_label = "BART"
        ),
        notes = character()
    )
    comparison_html <- create_model_comparison_dashboard_html(
        bracket_year = 2026L,
        model_comparison = comparison_bundle
    )
    expect_match(comparison_html, "Model Comparison")
    expect_match(comparison_html, "<meta name='viewport' content='width=device-width, initial-scale=1'>")
    expect_match(comparison_html, "Compare")
    expect_match(comparison_html, "Stan GLM")
    expect_match(comparison_html, "BART")
    expect_match(comparison_html, "Preferred engine verdict")
    expect_match(comparison_html, "Deeper evidence and metric tables")
    expect_match(comparison_html, "Preferred engine")
    expect_match(comparison_html, "Preferred engine from held-out backtest evidence")
    expect_match(comparison_html, "Monitoring only; does not change the engine verdict")
    expect_match(comparison_html, "Mean predicted is the model's average pregame win probability")
    expect_match(comparison_html, "and empirical rate is how often that side actually won")
    expect_match(comparison_html, "Live Tournament Performance - Stan GLM")
    expect_match(comparison_html, "Live Tournament Performance - BART")
    expect_true(sum(gregexpr("Matchup Model", comparison_html, fixed = TRUE)[[1]] > 0) >= 2)
    expect_true(sum(gregexpr("Total Points Model", comparison_html, fixed = TRUE)[[1]] > 0) >= 2)
    expect_true(sum(gregexpr("Additional model detail", comparison_html, fixed = TRUE)[[1]] > 0) >= 2)
    expect_match(render_model_comparison_link_html(comparison_bundle), "Open the full model comparison dashboard")

    comparison_with_split_current_overview <- comparison_bundle
    comparison_with_split_current_overview$current$model_overview <- model_overview$matchup
    comparison_with_split_current_overview$current$totals_overview <- model_overview$totals
    split_overview_html <- create_model_comparison_dashboard_html(
        bracket_year = 2026L,
        model_comparison = comparison_with_split_current_overview
    )
    expect_true(sum(gregexpr("Matchup Model", split_overview_html, fixed = TRUE)[[1]] > 0) >= 2)
    expect_true(sum(gregexpr("Total Points Model", split_overview_html, fixed = TRUE)[[1]] > 0) >= 2)

    live_conflict_bundle <- comparison_bundle
    live_conflict_bundle$backtest_comparison <- build_model_metric_comparison_table(
        current_summary = tibble::tibble(
            mean_log_loss = 0.401,
            mean_brier = 0.188,
            mean_accuracy = 0.713,
            mean_bracket_score = 85.4,
            mean_correct_picks = 42.7
        ),
        alternate_summary = tibble::tibble(
            mean_log_loss = 0.410,
            mean_brier = 0.194,
            mean_accuracy = 0.700,
            mean_bracket_score = 84.1,
            mean_correct_picks = 42.1
        ),
        current_label = "Stan GLM",
        alternate_label = "BART",
        kind = "backtest"
    )
    live_conflict_bundle$live_comparison <- build_model_metric_comparison_table(
        current_summary = tibble::tibble(
            games_played = 18L,
            log_loss = 0.410,
            brier = 0.190,
            accuracy = 0.650
        ),
        alternate_summary = tibble::tibble(
            games_played = 18L,
            log_loss = 0.360,
            brier = 0.160,
            accuracy = 0.760
        ),
        current_label = "Stan GLM",
        alternate_label = "BART",
        kind = "live"
    )
    live_conflict_verdict <- summarize_model_comparison_verdict(live_conflict_bundle)
    expect_equal(live_conflict_verdict$preferred_label, "Stan GLM")
    expect_match(live_conflict_verdict$justification, "monitoring-only")
    expect_match(live_conflict_verdict$caveat, "does not override the held-out backtest")

    html_without_quality <- create_technical_dashboard_html(
        bracket_year = 2026L,
        decision_sheet = decision_sheet,
        candidates = candidates,
        backtest = NULL,
        total_points_predictions = NULL,
        play_in_resolution = play_in_resolution
    )
    expect_match(html_without_quality, "Backtest not computed in this run.")
    expect_match(html_without_quality, "Backtest calibration was not available for this run.")

    ranked_matchups <- decision_sheet %>%
        dplyr::arrange(dplyr::desc(decision_score), round, region, matchup_number) %>%
        dplyr::pull(matchup_label)
    expect_true(length(ranked_matchups) >= 2)
    ranked_section_start <- regexpr("Ranked Decision Board", technical_html, fixed = TRUE)[[1]]
    ranked_section_end <- regexpr("Upset Opportunity Board", technical_html, fixed = TRUE)[[1]]
    ranked_section <- substr(technical_html, ranked_section_start, ranked_section_end - 1L)
    first_ranked_position <- regexpr(ranked_matchups[[1]], ranked_section, fixed = TRUE)[[1]]
    second_ranked_position <- regexpr(ranked_matchups[[2]], ranked_section, fixed = TRUE)[[1]]
    expect_true(first_ranked_position > 0)
    expect_true(second_ranked_position > first_ranked_position)

    diff_matchup <- decision_sheet %>%
        dplyr::filter(candidate_diff_flag) %>%
        dplyr::slice_head(n = 1) %>%
        dplyr::pull(matchup_label)
    if (length(diff_matchup) == 1L) {
        expect_match(technical_html, diff_matchup)
    }

    same_winner_board <- decision_sheet %>%
        dplyr::slice_head(n = 1) %>%
        dplyr::mutate(
            candidate_diff_flag = TRUE,
            candidate_1_pick = "Duke",
            candidate_2_pick = "Duke"
        )
    expect_match(render_ranked_decision_svg(same_winner_board), "Same winner, different path")

    technical_html_without_optional <- create_technical_dashboard_html(
        bracket_year = 2026L,
        decision_sheet = decision_sheet,
        candidates = candidates,
        backtest = NULL,
        total_points_predictions = NULL,
        play_in_resolution = play_in_resolution,
        model_quality_context = resolved_quality,
        model_overview = model_overview
    )
    expect_match(technical_html_without_optional, "Cached identical validation snapshot")
    expect_match(technical_html_without_optional, "Championship total-points distributions were not supplied for this run.")
    expect_match(technical_html_without_optional, "Championship Tiebreaker Comparison")
})

test_that("bracket dashboard context builder enriches matchup evidence deterministically", {
    team_file <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")
    fixture_paths <- write_fixture_data_files(team_file, results_file)

    config <- default_project_config()
    config$data$team_features_path <- fixture_paths$team_path
    config$data$game_results_path <- fixture_paths$results_path
    config$model$history_window <- 3L
    config$model$n_draws <- 25L

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
    total_model <- fit_total_points_model(
        historical_total_points = build_total_points_training_rows(loaded$historical_actual_results),
        random_seed = 123,
        include_diagnostics = FALSE
    )

    candidates <- generate_bracket_candidates(
        all_teams = loaded$current_teams,
        model_results = model_results,
        draws = 25L,
        actual_play_in_results = loaded$current_play_in_results,
        n_candidates = 2L,
        n_simulations = 8L,
        random_seed = 123
    )
    decision_sheet <- build_decision_sheet(candidates)
    total_predictions <- predict_candidate_total_points(
        candidates = candidates,
        current_teams = loaded$current_teams,
        total_points_model = total_model,
        draws = 25L
    )
    play_in_resolution <- summarize_play_in_resolution(
        current_teams = loaded$current_teams,
        actual_play_in_results = loaded$current_play_in_results
    )

    context_one <- build_bracket_dashboard_context(
        current_teams = loaded$current_teams,
        decision_sheet = decision_sheet,
        candidates = candidates,
        total_points_predictions = total_predictions,
        play_in_resolution = play_in_resolution
    )
    context_two <- build_bracket_dashboard_context(
        current_teams = loaded$current_teams,
        decision_sheet = decision_sheet,
        candidates = candidates,
        total_points_predictions = total_predictions,
        play_in_resolution = play_in_resolution
    )

    expect_true(all(c(
        "teamA_Seed", "teamB_Seed", "teamA_Conf", "teamB_Conf",
        "teamA_AdjOE", "teamB_AdjOE", "seed_diff", "barthag_logit_diff",
        "AdjOE_diff", "AdjDE_diff", "WAB_diff", "TOR_diff", "TORD_diff",
        "ORB_diff", "DRB_diff", "3P%_diff", "3P%D_diff", "Adj T._diff"
    ) %in% names(context_one$matchup_context_rows)))

    sample_row <- context_one$matchup_context_rows %>% dplyr::slice_head(n = 1)
    team_a_lookup <- loaded$current_teams %>%
        dplyr::mutate(team_key = normalize_team_key(Team)) %>%
        dplyr::filter(team_key == normalize_team_key(sample_row$teamA[[1]]))
    team_b_lookup <- loaded$current_teams %>%
        dplyr::mutate(team_key = normalize_team_key(Team)) %>%
        dplyr::filter(team_key == normalize_team_key(sample_row$teamB[[1]]))
    expect_equal(sample_row$teamA_Seed[[1]], team_a_lookup$Seed[[1]])
    expect_equal(sample_row$teamB_Seed[[1]], team_b_lookup$Seed[[1]])
    expect_equal(sample_row$teamA_Conf[[1]], team_a_lookup$Conf[[1]])
    expect_equal(sample_row$teamB_Conf[[1]], team_b_lookup$Conf[[1]])

    expect_identical(context_one$candidate_delta_rows$slot_key, context_two$candidate_delta_rows$slot_key)
    expect_identical(context_one$divergence_map_rows$total_count, context_two$divergence_map_rows$total_count)
    expect_identical(context_one$watchlist_rows$slot_key, context_two$watchlist_rows$slot_key)
    expect_identical(context_one$watchlist_rows$reason_surface, context_two$watchlist_rows$reason_surface)
    expect_true(all(c("comparison_summary_label", "comparison_summary_value") %in% names(context_one$candidate_summary_rows)))
    expect_identical(
        context_one$candidate_summary_rows$comparison_summary_value[context_one$candidate_summary_rows$candidate_id == 1L][[1]],
        "Baseline reference"
    )
    expect_identical(
        context_one$candidate_summary_rows$comparison_summary_label[context_one$candidate_summary_rows$candidate_id == 2L][[1]],
        "Changed slots from baseline"
    )

    expected_delta_order <- context_one$candidate_delta_rows %>%
        dplyr::arrange(factor(round, levels = round_levels()), factor(region, levels = bracket_region_levels()), matchup_slot) %>%
        dplyr::pull(slot_key)
    expect_identical(context_one$candidate_delta_rows$slot_key, expected_delta_order)
    if (nrow(context_one$candidate_delta_rows) > 0) {
        expect_true(all(context_one$candidate_delta_rows$candidate_diff_flag))
    }

    expect_true(nrow(context_one$watchlist_rows) > 0)
    expect_true(any(context_one$watchlist_rows$reason_surface == "Bracket-changing toss-ups"))
    upset_watch_rows <- context_one$watchlist_rows %>%
        dplyr::filter(reason_surface == "Upset pivots")
    if (nrow(upset_watch_rows) > 0) {
        expect_true(all(upset_watch_rows$candidate_1_upset | upset_watch_rows$candidate_2_upset))
        expect_true(all(nzchar(upset_watch_rows$why_this_matters)))
    }

    if (nrow(context_one$candidate_delta_rows) > 0) {
        expect_true(all(nzchar(context_one$candidate_delta_rows$downstream_implication_text)))
        expect_true(all(!is.na(context_one$candidate_delta_rows$why_swap_exists)))
    }

    expect_true(all(c(
        "round", "region", "total_count", "winner_change_count", "path_only_count",
        "late_round_only", "all_in_watchlist", "target_evidence_id"
    ) %in% names(context_one$divergence_map_rows)))
    expect_true(any(context_one$divergence_map_rows$total_count == 0L))
    expect_true(any(context_one$divergence_map_rows$late_round_only))
    expect_true(all(context_one$divergence_map_rows$winner_change_count + context_one$divergence_map_rows$path_only_count <= context_one$divergence_map_rows$total_count))

    differing_watch_row <- context_one$watchlist_rows %>%
        dplyr::filter(reason_surface == "Bracket-changing toss-ups", candidate_diff_flag) %>%
        dplyr::slice_head(n = 1)
    if (nrow(differing_watch_row) == 1) {
        expect_true(nzchar(differing_watch_row$downstream_implication_text[[1]]))
    }

    non_differing_watch_row <- context_one$watchlist_rows %>%
        dplyr::filter(!candidate_diff_flag) %>%
        dplyr::slice_head(n = 1)
    if (nrow(non_differing_watch_row) == 1) {
        expect_true(is.na(non_differing_watch_row$downstream_implication_text[[1]]))
    }
})

test_that("divergence map summary keeps empty buckets and path-only splits distinct", {
    matchup_context_rows <- tibble::tibble(
        round = c("Round of 64", "Round of 64", "Sweet 16", "Championship"),
        region = c("East", "South", "East", "National")
    )
    candidate_delta_rows <- tibble::tibble(
        round = c("Round of 64", "Sweet 16", "Championship"),
        region = c("East", "East", "National"),
        difference_mode = c("Path", "Winner", "Winner and path"),
        evidence_id = c("evidence-east-r64", "evidence-east-s16", "evidence-title"),
        slot_key = c("r64-east", "s16-east", "title-game")
    )
    watchlist_rows <- tibble::tibble(
        round = c("Round of 64", "Sweet 16"),
        region = c("East", "East"),
        candidate_diff_flag = c(TRUE, TRUE),
        evidence_id = c("evidence-east-r64", "evidence-east-s16"),
        slot_key = c("r64-east", "s16-east")
    )

    summary_rows <- build_divergence_map_rows(
        matchup_context_rows = matchup_context_rows,
        candidate_delta_rows = candidate_delta_rows,
        watchlist_rows = watchlist_rows
    )

    empty_bucket <- summary_rows %>%
        dplyr::filter(round == "Round of 64", region == "South")
    expect_equal(nrow(empty_bucket), 1L)
    expect_equal(empty_bucket$total_count[[1]], 0L)

    path_only_bucket <- summary_rows %>%
        dplyr::filter(round == "Round of 64", region == "East")
    expect_equal(path_only_bucket$total_count[[1]], 1L)
    expect_equal(path_only_bucket$winner_change_count[[1]], 0L)
    expect_equal(path_only_bucket$path_only_count[[1]], 1L)
    expect_true(path_only_bucket$all_in_watchlist[[1]])

    unsurfaced_bucket <- summary_rows %>%
        dplyr::filter(round == "Championship", region == "National")
    expect_equal(unsurfaced_bucket$total_count[[1]], 1L)
    expect_equal(unsurfaced_bucket$winner_change_count[[1]], 1L)
    expect_equal(unsurfaced_bucket$path_only_count[[1]], 0L)
    expect_false(unsurfaced_bucket$all_in_watchlist[[1]])
    expect_equal(unsurfaced_bucket$unsurfaced_count[[1]], 1L)
})

test_that("candidate usage helper formats picks consistently", {
    expect_identical(
        build_candidate_usage_label("Duke", "Florida", FALSE, TRUE),
        "C1: Duke (Favorite); C2: Florida (Underdog)"
    )
    expect_identical(
        build_candidate_usage_label("Houston", "Houston", NA, NA),
        "C1: Houston (Favorite); C2: Houston (Favorite)"
    )
    expect_identical(
        build_candidate_usage_label(NA_character_, "Auburn", NA, TRUE),
        "C1: n/a; C2: Auburn (Underdog)"
    )
})

test_that("posterior favorites are not mislabeled as underdogs when seed order disagrees", {
    matchup <- tibble::tibble(
        region = "East",
        round = "Round of 32",
        matchup_number = 2L,
        teamA = "Saint John's",
        teamA_seed = 5L,
        teamA_strength = 90,
        teamB = "Kansas",
        teamB_seed = 4L,
        teamB_strength = 88,
        win_prob_A = 0.793,
        model_win_prob_A = 0.793,
        line_prob_A = NA_real_,
        betting_blend_weight = NA_real_,
        used_betting_line = FALSE,
        ci_lower = 0.639,
        ci_upper = 0.895,
        prediction_sd = 0.1,
        winner = "Saint John's",
        upset = TRUE
    )

    augmented <- augment_matchup_decisions(matchup)

    expect_identical(augmented$posterior_favorite[[1]], "Saint John's")
    expect_false(augmented$upset[[1]])
    expect_identical(
        build_candidate_usage_label(
            augmented$winner[[1]],
            augmented$winner[[1]],
            augmented$upset[[1]],
            augmented$upset[[1]]
        ),
        "C1: Saint John's (Favorite); C2: Saint John's (Favorite)"
    )
})

test_that("actual First Four winners replace stale Round of 64 opponents", {
    team_file <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")
    team_data <- make_fixture_team_features(current_year = 2025, history_years = 2022:2024) %>%
        dplyr::mutate(
            Team = dplyr::case_when(
                Year == "2025" & Region == "West" & Seed == 6L ~ "BYU",
                Year == "2025" & Region == "West" & Seed == 11L & !stringr::str_detect(Team, "playin") ~ "NC State",
                Year == "2025" & Region == "West" & Seed == 16L & stringr::str_detect(Team, "playin") ~ "Texas",
                TRUE ~ Team
            ),
            Seed = dplyr::if_else(Year == "2025" & Team == "Texas", 11L, Seed)
        )
    results_data <- make_fixture_game_results(team_data, history_years = 2022:2024) %>%
        dplyr::bind_rows(
            tibble::tibble(
                Year = "2025",
                region = "First Four",
                round = "First Four",
                game_index = 1L,
                teamA = "NC State",
                teamB = "Texas",
                teamA_seed = 11L,
                teamB_seed = 11L,
                winner = "Texas"
            )
        )
    fixture_paths <- write_fixture_data_files(team_file, results_file, team_data = team_data, results_data = results_data)

    config <- default_project_config()
    config$data$team_features_path <- fixture_paths$team_path
    config$data$game_results_path <- fixture_paths$results_path
    config$model$history_window <- 3L
    config$model$n_draws <- 25L

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

    simulation <- simulate_full_bracket(
        loaded$current_teams,
        model_results,
        draws = 25L,
        actual_play_in_results = loaded$current_play_in_results
    )
    flattened <- flatten_matchup_results(simulation)
    west_round64 <- flattened %>%
        dplyr::filter(region == "West", round == "Round of 64")

    expect_true(any(flattened$round == "First Four" & flattened$winner == "Texas"))
    expect_true(any(west_round64$teamA == "BYU" & west_round64$teamB == "Texas"))
    expect_false(any(west_round64$teamA == "BYU" & west_round64$teamB == "NC State"))
})

test_that("dashboard warning tracks unresolved versus completed First Four slots", {
    team_file <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")
    team_data <- make_fixture_team_features(current_year = 2025, history_years = 2022:2024)
    results_data <- make_fixture_game_results(team_data, history_years = 2022:2024)

    current_duplicate_slots <- team_data %>%
        dplyr::filter(Year == "2025") %>%
        dplyr::count(Region, Seed, name = "n") %>%
        dplyr::filter(n > 1L) %>%
        dplyr::inner_join(
            team_data %>%
                dplyr::filter(Year == "2025") %>%
                dplyr::select(Year, Region, Seed, Team),
            by = c("Region", "Seed")
        ) %>%
        dplyr::arrange(Region, Seed, Team) %>%
        dplyr::group_by(Region, Seed) %>%
        dplyr::summarise(
            Year = dplyr::first(Year),
            teamA = dplyr::first(Team),
            teamB = dplyr::last(Team),
            winner = dplyr::first(Team),
            .groups = "drop"
        ) %>%
        dplyr::mutate(
            region = "First Four",
            round = "First Four",
            game_index = 1L,
            teamA_seed = Seed,
            teamB_seed = Seed
        ) %>%
        dplyr::select(Year, region, round, game_index, teamA, teamB, teamA_seed, teamB_seed, winner)

    partial_results <- dplyr::bind_rows(results_data, current_duplicate_slots[1, , drop = FALSE])
    fixture_paths <- write_fixture_data_files(team_file, results_file, team_data = team_data, results_data = partial_results)

    config <- default_project_config()
    config$data$team_features_path <- fixture_paths$team_path
    config$data$game_results_path <- fixture_paths$results_path
    config$model$history_window <- 3L
    config$model$n_draws <- 25L

    old_options <- options(
        mmBayes.stan_chains = 1L,
        mmBayes.stan_iter = 60L,
        mmBayes.stan_refresh = 0L
    )
    on.exit(options(old_options), add = TRUE)

    loaded_partial <- load_tournament_data(config)
    model_results <- fit_tournament_model(
        loaded_partial$historical_matchups,
        config$model$required_predictors,
        random_seed = 123,
        include_diagnostics = FALSE
    )
    candidates_partial <- generate_bracket_candidates(
        all_teams = loaded_partial$current_teams,
        model_results = model_results,
        draws = 25L,
        actual_play_in_results = loaded_partial$current_play_in_results,
        n_candidates = 2L,
        n_simulations = 8L,
        random_seed = 123
    )
    fake_quality_backtest <- list(
        summary = tibble::tibble(
            mean_log_loss = 0.401,
            mean_brier = 0.188,
            mean_accuracy = 0.713,
            mean_bracket_score = 85.4,
            mean_correct_picks = 42.7
        ),
        calibration = tibble::tibble(
            mean_predicted = c(0.35, 0.55, 0.75),
            empirical_rate = c(0.30, 0.58, 0.78),
            n_games = c(12L, 16L, 10L)
        )
    )
    decision_sheet_partial <- build_decision_sheet(candidates_partial)
    play_in_resolution_partial <- summarize_play_in_resolution(loaded_partial$current_teams, loaded_partial$current_play_in_results)
    context_partial <- build_bracket_dashboard_context(
        current_teams = loaded_partial$current_teams,
        decision_sheet = decision_sheet_partial,
        candidates = candidates_partial,
        play_in_resolution = play_in_resolution_partial
    )
    html_partial <- create_bracket_dashboard_html(
        bracket_year = loaded_partial$bracket_year,
        decision_sheet = decision_sheet_partial,
        candidates = candidates_partial,
        current_teams = loaded_partial$current_teams,
        backtest = fake_quality_backtest,
        dashboard_context = context_partial,
        play_in_resolution = play_in_resolution_partial
    )

    expect_match(html_partial, "Status: Simulated bracket path")
    expect_true(any(context_partial$matchup_context_rows$round == "First Four"))
    expect_true(any(context_partial$divergence_map_rows$round == "First Four"))
    expect_match(html_partial, "data-round='First Four'")

    write_fixture_data_files(team_file, results_file, team_data = team_data, results_data = dplyr::bind_rows(results_data, current_duplicate_slots))
    loaded_resolved <- load_tournament_data(config)
    candidates_resolved <- generate_bracket_candidates(
        all_teams = loaded_resolved$current_teams,
        model_results = model_results,
        draws = 25L,
        actual_play_in_results = loaded_resolved$current_play_in_results,
        n_candidates = 2L,
        n_simulations = 8L,
        random_seed = 123
    )
    decision_sheet_resolved <- build_decision_sheet(candidates_resolved)
    play_in_resolution_resolved <- summarize_play_in_resolution(loaded_resolved$current_teams, loaded_resolved$current_play_in_results)
    context_resolved <- build_bracket_dashboard_context(
        current_teams = loaded_resolved$current_teams,
        decision_sheet = decision_sheet_resolved,
        candidates = candidates_resolved,
        play_in_resolution = play_in_resolution_resolved
    )
    html_resolved <- create_bracket_dashboard_html(
        bracket_year = loaded_resolved$bracket_year,
        decision_sheet = decision_sheet_resolved,
        candidates = candidates_resolved,
        current_teams = loaded_resolved$current_teams,
        backtest = fake_quality_backtest,
        dashboard_context = context_resolved,
        play_in_resolution = play_in_resolution_resolved
    )

    expect_match(html_resolved, "Status: Final result")
    expect_no_match(html_resolved, "Status: Simulated bracket path")
    expect_true(any(context_resolved$matchup_context_rows$round == "First Four"))
    expect_false(any(context_resolved$watchlist_rows$round == "First Four"))
    expect_false(any(context_resolved$divergence_map_rows$round == "First Four"))
    expect_no_match(html_resolved, "data-round='First Four'")
    expect_no_match(html_resolved, "data-divergence-round='First Four'")
    expect_match(html_resolved, "<td>First Four</td>")
})
