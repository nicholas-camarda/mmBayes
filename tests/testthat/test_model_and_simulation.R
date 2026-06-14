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

test_that("stochastic bracket simulation can use one coherent posterior draw index", {
    team_data <- make_fixture_team_features(current_year = 2025, history_years = 2024)
    current_teams <- team_data %>%
        dplyr::filter(Year == "2025")

    testthat::local_mocked_bindings(
        calculate_win_probabilities = function(...) {
            list(
                mean = 0.5,
                ci_lower = 0,
                ci_upper = 1,
                sd = 0.5,
                draws = c(0, 1),
                model_mean = 0.5,
                line_prob = NA_real_,
                blend_weight = 0,
                used_betting_line = FALSE
            )
        },
        .package = "mmBayes"
    )

    set.seed(123)
    simulation <- simulate_full_bracket(
        all_teams = current_teams,
        model_results = list(),
        draws = 2L,
        deterministic = FALSE,
        log_matchups = FALSE,
        posterior_draw_index = 2L
    )
    flattened <- flatten_matchup_results(simulation)

    expect_true(all(flattened$posterior_draw_index == 2L))
    expect_true(all(flattened$decision_prob_A == 1))
    expect_true(all(flattened$winner == flattened$teamA))
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
    expect_equal(candidates[[1]]$path_support_label, "Deterministic posterior-mean reference bracket.")
    if (length(candidates) > 1) {
        expect_match(candidates[[2]]$path_support_label, "coherent posterior-draw simulations")
    }
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
        quality_signature = quality_signature,
        allow_fallback = TRUE
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
        live_performance = live_performance,
        allow_cached_quality = TRUE
    )
    saved_matchup_totals <- utils::read.csv(decision_outputs$matchup_total_points)

    expect_true(file.exists(decision_outputs$championship_tiebreaker_summary))
    expect_true(file.exists(decision_outputs$championship_tiebreaker_distribution))
    expect_true(file.exists(decision_outputs$matchup_total_points))
    expect_true(file.exists(decision_outputs$matchup_context))
    expect_true(file.exists(decision_outputs$bracket_payload))
    expect_true(file.exists(decision_outputs$technical_payload))
    expect_true(file.exists(decision_outputs$payload_js))
    expect_null(decision_outputs$dashboard)
    expect_null(decision_outputs$technical_dashboard)
    expect_match(live_performance$status, "Monitoring only")
    expect_match(live_performance$status, "Live tournament performance through")
    expect_true(nrow(live_performance$summary) == 1)
    expect_true(nrow(live_performance$main_bracket_summary) == 1)
    expect_true(live_performance$main_bracket_games_played > 0)
    expect_identical(as.character(live_performance$games$round[[1]]), "Round of 32")
    expect_match(live_performance$recent_games_title, "Recent Games")
    live_html <- render_live_performance_html(live_performance, model_label = "Stan GLM")
    expect_match(live_html, "Live Tournament Performance - Stan GLM")
    expect_match(live_html, "Main-bracket live performance")
    expect_match(live_html, "Monitoring only")
    expect_match(live_html, "Live By Round")
    expect_match(live_html, "average pregame win probability")
    expect_match(live_html, "Avg P\\(pick wins\\)")
    expect_match(live_html, "tech-svg--live-rounds")
    expect_match(live_html, "Ordered by recorded completion time")
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

    bracket_context <- build_bracket_dashboard_context(
        current_teams = loaded$current_teams,
        decision_sheet = decision_sheet,
        candidates = candidates,
        total_points_predictions = total_predictions,
        play_in_resolution = play_in_resolution,
        dashboard_build_metadata = list(
            rendered_at_label = "April 23, 2026 at 01:15 PM EDT",
            commit_short = "abc1234",
            repo_snapshot_label = "Synced to tracked repo output in this run"
        )
    )
    bracket_payload <- build_bracket_dashboard_payload(
        bracket_year = 2026L,
        candidates = candidates,
        decision_sheet = decision_sheet,
        dashboard_context = bracket_context,
        total_points_predictions = total_predictions,
        play_in_resolution = play_in_resolution
    )
    expect_invisible(validate_dashboard_payload(bracket_payload, "bracket"))
    expect_equal(bracket_payload$build_metadata$commit_short, "abc1234")
    expect_true(is.data.frame(bracket_payload$decision_sheet))
    expect_true(is.data.frame(bracket_payload$watchlist))
    expect_true(is.data.frame(bracket_payload$divergence_map))
    expect_true(is.data.frame(bracket_payload$matchup_context))
    expect_true(is.list(bracket_payload$bracket_tree))
    expect_true(is.data.frame(bracket_payload$candidate_summaries))
    expect_true(any(bracket_payload$watchlist$reason_surface %in% c("Bracket-changing toss-ups", "Upset pivots", "Fragile favorites")))
    expect_equal(length(bracket_payload$candidates), length(candidates))

    technical_backtest <- list(
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
        ),
        round_summary = tibble::tibble(
            round = c("Round of 64", "Round of 32"),
            games = c(32L, 16L),
            accuracy = c(0.72, 0.69),
            log_loss = c(0.42, 0.45),
            brier = c(0.18, 0.19),
            empirical_rate = c(0.70, 0.68)
        ),
        yearly_metrics = tibble::tibble(year = c(2018L, 2019L, 2020L))
    )
    technical_payload <- build_technical_dashboard_payload(
        bracket_year = 2026L,
        decision_sheet = decision_sheet,
        candidates = candidates,
        model_quality_context = list(source_label = resolved_quality$source_label, backtest = technical_backtest),
        build_metadata = bracket_context$build_metadata,
        model_overview = model_overview,
        total_points_predictions = total_predictions,
        play_in_resolution = play_in_resolution,
        live_performance = live_performance
    )
    expect_invisible(validate_dashboard_payload(technical_payload, "technical"))
    expect_equal(technical_payload$model_quality$source_label, resolved_quality$source_label)
    expect_true(is.data.frame(technical_payload$ranked_decisions))
    expect_true(is.data.frame(technical_payload$candidate_differences))
    expect_true(is.data.frame(technical_payload$upset_opportunities))
    expect_true(is.data.frame(technical_payload$championship_totals$candidate_summaries))
    expect_equal(technical_payload$backtest$summary$mean_log_loss, 0.401)
    expect_true(is.data.frame(technical_payload$backtest$round_performance))

    same_winner_board <- decision_sheet %>%
        dplyr::slice_head(n = 1) %>%
        dplyr::mutate(
            candidate_diff_flag = TRUE,
            candidate_1_pick = "Duke",
            candidate_2_pick = "Duke"
        )
    expect_match(render_ranked_decision_svg(same_winner_board), "Same winner, different path")
})

test_that("candidate divergence board uses per-candidate matchup uncertainty", {
    base_matchups <- tibble::tibble(
        region = "West",
        round = "Championship",
        matchup_number = 1L,
        teamA = "Arizona",
        teamB = "Michigan",
        teamA_seed = 1L,
        teamB_seed = 1L,
        winner = "Michigan",
        win_prob_A = 0.28,
        ci_lower = 0.08,
        ci_upper = 0.58,
        prediction_sd = 0.12
    ) %>%
        augment_matchup_decisions()
    alternate_matchups <- tibble::tibble(
        region = "West",
        round = "Championship",
        matchup_number = 1L,
        teamA = "Arizona",
        teamB = "Duke",
        teamA_seed = 1L,
        teamB_seed = 1L,
        winner = "Duke",
        win_prob_A = 0.39,
        ci_lower = 0.25,
        ci_upper = 0.55,
        prediction_sd = 0.09
    ) %>%
        augment_matchup_decisions()
    candidates <- list(
        list(candidate_id = 1L, matchups = base_matchups),
        list(candidate_id = 2L, matchups = alternate_matchups)
    )
    decision_sheet <- build_decision_sheet(candidates)

    divergence_html <- render_candidate_divergence_svg(decision_sheet, candidates)

    expect_match(divergence_html, "Candidate 1 matchup uncertainty")
    expect_match(divergence_html, "Candidate 2 matchup uncertainty")
    expect_match(divergence_html, "72.0%")
    expect_match(divergence_html, "61.0%")
    expect_false(grepl("No separate posterior uncertainty was found", divergence_html, fixed = TRUE))
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
    payload_partial <- build_bracket_dashboard_payload(
        bracket_year = loaded_partial$bracket_year,
        candidates = candidates_partial,
        decision_sheet = decision_sheet_partial,
        dashboard_context = context_partial,
        play_in_resolution = play_in_resolution_partial
    )

    expect_invisible(validate_dashboard_payload(payload_partial, "bracket"))
    expect_true(isTRUE(payload_partial$play_in_resolution$has_unresolved_slots[[1]]))
    expect_true(any(context_partial$matchup_context_rows$round == "First Four"))
    expect_true(any(context_partial$divergence_map_rows$round == "First Four"))
    expect_true(any(payload_partial$matchup_context$round == "First Four"))

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
    payload_resolved <- build_bracket_dashboard_payload(
        bracket_year = loaded_resolved$bracket_year,
        candidates = candidates_resolved,
        decision_sheet = decision_sheet_resolved,
        dashboard_context = context_resolved,
        play_in_resolution = play_in_resolution_resolved
    )

    expect_invisible(validate_dashboard_payload(payload_resolved, "bracket"))
    expect_false(isTRUE(payload_resolved$play_in_resolution$has_unresolved_slots[[1]]))
    expect_true(any(context_resolved$matchup_context_rows$round == "First Four"))
    expect_false(any(context_resolved$watchlist_rows$round == "First Four"))
    expect_false(any(context_resolved$divergence_map_rows$round == "First Four"))
    expect_false(any(payload_resolved$watchlist$round == "First Four"))
    expect_false(any(payload_resolved$divergence_map$round == "First Four"))
    expect_true(any(payload_resolved$matchup_context$round == "First Four"))
})
