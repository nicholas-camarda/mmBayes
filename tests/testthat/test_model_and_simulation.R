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
    expect_equal(model_results$engine, "bayes")
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

    expect_equal(p_ab + p_ba, 1, tolerance = 0.1)
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

    expect_true(length(candidates) >= 1)
    expect_true(all(c("decision_rank", "confidence_tier", "upset_leverage") %in% names(candidates[[1]]$matchups)))
    expect_true(all(c("candidate_1_pick", "candidate_2_pick", "candidate_diff_flag", "decision_score") %in% names(decision_sheet)))
    expect_equal(
        as.character(unique(decision_sheet$region[!is.na(decision_sheet$region)])),
        c("East", "South", "Midwest", "West")
    )
    expect_true(all(decision_sheet$confidence_tier %in% c("Lock", "Lean", "Toss-up", "Volatile")))

    dashboard_html <- create_bracket_dashboard_html(
        bracket_year = 2026L,
        decision_sheet = decision_sheet,
        candidates = candidates,
        backtest = NULL,
        play_in_resolution = play_in_resolution
    )
    expect_match(dashboard_html, "<th>winner</th>")
    expect_match(dashboard_html, "<th>winner</th><th>matchup</th>")
    if (isTRUE(play_in_resolution$has_unresolved_slots[[1]])) {
        expect_match(dashboard_html, "Status: Simulated bracket path")
        expect_match(dashboard_html, "generated brackets assume simulated First Four winners")
    } else {
        expect_match(dashboard_html, "Status: Final result")
        expect_match(dashboard_html, "First Four slots are resolved")
    }
    safe_pos <- regexpr("Safe Bracket #1 Sequence", dashboard_html, fixed = TRUE)[[1]]
    alternate_pos <- regexpr("Bracket #2 Sequence", dashboard_html, fixed = TRUE)[[1]]
    expect_true(safe_pos > 0)
    expect_true(alternate_pos > safe_pos)

    safe_chunk <- substr(dashboard_html, safe_pos, alternate_pos - 1L)
    firstfour_pos <- regexpr("First Four", safe_chunk, fixed = TRUE)[[1]]
    round64_pos <- regexpr("Round of 64", safe_chunk, fixed = TRUE)[[1]]
    round32_pos <- regexpr("Round of 32", safe_chunk, fixed = TRUE)[[1]]
    sweet16_pos <- regexpr("Sweet 16", safe_chunk, fixed = TRUE)[[1]]
    elite8_pos <- regexpr("Elite 8", safe_chunk, fixed = TRUE)[[1]]
    finalfour_pos <- regexpr("Final Four", safe_chunk, fixed = TRUE)[[1]]
    championship_pos <- regexpr("Championship", safe_chunk, fixed = TRUE)[[1]]

    expect_true(firstfour_pos > 0)
    expect_true(firstfour_pos < round64_pos)
    expect_true(round64_pos < round32_pos)
    expect_true(round32_pos < sweet16_pos)
    expect_true(sweet16_pos < elite8_pos)
    expect_true(elite8_pos < finalfour_pos)
    expect_true(finalfour_pos < championship_pos)
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
    html_partial <- create_bracket_dashboard_html(
        bracket_year = loaded_partial$bracket_year,
        decision_sheet = build_decision_sheet(candidates_partial),
        candidates = candidates_partial,
        backtest = NULL,
        play_in_resolution = summarize_play_in_resolution(loaded_partial$current_teams, loaded_partial$current_play_in_results)
    )

    expect_match(html_partial, "Warning: First Four still simulated")

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
    html_resolved <- create_bracket_dashboard_html(
        bracket_year = loaded_resolved$bracket_year,
        decision_sheet = build_decision_sheet(candidates_resolved),
        candidates = candidates_resolved,
        backtest = NULL,
        play_in_resolution = summarize_play_in_resolution(loaded_resolved$current_teams, loaded_resolved$current_play_in_results)
    )

    expect_no_match(html_resolved, "Warning: First Four still simulated")
})
