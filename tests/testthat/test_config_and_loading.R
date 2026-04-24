test_that("config loader merges matchup-model defaults with yaml values", {
    file <- tempfile(fileext = ".yml")
    writeLines(
        c(
            "default:",
            "  data:",
            "    team_features_path: custom_teams.xlsx",
            "  model:",
            "    n_draws: 25",
            "    history_window: 5",
            "  output:",
            "    prefix: custom_prefix"
        ),
        file
    )

    config <- load_project_config(file)

    expect_equal(config$data$team_features_path, "custom_teams.xlsx")
    expect_equal(config$model$n_draws, 25)
    expect_equal(config$model$history_window, 5)
    expect_equal(config$output$prefix, "custom_prefix")
    expect_true(length(config$model$required_predictors) > 0)
    expect_true(isTRUE(config$model$ensemble$enabled))
    expect_equal(config$model$ensemble$combiner, "logit_weight")
})

test_that("ensemble configuration validation rejects unsupported primary settings", {
    file <- tempfile(fileext = ".yml")
    writeLines(
        c(
            "default:",
            "  model:",
            "    ensemble:",
            "      enabled: true",
            "      component_engines:",
            "        - stan_glm",
            "      combiner: logit_weight"
        ),
        file
    )

    expect_error(
        load_project_config(file),
        "component_engines"
    )
})

test_that("default predictor contracts exclude outcome, score, betting, and leakage fields", {
    config <- default_project_config()
    disallowed <- c(
        "winner",
        "actual_outcome",
        "teamA_score",
        "teamB_score",
        "total_points",
        leakage_columns(),
        betting_matchup_feature_columns(),
        betting_total_points_feature_columns()
    )

    expect_false(any(config$model$required_predictors %in% disallowed))
    expect_false(any(core_matchup_predictor_columns(config$model$required_predictors) %in% disallowed))
    expect_false(any(core_total_points_predictor_columns() %in% disallowed))
})

test_that("load_tournament_data returns matchup history and current teams", {
    team_file <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")
    team_data <- make_fixture_team_features(current_year = 2025, history_years = 2022:2024)
    results_data <- dplyr::bind_rows(
        make_fixture_game_results(team_data, history_years = 2022:2024),
        make_fixture_current_year_completed_results(team_data, current_year = 2025)
    )
    fixture_paths <- write_fixture_data_files(team_file, results_file, team_data = team_data, results_data = results_data)

    config <- default_project_config()
    config$data$team_features_path <- fixture_paths$team_path
    config$data$game_results_path <- fixture_paths$results_path
    config$model$history_window <- 3L

    loaded <- load_tournament_data(config)

    expect_equal(loaded$bracket_year, "2025")
    expect_true(all(c("historical_matchups", "historical_teams", "historical_games", "current_teams", "historical_actual_results", "current_play_in_results", "current_completed_results") %in% names(loaded)))
    expect_equal(loaded$configured_history_window, 3L)
    expect_equal(loaded$effective_historical_years, 3L)
    expect_equal(loaded$history_summary$historical_years[[1]], "2022, 2023, 2024")
    expect_gt(nrow(loaded$historical_matchups), 0)
    expect_gt(nrow(loaded$historical_games), 0)
    expect_true(all(config$model$required_predictors %in% names(loaded$historical_matchups)))
    expect_false(any(leakage_columns() %in% names(loaded$historical_teams)))
    expect_true(all(c("teamA_score", "teamB_score", "total_points") %in% names(loaded$historical_games)))
    expect_true(all(c("teamA_score", "teamB_score", "total_points") %in% names(loaded$historical_actual_results)))
    expect_true(all(loaded$current_play_in_results$round == "First Four"))
    expect_gt(nrow(loaded$current_completed_results), 0)
    expect_true(any(loaded$current_completed_results$round != "First Four"))
    expect_false(any(is.na(loaded$historical_games$total_points)))
})

test_that("historical matchup builder duplicates rows symmetrically and preserves first four", {
    team_data <- make_fixture_team_features(history_years = 2024, current_year = 2025)
    results_data <- make_fixture_game_results(team_data, history_years = 2024)

    historical_teams <- team_data %>%
        dplyr::filter(Year == "2024")
    historical_results <- results_data %>%
        dplyr::filter(Year == "2024")

    matchups <- build_explicit_matchup_history(historical_teams, historical_results)

    forward_count <- nrow(historical_results)
    expect_equal(nrow(matchups), forward_count * 2)
    expect_true(any(matchups$round == "First Four"))

    first_forward <- matchups %>% dplyr::slice(1)
    first_reverse <- matchups %>% dplyr::filter(
        Year == first_forward$Year,
        round == first_forward$round,
        game_index == first_forward$game_index,
        teamA == first_forward$teamB,
        teamB == first_forward$teamA
    ) %>%
        dplyr::slice(1)

    diff_columns <- c(
        "seed_diff",
        "barthag_logit_diff",
        "AdjOE_diff",
        "AdjDE_diff",
        "WAB_diff",
        "TOR_diff",
        "TORD_diff",
        "ORB_diff",
        "DRB_diff",
        "3P%_diff",
        "3P%D_diff",
        "Adj T._diff"
    )
    for (column in diff_columns) {
        expect_equal(first_forward[[column]], -first_reverse[[column]], info = column)
    }
    expect_equal(first_forward$same_conf, first_reverse$same_conf)
    expect_equal(first_forward$actual_outcome, 1 - first_reverse$actual_outcome)
})

test_that("load_tournament_data is invariant to input row order", {
    team_data <- make_fixture_team_features(current_year = 2025, history_years = 2022:2024)
    results_data <- dplyr::bind_rows(
        make_fixture_game_results(team_data, history_years = 2022:2024),
        make_fixture_current_year_completed_results(team_data, current_year = 2025)
    )

    ordered_paths <- write_fixture_data_files(
        tempfile(fileext = ".xlsx"),
        tempfile(fileext = ".xlsx"),
        team_data = team_data,
        results_data = results_data
    )
    shuffled_paths <- write_fixture_data_files(
        tempfile(fileext = ".xlsx"),
        tempfile(fileext = ".xlsx"),
        team_data = team_data[sample.int(nrow(team_data)), ],
        results_data = results_data[sample.int(nrow(results_data)), ]
    )

    config <- default_project_config()
    config$model$history_window <- 3L
    config$data$team_features_path <- ordered_paths$team_path
    config$data$game_results_path <- ordered_paths$results_path
    ordered <- load_tournament_data(config)

    config$data$team_features_path <- shuffled_paths$team_path
    config$data$game_results_path <- shuffled_paths$results_path
    shuffled <- load_tournament_data(config)

    key_cols <- c("Year", "region", "round", "game_index", "teamA", "teamB", "actual_outcome")
    ordered_keys <- ordered$historical_matchups %>%
        dplyr::select(dplyr::all_of(key_cols)) %>%
        dplyr::arrange(dplyr::across(dplyr::everything()))
    shuffled_keys <- shuffled$historical_matchups %>%
        dplyr::select(dplyr::all_of(key_cols)) %>%
        dplyr::arrange(dplyr::across(dplyr::everything()))

    expect_equal(ordered$historical_years, shuffled$historical_years)
    expect_equal(ordered$current_teams %>% dplyr::arrange(Region, Seed, Team) %>% dplyr::pull(Team), shuffled$current_teams %>% dplyr::arrange(Region, Seed, Team) %>% dplyr::pull(Team))
    expect_equal(ordered_keys, shuffled_keys)
})
