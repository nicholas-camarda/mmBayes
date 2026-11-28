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
})

test_that("production winner model is Barthag-only over all seasons", {
    config <- default_project_config()
    expect_identical(as.character(unlist(config$model$required_predictors)), "barthag_logit_diff")
    expect_length(unlist(config$model$interaction_terms), 0L)
    expect_identical(config$model$engine, "stan_glm")
    expect_null(config$model$ensemble)

    repo_config <- load_project_config(test_path("..", "..", "config.yml"))
    expect_identical(as.character(unlist(repo_config$model$required_predictors)), "barthag_logit_diff")
    expect_identical(history_window_label(repo_config$model$history_window), "all")
})

test_that("history window accepts a season count or all", {
    expect_equal(resolve_history_window(8), 8L)
    expect_equal(resolve_history_window("all"), Inf)
    expect_error(resolve_history_window(0), "positive whole number")
    expect_error(resolve_history_window("most"), "positive whole number")
    expect_identical(select_historical_years(
        tibble::tibble(Year = c("2008", "2019", "2021", "2026")),
        tibble::tibble(Year = c("2008", "2019", "2021")),
        bracket_year = "2026",
        history_window = "all"
    ), c("2008", "2019", "2021"))
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
    expect_equal(loaded$configured_history_window, "3")
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

test_that("historical matchup builder keeps one row per game, preserves first four, and orients rows consistently", {
    team_data <- make_fixture_team_features(history_years = 2024, current_year = 2025)
    results_data <- make_fixture_game_results(team_data, history_years = 2024)

    historical_teams <- team_data %>%
        dplyr::filter(Year == "2024")
    historical_results <- results_data %>%
        dplyr::filter(Year == "2024")

    matchups <- build_explicit_matchup_history(historical_teams, historical_results)

    expect_equal(nrow(matchups), nrow(historical_results))
    expect_true(any(matchups$round == "First Four"))

    features <- normalize_team_features(historical_teams)
    seeds <- stats::setNames(features$Seed, features$Team)
    barthag <- stats::setNames(features$barthag_logit, features$Team)
    expect_equal(matchups$seed_diff, unname(seeds[matchups$teamA] - seeds[matchups$teamB]))
    expect_equal(matchups$barthag_logit_diff, unname(barthag[matchups$teamA] - barthag[matchups$teamB]))
    expect_equal(matchups$actual_outcome, as.numeric(matchups$winner == matchups$teamA))
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
