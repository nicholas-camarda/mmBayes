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

test_that("load_tournament_data returns matchup history and current teams", {
    team_file <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")
    fixture_paths <- write_fixture_data_files(team_file, results_file)

    config <- default_project_config()
    config$data$team_features_path <- fixture_paths$team_path
    config$data$game_results_path <- fixture_paths$results_path
    config$model$history_window <- 3L

    loaded <- load_tournament_data(config)

    expect_equal(loaded$bracket_year, "2025")
    expect_true(all(c("historical_matchups", "historical_teams", "current_teams", "historical_actual_results") %in% names(loaded)))
    expect_gt(nrow(loaded$historical_matchups), 0)
    expect_true(all(config$model$required_predictors %in% names(loaded$historical_matchups)))
    expect_false(any(leakage_columns() %in% names(loaded$historical_teams)))
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

    expect_equal(first_forward$seed_diff, -first_reverse$seed_diff)
    expect_equal(first_forward$actual_outcome, 1 - first_reverse$actual_outcome)
})
