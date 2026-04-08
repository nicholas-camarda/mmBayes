test_that("fixture betting history refuses to write into canonical runtime or cloud roots", {
    team_data <- make_fixture_team_features(current_year = 2025, history_years = 2023:2024)
    results_data <- make_fixture_game_results(team_data, history_years = 2023:2024)

    expect_error(
        write_fixture_betting_history(default_runtime_history_root(), team_data, results_data, current_year = 2025),
        "Refusing to write synthetic betting history"
    )
    expect_error(
        write_fixture_betting_history(default_cloud_history_root(), team_data, results_data, current_year = 2025),
        "Refusing to write synthetic betting history"
    )
})

test_that("fixture betting history writes successfully inside a temp sandbox", {
    team_data <- make_fixture_team_features(current_year = 2025, history_years = 2023:2024)
    results_data <- make_fixture_game_results(team_data, history_years = 2023:2024)
    history_dir <- file.path(tempdir(), "fixture-history-safe")

    write_fixture_betting_history(history_dir, team_data, results_data, current_year = 2025)

    expect_true(file.exists(file.path(history_dir, "2023", "closing_lines.csv")))
    expect_true(file.exists(file.path(history_dir, "2024", "closing_lines.csv")))
    expect_true(file.exists(file.path(history_dir, "2025", "latest_lines_matchups.csv")))
})
