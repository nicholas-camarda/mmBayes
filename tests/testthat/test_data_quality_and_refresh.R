test_that("team name canonicalization reconciles known source aliases", {
    aliases <- c(
        "UNC",
        "UConn",
        "Pitt",
        "UCSB",
        "St. John's (NY)",
        "St. Peter's",
        "Texas A&M Corpus Chris",
        "Wichita St.",
        "Miami FL"
    )

    expect_equal(
        canonicalize_team_name(aliases),
        c(
            "North Carolina",
            "Connecticut",
            "Pittsburgh",
            "UC Santa Barbara",
            "Saint John's",
            "Saint Peter's",
            "Texas A&M Corpus Christi",
            "Wichita State",
            "Miami (FL)"
        )
    )
    expect_equal(normalize_team_key("UNC"), normalize_team_key("North Carolina"))
    expect_equal(normalize_team_key("UConn"), normalize_team_key("Connecticut"))
    expect_equal(normalize_team_key("St. John's (NY)"), normalize_team_key("Saint John's"))
})

test_that("loader succeeds when result teams use aliased source names", {
    team_file <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")

    team_data <- make_fixture_team_features()
    team_data <- team_data %>%
        dplyr::mutate(
            Team = dplyr::case_when(
                Region == "East" & Seed == 1L ~ "North Carolina",
                Region == "West" & Seed == 1L ~ "Connecticut",
                Region == "South" & Seed == 1L ~ "Pittsburgh",
                Region == "Midwest" & Seed == 1L ~ "Saint John's",
                TRUE ~ Team
            )
        )
    results_data <- make_fixture_game_results(team_data) %>%
        dplyr::mutate(
            teamA = dplyr::recode(
                teamA,
                "North Carolina" = "UNC",
                "Connecticut" = "UConn",
                "Pittsburgh" = "Pitt",
                "Saint John's" = "St. John's (NY)"
            ),
            teamB = dplyr::recode(
                teamB,
                "North Carolina" = "UNC",
                "Connecticut" = "UConn",
                "Pittsburgh" = "Pitt",
                "Saint John's" = "St. John's (NY)"
            ),
            winner = dplyr::recode(
                winner,
                "North Carolina" = "UNC",
                "Connecticut" = "UConn",
                "Pittsburgh" = "Pitt",
                "Saint John's" = "St. John's (NY)"
            )
        )

    write_fixture_data_files(team_file, results_file, team_data = team_data, results_data = results_data)

    config <- default_project_config()
    config$data$team_features_path <- team_file
    config$data$game_results_path <- results_file
    config$model$history_window <- 3L

    loaded <- load_tournament_data(config)

    expect_gt(nrow(loaded$historical_matchups), 0)
    expect_gt(nrow(loaded$current_teams), 0)
    expect_true(all(c("historical_matchups", "historical_teams", "current_teams") %in% names(loaded)))
})

test_that("year-wide Bart join preserves the full tournament roster", {
    team_data <- make_fixture_team_features(current_year = 2025, history_years = 2024)
    conf_assignments <- make_fixture_conf_assignments(team_data)
    bart_ratings <- make_fixture_bart_ratings(team_data)

    dataset <- build_team_feature_dataset(bart_ratings, conf_assignments)

    expect_equal(dataset %>% dplyr::count(Year) %>% dplyr::pull(n), c(68L, 68L))
    expect_true(all(pre_tournament_feature_columns() %in% names(dataset)))
    expect_equal(
        dataset %>%
            dplyr::count(Year, Region, Seed, name = "n") %>%
            dplyr::filter(n > 1) %>%
            dplyr::count(Year, name = "duplicate_slots") %>%
            dplyr::pull(duplicate_slots),
        c(4L, 4L)
    )
})

test_that("year-wide Bart join fails fast when ratings miss tournament teams", {
    team_data <- make_fixture_team_features(current_year = 2025, history_years = 2024)
    conf_assignments <- make_fixture_conf_assignments(team_data)
    bart_ratings <- make_fixture_bart_ratings(team_data) %>%
        dplyr::filter(!(Year == "2024" & Team == "East_01_2024"))

    expect_error(
        build_team_feature_dataset(bart_ratings, conf_assignments),
        regexp = "did not cover every tournament team"
    )
})

test_that("year-wide Bart join fails fast on duplicate rating rows", {
    team_data <- make_fixture_team_features(current_year = 2025, history_years = 2024)
    conf_assignments <- make_fixture_conf_assignments(team_data)
    bart_ratings <- make_fixture_bart_ratings(team_data)
    duplicate_row <- bart_ratings %>%
        dplyr::filter(Year == "2024", Team == "East_01_2024")

    expect_error(
        build_team_feature_dataset(dplyr::bind_rows(bart_ratings, duplicate_row), conf_assignments),
        regexp = "duplicate teams for the same year"
    )
})

test_that("quality gates flag suspicious First Four rows and round-count drift", {
    team_data <- make_fixture_team_features()
    results_data <- make_fixture_game_results(team_data) %>%
        dplyr::mutate(
            region = dplyr::if_else(round == "Championship", "First Four", region),
            round = dplyr::if_else(round == "Championship", "First Four", round),
            game_index = dplyr::if_else(region == "First Four" & round == "First Four" & teamA_seed == 1L & teamB_seed == 1L, 5L, game_index)
        )

    expect_error(
        assert_canonical_data_quality(team_data, results_data),
        regexp = "Suspicious First Four rows|Unexpected per-year round counts"
    )
})

test_that("parser keeps national 1-seed games out of First Four for affected years", {
    for (year in c(2018L, 2021L, 2024L, 2025L)) {
        parsed <- parse_tournament_results_lines(make_parser_fixture_lines(year), year)
        counts <- parsed %>%
            dplyr::count(round, name = "games")

        expected <- tibble::tibble(
            round = names(expected_completed_round_counts()),
            games = unname(expected_completed_round_counts())
        )

        expect_equal(
            counts %>% dplyr::arrange(round),
            expected %>% dplyr::arrange(round)
        )
        expect_false(any(parsed$region == "First Four" & parsed$teamA_seed == 1L & parsed$teamB_seed == 1L))
        expect_equal(sum(parsed$region == "National" & parsed$round == "Final Four"), 2)
        expect_equal(sum(parsed$region == "National" & parsed$round == "Championship"), 1)
    }
})
