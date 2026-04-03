test_that("team name canonicalization reconciles known source aliases", {
    aliases <- c(
        "UNC",
        "UConn",
        "Pitt",
        "California Baptist",
        "UCSB",
        "Queens (NC)",
        "Tennessee State",
        "St. John's (NY)",
        "St. Peter's",
        "North Carolina St.",
        "Louisiana Lafayette",
        "LIU Brooklyn",
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
            "Cal Baptist",
            "UC Santa Barbara",
            "Queens",
            "Tennessee St.",
            "Saint John's",
            "Saint Peter's",
            "NC State",
            "Louisiana",
            "LIU",
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

test_that("quality gates allow current-year completed results without weakening historical checks", {
    team_data <- make_fixture_team_features(current_year = 2025, history_years = 2024)
    results_data <- make_fixture_game_results(team_data, history_years = 2024)
    current_play_in_result <- tibble::tibble(
        Year = "2025",
        region = "First Four",
        round = "First Four",
        game_index = 1L,
        teamA = "East_11_2025",
        teamB = "East_11_2025_playin",
        teamA_seed = 11L,
        teamB_seed = 11L,
        teamA_score = 71L,
        teamB_score = 68L,
        total_points = 139L,
        winner = "East_11_2025"
    )
    current_round64_result <- tibble::tibble(
        Year = "2025",
        region = "East",
        round = "Round of 64",
        game_index = 1L,
        teamA = "East_01_2025",
        teamB = "East_16_2025",
        teamA_seed = 1L,
        teamB_seed = 16L,
        teamA_score = 84L,
        teamB_score = 66L,
        total_points = 150L,
        winner = "East_01_2025"
    )

    expect_no_error(
        assert_canonical_data_quality(team_data, dplyr::bind_rows(results_data, current_play_in_result, current_round64_result))
    )
})

test_that("quality gates reject impossible current-year round layouts", {
    team_data <- make_fixture_team_features(current_year = 2025, history_years = 2024)
    results_data <- make_fixture_game_results(team_data, history_years = 2024)
    impossible_current_result <- tibble::tibble(
        Year = "2025",
        region = "East",
        round = "Championship",
        game_index = 3L,
        teamA = "East_01_2025",
        teamB = "East_16_2025",
        teamA_seed = 1L,
        teamB_seed = 16L,
        teamA_score = 84L,
        teamB_score = 66L,
        total_points = 150L,
        winner = "East_01_2025"
    )

    expect_error(
        assert_canonical_data_quality(team_data, dplyr::bind_rows(results_data, impossible_current_result)),
        regexp = "impossible region/round combinations|exceed tournament round maxima|impossible game-index assignments"
    )
})

test_that("quality gates still fail on incomplete historical data", {
    team_data <- make_fixture_team_features(current_year = 2025, history_years = 2024)
    results_data <- make_fixture_game_results(team_data, history_years = 2024) %>%
        dplyr::filter(!(Year == "2024" & round == "Championship"))

    expect_error(
        assert_canonical_data_quality(team_data, results_data),
        regexp = "Unexpected per-year round counts"
    )
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
        expect_true(all(c("teamA_score", "teamB_score", "total_points") %in% names(parsed)))
        expect_true(all(parsed$total_points == parsed$teamA_score + parsed$teamB_score))
    }
})

test_that("quality gates reject score rows with inconsistent totals", {
    team_data <- make_fixture_team_features()
    results_data <- make_fixture_game_results(team_data) %>%
        dplyr::mutate(
            total_points = dplyr::if_else(row_number() == 1L, total_points + 1L, total_points)
        )

    expect_error(
        assert_canonical_data_quality(team_data, results_data),
        regexp = "Invalid score-bearing rows"
    )
})

test_that("current-year First Four fallback fills unresolved slots from a secondary source", {
    team_data <- make_fixture_team_features(current_year = 2025, history_years = 2024)
    historical_results <- make_fixture_game_results(team_data, history_years = 2024)
    partial_current_results <- tibble::tibble(
        Year = "2025",
        region = "First Four",
        round = "First Four",
        game_index = 1L,
        teamA = "East_11_2025",
        teamB = "East_11_2025_playin",
        teamA_seed = 11L,
        teamB_seed = 11L,
        teamA_score = 71L,
        teamB_score = 68L,
        total_points = 139L,
        winner = "East_11_2025"
    )
    fallback_results <- tibble::tibble(
        Year = "2025",
        region = "Midwest",
        round = "First Four",
        game_index = 99L,
        teamA = "Midwest_16_2025",
        teamB = "Midwest_16_2025_playin",
        teamA_seed = 16L,
        teamB_seed = 16L,
        teamA_score = 65L,
        teamB_score = 61L,
        total_points = 126L,
        winner = "Midwest_16_2025"
    )

    merged <- fill_current_year_first_four_results(
        game_results = dplyr::bind_rows(historical_results, partial_current_results),
        team_features = team_data,
        bracket_year = 2025L,
        fallback_results = fallback_results
    )

    current_play_in <- merged %>%
        dplyr::filter(Year == "2025", round == "First Four") %>%
        dplyr::arrange(game_index)

    expect_equal(nrow(current_play_in), 2L)
    expect_false(any(duplicated(dplyr::select(current_play_in, teamA, teamB))))
    expect_true(any(current_play_in$winner == "East_11_2025"))
    expect_true(any(current_play_in$winner == "Midwest_16_2025"))
    expect_true(all(current_play_in$total_points == current_play_in$teamA_score + current_play_in$teamB_score))
})

test_that("parser handles no-contest bracket slots without shifting later rounds", {
    west_lines <- c(
        purrr::map_chr(seq_len(7), function(idx) {
            sprintf("(1) West_R64_%02dA 80, (16) West_R64_%02dB 60", idx, idx)
        }),
        "team 7 Oregon",
        "team 10 VCU",
        purrr::map_chr(seq_len(4), function(idx) {
            sprintf("(1) West_R32_%02dA 80, (8) West_R32_%02dB 60", idx, idx)
        }),
        purrr::map_chr(seq_len(2), function(idx) {
            sprintf("(1) West_S16_%02dA 80, (4) West_S16_%02dB 60", idx, idx)
        }),
        "(1) West_E8_A 80, (2) West_E8_B 60"
    )

    lines <- c(
        "East",
        "Midwest",
        "South",
        "West",
        "National",
        purrr::map_chr(seq_len(45), function(idx) {
            sprintf("(1) Team_%02dA 80, (16) Team_%02dB 60", idx, idx)
        }),
        west_lines,
        "(11) UCLA 90, (1) Gonzaga 93",
        "(2) Houston 59, (1) Baylor 78",
        "(1) Gonzaga 70, (1) Baylor 86"
    )

    parsed <- parse_tournament_results_lines(lines, 2021L)

    expect_equal(sum(parsed$round == "Round of 64"), 31L)
    expect_equal(sum(parsed$region == "National" & parsed$round == "Final Four"), 2L)
    expect_equal(sum(parsed$region == "National" & parsed$round == "Championship"), 1L)
    expect_false(any(parsed$teamA == "Oregon" & parsed$teamB == "VCU"))
})

test_that("update_tournament_data returns success when refresh sources complete cleanly", {
    team_data <- make_fixture_team_features(current_year = 2025, history_years = 2024)
    bart_ratings <- make_fixture_bart_ratings(team_data)
    conf_assignments <- make_fixture_conf_assignments(team_data)
    historical_results <- make_fixture_game_results(team_data, history_years = 2024)
    current_results <- make_fixture_current_year_completed_results(team_data, current_year = 2025)

    team_file <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")
    config <- default_project_config()
    config$data$team_features_path <- team_file
    config$data$game_results_path <- results_file

    testthat::local_mocked_bindings(
        scrape_bart_data = function(year) {
            bart_ratings %>% dplyr::filter(Year == as.character(year))
        },
        scrape_conf_assignments = function(year) {
            conf_assignments %>% dplyr::filter(Year == as.character(year))
        },
        scrape_tournament_results = function(year) {
            historical_results %>% dplyr::filter(Year == as.character(year))
        },
        scrape_espn_tournament_results = function(...) {
            current_results
        }
    )

    result <- update_tournament_data(
        config = config,
        start_year = 2024L,
        bracket_year = 2025L,
        history_window = 1L
    )

    expect_identical(result$status, "success")
    expect_equal(result$warning_count, 0L)
    expect_equal(nrow(result$refresh_issues), 0L)
    expect_equal(nrow(result$warning_summary), 0L)
    expect_true(file.exists(result$team_features))
    expect_true(file.exists(result$game_results))
})

test_that("update_tournament_data returns degraded success for optional fallback warnings", {
    team_data <- make_fixture_team_features(current_year = 2025, history_years = 2024)
    bart_ratings <- make_fixture_bart_ratings(team_data)
    conf_assignments <- make_fixture_conf_assignments(team_data)
    historical_results <- make_fixture_game_results(team_data, history_years = 2024)

    team_file <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")
    config <- default_project_config()
    config$data$team_features_path <- team_file
    config$data$game_results_path <- results_file

    testthat::local_mocked_bindings(
        scrape_bart_data = function(year) {
            bart_ratings %>% dplyr::filter(Year == as.character(year))
        },
        scrape_conf_assignments = function(year) {
            conf_assignments %>% dplyr::filter(Year == as.character(year))
        },
        scrape_tournament_results = function(year) {
            historical_results %>% dplyr::filter(Year == as.character(year))
        },
        scrape_espn_tournament_results = function(...) {
            stop("ESPN scoreboard temporarily unavailable")
        }
    )

    result <- update_tournament_data(
        config = config,
        start_year = 2024L,
        bracket_year = 2025L,
        history_window = 1L
    )

    expect_identical(result$status, "degraded_success")
    expect_gte(result$warning_count, 1L)
    expect_true(file.exists(result$team_features))
    expect_true(file.exists(result$game_results))
    expect_true(any(result$refresh_issues$step == "current_year_scoreboard_fallback"))
    expect_true(any(grepl("ESPN scoreboard temporarily unavailable", result$refresh_issues$message, fixed = TRUE)))
    expect_true(any(result$warning_summary$step == "current_year_scoreboard_fallback"))
})

test_that("update_tournament_data degrades when current-year fallback is empty but allowed", {
    team_data <- make_fixture_team_features(current_year = 2025, history_years = 2024)
    bart_ratings <- make_fixture_bart_ratings(team_data)
    conf_assignments <- make_fixture_conf_assignments(team_data)
    historical_results <- make_fixture_game_results(team_data, history_years = 2024)

    team_file <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")
    config <- default_project_config()
    config$data$team_features_path <- team_file
    config$data$game_results_path <- results_file

    testthat::local_mocked_bindings(
        scrape_bart_data = function(year) {
            bart_ratings %>% dplyr::filter(Year == as.character(year))
        },
        scrape_conf_assignments = function(year) {
            conf_assignments %>% dplyr::filter(Year == as.character(year))
        },
        scrape_tournament_results = function(year) {
            historical_results %>% dplyr::filter(Year == as.character(year))
        },
        scrape_espn_tournament_results = function(...) {
            empty_game_results_table()
        }
    )

    result <- update_tournament_data(
        config = config,
        start_year = 2024L,
        bracket_year = 2025L,
        history_window = 1L
    )

    expect_identical(result$status, "degraded_success")
    expect_true(any(grepl("No completed current-year fallback results were available", result$refresh_issues$message, fixed = TRUE)))

    summary_lines <- format_refresh_status_summary(result, log_path = "/tmp/data_refresh.log")
    expect_true(any(grepl("^Refresh status: Degraded success$", summary_lines)))
    expect_true(any(grepl("Warnings: 1", summary_lines, fixed = TRUE)))
    expect_true(any(grepl("/tmp/data_refresh.log", summary_lines, fixed = TRUE)))
})

test_that("update_tournament_data still fails on required source errors", {
    team_file <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")
    config <- default_project_config()
    config$data$team_features_path <- team_file
    config$data$game_results_path <- results_file

    testthat::local_mocked_bindings(
        scrape_bart_data = function(year) {
            stop(sprintf("Could not pass browser verification for %s", year))
        }
    )

    expect_error(
        update_tournament_data(
            config = config,
            start_year = 2024L,
            bracket_year = 2025L,
            history_window = 1L
        ),
        regexp = "Could not pass browser verification"
    )
})
