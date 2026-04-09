test_that("OddsPapi v4 historical parser derives moneyline and spread", {
    markets_catalog <- tibble::tribble(
        ~market_id, ~outcome_id, ~outcome_name, ~market_type, ~market_name, ~market_name_short, ~period, ~handicap, ~market_length, ~player_prop, ~sport_id,
        111L, 111L, "1", "moneyline", "Winner", "Winner", "result", 0, 2L, FALSE, 11L,
        111L, 112L, "2", "moneyline", "Winner", "Winner", "result", 0, 2L, FALSE, 11L,
        211L, 211L, "1", "spread", "Spread", "Spread", "result", -5.5, 2L, FALSE, 11L,
        211L, 212L, "2", "spread", "Spread", "Spread", "result", -5.5, 2L, FALSE, 11L
    )

    historical_response <- list(
        fixtureId = "fixture-1",
        startTime = "2025-03-20T18:00:00.000Z",
        participant1Name = "Duke Blue Devils",
        participant2Name = "Siena Saints",
        bookmakers = list(
            pinnacle = list(
                markets = list(
                    "111" = list(
                        outcomes = list(
                            "111" = list(players = list("0" = list(
                                list(price = 1.25, active = TRUE, createdAt = "2025-03-20T17:50:00.000000+00:00")
                            ))),
                            "112" = list(players = list("0" = list(
                                list(price = 4.40, active = TRUE, createdAt = "2025-03-20T17:50:00.000000+00:00")
                            )))
                        )
                    ),
                    "211" = list(
                        outcomes = list(
                            "211" = list(players = list("0" = list(
                                list(price = 1.91, active = TRUE, createdAt = "2025-03-20T17:49:00.000000+00:00")
                            ))),
                            "212" = list(players = list("0" = list(
                                list(price = 1.91, active = TRUE, createdAt = "2025-03-20T17:49:00.000000+00:00")
                            )))
                        )
                    )
                )
            )
        )
    )

    derived <- derive_oddspapi_bookmaker_closing_line(historical_response, markets_catalog)

    expect_equal(derived$bookmaker[[1]], "pinnacle")
    expect_true(derived$moneyline_available[[1]])
    expect_true(derived$spread_available[[1]])
    expect_equal(derived$participant1[[1]], "Duke Blue Devils")
    expect_equal(derived$spread_participant1[[1]], -5.5)
    expect_equal(round(derived$prob_participant1[[1]] + derived$prob_participant2[[1]], 6), 1)
})

test_that("OddsPapi import defaults to eligible post-2026 seasons only", {
    historical <- resolve_oddspapi_historical_config(default_project_config())

    resolved <- resolve_oddspapi_import_years(
        historical = historical,
        bracket_year = 2026L,
        requested_years = NULL,
        reference_date = as.Date("2026-04-08")
    )

    expect_equal(resolved$eligible_years, 2026L)
    expect_equal(nrow(resolved$skipped_years), 0L)

    explicitly_old <- resolve_oddspapi_import_years(
        historical = historical,
        bracket_year = 2026L,
        requested_years = c(2025L, 2026L, 2027L),
        reference_date = as.Date("2026-04-08")
    )

    expect_equal(explicitly_old$eligible_years, 2026L)
    expect_equal(
        explicitly_old$skipped_years$reason,
        c("before_archive_start_year", "future_season")
    )
})

test_that("OddsPapi historical config honors explicit bookmaker and window overrides", {
    config <- default_project_config()
    config$betting$historical$bookmaker_sets <- list(c("pinnacle"))
    config$betting$historical$date_windows <- list(list(from = "03-15", to = "04-10"))

    historical <- resolve_oddspapi_historical_config(config)

    expect_equal(historical$bookmaker_sets, list(c("pinnacle")))
    expect_equal(historical$date_windows, list(list(from = "03-15", to = "04-10")))
})

test_that("OddsPapi fixture reconciliation matches canonical teams with mascot suffixes", {
    results_year <- tibble::tibble(
        Year = "2025",
        region = "East",
        round = "Round of 64",
        game_index = 1L,
        teamA = "Michigan",
        teamB = "UC San Diego",
        winner = "Michigan"
    )

    fixtures <- list(
        list(
            fixtureId = "fixture-123",
            startTime = "2025-03-20T19:00:00.000Z",
            participant1Name = "Michigan Wolverines",
            participant2Name = "UC San Diego Tritons",
            seasonId = 1L,
            tournamentName = "NCAA Division I National Championship"
        )
    )

    reconciled <- reconcile_oddspapi_fixtures_to_results(fixtures, results_year)

    expect_equal(nrow(reconciled$fixture_table), 1L)
    expect_equal(reconciled$matched$fixture_id[[1]], "fixture-123")
    expect_equal(reconciled$matched$participant1[[1]], "Michigan")
    expect_equal(reconciled$matched$participant2[[1]], "UC San Diego")
})

test_that("historical betting features preserve missing-line seasons gracefully", {
    closing_lines <- tibble::tibble(
        Year = "2026",
        round = "Round of 64",
        teamA = "Duke",
        teamB = "Siena",
        implied_prob_teamA = NA_real_,
        spread_teamA = NA_real_,
        n_bookmakers = NA_integer_,
        closing_snapshot_time_utc = as.POSIXct(NA, tz = "UTC"),
        commence_time_utc = as.POSIXct(NA, tz = "UTC"),
        prob_dispersion_a = NA_real_,
        spread_dispersion_a = NA_real_
    )

    features <- build_historical_betting_feature_table(closing_lines)

    expect_equal(features$line_available[[1]], 0)
    expect_equal(features$implied_prob_teamA[[1]], 0.5)
    expect_equal(features$spread_teamA[[1]], 0)
    expect_equal(features$n_bookmakers[[1]], 0)
})

test_that("historical closing-lines loader tolerates OddsPapi diagnostic columns", {
    team_file <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")
    runtime_root <- tempfile(pattern = "mmBayes-runtime-")

    team_data <- make_fixture_team_features(current_year = 2027, history_years = 2025:2026)
    results_data <- dplyr::bind_rows(
        make_fixture_game_results(team_data, history_years = 2025:2026),
        make_fixture_current_year_completed_results(team_data, current_year = 2027)
    )
    fixture_paths <- write_fixture_data_files(team_file, results_file, team_data = team_data, results_data = results_data)

    config <- default_project_config()
    config$data$team_features_path <- fixture_paths$team_path
    config$data$game_results_path <- fixture_paths$results_path
    config$runtime$root <- runtime_root
    config$betting$history_dir <- file.path(runtime_root, "data", "odds_history")
    config$output$path <- file.path(runtime_root, "output")
    config <- normalize_project_paths(config)

    write_fixture_betting_history(config$betting$history_dir, team_data, results_data, current_year = 2027)

    closing_paths <- list.files(config$betting$history_dir, pattern = "^closing_lines\\.csv$", recursive = TRUE, full.names = TRUE)
    expect_true(length(closing_paths) > 0)
    closing_tbl <- utils::read.csv(closing_paths[[1]], stringsAsFactors = FALSE)
    closing_tbl$provider_fixture_id <- "fixture-123"
    closing_tbl$provider_gap_reason <- NA_character_
    closing_tbl$provider_recovery_attempt <- 1L
    utils::write.csv(closing_tbl, closing_paths[[1]], row.names = FALSE)

    loaded <- load_tournament_data(config, include_betting_history = TRUE)

    expect_true(nrow(loaded$historical_betting_features) > 0)
    expect_true(all(betting_matchup_feature_columns() %in% names(loaded$historical_matchups)))
})

test_that("historical closing-lines loader ignores stale or unusable OddsPapi artifacts", {
    history_dir <- tempfile(pattern = "odds-history-")
    dir.create(history_dir, recursive = TRUE, showWarnings = FALSE)

    stale_paths <- build_odds_history_paths(2025L, history_dir = history_dir)
    ensure_odds_history_dirs(stale_paths)
    utils::write.csv(
        tibble::tibble(
            Year = "2025",
            round = "Round of 64",
            teamA = "Duke",
            teamB = "Siena",
            implied_prob_teamA = NA_real_,
            spread_teamA = NA_real_,
            n_bookmakers = NA_integer_
        ),
        stale_paths$closing_lines,
        row.names = FALSE
    )

    current_paths <- build_odds_history_paths(2026L, history_dir = history_dir)
    ensure_odds_history_dirs(current_paths)
    utils::write.csv(
        tibble::tibble(
            Year = "2026",
            round = "Round of 64",
            teamA = c("Duke", "Arizona"),
            teamB = c("Siena", "Akron"),
            implied_prob_teamA = c(0.82, NA_real_),
            spread_teamA = c(-12.5, NA_real_),
            n_bookmakers = c(3L, NA_integer_)
        ),
        current_paths$closing_lines,
        row.names = FALSE
    )

    loaded <- load_historical_closing_lines(
        history_dir,
        min_year = 2026L,
        require_usable_lines = TRUE
    )

    expect_equal(nrow(loaded), 1L)
    expect_equal(unique(loaded$Year), 2026L)
    expect_equal(loaded$teamA[[1]], "Duke")
})

test_that("historical closing-lines loader ignores empty stale years with mismatched column types", {
    history_dir <- tempfile(pattern = "odds-history-")
    dir.create(history_dir, recursive = TRUE, showWarnings = FALSE)

    stale_paths <- build_odds_history_paths(2025L, history_dir = history_dir)
    ensure_odds_history_dirs(stale_paths)
    utils::write.csv(
        tibble::tibble(
            Year = "2025",
            round = "Round of 64",
            teamA = "Duke",
            teamB = "Siena",
            implied_prob_teamA = NA_real_,
            spread_teamA = NA_real_,
            n_bookmakers = NA_integer_,
            closing_snapshot_time_utc = NA,
            commence_time_utc = NA
        ),
        stale_paths$closing_lines,
        row.names = FALSE
    )

    current_paths <- build_odds_history_paths(2026L, history_dir = history_dir)
    ensure_odds_history_dirs(current_paths)
    utils::write.csv(
        tibble::tibble(
            Year = "2026",
            round = "Round of 64",
            teamA = "Duke",
            teamB = "Siena",
            implied_prob_teamA = 0.82,
            spread_teamA = -12.5,
            n_bookmakers = 3L,
            closing_snapshot_time_utc = "2026-03-20 17:59:00",
            commence_time_utc = "2026-03-20 18:10:00"
        ),
        current_paths$closing_lines,
        row.names = FALSE
    )

    loaded <- load_historical_closing_lines(
        history_dir,
        min_year = 2026L,
        require_usable_lines = TRUE
    )

    expect_equal(nrow(loaded), 1L)
    expect_equal(loaded$Year[[1]], 2026L)
    expect_true(is.character(loaded$closing_snapshot_time_utc))
})

test_that("import-summary coverage reports provider-limited gaps and separate market rates", {
    import_summary <- tibble::tibble(
        provider_fixture_id = c("fixture-1", "fixture-2", NA_character_),
        recovered_moneyline = c(TRUE, TRUE, FALSE),
        recovered_spread = c(TRUE, FALSE, FALSE),
        provider_gap_reason = c(NA_character_, "spread_not_found", "fixture_not_found")
    )

    coverage <- summarize_import_summary_coverage(import_summary)

    expect_equal(coverage$target_games, 3L)
    expect_equal(coverage$matched_games, 2L)
    expect_equal(coverage$recovered_moneyline_games, 2L)
    expect_equal(coverage$recovered_spread_games, 1L)
    expect_equal(coverage$recovered_both_games, 1L)
    expect_equal(coverage$season_status, "provider-limited")
    expect_equal(coverage$moneyline_completeness_rate, 2 / 3)
    expect_equal(coverage$spread_completeness_rate, 1 / 3)
    expect_equal(coverage$full_completeness_rate, 1 / 3)
    expect_equal(coverage$provider_gap_reasons$fixture_not_found, 1L)
})

test_that("current-year closing-line coverage requires both moneyline and spread for complete status", {
    team_data <- make_fixture_team_features(current_year = 2026, history_years = 2024:2025)
    actual_results <- make_fixture_current_year_completed_results(team_data, current_year = 2026)
    complete_lines <- make_fixture_closing_lines(team_data, actual_results) %>%
        dplyr::filter(Year == "2026")

    complete_coverage <- summarize_current_year_closing_line_coverage(complete_lines, actual_results)
    expect_equal(complete_coverage$season_status, "complete")
    expect_equal(complete_coverage$moneyline_completeness_rate, 1)
    expect_equal(complete_coverage$spread_completeness_rate, 1)
    expect_equal(complete_coverage$full_completeness_rate, 1)

    partial_lines <- complete_lines
    partial_lines$spread_teamA[[1]] <- NA_real_
    partial_coverage <- summarize_current_year_closing_line_coverage(partial_lines, actual_results)
    expect_equal(partial_coverage$season_status, "partial")
    expect_equal(partial_coverage$moneyline_completeness_rate, 1)
    expect_lt(partial_coverage$spread_completeness_rate, 1)
    expect_lt(partial_coverage$full_completeness_rate, 1)
})

test_that("historical betting impact evaluation reports deployable and retrospective analyses when complete coverage exists", {
    team_file <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")
    runtime_root <- tempfile(pattern = "mmBayes-runtime-")

    team_data <- make_fixture_team_features(current_year = 2026, history_years = 2024:2025)
    results_data <- dplyr::bind_rows(
        make_fixture_game_results(team_data, history_years = 2024:2025),
        make_fixture_current_year_completed_results(team_data, current_year = 2026)
    )
    fixture_paths <- write_fixture_data_files(team_file, results_file, team_data = team_data, results_data = results_data)

    config <- default_project_config()
    config$data$team_features_path <- fixture_paths$team_path
    config$data$game_results_path <- fixture_paths$results_path
    config$runtime$root <- runtime_root
    config$betting$history_dir <- file.path(runtime_root, "data", "odds_history")
    config$betting$historical$archive_start_year <- 2024L
    config$output$path <- file.path(runtime_root, "output")
    config$output$use_model_cache <- FALSE
    config <- normalize_project_paths(config)

    write_fixture_betting_history(config$betting$history_dir, team_data, results_data, current_year = 2026)
    current_paths <- build_odds_history_paths(2026L, history_dir = config$betting$history_dir)
    ensure_odds_history_dirs(current_paths)
    utils::write.csv(
        make_fixture_closing_lines(team_data, results_data) %>% dplyr::filter(Year == "2026"),
        current_paths$closing_lines,
        row.names = FALSE
    )

    old_options <- options(
        mmBayes.stan_chains = 1L,
        mmBayes.stan_iter = 60L,
        mmBayes.stan_refresh = 0L
    )
    on.exit(options(old_options), add = TRUE)

    evaluation <- evaluate_historical_betting_bracket_impact(
        config = config,
        draws = 20L,
        n_candidates = 2L,
        n_simulations = 10L
    )

    expect_true(all(c(
        "deployable_historical_training",
        "retrospective_current_year_direct_substitution",
        "retrospective_current_year_candidate_overlay"
    ) %in% evaluation$summary$analysis))
    expect_true(all(c("baseline_no_betting", "historical_betting_features") %in% evaluation$summary$model))
    expect_true(all(c(
        "baseline_candidate_pool",
        "current_year_closing_lines_overlay"
    ) %in% evaluation$summary$model))
    expect_true(all(evaluation$summary$current_year_season_status == "complete"))
    expect_true(file.exists(evaluation$output_paths$summary))
    expect_true(file.exists(evaluation$output_paths$differences))
    expect_true(file.exists(evaluation$output_paths$report))
})

test_that("historical betting impact evaluation keeps retrospective studies when only current-year lines exist", {
    team_file <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")
    runtime_root <- tempfile(pattern = "mmBayes-runtime-")

    team_data <- make_fixture_team_features(current_year = 2026, history_years = 2024:2025)
    results_data <- dplyr::bind_rows(
        make_fixture_game_results(team_data, history_years = 2024:2025),
        make_fixture_current_year_completed_results(team_data, current_year = 2026)
    )
    fixture_paths <- write_fixture_data_files(team_file, results_file, team_data = team_data, results_data = results_data)

    config <- default_project_config()
    config$data$team_features_path <- fixture_paths$team_path
    config$data$game_results_path <- fixture_paths$results_path
    config$runtime$root <- runtime_root
    config$betting$history_dir <- file.path(runtime_root, "data", "odds_history")
    config$output$path <- file.path(runtime_root, "output")
    config$output$use_model_cache <- FALSE
    config <- normalize_project_paths(config)

    current_paths <- build_odds_history_paths(2026L, history_dir = config$betting$history_dir)
    ensure_odds_history_dirs(current_paths)
    current_closing_lines <- make_fixture_closing_lines(team_data, results_data) %>%
        dplyr::filter(Year == "2026")
    utils::write.csv(current_closing_lines, current_paths$closing_lines, row.names = FALSE)

    old_options <- options(
        mmBayes.stan_chains = 1L,
        mmBayes.stan_iter = 60L,
        mmBayes.stan_refresh = 0L
    )
    on.exit(options(old_options), add = TRUE)

    evaluation <- evaluate_historical_betting_bracket_impact(
        config = config,
        draws = 20L,
        n_candidates = 2L,
        n_simulations = 10L
    )

    deployable_rows <- evaluation$summary %>%
        dplyr::filter(analysis == "deployable_historical_training")
    retrospective_rows <- evaluation$summary %>%
        dplyr::filter(analysis != "deployable_historical_training")

    expect_true(any(is.na(deployable_rows$bracket_score[deployable_rows$model == "historical_betting_features"])))
    expect_true(any(retrospective_rows$model == "current_year_closing_lines_direct"))
    expect_true(any(retrospective_rows$model == "current_year_closing_lines_overlay"))
    expect_true(all(retrospective_rows$current_year_season_status == "complete"))
    expect_true(any(!is.na(retrospective_rows$bracket_score)))
    expect_match(
        unique(stats::na.omit(deployable_rows$note)),
        "outside the training seasons"
    )
    expect_true(file.exists(evaluation$output_paths$report))
})

test_that("historical betting impact evaluation marks retrospective analyses unavailable when current-year coverage is partial", {
    team_file <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")
    runtime_root <- tempfile(pattern = "mmBayes-runtime-")

    team_data <- make_fixture_team_features(current_year = 2026, history_years = 2024:2025)
    results_data <- dplyr::bind_rows(
        make_fixture_game_results(team_data, history_years = 2024:2025),
        make_fixture_current_year_completed_results(team_data, current_year = 2026)
    )
    fixture_paths <- write_fixture_data_files(team_file, results_file, team_data = team_data, results_data = results_data)

    config <- default_project_config()
    config$data$team_features_path <- fixture_paths$team_path
    config$data$game_results_path <- fixture_paths$results_path
    config$runtime$root <- runtime_root
    config$betting$history_dir <- file.path(runtime_root, "data", "odds_history")
    config$output$path <- file.path(runtime_root, "output")
    config$output$use_model_cache <- FALSE
    config <- normalize_project_paths(config)

    current_paths <- build_odds_history_paths(2026L, history_dir = config$betting$history_dir)
    ensure_odds_history_dirs(current_paths)
    partial_lines <- make_fixture_closing_lines(team_data, results_data) %>%
        dplyr::filter(Year == "2026")
    partial_lines$spread_teamA[[1]] <- NA_real_
    utils::write.csv(partial_lines, current_paths$closing_lines, row.names = FALSE)

    old_options <- options(
        mmBayes.stan_chains = 1L,
        mmBayes.stan_iter = 60L,
        mmBayes.stan_refresh = 0L
    )
    on.exit(options(old_options), add = TRUE)

    evaluation <- evaluate_historical_betting_bracket_impact(
        config = config,
        draws = 20L,
        n_candidates = 2L,
        n_simulations = 10L
    )

    retrospective_rows <- evaluation$summary %>%
        dplyr::filter(analysis %in% c(
            "retrospective_current_year_direct_substitution",
            "retrospective_current_year_candidate_overlay"
        ))

    expect_true(all(is.na(retrospective_rows$bracket_score)))
    expect_true(all(retrospective_rows$outcome_vs_baseline == "unknown"))
    expect_true(all(retrospective_rows$current_year_season_status == "partial"))
    expect_match(
        paste(stats::na.omit(retrospective_rows$note), collapse = " "),
        "could not run"
    )
})
