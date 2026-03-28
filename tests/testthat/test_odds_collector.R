make_fake_odds_event <- function(id, commence_time, home_team, away_team) {
    list(
        id = id,
        commence_time = commence_time,
        home_team = home_team,
        away_team = away_team,
        bookmakers = list()
    )
}

make_fake_odds_fetch_response <- function(events, retrieved_at = as.POSIXct("2026-03-15 12:00:00", tz = "UTC")) {
    list(
        raw_json = jsonlite::toJSON(events, auto_unbox = TRUE),
        parsed = events,
        headers = list(),
        retrieved_at = retrieved_at
    )
}

test_that("schedule grouping keeps close cross-midnight games in the same slate", {
    current_teams <- tibble::tibble(
        Team = c("Alpha", "Beta", "Gamma", "Delta", "Epsilon", "Zeta")
    )
    events <- list(
        make_fake_odds_event("evt1", "2026-03-15T23:10:00Z", "Alpha", "Beta"),
        make_fake_odds_event("evt2", "2026-03-16T00:05:00Z", "Gamma", "Delta"),
        make_fake_odds_event("evt3", "2026-03-16T05:30:00Z", "Epsilon", "Zeta")
    )

    normalized <- normalize_odds_api_event_schedule(events, bracket_year = 2026L, current_teams = current_teams)
    windows <- group_odds_schedule_into_slates(normalized, gap_minutes = 180L)

    expect_equal(nrow(normalized), 3L)
    expect_equal(nrow(windows), 2L)
    expect_equal(windows$event_count, c(2L, 1L))
    expect_equal(windows$slate_id, c("2026_slate_01", "2026_slate_02"))
})

test_that("normalize_odds_api_event_schedule skips malformed events with missing team fields", {
    current_teams <- tibble::tibble(
        Team = c("Alpha", "Beta")
    )
    events <- list(
        make_fake_odds_event("evt1", "2026-03-15T23:10:00Z", "Alpha", "Beta"),
        make_fake_odds_event("evt_bad", "2026-03-16T00:05:00Z", character(), "Beta")
    )

    normalized <- normalize_odds_api_event_schedule(events, bracket_year = 2026L, current_teams = current_teams)

    expect_equal(nrow(normalized), 1L)
    expect_equal(normalized$event_id, "evt1")
})

test_that("refresh_tournament_odds_schedule writes the schedule cache from fixture events", {
    tmp <- tempfile(pattern = "odds-history-")
    config <- default_project_config()
    config$betting$history_dir <- tmp

    current_teams <- tibble::tibble(
        Team = c("Alpha", "Beta", "Gamma", "Delta", "Epsilon", "Zeta")
    )
    events <- list(
        make_fake_odds_event("evt1", "2026-03-15T23:10:00Z", "Alpha", "Beta"),
        make_fake_odds_event("evt2", "2026-03-16T00:05:00Z", "Gamma", "Delta"),
        make_fake_odds_event("evt3", "2026-03-16T05:30:00Z", "Epsilon", "Zeta")
    )

    fake_fetcher <- function(...) {
        make_fake_odds_fetch_response(events)
    }

    schedule <- refresh_tournament_odds_schedule(
        config = config,
        bracket_year = 2026L,
        current_teams = current_teams,
        now = as.POSIXct("2026-03-15 12:00:00", tz = "UTC"),
        events_fetcher = fake_fetcher
    )

    expect_true(file.exists(schedule$paths$schedule_events))
    expect_true(file.exists(schedule$paths$schedule_windows))
    expect_equal(nrow(schedule$events), 3L)
    expect_equal(nrow(schedule$windows), 2L)

    cached_events <- utils::read.csv(schedule$paths$schedule_events, stringsAsFactors = FALSE)
    cached_windows <- utils::read.csv(schedule$paths$schedule_windows, stringsAsFactors = FALSE)
    expect_equal(nrow(cached_events), 3L)
    expect_equal(nrow(cached_windows), 2L)
})

test_that("run_tournament_odds_collector stays idle outside the tournament window", {
    team_file <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")
    fixture_paths <- write_fixture_data_files(team_file, results_file)

    runtime_root <- tempfile(pattern = "mmBayes-runtime-")
    config <- default_project_config()
    config$data$team_features_path <- fixture_paths$team_path
    config$data$game_results_path <- fixture_paths$results_path
    config$runtime$root <- runtime_root
    config$betting$history_dir <- file.path(runtime_root, "data", "odds_history")
    config$output$path <- file.path(runtime_root, "output")
    config <- normalize_project_paths(config)
    config$model$history_window <- 3L

    events_called <- FALSE
    snapshot_called <- FALSE

    fail_if_called <- function(...) {
        stop("collector should not call Odds API outside the tournament window")
    }

    result <- run_tournament_odds_collector(
        config = config,
        force = FALSE,
        now = as.POSIXct("2025-01-15 12:00:00", tz = "UTC"),
        events_fetcher = function(...) {
            events_called <<- TRUE
            fail_if_called()
        },
        snapshot_capturer = function(...) {
            snapshot_called <<- TRUE
            fail_if_called()
        }
    )

    expect_false(events_called)
    expect_false(snapshot_called)
    expect_equal(result$plan$reason, "outside_tournament_window")
    expect_false(isTRUE(result$schedule$season_active))
    expect_true(file.exists(result$paths$collector_state))
})

test_that("run_tournament_odds_collector marks the season active during in-window runs", {
    team_file <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")
    fixture_paths <- write_fixture_data_files(team_file, results_file)

    runtime_root <- tempfile(pattern = "mmBayes-runtime-")
    config <- default_project_config()
    config$data$team_features_path <- fixture_paths$team_path
    config$data$game_results_path <- fixture_paths$results_path
    config$runtime$root <- runtime_root
    config$betting$history_dir <- file.path(runtime_root, "data", "odds_history")
    config$output$path <- file.path(runtime_root, "output")
    config <- normalize_project_paths(config)
    config$model$history_window <- 3L

    result <- run_tournament_odds_collector(
        config = config,
        force = FALSE,
        now = as.POSIXct("2025-03-15 12:00:00", tz = "UTC"),
        events_fetcher = function(...) {
            make_fake_odds_fetch_response(list())
        },
        snapshot_capturer = function(...) {
            stop("collector should not capture a snapshot when no window is due")
        }
    )

    expect_true(isTRUE(result$schedule$season_active))
    expect_true(file.exists(result$paths$collector_state))
})

test_that("run_tournament_odds_collector stops once remaining credits reach the reserve floor", {
    team_file <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")
    fixture_paths <- write_fixture_data_files(team_file, results_file)

    runtime_root <- tempfile(pattern = "mmBayes-runtime-")
    config <- default_project_config()
    config$data$team_features_path <- fixture_paths$team_path
    config$data$game_results_path <- fixture_paths$results_path
    config$runtime$root <- runtime_root
    config$betting$history_dir <- file.path(runtime_root, "data", "odds_history")
    config$output$path <- file.path(runtime_root, "output")
    config <- normalize_project_paths(config)
    config$model$history_window <- 3L

    loaded <- load_tournament_data(config, include_betting_history = FALSE)
    team_a <- loaded$current_teams$Team[[1]]
    team_b <- loaded$current_teams$Team[[2]]
    team_c <- loaded$current_teams$Team[[3]]
    team_d <- loaded$current_teams$Team[[4]]
    events <- list(
        make_fake_odds_event("evt1", "2025-03-15T12:15:00Z", team_a, team_b),
        make_fake_odds_event("evt2", "2025-03-15T12:20:00Z", team_c, team_d)
    )
    event_calls <- 0L
    snapshot_calls <- 0L

    fake_fetcher <- function(...) {
        event_calls <<- event_calls + 1L
        make_fake_odds_fetch_response(events, retrieved_at = as.POSIXct("2025-03-15 12:00:00", tz = "UTC"))
    }

    fake_snapshot <- function(...) {
        snapshot_calls <<- snapshot_calls + 1L
        list(
            snapshot_path = file.path(runtime_root, sprintf("snapshot_%02d.json", snapshot_calls)),
            lines_long = tibble::tibble(),
            lines_matchups = tibble::tibble(
                snapshot_time_utc = as.POSIXct("2025-03-15 12:00:00", tz = "UTC"),
                bracket_year = 2025L,
                event_id = "evt1",
                commence_time = "2025-03-15T12:15:00Z",
                home_team = team_a,
                away_team = team_b,
                key = build_matchup_key(team_a, team_b),
                team_a = team_a,
                team_b = team_b,
                n_bookmakers = 2L,
                bookmakers = "draftkings,fanduel",
                consensus_prob_a = 0.6,
                consensus_prob_b = 0.4,
                prob_dispersion_a = 0.02,
                consensus_spread_a = -3.5,
                consensus_spread_b = 3.5,
                spread_dispersion_a = 0.25
            ),
            latest_lines_matchups_path = file.path(runtime_root, "latest_lines_matchups.csv"),
            headers = list(
                `x-requests-remaining` = "100",
                `x-requests-last` = "2"
            ),
            retrieved_at = as.POSIXct("2025-03-15 12:00:00", tz = "UTC"),
            skipped = FALSE
        )
    }

    first_run <- run_tournament_odds_collector(
        config = config,
        force = FALSE,
        now = as.POSIXct("2025-03-15 12:00:00", tz = "UTC"),
        events_fetcher = fake_fetcher,
        snapshot_capturer = fake_snapshot
    )

    second_run <- run_tournament_odds_collector(
        config = config,
        force = FALSE,
        now = as.POSIXct("2025-03-15 12:00:00", tz = "UTC"),
        events_fetcher = fake_fetcher,
        snapshot_capturer = fake_snapshot
    )

    expect_equal(event_calls, 2L)
    expect_equal(snapshot_calls, 1L)
    expect_equal(first_run$state$remaining_credits, 100L)
    expect_equal(second_run$plan$reason, "quota_reserve")
    expect_false(isTRUE(second_run$plan$should_capture_now))
    expect_equal(second_run$state$remaining_credits, 100L)
})
