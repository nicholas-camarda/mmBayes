test_that("load_dotenv_file loads key-value pairs without overriding by default", {
    env_file <- tempfile(fileext = ".env")
    writeLines(c(
        "# comment",
        "FOO=bar",
        "QUOTED=\"baz\"",
        "WITH_EQUALS=a=b=c"
    ), env_file)

    old <- Sys.getenv(c("FOO", "QUOTED", "WITH_EQUALS"), unset = NA_character_)
    on.exit({
        if (!is.na(old[[1]])) Sys.setenv(FOO = old[[1]]) else Sys.unsetenv("FOO")
        if (!is.na(old[[2]])) Sys.setenv(QUOTED = old[[2]]) else Sys.unsetenv("QUOTED")
        if (!is.na(old[[3]])) Sys.setenv(WITH_EQUALS = old[[3]]) else Sys.unsetenv("WITH_EQUALS")
    }, add = TRUE)

    Sys.unsetenv(c("FOO", "QUOTED", "WITH_EQUALS"))
    loaded <- load_dotenv_file(env_file, override = FALSE)

    expect_true(all(c("FOO", "QUOTED", "WITH_EQUALS") %in% loaded))
    expect_equal(Sys.getenv("FOO"), "bar")
    expect_equal(Sys.getenv("QUOTED"), "baz")
    expect_equal(Sys.getenv("WITH_EQUALS"), "a=b=c")
})

test_that("ensure_odds_history_dirs creates snapshots directory", {
    tmp <- tempfile(pattern = "odds-history-")
    paths <- build_odds_history_paths(2026L, history_dir = tmp)
    expect_false(dir.exists(paths$snapshots_dir))
    ensure_odds_history_dirs(paths)
    expect_true(dir.exists(paths$snapshots_dir))
})

test_that("capture_tournament_odds_snapshot respects snapshot cooldown when history exists", {
    tmp <- tempfile(pattern = "odds-history-")
    config <- default_project_config()
    config$betting$history_dir <- tmp
    config$betting$snapshot_cooldown_minutes <- 60
    config$betting$api_key_env <- "MMBAYES_TEST_ODDS_KEY"
    Sys.unsetenv("MMBAYES_TEST_ODDS_KEY")

    bracket_year <- 2026L
    paths <- build_odds_history_paths(bracket_year, history_dir = tmp)
    ensure_odds_history_dirs(paths)

    latest <- tibble::tibble(
        snapshot_time_utc = as.POSIXct(Sys.time(), tz = "UTC"),
        bracket_year = bracket_year,
        event_id = "test",
        commence_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
        home_team = "Team A",
        away_team = "Team B",
        key = build_matchup_key("Team A", "Team B"),
        team_a = "Team A",
        team_b = "Team B",
        n_bookmakers = 1L,
        bookmakers = "draftkings",
        consensus_prob_a = 0.6,
        consensus_prob_b = 0.4
    )
    utils::write.csv(latest, paths$latest_lines_matchups, row.names = FALSE)

    snapshot <- capture_tournament_odds_snapshot(
        config = config,
        bracket_year = bracket_year,
        current_teams = tibble::tibble(Team = c("Team A", "Team B")),
        force = FALSE
    )

    expect_true(isTRUE(snapshot$skipped %||% FALSE))
    expect_equal(snapshot$skip_reason, "cooldown")
    expect_true(file.exists(paths$latest_lines_matchups))
})
