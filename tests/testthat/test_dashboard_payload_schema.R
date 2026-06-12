test_that("dashboard schema files exist and declare version 1.0.0", {
    for (dashboard in c("bracket", "technical")) {
        path <- dashboard_schema_path(dashboard)
        expect_true(file.exists(path))
        schema <- jsonlite::fromJSON(path, simplifyVector = FALSE)
        expect_equal(schema$properties$dashboard_schema_version$const, "1.0.0")
        expect_true("dashboard_schema_version" %in% unlist(schema$required))
    }
})

test_that("validate_dashboard_payload accepts a minimal valid bracket payload", {
    payload <- list(
        dashboard_schema_version = "1.0.0",
        dashboard = "bracket",
        bracket_year = 2026L,
        generated_at = "2026-06-12T10:00:00-0400",
        build_metadata = list(),
        candidates = list(list(candidate_id = 1L, type = "primary", champion = "Duke")),
        decision_sheet = data.frame(slot_key = "East|Round of 64|1", winner = "Duke")
    )
    expect_invisible(validate_dashboard_payload(payload, "bracket"))
})

test_that("validate_dashboard_payload fails closed on missing required fields", {
    payload <- list(
        dashboard_schema_version = "1.0.0",
        dashboard = "bracket",
        bracket_year = 2026L,
        generated_at = "2026-06-12T10:00:00-0400",
        build_metadata = list(),
        decision_sheet = data.frame(slot_key = "x")
    )
    expect_error(
        validate_dashboard_payload(payload, "bracket"),
        "missing required field: candidates"
    )
})

test_that("validate_dashboard_payload rejects schema version mismatches", {
    payload <- list(
        dashboard_schema_version = "9.0.0",
        dashboard = "technical",
        bracket_year = 2026L,
        generated_at = "2026-06-12T10:00:00-0400",
        build_metadata = list()
    )
    expect_error(validate_dashboard_payload(payload, "technical"), "dashboard_schema_version")
})

test_that("validate_dashboard_payload tolerates absent optional sections", {
    payload <- list(
        dashboard_schema_version = "1.0.0",
        dashboard = "technical",
        bracket_year = 2026L,
        generated_at = "2026-06-12T10:00:00-0400",
        build_metadata = list(render_timestamp = "now")
    )
    expect_invisible(validate_dashboard_payload(payload, "technical"))
})

make_payload_test_candidates <- function() {
    matchups <- tibble::tibble(
        slot_key = c("East|Round of 64|1", "East|Round of 64|2"),
        region = "East",
        round = "Round of 64",
        matchup_number = c(1L, 2L),
        teamA = c("Duke", "Kansas"),
        teamB = c("UNC Wilmington", "Vermont"),
        teamA_seed = c(1L, 4L),
        teamB_seed = c(16L, 13L),
        winner = c("Duke", "Kansas"),
        win_prob_A = c(0.97, 0.81)
    )
    list(
        list(
            candidate_id = 1L, type = "primary", champion = "Duke",
            final_four = "Duke, Kansas, Houston, UConn",
            bracket_log_prob = -10.2, mean_game_prob = 0.74,
            title_path_mean_prob = 0.71, frequency = NA_integer_,
            path_support_label = "Deterministic posterior-mean reference bracket.",
            matchups = matchups
        ),
        list(
            candidate_id = 2L, type = "alternate", champion = "Kansas",
            final_four = "Duke, Kansas, Houston, UConn",
            bracket_log_prob = -11.4, mean_game_prob = 0.70,
            title_path_mean_prob = 0.62, frequency = 3L,
            path_support_label = "Supported by coherent posterior-draw simulations.",
            matchups = matchups
        )
    )
}

test_that("build_bracket_dashboard_payload produces a valid versioned payload", {
    candidates <- make_payload_test_candidates()
    decision_sheet <- data.frame(slot_key = "East|Round of 64|1", winner = "Duke")
    payload <- build_bracket_dashboard_payload(
        bracket_year = 2026L,
        candidates = candidates,
        decision_sheet = decision_sheet,
        dashboard_context = list(build_metadata = list(git_commit = "abc1234"))
    )
    expect_invisible(validate_dashboard_payload(payload, "bracket"))
    expect_equal(payload$dashboard_schema_version, "1.0.0")
    expect_equal(payload$dashboard, "bracket")
    expect_equal(length(payload$candidates), 2L)
    expect_equal(payload$candidates[[1]]$champion, "Duke")
    expect_true(is.data.frame(payload$candidates[[1]]$matchups))
    expect_false("matchup_context" %in% names(payload))
})

test_that("build_technical_dashboard_payload produces a valid versioned payload", {
    decision_sheet <- data.frame(
        slot_key = c("a", "b"),
        confidence_tier = c("Lock", "Toss-up"),
        candidate_diff_flag = c(FALSE, TRUE)
    )
    payload <- build_technical_dashboard_payload(
        bracket_year = 2026L,
        decision_sheet = decision_sheet,
        candidates = make_payload_test_candidates(),
        model_quality_context = list(source_label = "current run", used_cached_quality = FALSE),
        build_metadata = list(git_commit = "abc1234")
    )
    expect_invisible(validate_dashboard_payload(payload, "technical"))
    expect_equal(payload$candidate_count, 2L)
    expect_equal(payload$decision_summary$n_decisions, 2L)
    expect_equal(payload$decision_summary$n_divergent, 1L)
    expect_equal(payload$model_quality$source_label, "current run")
})

test_that("dashboard payload JSON round-trips with rows orientation", {
    candidates <- make_payload_test_candidates()
    payload <- build_bracket_dashboard_payload(
        bracket_year = 2026L,
        candidates = candidates,
        decision_sheet = data.frame(slot_key = "x", winner = "Duke"),
        dashboard_context = list(build_metadata = list())
    )
    json <- dashboard_payload_json(payload)
    parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
    expect_equal(parsed$dashboard_schema_version, "1.0.0")
    expect_equal(parsed$candidates[[1]]$matchups[[1]]$teamA, "Duke")
})
