test_that("dashboard schema files exist and declare expected versions", {
    bracket_schema <- jsonlite::fromJSON(dashboard_schema_path("bracket"), simplifyVector = FALSE)
    technical_schema <- jsonlite::fromJSON(dashboard_schema_path("technical"), simplifyVector = FALSE)
    expect_equal(bracket_schema$properties$dashboard_schema_version$const, "1.1.0")
    expect_equal(technical_schema$properties$dashboard_schema_version$const, "1.1.0")
})

test_that("validate_dashboard_payload accepts a minimal valid bracket payload", {
    payload <- list(
        dashboard_schema_version = "1.1.0",
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
        dashboard_schema_version = "1.1.0",
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
        dashboard_schema_version = "1.1.0",
        dashboard = "technical",
        bracket_year = 2026L,
        generated_at = "2026-06-12T10:00:00-0400",
        build_metadata = list(render_timestamp = "now")
    )
    expect_invisible(validate_dashboard_payload(payload, "technical"))
})

make_payload_test_candidates <- function() {
    matchups <- tibble::tibble(
        region = "East",
        round = "Round of 64",
        matchup_number = c(1L, 2L),
        teamA = c("Duke", "Kansas"),
        teamB = c("UNC Wilmington", "Vermont"),
        teamA_seed = c(1L, 4L),
        teamB_seed = c(16L, 13L),
        winner = c("Duke", "Kansas"),
        win_prob_A = c(0.97, 0.81),
        ci_lower = c(0.90, 0.68),
        ci_upper = c(0.99, 0.90),
        prediction_sd = c(0.04, 0.08)
    ) %>%
        augment_matchup_decisions()
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
    expect_equal(payload$dashboard_schema_version, "1.1.0")
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

test_that("build_technical_dashboard_payload serializes backtest and decision tables", {
    decision_sheet <- data.frame(
        slot_key = c("a", "b"),
        round = c("Round of 64", "Sweet 16"),
        region = c("East", "West"),
        matchup_number = c(1L, 2L),
        matchup_label = c("A vs B", "C vs D"),
        posterior_favorite = c("A", "C"),
        win_prob_favorite = c(0.7, 0.55),
        ci_lower = c(0.6, 0.45),
        ci_upper = c(0.8, 0.65),
        confidence_tier = c("Lock", "Toss-up"),
        candidate_diff_flag = c(FALSE, TRUE),
        candidate_1_pick = c("A", "C"),
        candidate_2_pick = c("A", "D"),
        decision_rank = c(1L, 2L),
        decision_score = c(2.1, 1.4),
        inspection_level = c("primary", "secondary"),
        rationale_short = c("r1", "r2"),
        upset_leverage = c(0.1, 0.4),
        underdog = c("B", "D"),
        win_prob_underdog = c(0.3, 0.45),
        alternate_rationale = c(NA_character_, "swap")
    )
    backtest <- list(
        summary = tibble::tibble(
            mean_log_loss = 0.401,
            mean_brier = 0.188,
            mean_accuracy = 0.713,
            mean_bracket_score = 85.4,
            mean_correct_picks = 42.7
        ),
        calibration = tibble::tibble(
            bin = "(0.5,0.6]",
            mean_predicted = 0.55,
            empirical_rate = 0.58,
            n_games = 120L
        ),
        predictions = tibble::tibble(
            round = c("Round of 64", "Sweet 16"),
            predicted_prob = c(0.7, 0.55),
            actual_outcome = c(1, 0)
        )
    )
    payload <- build_technical_dashboard_payload(
        bracket_year = 2026L,
        decision_sheet = decision_sheet,
        candidates = make_payload_test_candidates(),
        model_quality_context = list(source_label = "fixture backtest", backtest = backtest),
        build_metadata = list(git_commit = "abc1234")
    )
    expect_invisible(validate_dashboard_payload(payload, "technical"))
    expect_equal(payload$dashboard_schema_version, "1.1.0")
    expect_true(is.data.frame(payload$ranked_decisions))
    expect_true(is.data.frame(payload$candidate_differences))
    expect_equal(payload$backtest$summary$mean_log_loss, 0.401)
    expect_true(is.data.frame(payload$backtest$round_performance))
    expect_gt(nrow(payload$backtest$round_performance), 0L)
    expect_true(length(payload$key_warnings) >= 1L)
    expect_true(is.data.frame(payload$upset_opportunities))
    expect_true(is.list(payload$candidate_profiles))
    expect_gt(length(payload$candidate_profiles), 0L)
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
    expect_equal(parsed$dashboard_schema_version, "1.1.0")
    expect_equal(parsed$candidates[[1]]$matchups[[1]]$teamA, "Duke")
})

test_that("write_dashboard_outputs writes validated React app payload artifacts only", {
    output_dir <- file.path(tempdir(), paste0("payload_test_", as.integer(Sys.time())))
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
    candidates <- make_payload_test_candidates()
    result <- write_dashboard_outputs(
        bracket_year = 2026L,
        candidates = candidates,
        output_dir = output_dir,
        backtest = list(
            summary = tibble::tibble(
                mean_log_loss = 0.401,
                mean_brier = 0.188,
                mean_accuracy = 0.713,
                mean_bracket_score = 85.4,
                mean_correct_picks = 42.7
            )
        )
    )
    expect_false(file.exists(file.path(output_dir, "bracket_dashboard.html")))
    expect_false(file.exists(file.path(output_dir, "technical_dashboard.html")))
    expect_true(file.exists(file.path(output_dir, "bracket_dashboard_payload.json")))
    expect_true(file.exists(file.path(output_dir, "technical_dashboard_payload.json")))
    expect_true(file.exists(file.path(output_dir, "dashboard_payloads.js")))
    expect_null(result$dashboard)
    expect_null(result$technical_dashboard)
    expect_equal(result$bracket_payload, file.path(output_dir, "bracket_dashboard_payload.json"))

    parsed <- jsonlite::fromJSON(
        file.path(output_dir, "bracket_dashboard_payload.json"),
        simplifyVector = FALSE
    )
    expect_equal(parsed$dashboard_schema_version, "1.1.0")
    expect_equal(parsed$dashboard, "bracket")

    js_first_line <- readLines(file.path(output_dir, "dashboard_payloads.js"), n = 1L)
    expect_match(js_first_line, "^window\\.__MMBAYES_PAYLOADS__ = \\{")
})

test_that("tracked output app payload is not the Vitest fixture shim", {
    repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
    payload_js <- file.path(repo_root, "output", "app", "dashboard_payloads.js")
    skip_if_not(file.exists(payload_js), "output/app payload not synced in this checkout")
    payload_line <- readLines(payload_js, n = 1L, warn = FALSE)
    expect_no_match(payload_line, '"git_commit":"fixture"', fixed = TRUE)
    expect_no_match(payload_line, "West_01_2025", fixed = TRUE)
})

test_that("sync_frontend_app fails clearly when dist is missing", {
    project_root <- file.path(tempdir(), paste0("no_dist_", as.integer(Sys.time())))
    runtime_dir <- file.path(project_root, "runtime_output")
    dir.create(runtime_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(project_root, recursive = TRUE), add = TRUE)
    expect_error(
        sync_frontend_app(project_root, runtime_output_dir = runtime_dir),
        "Frontend app build not found"
    )
})

test_that("sync_frontend_app copies the built app and payload shim to both targets", {
    project_root <- file.path(tempdir(), paste0("with_dist_", as.integer(Sys.time())))
    dist_dir <- file.path(project_root, "frontend", "dist", "assets")
    runtime_dir <- file.path(project_root, "runtime_output")
    repo_dir <- file.path(project_root, "repo_output")
    dir.create(dist_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(runtime_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(repo_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(project_root, recursive = TRUE), add = TRUE)
    writeLines(
        '<html><body><script src="./dashboard_payloads.js"></script></body></html>',
        file.path(project_root, "frontend", "dist", "index.html")
    )
    writeLines("console.log(1)", file.path(dist_dir, "index.js"))
    writeLines("window.__MMBAYES_PAYLOADS__ = {\"bracket\":{\"build_metadata\":{\"commit_short\":\"syncfix\"}}};", file.path(runtime_dir, "dashboard_payloads.js"))

    synced <- sync_frontend_app(project_root, runtime_output_dir = runtime_dir, repo_output_dir = repo_dir)

    expect_equal(sort(synced), sort(c(file.path(runtime_dir, "app"), file.path(repo_dir, "app"))))
    for (target in synced) {
        expect_true(file.exists(file.path(target, "index.html")))
        expect_true(file.exists(file.path(target, "assets", "index.js")))
        expect_true(file.exists(file.path(target, "dashboard_payloads.js")))
        index_html <- readLines(file.path(target, "index.html"), warn = FALSE)
        expect_match(paste(index_html, collapse = "\n"), 'dashboard_payloads\\.js\\?v=')
    }
})

test_that("extract_payload_cache_token prefers commit_short then generated_at", {
    payload_js <- tempfile(fileext = ".js")
    on.exit(unlink(payload_js), add = TRUE)
    writeLines(
        'window.__MMBAYES_PAYLOADS__ = {"bracket":{"generated_at":"2026-06-12T17:40:50-0400","build_metadata":{"commit_short":"abc1234"}}};',
        payload_js
    )
    expect_equal(extract_payload_cache_token(payload_js), "abc1234")
})
