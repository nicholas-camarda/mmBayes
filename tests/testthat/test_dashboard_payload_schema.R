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
