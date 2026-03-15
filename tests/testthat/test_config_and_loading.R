test_that("config loader merges defaults with yaml values", {
    file <- tempfile(fileext = ".yml")
    writeLines(
        c(
            "default:",
            "  data_path: custom.xlsx",
            "  model:",
            "    n_draws: 25",
            "  output:",
            "    prefix: custom_prefix"
        ),
        file
    )

    config <- load_project_config(file)

    expect_equal(config$data_path, "custom.xlsx")
    expect_equal(config$model$n_draws, 25)
    expect_equal(config$output$prefix, "custom_prefix")
    expect_true(length(config$metrics_to_use) > 0)
})

test_that("load_tournament_data falls back to the newest available year", {
    workbook <- tempfile(fileext = ".xlsx")
    write_fixture_workbook(workbook)

    config <- default_project_config()
    config$data_path <- workbook
    loaded <- load_tournament_data(config)

    expect_equal(loaded$bracket_year, "2025")
    expect_equal(nrow(loaded$current), 64)
    expect_gt(nrow(loaded$historical), 64)
    expect_true(all(c("seed_strength", "conf_power", "historical_performance") %in% names(loaded$historical)))
})
