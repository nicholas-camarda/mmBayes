test_that("run_tournament_simulation writes expected outputs", {
    workbook <- tempfile(fileext = ".xlsx")
    output_dir <- tempfile(pattern = "mmBayes-output-")
    dir.create(output_dir, recursive = TRUE)
    write_fixture_workbook(workbook)

    config <- default_project_config()
    config$data_path <- workbook
    config$model$n_draws <- 25
    config$output$path <- output_dir
    config$output$prefix <- "fixture"

    old_options <- options(
        mmBayes.stan_chains = 1L,
        mmBayes.stan_iter = 60L,
        mmBayes.stan_refresh = 0L
    )
    on.exit(options(old_options), add = TRUE)

    results <- run_tournament_simulation(config)

    expect_true(file.exists(file.path(output_dir, "fixture.rds")))
    expect_true(file.exists(file.path(output_dir, "fixture_model_summary.txt")))
    expect_true(file.exists(file.path(output_dir, "fixture_bracket.png")))
    expect_true(is.list(results$final_four))
    expect_equal(results$model$engine, "bayes")
    expect_true(inherits(results$visualization, "patchwork"))
})
