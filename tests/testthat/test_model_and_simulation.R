test_that("fit_tournament_model returns a structured model object", {
    workbook <- tempfile(fileext = ".xlsx")
    write_fixture_workbook(workbook)

    config <- default_project_config()
    config$data_path <- workbook
    loaded <- load_tournament_data(config)

    old_options <- options(
        mmBayes.stan_chains = 1L,
        mmBayes.stan_iter = 60L,
        mmBayes.stan_refresh = 0L
    )
    on.exit(options(old_options), add = TRUE)

    model_results <- fit_tournament_model(
        loaded$historical,
        config$model$required_metrics,
        random_seed = 123
    )

    expect_true(is.list(model_results))
    expect_true(all(c("engine", "model", "diagnostics", "scaling_reference", "conf_levels") %in% names(model_results)))
    expect_equal(model_results$engine, "bayes")
})

test_that("simulate_full_bracket returns region and final four structure", {
    workbook <- tempfile(fileext = ".xlsx")
    write_fixture_workbook(workbook)

    config <- default_project_config()
    config$data_path <- workbook
    config$model$n_draws <- 50

    old_options <- options(
        mmBayes.stan_chains = 1L,
        mmBayes.stan_iter = 60L,
        mmBayes.stan_refresh = 0L
    )
    on.exit(options(old_options), add = TRUE)

    loaded <- load_tournament_data(config)
    model_results <- fit_tournament_model(
        loaded$historical,
        config$model$required_metrics,
        random_seed = 123
    )
    simulation <- simulate_full_bracket(loaded$current, model_results, draws = 50)

    expect_true(all(c("region_results", "final_four") %in% names(simulation)))
    expect_equal(sort(names(simulation$region_results)), sort(c("East", "West", "South", "Midwest")))
    expect_true(all(c("semifinalists", "semifinals", "championship", "champion") %in% names(simulation$final_four)))
})
