test_that("fit_tournament_model returns a matchup-model object", {
    team_file <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")
    fixture_paths <- write_fixture_data_files(team_file, results_file)

    config <- default_project_config()
    config$data$team_features_path <- fixture_paths$team_path
    config$data$game_results_path <- fixture_paths$results_path
    config$model$history_window <- 3L

    loaded <- load_tournament_data(config)

    old_options <- options(
        mmBayes.stan_chains = 1L,
        mmBayes.stan_iter = 60L,
        mmBayes.stan_refresh = 0L
    )
    on.exit(options(old_options), add = TRUE)

    model_results <- fit_tournament_model(
        loaded$historical_matchups,
        config$model$required_predictors,
        random_seed = 123
    )

    expect_true(is.list(model_results))
    expect_true(all(c("engine", "model", "diagnostics", "scaling_reference", "predictor_columns") %in% names(model_results)))
    expect_equal(model_results$engine, "bayes")
})

test_that("fit_tournament_model reuses a cached fit when the inputs are unchanged", {
    team_file <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")
    fixture_paths <- write_fixture_data_files(team_file, results_file)

    config <- default_project_config()
    config$data$team_features_path <- fixture_paths$team_path
    config$data$game_results_path <- fixture_paths$results_path
    config$model$history_window <- 3L

    loaded <- load_tournament_data(config)
    cache_dir <- tempfile(pattern = "mmBayes-model-cache-")
    dir.create(cache_dir, recursive = TRUE)

    old_options <- options(
        mmBayes.stan_chains = 1L,
        mmBayes.stan_iter = 60L,
        mmBayes.stan_refresh = 0L
    )
    on.exit(options(old_options), add = TRUE)

    first_fit <- fit_tournament_model(
        loaded$historical_matchups,
        config$model$required_predictors,
        random_seed = 123,
        include_diagnostics = FALSE,
        cache_dir = cache_dir
    )

    expect_true(file.exists(first_fit$cache_path))

    cached_result <- readRDS(first_fit$cache_path)
    cached_result$cache_marker <- "cached"
    saveRDS(cached_result, first_fit$cache_path)

    second_fit <- fit_tournament_model(
        loaded$historical_matchups,
        config$model$required_predictors,
        random_seed = 123,
        include_diagnostics = FALSE,
        cache_dir = cache_dir
    )

    expect_equal(second_fit$cache_marker, "cached")
    expect_equal(second_fit$cache_path, first_fit$cache_path)
    expect_equal(second_fit$predictor_columns, first_fit$predictor_columns)
})

test_that("matchup predictions are approximately antisymmetric", {
    team_file <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")
    fixture_paths <- write_fixture_data_files(team_file, results_file)

    config <- default_project_config()
    config$data$team_features_path <- fixture_paths$team_path
    config$data$game_results_path <- fixture_paths$results_path
    config$model$history_window <- 3L
    config$model$n_draws <- 25L

    loaded <- load_tournament_data(config)

    old_options <- options(
        mmBayes.stan_chains = 1L,
        mmBayes.stan_iter = 60L,
        mmBayes.stan_refresh = 0L
    )
    on.exit(options(old_options), add = TRUE)

    model_results <- fit_tournament_model(
        loaded$historical_matchups,
        config$model$required_predictors,
        random_seed = 123
    )

    east_teams <- loaded$current_teams %>%
        dplyr::filter(Region == "East") %>%
        dplyr::arrange(Seed, Team)
    team_a <- east_teams[1, , drop = FALSE]
    team_b <- east_teams[2, , drop = FALSE]

    p_ab <- mean(predict_matchup_probabilities(team_a, team_b, "Round of 64", model_results, draws = 25))
    p_ba <- mean(predict_matchup_probabilities(team_b, team_a, "Round of 64", model_results, draws = 25))

    expect_equal(p_ab + p_ba, 1, tolerance = 0.1)
})

test_that("simulate_full_bracket returns region and final four structure", {
    team_file <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")
    fixture_paths <- write_fixture_data_files(team_file, results_file)

    config <- default_project_config()
    config$data$team_features_path <- fixture_paths$team_path
    config$data$game_results_path <- fixture_paths$results_path
    config$model$history_window <- 3L
    config$model$n_draws <- 25L

    old_options <- options(
        mmBayes.stan_chains = 1L,
        mmBayes.stan_iter = 60L,
        mmBayes.stan_refresh = 0L
    )
    on.exit(options(old_options), add = TRUE)

    loaded <- load_tournament_data(config)
    model_results <- fit_tournament_model(
        loaded$historical_matchups,
        config$model$required_predictors,
        random_seed = 123
    )
    simulation <- simulate_full_bracket(loaded$current_teams, model_results, draws = 25)
    flattened <- flatten_matchup_results(simulation)

    expect_true(all(c("region_results", "final_four") %in% names(simulation)))
    expect_equal(sort(names(simulation$region_results)), sort(c("East", "West", "South", "Midwest")))
    expect_true(any(flattened$round == "First Four"))
    expect_true(all(c("semifinalists", "semifinals", "championship", "champion") %in% names(simulation$final_four)))
})

test_that("candidate generation adds decision metadata and an alternate bracket", {
    team_file <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")
    fixture_paths <- write_fixture_data_files(team_file, results_file)

    config <- default_project_config()
    config$data$team_features_path <- fixture_paths$team_path
    config$data$game_results_path <- fixture_paths$results_path
    config$model$history_window <- 3L
    config$model$n_draws <- 25L

    old_options <- options(
        mmBayes.stan_chains = 1L,
        mmBayes.stan_iter = 60L,
        mmBayes.stan_refresh = 0L
    )
    on.exit(options(old_options), add = TRUE)

    loaded <- load_tournament_data(config)
    model_results <- fit_tournament_model(
        loaded$historical_matchups,
        config$model$required_predictors,
        random_seed = 123,
        include_diagnostics = FALSE
    )

    candidates <- generate_bracket_candidates(
        all_teams = loaded$current_teams,
        model_results = model_results,
        draws = 25L,
        n_candidates = 2L,
        n_simulations = 8L,
        random_seed = 123
    )
    decision_sheet <- build_decision_sheet(candidates)

    expect_true(length(candidates) >= 1)
    expect_true(all(c("decision_rank", "confidence_tier", "upset_leverage") %in% names(candidates[[1]]$matchups)))
    expect_true(all(c("candidate_1_pick", "candidate_2_pick", "candidate_diff_flag", "decision_score") %in% names(decision_sheet)))
    expect_equal(unique(decision_sheet$region), c("East", "South", "Midwest", "West"))
    expect_true(all(decision_sheet$confidence_tier %in% c("Lock", "Lean", "Toss-up", "Volatile")))

    dashboard_html <- create_bracket_dashboard_html(
        bracket_year = 2026L,
        decision_sheet = decision_sheet,
        candidates = candidates,
        backtest = NULL
    )
    expect_match(dashboard_html, "<th>winner</th>")
    expect_match(dashboard_html, "<th>matchup</th><th>winner</th>")
})
