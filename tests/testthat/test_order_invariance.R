#' Fit small Stan GLM and BART components on fixture data
#'
#' @return A list with `stan` and `bart` model bundles and the loaded
#'   fixture data.
#' @keywords internal
fit_symmetry_fixture_models <- function() {
    team_file <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")
    team_data <- make_fixture_team_features(current_year = 2025, history_years = 2023:2024)
    results_data <- make_fixture_game_results(team_data, history_years = 2023:2024)
    paths <- write_fixture_data_files(team_file, results_file, team_data = team_data, results_data = results_data)

    config <- default_project_config()
    config$data$team_features_path <- paths$team_path
    config$data$game_results_path <- paths$results_path
    config$model$history_window <- 2L
    loaded <- load_tournament_data(config)

    old_options <- options(mmBayes.stan_chains = 1L, mmBayes.stan_iter = 80L, mmBayes.stan_refresh = 0L)
    on.exit(options(old_options), add = TRUE)
    bart_config <- list(n_trees = 20L, n_burn = 5L, n_post = 40L, k = 2, power = 2)
    stan <- fit_tournament_model(
        historical_matchups = loaded$historical_matchups,
        predictor_columns = selection_full_feature_set()$predictors,
        engine = "stan_glm",
        interaction_terms = selection_full_feature_set()$interaction_terms,
        random_seed = 7,
        include_diagnostics = FALSE,
        use_cache = FALSE
    )
    invisible(capture.output(bart <- fit_tournament_model(
        historical_matchups = loaded$historical_matchups,
        predictor_columns = selection_full_feature_set()$predictors,
        engine = "bart",
        bart_config = bart_config,
        random_seed = 7,
        include_diagnostics = FALSE,
        use_cache = FALSE
    )))
    list(stan = stan, bart = bart, loaded = loaded)
}

test_that("historical training data has one row per game", {
    team_data <- make_fixture_team_features(current_year = 2025, history_years = 2023:2024)
    results_data <- make_fixture_game_results(team_data, history_years = 2023:2024)
    historical <- results_data %>% dplyr::filter(Year %in% c("2023", "2024"))

    rows <- build_explicit_matchup_history(normalize_team_features(team_data), normalize_game_results(historical))

    expect_equal(nrow(rows), nrow(historical))
    expect_equal(nrow(dplyr::distinct(rows, Year, region, round, game_index)), nrow(historical))
    flipped_share <- mean(rows$teamA != historical$teamA[match(
        paste(rows$Year, rows$region, rows$round, rows$game_index),
        paste(historical$Year, historical$region, historical$round, historical$game_index)
    )])
    expect_gt(flipped_share, 0.25)
    expect_lt(flipped_share, 0.75)
    expect_true(all(rows$winner %in% c(rows$teamA, rows$teamB)))
    expect_equal(rows$actual_outcome, as.numeric(rows$winner == rows$teamA))
})

test_that("training orientation is reproducible and ignores the global seed", {
    team_data <- make_fixture_team_features(current_year = 2025, history_years = 2024)
    results_data <- make_fixture_game_results(team_data, history_years = 2024)
    historical <- normalize_game_results(results_data %>% dplyr::filter(Year == "2024"))
    features <- normalize_team_features(team_data)

    set.seed(1)
    first <- build_explicit_matchup_history(features, historical)
    set.seed(999)
    second <- build_explicit_matchup_history(features, historical)
    expect_identical(first$teamA, second$teamA)
})

test_that("swapping orientation negates difference predictors and complements the outcome", {
    row <- tibble::tibble(
        Year = "2025", teamA = "A", teamB = "B", round = "Sweet 16", same_conf = 1L,
        seed_diff = -3, barthag_logit_diff = 0.8, `3P%_diff` = 1.5, actual_outcome = 1
    )
    swapped <- swap_matchup_orientation(row)
    expect_identical(c(swapped$teamA, swapped$teamB), c("B", "A"))
    expect_equal(c(swapped$seed_diff, swapped$barthag_logit_diff, swapped$`3P%_diff`), c(3, -0.8, -1.5))
    expect_equal(swapped$actual_outcome, 0)
    expect_identical(swapped$round, "Sweet 16")
    expect_identical(swapped$same_conf, 1L)
    expect_equal(swap_matchup_orientation(swapped), row)
})

test_that("winner-model formula has no intercept or order-invariant main effects", {
    formula <- build_model_formula(
        c("round", "same_conf", "seed_diff", "barthag_logit_diff"),
        interaction_terms = "barthag_logit_diff:round"
    )
    term_labels <- attr(stats::terms(formula), "term.labels")
    expect_identical(attr(stats::terms(formula), "intercept"), 0L)
    expect_false(any(c("round", "same_conf") %in% term_labels))
    expect_true("barthag_logit_diff:round" %in% term_labels)
})

test_that("winner-model scaling does not centre difference predictors", {
    data <- tibble::tibble(seed_diff = c(1, 3, 8), barthag_logit_diff = c(0.2, 0.4, 1.0))
    reference <- build_scaling_reference(data, c("seed_diff", "barthag_logit_diff"), center = FALSE)
    expect_equal(reference$seed_diff$center, 0)
    expect_equal(reference$seed_diff$scale, sqrt(mean(c(1, 9, 64))))
    centred <- build_scaling_reference(data, c("seed_diff"), center = TRUE)
    expect_equal(centred$seed_diff$center, 4)
})

test_that("Stan GLM and BART predictions are order-invariant draw by draw", {
    if (!requireNamespace("BART", quietly = TRUE)) {
        skip("BART not installed")
    }
    models <- fit_symmetry_fixture_models()
    rows <- models$loaded$historical_matchups[1:8, , drop = FALSE]
    swapped <- swap_matchup_orientation(rows)

    for (name in c("stan", "bart")) {
        model <- models[[name]]
        forward <- withr::with_seed(11L, predict_matchup_rows(rows, model, draws = 20))
        reverse <- withr::with_seed(11L, predict_matchup_rows(swapped, model, draws = 20))
        expect_equal(forward + reverse, matrix(1, nrow(forward), ncol(forward)), tolerance = 1e-8, info = name)
    }
})

test_that("total-points formula keeps its intercept and round effects", {
    formula <- build_total_points_formula(default_total_points_predictors())
    labels <- attr(stats::terms(formula), "term.labels")
    expect_identical(attr(stats::terms(formula), "intercept"), 1L)
    expect_true(all(c("round", "same_conf", "barthag_logit_sum") %in% labels))
})

test_that("total-points model predicts totals on the scale of real games", {
    team_data <- make_fixture_team_features(current_year = 2025, history_years = 2022:2024)
    results_data <- make_fixture_game_results(team_data, history_years = 2022:2024)
    reference <- build_actual_game_reference(normalize_team_features(team_data), normalize_game_results(results_data))
    rows <- build_total_points_training_rows(reference)

    old_options <- options(mmBayes.stan_chains = 1L, mmBayes.stan_iter = 200L, mmBayes.stan_refresh = 0L)
    on.exit(options(old_options), add = TRUE)
    model <- fit_total_points_model(rows, engine = "stan_glm", random_seed = 3, include_diagnostics = FALSE, use_cache = FALSE)
    predicted <- colMeans(predict_total_points_rows(rows[1:10, , drop = FALSE], model, draws = 50))
    expect_lt(abs(mean(predicted) - mean(rows$total_points)), 0.25 * mean(rows$total_points))
})

test_that("model cache keys change when the model specification changes", {
    data <- tibble::tibble(Year = "2024", x = 1:3)
    key_a <- build_model_fit_cache_key(data, "barthag_logit_diff", 1, FALSE, model_specification = "y ~ 0 + x")
    key_b <- build_model_fit_cache_key(data, "barthag_logit_diff", 1, FALSE, model_specification = "y ~ x")
    expect_false(identical(key_a, key_b))
    expect_false(identical(
        winner_model_specification("barthag_logit_diff"),
        winner_model_specification(c("barthag_logit_diff", "seed_diff"))
    ))
})
