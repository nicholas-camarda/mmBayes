test_that("build_model_formula includes interaction terms when provided", {
    preds <- c("round", "same_conf", "seed_diff", "barthag_logit_diff")
    f_no_interaction <- build_model_formula(preds)
    f_with_interaction <- build_model_formula(preds, interaction_terms = "barthag_logit_diff:seed_diff")

    expect_true(inherits(f_no_interaction, "formula"))
    expect_true(inherits(f_with_interaction, "formula"))

    f_str <- paste(deparse(f_with_interaction), collapse = " ")
    expect_true(grepl("barthag_logit_diff:seed_diff", f_str, fixed = TRUE))
    expect_false(grepl("barthag_logit_diff:seed_diff", paste(deparse(f_no_interaction), collapse = " "), fixed = TRUE))
})

test_that("build_model_formula with NULL interaction_terms equals formula without them", {
    preds <- c("round", "seed_diff", "barthag_logit_diff")
    expect_equal(
        deparse(build_model_formula(preds, interaction_terms = NULL)),
        deparse(build_model_formula(preds, interaction_terms = character(0)))
    )
})

test_that("build_model_formula supports multiple interaction terms", {
    preds <- c("round", "seed_diff", "barthag_logit_diff", "WAB_diff")
    terms <- c("barthag_logit_diff:seed_diff", "barthag_logit_diff:WAB_diff")
    f <- build_model_formula(preds, interaction_terms = terms)
    f_str <- paste(deparse(f), collapse = " ")
    expect_true(grepl("barthag_logit_diff:seed_diff", f_str, fixed = TRUE))
    expect_true(grepl("barthag_logit_diff:WAB_diff", f_str, fixed = TRUE))
})

test_that("configure_priors returns normal priors by default", {
    priors <- configure_priors()
    expect_true(is.list(priors))
    expect_true(all(c("fixed", "intercept") %in% names(priors)))
})

test_that("configure_priors returns horseshoe priors when prior_type is 'hs'", {
    priors_hs <- configure_priors(prior_type = "hs")
    expect_true(is.list(priors_hs))
    expect_true(all(c("fixed", "intercept") %in% names(priors_hs)))
    priors_normal <- configure_priors(prior_type = "normal")
    expect_false(identical(priors_hs$fixed, priors_normal$fixed))
})

test_that("configure_priors errors on unknown prior_type", {
    expect_error(configure_priors(prior_type = "horsehoe"), regexp = "Unknown prior_type")
    expect_error(configure_priors(prior_type = "laplace"), regexp = "Unknown prior_type")
})

test_that("fit_tournament_model stores interaction_terms and prior_type in result bundle", {
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
        random_seed = 123,
        include_diagnostics = FALSE,
        interaction_terms = "barthag_logit_diff:seed_diff",
        prior_type = "normal"
    )

    expect_equal(model_results$interaction_terms, "barthag_logit_diff:seed_diff")
    expect_equal(model_results$prior_type, "normal")

    f_str <- paste(deparse(model_results$formula), collapse = " ")
    expect_true(grepl("barthag_logit_diff:seed_diff", f_str, fixed = TRUE))
})

test_that("fit_tournament_model with interaction_terms can predict matchup probabilities", {
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
        random_seed = 123,
        include_diagnostics = FALSE,
        interaction_terms = "barthag_logit_diff:seed_diff"
    )

    east_teams <- loaded$current_teams %>%
        dplyr::filter(Region == "East") %>%
        dplyr::arrange(Seed)
    team_a <- east_teams[1, , drop = FALSE]
    team_b <- east_teams[2, , drop = FALSE]

    probs <- predict_matchup_probabilities(team_a, team_b, "Round of 64", model_results, draws = 25L)
    expect_true(is.numeric(probs))
    expect_true(all(probs >= 0 & probs <= 1))
})

test_that("default_project_config includes interaction_terms and prior_type", {
    config <- default_project_config()
    expect_true("interaction_terms" %in% names(config$model))
    expect_true("prior_type" %in% names(config$model))
    expect_true("compare_engines" %in% names(config$model))
    expect_equal(config$model$prior_type, "normal")
    expect_true(isTRUE(config$model$compare_engines))
    expect_equal(length(config$model$interaction_terms), 0L)
})

test_that("compare_model_configurations returns a valid comparison bundle", {
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

    config_a <- list(predictor_columns = config$model$required_predictors)
    config_b <- list(
        predictor_columns = config$model$required_predictors,
        interaction_terms = "barthag_logit_diff:seed_diff"
    )

    comparison <- compare_model_configurations(
        historical_teams = loaded$historical_teams,
        historical_actual_results = loaded$historical_actual_results,
        config_a = config_a,
        config_b = config_b,
        config_a_label = "Baseline",
        config_b_label = "With Interactions",
        random_seed = 123,
        draws = 25L
    )

    expect_true(is.list(comparison))
    expect_true(all(c("config_a", "config_b", "delta", "comparison_table") %in% names(comparison)))
    expect_equal(nrow(comparison$comparison_table), 2L)
    expect_true(all(c("label", "mean_log_loss", "mean_brier", "mean_accuracy") %in% names(comparison$comparison_table)))
    expect_true(is.numeric(comparison$delta))
    expect_true(all(c("mean_log_loss", "mean_brier", "mean_accuracy") %in% names(comparison$delta)))
})

test_that("committed fixture CSV files load correctly as team features and game results", {
    fixture_team_path <- testthat::test_path("fixtures", "team_features.csv")
    fixture_results_path <- testthat::test_path("fixtures", "game_results.csv")

    skip_if_not(file.exists(fixture_team_path), "Committed fixture files not found")

    team_data <- read_table_file(fixture_team_path)
    results_data <- read_table_file(fixture_results_path)

    required_team_cols <- c("Year", "Region", "Seed", "Team", "Conf", "Barthag",
                            "AdjOE", "AdjDE", "WAB", "TOR", "TORD", "ORB", "DRB",
                            "3P%", "3P%D", "Adj T.")
    expect_true(all(required_team_cols %in% names(team_data)),
                info = paste("Missing columns:", paste(setdiff(required_team_cols, names(team_data)), collapse = ", ")))
    expect_true(nrow(team_data) > 0)
    expect_true(nrow(results_data) > 0)
    expect_true(length(unique(team_data$Year)) >= 2L)
})

# ── Horseshoe prior tests ──────────────────────────────────────────────────────

test_that("fit_tournament_model with prior_type='hs' records horseshoe in result bundle", {
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

    model_hs <- fit_tournament_model(
        loaded$historical_matchups,
        config$model$required_predictors,
        random_seed = 123,
        include_diagnostics = FALSE,
        prior_type = "hs"
    )

    expect_equal(model_hs$prior_type, "hs")
    expect_true(is.list(model_hs))
    expect_true(all(c("engine", "model", "scaling_reference", "predictor_columns", "prior_type") %in% names(model_hs)))
})

test_that("horseshoe prior model produces valid win probabilities", {
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

    model_hs <- fit_tournament_model(
        loaded$historical_matchups,
        config$model$required_predictors,
        random_seed = 123,
        include_diagnostics = FALSE,
        prior_type = "hs"
    )

    east_teams <- loaded$current_teams %>%
        dplyr::filter(Region == "East") %>%
        dplyr::arrange(Seed)
    team_a <- east_teams[1, , drop = FALSE]
    team_b <- east_teams[16, , drop = FALSE]

    probs_hs <- predict_matchup_probabilities(team_a, team_b, "Round of 64", model_hs, draws = 25L)

    expect_true(is.numeric(probs_hs))
    expect_length(probs_hs, 25L)
    expect_true(all(probs_hs >= 0 & probs_hs <= 1))
    # seed-1 team should generally beat seed-16 (mean > 0.5)
    expect_true(mean(probs_hs) > 0.5)
})

test_that("horseshoe and normal prior models are both antisymmetric under team swap", {
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

    east_teams <- loaded$current_teams %>%
        dplyr::filter(Region == "East") %>%
        dplyr::arrange(Seed)
    team_a <- east_teams[1, , drop = FALSE]
    team_b <- east_teams[16, , drop = FALSE]

    for (pt in c("normal", "hs")) {
        model <- fit_tournament_model(
            loaded$historical_matchups,
            config$model$required_predictors,
            random_seed = 123,
            include_diagnostics = FALSE,
            prior_type = pt
        )
        p_ab <- mean(predict_matchup_probabilities(team_a, team_b, "Round of 64", model, draws = 25L))
        p_ba <- mean(predict_matchup_probabilities(team_b, team_a, "Round of 64", model, draws = 25L))
        expect_equal(p_ab + p_ba, 1, tolerance = 0.1,
                     label = paste("antisymmetry check failed for prior_type =", pt))
    }
})

test_that("compare_model_configurations correctly compares horseshoe vs normal prior", {
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

    normal_config <- list(
        predictor_columns = config$model$required_predictors,
        prior_type = "normal"
    )
    hs_config <- list(
        predictor_columns = config$model$required_predictors,
        prior_type = "hs"
    )

    comparison <- compare_model_configurations(
        historical_teams = loaded$historical_teams,
        historical_actual_results = loaded$historical_actual_results,
        config_a = normal_config,
        config_b = hs_config,
        config_a_label = "Normal Prior",
        config_b_label = "Horseshoe Prior",
        random_seed = 123,
        draws = 25L
    )

    # Structural checks
    expect_equal(nrow(comparison$comparison_table), 2L)
    expect_equal(comparison$comparison_table$label, c("Normal Prior", "Horseshoe Prior"))

    # Both models must have produced finite, valid metrics
    for (metric in c("mean_log_loss", "mean_brier", "mean_accuracy")) {
        expect_true(is.finite(comparison$comparison_table[[metric]][[1]]),
                    label = paste("Normal prior metric not finite:", metric))
        expect_true(is.finite(comparison$comparison_table[[metric]][[2]]),
                    label = paste("Horseshoe prior metric not finite:", metric))
    }

    # Log-loss and Brier score must be in plausible range for a binary outcome model
    expect_true(all(comparison$comparison_table$mean_log_loss > 0),
                info = "Log-loss must be positive")
    expect_true(all(comparison$comparison_table$mean_brier >= 0 &
                    comparison$comparison_table$mean_brier <= 1),
                info = "Brier score must be in [0, 1]")
    expect_true(all(comparison$comparison_table$mean_accuracy >= 0 &
                    comparison$comparison_table$mean_accuracy <= 1),
                info = "Accuracy must be in [0, 1]")

    # delta vector must have the right names and be numeric
    expect_true(is.numeric(comparison$delta))
    expect_true(all(c("mean_log_loss", "mean_brier", "mean_accuracy",
                      "mean_bracket_score", "mean_correct_picks") %in% names(comparison$delta)))

    # Log the delta direction for informational purposes (not a strict pass/fail,
    # because with synthetic fixture data either prior may win on any given seed)
    delta_log_loss <- comparison$delta[["mean_log_loss"]]
    message(sprintf(
        "Horseshoe vs Normal prior delta (log-loss): %.4f  [negative = horseshoe improved]",
        delta_log_loss
    ))
})
