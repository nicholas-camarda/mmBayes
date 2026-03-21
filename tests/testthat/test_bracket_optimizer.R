# Tests for the Bayesian bracket path optimizer (R/bracket_optimizer.R).
#
# These tests require a fitted model (rstanarm) but use the minimal fixture
# helpers to keep runtime short.  Run individually with:
#   testthat::test_file("tests/testthat/test_bracket_optimizer.R")

# ── compute_slot_win_probabilities ────────────────────────────────────────────

test_that("compute_slot_win_probabilities returns a valid tibble structure", {
    team_file    <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")
    fixture_paths <- write_fixture_data_files(team_file, results_file)

    config <- default_project_config()
    config$data$team_features_path <- fixture_paths$team_path
    config$data$game_results_path  <- fixture_paths$results_path
    config$model$history_window    <- 3L

    loaded <- load_tournament_data(config)

    old_options <- options(
        mmBayes.stan_chains = 1L,
        mmBayes.stan_iter   = 60L,
        mmBayes.stan_refresh = 0L
    )
    on.exit(options(old_options), add = TRUE)

    model_results <- fit_tournament_model(
        loaded$historical_matchups,
        config$model$required_predictors,
        random_seed = 42L,
        include_diagnostics = FALSE
    )

    result <- compute_slot_win_probabilities(
        all_teams     = loaded$current_teams,
        model_results = model_results,
        draws         = 20L,
        n_simulations = 30L,
        random_seed   = 1L
    )

    expect_true(is.data.frame(result))
    expected_cols <- c("region", "round", "matchup_number", "team", "slot_win_prob")
    expect_true(all(expected_cols %in% names(result)),
                info = paste("Missing cols:", paste(setdiff(expected_cols, names(result)), collapse = ", ")))
    expect_true(nrow(result) > 0L)
    expect_true(all(result$slot_win_prob >= 0 & result$slot_win_prob <= 1))
})

test_that("compute_slot_win_probabilities: slot-win probs within a slot sum to <= 1", {
    team_file    <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")
    fixture_paths <- write_fixture_data_files(team_file, results_file)

    config <- default_project_config()
    config$data$team_features_path <- fixture_paths$team_path
    config$data$game_results_path  <- fixture_paths$results_path
    config$model$history_window    <- 3L

    loaded <- load_tournament_data(config)

    old_options <- options(
        mmBayes.stan_chains = 1L,
        mmBayes.stan_iter   = 60L,
        mmBayes.stan_refresh = 0L
    )
    on.exit(options(old_options), add = TRUE)

    model_results <- fit_tournament_model(
        loaded$historical_matchups,
        config$model$required_predictors,
        random_seed = 42L,
        include_diagnostics = FALSE
    )

    result <- compute_slot_win_probabilities(
        all_teams     = loaded$current_teams,
        model_results = model_results,
        draws         = 20L,
        n_simulations = 40L,
        random_seed   = 7L
    )

    # Within each (region, round, matchup_number) slot, the probabilities of
    # all teams in that slot must sum to exactly 1 (since in each simulation
    # exactly one team wins).
    slot_sums <- result %>%
        dplyr::group_by(region, round, matchup_number) %>%
        dplyr::summarise(total = sum(slot_win_prob), .groups = "drop")

    expect_true(all(abs(slot_sums$total - 1) < 1e-9),
                info = "Slot-win probs within a slot must sum to 1")
})

# ── find_optimal_bracket ──────────────────────────────────────────────────────

test_that("find_optimal_bracket returns the expected list structure", {
    team_file    <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")
    fixture_paths <- write_fixture_data_files(team_file, results_file)

    config <- default_project_config()
    config$data$team_features_path <- fixture_paths$team_path
    config$data$game_results_path  <- fixture_paths$results_path
    config$model$history_window    <- 3L

    loaded <- load_tournament_data(config)

    old_options <- options(
        mmBayes.stan_chains = 1L,
        mmBayes.stan_iter   = 60L,
        mmBayes.stan_refresh = 0L
    )
    on.exit(options(old_options), add = TRUE)

    model_results <- fit_tournament_model(
        loaded$historical_matchups,
        config$model$required_predictors,
        random_seed = 42L,
        include_diagnostics = FALSE
    )

    opt <- find_optimal_bracket(
        all_teams     = loaded$current_teams,
        model_results = model_results,
        draws         = 20L,
        n_simulations = 30L,
        random_seed   = 2L
    )

    expect_true(is.list(opt))
    expect_true(all(c("optimal_picks", "expected_bracket_score", "slot_win_probs", "n_simulations") %in% names(opt)))
    expect_true(is.data.frame(opt$optimal_picks))
    expect_true(is.numeric(opt$expected_bracket_score))
    expect_true(is.finite(opt$expected_bracket_score))
    expect_true(opt$expected_bracket_score >= 0)
    expect_equal(opt$n_simulations, 30L)
})

test_that("find_optimal_bracket: optimal_picks has one row per bracket slot", {
    team_file    <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")
    fixture_paths <- write_fixture_data_files(team_file, results_file)

    config <- default_project_config()
    config$data$team_features_path <- fixture_paths$team_path
    config$data$game_results_path  <- fixture_paths$results_path
    config$model$history_window    <- 3L

    loaded <- load_tournament_data(config)

    old_options <- options(
        mmBayes.stan_chains = 1L,
        mmBayes.stan_iter   = 60L,
        mmBayes.stan_refresh = 0L
    )
    on.exit(options(old_options), add = TRUE)

    model_results <- fit_tournament_model(
        loaded$historical_matchups,
        config$model$required_predictors,
        random_seed = 42L,
        include_diagnostics = FALSE
    )

    opt <- find_optimal_bracket(
        all_teams     = loaded$current_teams,
        model_results = model_results,
        draws         = 20L,
        n_simulations = 30L,
        random_seed   = 3L
    )

    # Each (region, round, matchup_number) combination should appear exactly once
    deduped <- opt$optimal_picks %>%
        dplyr::distinct(region, round, matchup_number)
    expect_equal(nrow(deduped), nrow(opt$optimal_picks))

    # slot_win_prob for each optimal pick must be in [0, 1]
    expect_true(all(opt$optimal_picks$slot_win_prob >= 0 & opt$optimal_picks$slot_win_prob <= 1))
})

test_that("find_optimal_bracket: expected_bracket_score is consistent with slot probs", {
    team_file    <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")
    fixture_paths <- write_fixture_data_files(team_file, results_file)

    config <- default_project_config()
    config$data$team_features_path <- fixture_paths$team_path
    config$data$game_results_path  <- fixture_paths$results_path
    config$model$history_window    <- 3L

    loaded <- load_tournament_data(config)

    old_options <- options(
        mmBayes.stan_chains = 1L,
        mmBayes.stan_iter   = 60L,
        mmBayes.stan_refresh = 0L
    )
    on.exit(options(old_options), add = TRUE)

    model_results <- fit_tournament_model(
        loaded$historical_matchups,
        config$model$required_predictors,
        random_seed = 42L,
        include_diagnostics = FALSE
    )

    rw <- default_round_weights()
    opt <- find_optimal_bracket(
        all_teams     = loaded$current_teams,
        model_results = model_results,
        draws         = 20L,
        n_simulations = 30L,
        random_seed   = 5L,
        round_weights = rw
    )

    # Manually recompute expected score from the optimal picks table
    manual_score <- opt$optimal_picks %>%
        dplyr::mutate(
            rw = unname(rw[round]),
            rw = dplyr::coalesce(rw, 0)
        ) %>%
        dplyr::summarise(score = sum(slot_win_prob * rw, na.rm = TRUE)) %>%
        dplyr::pull(score)

    expect_equal(opt$expected_bracket_score, manual_score, tolerance = 1e-9)
})

# ── compare_optimal_to_candidate ─────────────────────────────────────────────

test_that("compare_optimal_to_candidate returns correct structure", {
    team_file    <- tempfile(fileext = ".xlsx")
    results_file <- tempfile(fileext = ".xlsx")
    fixture_paths <- write_fixture_data_files(team_file, results_file)

    config <- default_project_config()
    config$data$team_features_path <- fixture_paths$team_path
    config$data$game_results_path  <- fixture_paths$results_path
    config$model$history_window    <- 3L

    loaded <- load_tournament_data(config)

    old_options <- options(
        mmBayes.stan_chains = 1L,
        mmBayes.stan_iter   = 60L,
        mmBayes.stan_refresh = 0L
    )
    on.exit(options(old_options), add = TRUE)

    model_results <- fit_tournament_model(
        loaded$historical_matchups,
        config$model$required_predictors,
        random_seed = 42L,
        include_diagnostics = FALSE
    )

    opt <- find_optimal_bracket(
        all_teams     = loaded$current_teams,
        model_results = model_results,
        draws         = 20L,
        n_simulations = 30L,
        random_seed   = 6L
    )

    det_bracket <- simulate_full_bracket(
        all_teams     = loaded$current_teams,
        model_results = model_results,
        draws         = 20L,
        deterministic = TRUE,
        log_matchups  = FALSE
    )
    candidate_matchups <- augment_matchup_decisions(flatten_matchup_results(det_bracket))

    comparison <- compare_optimal_to_candidate(opt, candidate_matchups)

    expect_true(is.data.frame(comparison))
    expect_true(all(c("region", "round", "matchup_number",
                       "candidate_pick", "optimal_pick",
                       "slot_win_prob", "differs_from_optimal") %in% names(comparison)))
    expect_true(is.logical(comparison$differs_from_optimal))
    # All differs_from_optimal flags must be TRUE or FALSE (no NA from the join
    # when optimal_pick is present for matching slots)
    matched <- comparison[!is.na(comparison$optimal_pick), ]
    expect_true(all(!is.na(matched$differs_from_optimal)))
})
