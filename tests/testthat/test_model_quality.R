test_that("model quality artifacts save and resolve from the latest snapshot", {
    output_dir <- tempfile(pattern = "mmBayes-quality-")
    dir.create(output_dir, recursive = TRUE)
    quality_signature <- "fixture-quality-signature"

    fake_backtest <- list(
        summary = tibble::tibble(
            mean_log_loss = 0.412,
            mean_brier = 0.197,
            mean_accuracy = 0.702,
            mean_bracket_score = 84.1,
            mean_correct_picks = 41.8
        ),
        calibration = tibble::tibble(
            mean_predicted = c(0.25, 0.50, 0.75),
            empirical_rate = c(0.28, 0.48, 0.73),
            n_games = c(10L, 12L, 8L)
        )
    )

    first_artifact <- save_model_quality_artifact(
        fake_backtest,
        output_dir = output_dir,
        quality_signature = quality_signature
    )

    expect_true(file.exists(first_artifact$latest_path))
    expect_identical(first_artifact$archive_path, first_artifact$latest_path)
    expect_equal(length(list.files(file.path(output_dir, "model_quality"), pattern = "^model_quality_.*_pid.*\\.rds$")), 0L)

    resolved <- resolve_model_quality_context(
        backtest = NULL,
        output_dir = output_dir,
        quality_signature = quality_signature
    )

    expect_true(isTRUE(resolved$used_fallback))
    expect_match(resolved$source_label, "Cached identical validation snapshot")
    expect_true(model_quality_has_backtest(resolved$backtest))
    expect_equal(resolved$backtest$summary$mean_log_loss[[1]], fake_backtest$summary$mean_log_loss[[1]])
    expect_equal(resolved$quality_signature, quality_signature)

    updated_backtest <- fake_backtest
    updated_backtest$summary$mean_log_loss <- 0.331
    updated_backtest$summary$mean_brier <- 0.141

    second_artifact <- save_model_quality_artifact(
        updated_backtest,
        output_dir = output_dir,
        quality_signature = quality_signature
    )

    expect_identical(second_artifact$latest_path, first_artifact$latest_path)
    expect_identical(second_artifact$archive_path, second_artifact$latest_path)
    expect_equal(length(list.files(file.path(output_dir, "model_quality"), pattern = "^model_quality_.*_pid.*\\.rds$")), 0L)
    overwritten <- readRDS(second_artifact$latest_path)
    expect_equal(overwritten$backtest$summary$mean_log_loss[[1]], updated_backtest$summary$mean_log_loss[[1]])
    expect_equal(overwritten$backtest$summary$mean_brier[[1]], updated_backtest$summary$mean_brier[[1]])

    expect_error(
        resolve_model_quality_context(
            backtest = NULL,
            output_dir = output_dir,
            quality_signature = "different-signature"
        ),
        "does not match the current model/data fingerprint"
    )
})

test_that("model quality resolution tolerates a missing cache", {
    output_dir <- tempfile(pattern = "mmBayes-quality-missing-")
    dir.create(output_dir, recursive = TRUE)

    resolved <- resolve_model_quality_context(
        backtest = NULL,
        output_dir = output_dir,
        quality_signature = "missing-signature"
    )

    expect_false(model_quality_has_backtest(resolved$backtest))
    expect_false(isTRUE(resolved$used_cached_quality))
    expect_match(resolved$source_label, "Backtest not available")
})
