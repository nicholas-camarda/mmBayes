test_that("model quality artifacts save and resolve from the latest snapshot", {
    output_dir <- tempfile(pattern = "mmBayes-quality-")
    dir.create(output_dir, recursive = TRUE)

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

    artifact <- save_model_quality_artifact(fake_backtest, output_dir = output_dir)

    expect_true(file.exists(artifact$archive_path))
    expect_true(file.exists(artifact$latest_path))

    resolved <- resolve_model_quality_context(backtest = NULL, output_dir = output_dir)

    expect_true(isTRUE(resolved$used_fallback))
    expect_match(resolved$source_label, "Latest saved model-quality snapshot")
    expect_true(model_quality_has_backtest(resolved$backtest))
    expect_equal(resolved$backtest$summary$mean_log_loss[[1]], fake_backtest$summary$mean_log_loss[[1]])
})
