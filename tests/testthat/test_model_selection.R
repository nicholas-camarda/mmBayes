test_that("recency weights halve after each half-life and equal weighting is flat", {
    expect_equal(season_recency_weights(c(2024, 2023, 2021), 2025, half_life = 3), c(1, 0.5^(1 / 3), 0.5))
    expect_equal(season_recency_weights(c(2010, 2024), 2025, half_life = Inf), c(1, 1))
    expect_error(season_recency_weights(2025, 2025, 3), "earlier than the holdout")
})

test_that("training seasons use only earlier years and respect the window", {
    years <- c("2015", "2016", "2017", "2018", "2019", "2021", "2022")
    expect_identical(training_seasons(years, 2021, window = Inf), c("2015", "2016", "2017", "2018", "2019"))
    expect_identical(training_seasons(years, 2021, window = 2), c("2018", "2019"))
    expect_identical(training_seasons(years, 2015, window = 8), character())
})

test_that("holdout metrics and calibration behave on known inputs", {
    m <- holdout_metrics(c(0.8, 0.3), c(1, 0))
    expect_equal(m$log_loss, -mean(c(log(0.8), log(0.7))))
    expect_equal(m$brier, mean(c(0.04, 0.09)))
    expect_equal(m$accuracy, 1)

    set.seed(5)
    x <- stats::rnorm(4000)
    y <- stats::rbinom(4000, 1, stats::plogis(x))
    cal <- calibration_fit(stats::plogis(2 * x), y)
    expect_lt(abs(cal$calibration_slope - 0.5), 0.08)
    expect_lt(abs(cal$calibration_intercept), 0.1)
})

test_that("log5 is order-invariant and gives one half for equal teams", {
    expect_equal(log5_probability(0.9, 0.6) + log5_probability(0.6, 0.9), 1)
    expect_equal(log5_probability(0.7, 0.7), 0.5)
})

test_that("history grid fits only on earlier seasons and scores main-bracket games", {
    set.seed(9)
    years <- c(2016:2019, 2021:2022)
    games <- purrr::map_dfr(years, function(y) {
        tibble::tibble(
            Year = as.character(y),
            round = rep(c("First Four", "Round of 64"), c(4, 60)),
            barthag_logit_diff = stats::rnorm(64),
            seed_diff = stats::rnorm(64)
        )
    }) %>%
        dplyr::mutate(actual_outcome = stats::rbinom(dplyr::n(), 1, stats::plogis(1.5 * barthag_logit_diff)))

    grid <- evaluate_logistic_history_grid(
        games,
        holdout_years = c(2021, 2022),
        formulas = list(barthag = actual_outcome ~ 0 + barthag_logit_diff),
        windows = c(2, Inf),
        half_lives = c(Inf, 2)
    )
    expect_equal(nrow(grid), 8L)
    expect_true(all(grid$games == 60L))
    expect_equal(grid$training_seasons[grid$window == 2], rep(2L, 4))
    expect_equal(grid$training_seasons[grid$window == Inf & grid$holdout_year == "2021"], c(4L, 4L))
    expect_equal(grid$training_seasons[grid$window == Inf & grid$holdout_year == "2022"], c(5L, 5L))

    summary <- summarise_history_grid(grid)
    expect_equal(nrow(summary), 4L)
    expect_true(all(summary$holdout_years == 2L))
    expect_true(all(c("mean_log_loss", "se_log_loss", "calibration_slope") %in% names(summary)))
})
