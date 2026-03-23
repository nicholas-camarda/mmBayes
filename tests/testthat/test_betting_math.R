test_that("American odds convert to implied probabilities", {
    expect_equal(american_to_implied_prob(100), 0.5)
    expect_equal(american_to_implied_prob(200), 100 / 300)
    expect_equal(american_to_implied_prob(-100), 0.5)
    expect_equal(american_to_implied_prob(-200), 200 / 300)
    expect_true(is.na(american_to_implied_prob(NA_real_)))
})

test_that("Two-way vig removal renormalizes probabilities", {
    implied <- c(0.6, 0.5)
    vig_free <- remove_vig_two_way(implied[[1]], implied[[2]])

    expect_equal(length(vig_free), 2L)
    expect_equal(sum(vig_free), 1, tolerance = 1e-10)
    expect_true(all(vig_free > 0))
})

