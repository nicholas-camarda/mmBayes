# Tests for the betting-lines module (R/betting_lines.R).
#
# Each test_that block is fully self-contained and does NOT require a fitted
# model or Stan/rstanarm to be available.  All fixtures are built inline.
# Run individually with:
#   testthat::test_file("tests/testthat/test_betting_lines.R")

# ── american_odds_to_prob ─────────────────────────────────────────────────────

test_that("american_odds_to_prob converts positive odds correctly", {
    # +100 is an even-money bet: implied prob = 0.5
    expect_equal(american_odds_to_prob(100), 100 / (100 + 100))

    # +150: prob = 100 / 250
    expect_equal(american_odds_to_prob(150), 100 / 250)

    # +300: prob = 100 / 400
    expect_equal(american_odds_to_prob(300), 100 / 400)
})

test_that("american_odds_to_prob converts negative odds correctly", {
    # -100 is even money: prob = 100 / 200
    expect_equal(american_odds_to_prob(-100), 100 / 200)

    # -200: prob = 200 / 300
    expect_equal(american_odds_to_prob(-200), 200 / 300)

    # -110 (standard juice): prob = 110 / 210
    expect_equal(american_odds_to_prob(-110), 110 / 210)
})

test_that("american_odds_to_prob returns values in (0, 1)", {
    odds_vec <- c(-500, -200, -110, +100, +150, +400)
    probs <- american_odds_to_prob(odds_vec)
    expect_true(all(probs > 0 & probs < 1))
})

test_that("american_odds_to_prob handles vectorised input", {
    odds <- c(-110, +130)
    expected <- c(110 / 210, 100 / 230)
    expect_equal(american_odds_to_prob(odds), expected)
})

# ── decimal_odds_to_prob ──────────────────────────────────────────────────────

test_that("decimal_odds_to_prob converts correctly", {
    # 2.0 is even money
    expect_equal(decimal_odds_to_prob(2.0), 0.5)
    # 1.5 → 2/3
    expect_equal(decimal_odds_to_prob(1.5), 1 / 1.5)
    # 4.0 → 0.25
    expect_equal(decimal_odds_to_prob(4.0), 0.25)
})

test_that("decimal_odds_to_prob errors on odds <= 1", {
    expect_error(decimal_odds_to_prob(1.0), regexp = "greater than 1")
    expect_error(decimal_odds_to_prob(0.9), regexp = "greater than 1")
})

test_that("decimal_odds_to_prob returns values in (0, 1) for valid inputs", {
    odds_vec <- c(1.1, 1.5, 2.0, 3.0, 10.0)
    probs <- decimal_odds_to_prob(odds_vec)
    expect_true(all(probs > 0 & probs < 1))
})

# ── remove_vig ────────────────────────────────────────────────────────────────

test_that("remove_vig normalises probabilities to sum to 1", {
    # A typical American -110 / -110 market overrounds at ~52.38% each
    raw_a <- american_odds_to_prob(-110)
    raw_b <- american_odds_to_prob(-110)
    expect_gt(raw_a + raw_b, 1)

    fair <- remove_vig(raw_a, raw_b)
    expect_equal(fair$prob_a + fair$prob_b, 1, tolerance = 1e-9)
})

test_that("remove_vig preserves relative probabilities", {
    # Favourite implied at 70%, underdog at 40% (sum = 110%)
    raw_a <- 0.70
    raw_b <- 0.40
    fair <- remove_vig(raw_a, raw_b)
    expect_gt(fair$prob_a, fair$prob_b)
    expect_equal(fair$prob_a + fair$prob_b, 1, tolerance = 1e-9)
})

test_that("remove_vig with only prob_a uses 1 - prob_a as the complement", {
    fair <- remove_vig(0.55)
    expect_equal(fair$prob_a + fair$prob_b, 1, tolerance = 1e-9)
})

test_that("remove_vig errors when overround is not positive", {
    expect_error(remove_vig(0, 0), regexp = "positive")
})

# ── blend_market_odds ─────────────────────────────────────────────────────────

test_that("blend_market_odds with weight=0 returns posterior unchanged", {
    posterior <- c(0.65, 0.40, 0.50)
    market    <- c(0.70, 0.35, 0.45)
    blended   <- blend_market_odds(posterior, market, weight = 0)
    expect_equal(blended, pmin(pmax(posterior, 1e-6), 1 - 1e-6), tolerance = 1e-9)
})

test_that("blend_market_odds with weight=1 returns market probability unchanged", {
    posterior <- c(0.65, 0.40)
    market    <- c(0.70, 0.35)
    blended   <- blend_market_odds(posterior, market, weight = 1)
    expect_equal(blended, pmin(pmax(market, 1e-6), 1 - 1e-6), tolerance = 1e-9)
})

test_that("blend_market_odds with weight=0.5 is the midpoint", {
    posterior <- 0.60
    market    <- 0.40
    blended   <- blend_market_odds(posterior, market, weight = 0.5)
    expect_equal(blended, 0.50, tolerance = 1e-9)
})

test_that("blend_market_odds returns values in (0, 1)", {
    posterior <- c(0.01, 0.50, 0.99)
    market    <- c(0.05, 0.55, 0.95)
    blended   <- blend_market_odds(posterior, market, weight = 0.3)
    expect_true(all(blended > 0 & blended < 1))
})

test_that("blend_market_odds errors on out-of-range weight", {
    expect_error(blend_market_odds(0.6, 0.5, weight = -0.1), regexp = "weight")
    expect_error(blend_market_odds(0.6, 0.5, weight = 1.1),  regexp = "weight")
})

test_that("blend_market_odds errors on out-of-range posterior_prob", {
    expect_error(blend_market_odds(1.5, 0.5, weight = 0.3), regexp = "posterior_prob")
})

test_that("blend_market_odds errors on out-of-range market_implied_prob", {
    expect_error(blend_market_odds(0.6, -0.1, weight = 0.3), regexp = "market_implied_prob")
})

# ── attach_market_odds ────────────────────────────────────────────────────────

make_toy_matchups <- function() {
    tibble::tibble(
        region       = c("East", "East"),
        round        = c("Round of 64", "Round of 64"),
        matchup_number = c(1L, 2L),
        teamA        = c("Alpha", "Gamma"),
        teamB        = c("Beta", "Delta"),
        teamA_seed   = c(1L, 2L),
        teamB_seed   = c(16L, 15L),
        win_prob_A   = c(0.85, 0.70),
        winner       = c("Alpha", "Gamma")
    )
}

test_that("attach_market_odds joins American-odds correctly", {
    matchups <- make_toy_matchups()
    market_odds <- tibble::tibble(
        teamA  = c("Alpha"),
        teamB  = c("Beta"),
        odds_a = c(-300),
        odds_b = c(+240)
    )
    result <- attach_market_odds(matchups, market_odds, weight = 0.5)

    expect_true("market_prob_A" %in% names(result))
    expect_true("market_prob_B" %in% names(result))
    expect_true("blended_prob_A" %in% names(result))

    # The joined row should have a blended probability
    joined <- result[result$teamA == "Alpha", ]
    expect_true(!is.na(joined$market_prob_A))
    expect_true(joined$blended_prob_A > 0 & joined$blended_prob_A < 1)

    # Rows without market data keep the posterior as the blend
    unjoined <- result[result$teamA == "Gamma", ]
    expect_true(is.na(unjoined$market_prob_A))
    expect_equal(unjoined$blended_prob_A, unjoined$win_prob_A)
})

test_that("attach_market_odds joins decimal-odds correctly", {
    matchups <- make_toy_matchups()
    market_odds <- tibble::tibble(
        teamA     = c("Alpha"),
        teamB     = c("Beta"),
        decimal_a = c(1.33),
        decimal_b = c(3.50)
    )
    result <- attach_market_odds(matchups, market_odds, weight = 0.3)
    joined <- result[result$teamA == "Alpha", ]
    expect_true(!is.na(joined$market_prob_A))
    expect_equal(joined$market_prob_A + joined$market_prob_B, 1, tolerance = 1e-9)
})

test_that("attach_market_odds joins pre-computed implied probabilities correctly", {
    matchups <- make_toy_matchups()
    market_odds <- tibble::tibble(
        teamA          = c("Alpha"),
        teamB          = c("Beta"),
        implied_prob_a = c(0.78)
    )
    result <- attach_market_odds(matchups, market_odds, weight = 0.3)
    joined <- result[result$teamA == "Alpha", ]
    expect_equal(joined$market_prob_A[[1]], 0.78, tolerance = 1e-9)
})

test_that("attach_market_odds errors when no recognised odds column is present", {
    matchups <- make_toy_matchups()
    bad_odds <- tibble::tibble(teamA = "Alpha", teamB = "Beta", win_pct = 0.8)
    expect_error(attach_market_odds(matchups, bad_odds), regexp = "market_odds must contain")
})

test_that("attach_market_odds returns posterior intact when market_odds is empty", {
    matchups <- make_toy_matchups()
    empty_odds <- tibble::tibble(teamA = character(), teamB = character(), implied_prob_a = numeric())
    result <- attach_market_odds(matchups, empty_odds)
    expect_equal(result$blended_prob_A, result$win_prob_A)
})

# ── default_project_config includes market_odds_weight ────────────────────────

test_that("default_project_config includes market_odds_weight", {
    config <- default_project_config()
    expect_true("market_odds_weight" %in% names(config$model))
    expect_equal(config$model$market_odds_weight, 0.3)
})
