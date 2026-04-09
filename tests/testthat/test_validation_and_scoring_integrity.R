test_that("validate_game_results rejects winners that do not belong to the same row", {
    valid_row <- tibble::tibble(
        Year = "2025",
        region = "East",
        round = "Round of 64",
        game_index = 1L,
        teamA = "Alpha",
        teamB = "Beta",
        teamA_seed = 1L,
        teamB_seed = 16L,
        teamA_score = NA_integer_,
        teamB_score = NA_integer_,
        total_points = NA_integer_,
        winner = "Alpha"
    )
    invalid_row <- tibble::tibble(
        Year = "2025",
        region = "West",
        round = "Round of 64",
        game_index = 1L,
        teamA = "Gamma",
        teamB = "Delta",
        teamA_seed = 8L,
        teamB_seed = 9L,
        teamA_score = NA_integer_,
        teamB_score = NA_integer_,
        total_points = NA_integer_,
        winner = "Alpha"
    )

    expect_error(
        validate_game_results(dplyr::bind_rows(valid_row, invalid_row)),
        regexp = "Each game result must name either teamA or teamB as the winner"
    )
})

test_that("validate_game_results rejects impossible scored rows", {
    wrong_total <- tibble::tibble(
        Year = "2025",
        region = "East",
        round = "Round of 64",
        game_index = 1L,
        teamA = "Alpha",
        teamB = "Beta",
        teamA_seed = 1L,
        teamB_seed = 16L,
        teamA_score = 80L,
        teamB_score = 70L,
        total_points = 149L,
        winner = "Alpha"
    )
    wrong_winner <- wrong_total %>%
        dplyr::mutate(total_points = 150L, winner = "Beta")

    expect_error(
        validate_game_results(wrong_total),
        regexp = "total_points values must equal teamA_score \\+ teamB_score"
    )
    expect_error(
        validate_game_results(wrong_winner),
        regexp = "Game winners must match the higher score"
    )
})

test_that("validate_game_results rejects tied scored rows", {
    tied_game <- tibble::tibble(
        Year = "2025",
        region = "East",
        round = "Round of 64",
        game_index = 1L,
        teamA = "Alpha",
        teamB = "Beta",
        teamA_seed = 1L,
        teamB_seed = 16L,
        teamA_score = 75L,
        teamB_score = 75L,
        total_points = 150L,
        winner = "Alpha"
    )

    expect_error(
        validate_game_results(tied_game),
        regexp = "Tied scored games are not allowed"
    )
})

test_that("score_bracket_against_results preserves the single-year path when actuals include Year", {
    predicted_matchups <- tibble::tibble(
        region = "East",
        round = "Round of 64",
        matchup_number = 1L,
        winner = "Alpha"
    )
    actual_results <- tibble::tibble(
        Year = "2025",
        region = "East",
        round = "Round of 64",
        game_index = 1L,
        winner = "Alpha"
    )

    scored <- score_bracket_against_results(predicted_matchups, actual_results)

    expect_equal(scored$summary$correct_picks, 1)
    expect_equal(scored$summary$total_games, 1)
    expect_equal(scored$summary$bracket_score, unname(default_round_weights()[["Round of 64"]]))
    expect_true("Year" %in% names(scored$comparison))
})

test_that("score_bracket_against_results requires Year in actual results", {
    predicted_matchups <- tibble::tibble(
        region = "East",
        round = "Round of 64",
        matchup_number = 1L,
        winner = "Alpha"
    )
    actual_results <- tibble::tibble(
        region = "East",
        round = "Round of 64",
        game_index = 1L,
        winner = "Alpha"
    )

    expect_error(
        score_bracket_against_results(predicted_matchups, actual_results),
        regexp = "Year is required for scoring actual tournament results"
    )
})

test_that("score_bracket_against_results rejects ambiguous actual results", {
    predicted_matchups <- tibble::tibble(
        Year = "2025",
        region = "East",
        round = "Round of 64",
        matchup_number = 1L,
        winner = "Alpha"
    )
    actual_results <- tibble::tibble(
        Year = c("2025", "2025"),
        region = c("East", "East"),
        round = c("Round of 64", "Round of 64"),
        game_index = c(1L, 1L),
        winner = c("Alpha", "Alpha")
    )

    expect_error(
        score_bracket_against_results(predicted_matchups, actual_results),
        regexp = "Actual tournament results are ambiguous"
    )
})
