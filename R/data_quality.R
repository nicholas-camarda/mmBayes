library(dplyr)
library(tibble)

#' Summarize unresolved result-team references by year
#'
#' @param missing_rows A data frame of unresolved result-team references.
#'
#' @return A character scalar summarizing unresolved teams grouped by year.
#' @keywords internal
format_missing_team_summary <- function(missing_rows) {
    if (nrow(missing_rows) == 0) {
        return("")
    }

    yearly <- missing_rows %>%
        dplyr::arrange(Year, Team, source_column) %>%
        dplyr::group_by(Year) %>%
        dplyr::summarise(
            teams = paste(unique(Team), collapse = ", "),
            .groups = "drop"
        )

    paste(
        sprintf("%s: %s", yearly$Year, yearly$teams),
        collapse = "; "
    )
}

#' Identify unresolved tournament-result teams
#'
#' @param team_features A normalized team feature table.
#' @param game_results A normalized game-results table.
#'
#' @return A tibble of result-team names that could not be matched by year.
#' @keywords internal
find_unresolved_result_teams <- function(team_features, game_results) {
    team_lookup <- team_features %>%
        dplyr::transmute(
            Year = as.character(Year),
            team_key = normalize_team_key(Team)
        ) %>%
        dplyr::distinct()

    result_teams <- dplyr::bind_rows(
        game_results %>%
            dplyr::transmute(Year = as.character(Year), source_column = "teamA", Team = as.character(teamA)),
        game_results %>%
            dplyr::transmute(Year = as.character(Year), source_column = "teamB", Team = as.character(teamB)),
        game_results %>%
            dplyr::transmute(Year = as.character(Year), source_column = "winner", Team = as.character(winner))
    ) %>%
        dplyr::mutate(
            Team = canonicalize_team_name(Team),
            team_key = normalize_team_key(Team)
        ) %>%
        dplyr::distinct()

    result_teams %>%
        dplyr::anti_join(team_lookup, by = c("Year", "team_key")) %>%
        dplyr::select(Year, source_column, Team) %>%
        dplyr::arrange(Year, Team, source_column)
}

#' Evaluate canonical data quality for modeling readiness
#'
#' @param team_features A team feature table.
#' @param game_results A game-results table.
#'
#' @return A list containing quality summaries, issue tables, and a pass flag.
#' @keywords internal
evaluate_canonical_data_quality <- function(team_features, game_results) {
    team_features <- normalize_team_features(team_features)
    game_results <- normalize_game_results(game_results)

    current_year <- max(suppressWarnings(as.integer(team_features$Year)), na.rm = TRUE)
    current_year <- as.character(current_year)
    historical_results <- game_results %>%
        dplyr::filter(Year != current_year)
    current_year_results <- game_results %>%
        dplyr::filter(Year == current_year)
    current_year_completed_results <- current_year_results %>%
        dplyr::filter(!is.na(teamA_score), !is.na(teamB_score))

    years <- sort(unique(as.character(historical_results$Year)))
    games_per_year <- historical_results %>%
        dplyr::count(Year, name = "games") %>%
        dplyr::rowwise() %>%
        dplyr::mutate(expected_games = sum(expected_completed_round_counts(Year))) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(ok = games == expected_games)

    round_counts <- historical_results %>%
        dplyr::count(Year, round, name = "games") %>%
        dplyr::mutate(round = factor(round, levels = names(expected_completed_round_counts()))) %>%
        dplyr::arrange(Year, round)

    expected_round_grid <- purrr::map_dfr(years, function(year) {
        tibble::tibble(
            Year = year,
            round = names(expected_completed_round_counts(year)),
            expected_games = unname(expected_completed_round_counts(year))
        )
    })

    round_count_issues <- expected_round_grid %>%
        dplyr::left_join(round_counts, by = c("Year", "round")) %>%
        dplyr::mutate(
            games = dplyr::coalesce(games, 0L),
            ok = games == expected_games
        ) %>%
        dplyr::filter(!ok)

    current_play_in_results <- prepare_current_play_in_results(team_features, game_results, current_year)
    valid_current_slots <- team_features %>%
        dplyr::filter(Year == current_year) %>%
        dplyr::count(Region, Seed, name = "n") %>%
        dplyr::filter(n > 1L) %>%
        dplyr::transmute(play_in_region = Region, slot_seed = Seed)

    current_year_non_play_in <- current_year_results %>%
        dplyr::filter(round != "First Four")

    current_year_round_scope_issues <- current_year_completed_results %>%
        dplyr::filter(
            (region %in% bracket_region_levels() & round %in% c("Final Four", "Championship")) |
                (region == "National" & round %in% c("Round of 64", "Round of 32", "Sweet 16", "Elite 8")) |
                (region == "First Four" & round != "First Four") |
                (round == "First Four" & region != "First Four")
        ) %>%
        dplyr::arrange(round, region, game_index)

    current_year_round_count_issues <- current_year_completed_results %>%
        dplyr::count(round, name = "games") %>%
        dplyr::mutate(
            expected_games = unname(expected_completed_round_counts()[round]),
            exceeds_expected = !is.na(expected_games) & games > expected_games
        ) %>%
        dplyr::filter(exceeds_expected)

    current_year_game_index_limits <- tibble::tibble(
        region = c(rep(bracket_region_levels(), each = 4L), "National", "National", "First Four"),
        round = c(
            rep(c("Round of 64", "Round of 32", "Sweet 16", "Elite 8"), times = length(bracket_region_levels())),
            "Final Four",
            "Championship",
            "First Four"
        ),
        max_game_index = c(rep(c(8L, 4L, 2L, 1L), times = length(bracket_region_levels())), 2L, 1L, 4L)
    )

    current_year_game_index_issues <- current_year_completed_results %>%
        dplyr::left_join(current_year_game_index_limits, by = c("region", "round")) %>%
        dplyr::filter(
            is.na(max_game_index) |
                is.na(game_index) |
                game_index < 1L |
                game_index > max_game_index
        ) %>%
        dplyr::select(Year, region, round, game_index, teamA, teamB, max_game_index) %>%
        dplyr::arrange(round, region, game_index)

    invalid_current_play_in <- current_play_in_results %>%
        dplyr::filter(
            is.na(play_in_region) |
                is.na(slot_seed) |
                teamA_seed != teamB_seed
        ) %>%
        dplyr::select(Year, teamA, teamB, winner, play_in_region, slot_seed)

    unmatched_current_slots <- current_play_in_results %>%
        dplyr::distinct(play_in_region, slot_seed) %>%
        dplyr::anti_join(valid_current_slots, by = c("play_in_region", "slot_seed"))

    duplicate_current_slots <- current_play_in_results %>%
        dplyr::count(play_in_region, slot_seed, name = "n") %>%
        dplyr::filter(n > 1L)

    suspicious_first_four <- game_results %>%
        dplyr::filter(
            region == "First Four",
            round == "First Four",
            teamA_seed == 1L,
            teamB_seed == 1L
        ) %>%
        dplyr::arrange(Year, game_index)

    invalid_score_rows <- game_results %>%
        dplyr::filter(
            xor(is.na(teamA_score), is.na(teamB_score)) |
                (
                    !is.na(teamA_score) &
                        !is.na(teamB_score) &
                        (
                            is.na(total_points) |
                                total_points != teamA_score + teamB_score |
                                winner != ifelse(teamA_score > teamB_score, teamA, teamB)
                        )
                )
        ) %>%
        dplyr::arrange(Year, round, region, game_index)

    unresolved_teams <- find_unresolved_result_teams(team_features, game_results)

    summary <- tibble::tibble(
        team_rows = nrow(team_features),
        result_rows = nrow(game_results),
        years = paste(sort(unique(as.character(game_results$Year))), collapse = ","),
        bad_game_years = sum(!games_per_year$ok),
        bad_round_rows = nrow(round_count_issues),
        current_year_completed_rows = nrow(current_year_completed_results),
        current_year_non_play_in_rows = nrow(current_year_non_play_in),
        current_year_round_scope_issue_rows = nrow(current_year_round_scope_issues),
        current_year_round_count_issue_rows = nrow(current_year_round_count_issues),
        current_year_game_index_issue_rows = nrow(current_year_game_index_issues),
        invalid_current_play_in_rows = nrow(invalid_current_play_in),
        unmatched_current_slot_rows = nrow(unmatched_current_slots),
        duplicate_current_slot_rows = nrow(duplicate_current_slots),
        suspicious_first_four_rows = nrow(suspicious_first_four),
        invalid_score_rows = nrow(invalid_score_rows),
        unresolved_team_rows = nrow(unresolved_teams),
        passed = all(games_per_year$ok) &&
            nrow(round_count_issues) == 0 &&
            nrow(current_year_round_scope_issues) == 0 &&
            nrow(current_year_round_count_issues) == 0 &&
            nrow(current_year_game_index_issues) == 0 &&
            nrow(invalid_current_play_in) == 0 &&
            nrow(unmatched_current_slots) == 0 &&
            nrow(duplicate_current_slots) == 0 &&
            nrow(suspicious_first_four) == 0 &&
            nrow(invalid_score_rows) == 0 &&
            nrow(unresolved_teams) == 0
    )

    list(
        summary = summary,
        games_per_year = games_per_year,
        round_counts = round_counts,
        round_count_issues = round_count_issues,
        current_year_completed_results = current_year_completed_results,
        current_year_non_play_in = current_year_non_play_in,
        current_year_round_scope_issues = current_year_round_scope_issues,
        current_year_round_count_issues = current_year_round_count_issues,
        current_year_game_index_issues = current_year_game_index_issues,
        current_play_in_results = current_play_in_results,
        invalid_current_play_in = invalid_current_play_in,
        unmatched_current_slots = unmatched_current_slots,
        duplicate_current_slots = duplicate_current_slots,
        suspicious_first_four = suspicious_first_four,
        invalid_score_rows = invalid_score_rows,
        unresolved_teams = unresolved_teams,
        passed = isTRUE(summary$passed[[1]])
    )
}

#' Assert that canonical data is ready for modeling
#'
#' @param team_features A team feature table.
#' @param game_results A game-results table.
#'
#' @return Invisibly returns the quality report when all checks pass.
#' @keywords internal
assert_canonical_data_quality <- function(team_features, game_results) {
    report <- evaluate_canonical_data_quality(team_features, game_results)

    issues <- character()
    bad_game_years <- report$games_per_year %>%
        dplyr::filter(!ok)
    if (nrow(bad_game_years) > 0) {
        issues <- c(
            issues,
            paste(
                "Unexpected completed-tournament game counts:",
                paste(sprintf("%s=%s", bad_game_years$Year, bad_game_years$games), collapse = ", ")
            )
        )
    }

    if (nrow(report$round_count_issues) > 0) {
        round_text <- report$round_count_issues %>%
            dplyr::mutate(text = sprintf("%s %s=%s (expected %s)", Year, round, games, expected_games)) %>%
            dplyr::pull(text)
        issues <- c(
            issues,
            paste("Unexpected per-year round counts:", paste(round_text, collapse = "; "))
        )
    }

    if (nrow(report$current_year_round_scope_issues) > 0) {
        current_text <- report$current_year_round_scope_issues %>%
            dplyr::mutate(text = sprintf("%s %s %s vs %s (%s)", Year, region, teamA, teamB, round)) %>%
            dplyr::pull(text)
        issues <- c(
            issues,
            paste("Current-year completed rows include impossible region/round combinations:", paste(current_text, collapse = "; "))
        )
    }

    if (nrow(report$current_year_round_count_issues) > 0) {
        current_round_text <- report$current_year_round_count_issues %>%
            dplyr::mutate(text = sprintf("%s=%s (expected at most %s)", round, games, expected_games)) %>%
            dplyr::pull(text)
        issues <- c(
            issues,
            paste("Current-year completed rows exceed tournament round maxima:", paste(current_round_text, collapse = "; "))
        )
    }

    if (nrow(report$current_year_game_index_issues) > 0) {
        current_game_index_text <- report$current_year_game_index_issues %>%
            dplyr::mutate(text = sprintf("%s %s #%s (max %s) %s vs %s", region, round, game_index, max_game_index, teamA, teamB)) %>%
            dplyr::pull(text)
        issues <- c(
            issues,
            paste("Current-year completed rows contain impossible game-index assignments:", paste(current_game_index_text, collapse = "; "))
        )
    }

    if (nrow(report$invalid_current_play_in) > 0) {
        invalid_text <- report$invalid_current_play_in %>%
            dplyr::mutate(text = sprintf("%s %s vs %s", Year, teamA, teamB)) %>%
            dplyr::pull(text)
        issues <- c(
            issues,
            paste("Current-year First Four rows could not be mapped to a valid region/seed slot:", paste(invalid_text, collapse = "; "))
        )
    }

    if (nrow(report$unmatched_current_slots) > 0) {
        slot_text <- report$unmatched_current_slots %>%
            dplyr::mutate(text = sprintf("%s %s-seed", play_in_region, slot_seed)) %>%
            dplyr::pull(text)
        issues <- c(
            issues,
            paste("Current-year First Four rows do not match the active bracket's duplicate seed slots:", paste(slot_text, collapse = "; "))
        )
    }

    if (nrow(report$duplicate_current_slots) > 0) {
        duplicate_text <- report$duplicate_current_slots %>%
            dplyr::mutate(text = sprintf("%s %s-seed=%s", play_in_region, slot_seed, n)) %>%
            dplyr::pull(text)
        issues <- c(
            issues,
            paste("Current-year First Four rows contain duplicate completed slots:", paste(duplicate_text, collapse = "; "))
        )
    }

    if (nrow(report$suspicious_first_four) > 0) {
        suspicious_text <- report$suspicious_first_four %>%
            dplyr::mutate(text = sprintf("%s %s vs %s", Year, teamA, teamB)) %>%
            dplyr::pull(text)
        issues <- c(
            issues,
            paste("Suspicious First Four rows with 1-seeds:", paste(suspicious_text, collapse = "; "))
        )
    }

    if (nrow(report$invalid_score_rows) > 0) {
        score_text <- report$invalid_score_rows %>%
            dplyr::mutate(
                text = sprintf(
                    "%s %s %s #%s (%s %s, %s %s, winner %s)",
                    Year,
                    round,
                    region,
                    game_index,
                    teamA,
                    teamA_score,
                    teamB,
                    teamB_score,
                    winner
                )
            ) %>%
            dplyr::pull(text)
        issues <- c(
            issues,
            paste("Invalid score-bearing rows detected:", paste(score_text, collapse = "; "))
        )
    }

    if (nrow(report$unresolved_teams) > 0) {
        issues <- c(
            issues,
            paste("Result teams missing matching pre-tournament features after aliasing:", format_missing_team_summary(report$unresolved_teams))
        )
    }

    if (length(issues) > 0) {
        stop_with_message(paste(issues, collapse = "\n"))
    }

    invisible(report)
}
