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

    years <- sort(unique(as.character(game_results$Year)))
    games_per_year <- game_results %>%
        dplyr::count(Year, name = "games") %>%
        dplyr::rowwise() %>%
        dplyr::mutate(expected_games = sum(expected_completed_round_counts(Year))) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(ok = games == expected_games)

    round_counts <- game_results %>%
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

    suspicious_first_four <- game_results %>%
        dplyr::filter(
            region == "First Four",
            round == "First Four",
            teamA_seed == 1L,
            teamB_seed == 1L
        ) %>%
        dplyr::arrange(Year, game_index)

    unresolved_teams <- find_unresolved_result_teams(team_features, game_results)

    summary <- tibble::tibble(
        team_rows = nrow(team_features),
        result_rows = nrow(game_results),
        years = paste(years, collapse = ","),
        bad_game_years = sum(!games_per_year$ok),
        bad_round_rows = nrow(round_count_issues),
        suspicious_first_four_rows = nrow(suspicious_first_four),
        unresolved_team_rows = nrow(unresolved_teams),
        passed = all(games_per_year$ok) &&
            nrow(round_count_issues) == 0 &&
            nrow(suspicious_first_four) == 0 &&
            nrow(unresolved_teams) == 0
    )

    list(
        summary = summary,
        games_per_year = games_per_year,
        round_counts = round_counts,
        round_count_issues = round_count_issues,
        suspicious_first_four = suspicious_first_four,
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

    if (nrow(report$suspicious_first_four) > 0) {
        suspicious_text <- report$suspicious_first_four %>%
            dplyr::mutate(text = sprintf("%s %s vs %s", Year, teamA, teamB)) %>%
            dplyr::pull(text)
        issues <- c(
            issues,
            paste("Suspicious First Four rows with 1-seeds:", paste(suspicious_text, collapse = "; "))
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
