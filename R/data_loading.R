library(dplyr)
library(logger)

#' Drop leakage-prone columns from a team feature table
#'
#' @param data A data frame of pre-tournament team features.
#'
#' @return The input data with disallowed post-tournament columns removed.
#' @keywords internal
strip_leakage_columns <- function(data) {
    removable <- intersect(leakage_columns(), names(data))
    dplyr::select(data, -dplyr::all_of(removable))
}

#' Normalize the team feature table
#'
#' @param data A raw team-level feature table.
#'
#' @return A cleaned team feature table with safe derived fields and normalized keys.
#' @keywords internal
normalize_team_features <- function(data) {
    data %>%
        add_safe_pre_tournament_features() %>%
        strip_leakage_columns() %>%
        impute_numeric_columns() %>%
        dplyr::mutate(
            Year = as.character(Year),
            Team = canonicalize_team_name(Team),
            team_key = normalize_team_key(Team),
            Region = as.character(Region),
            Conf = as.character(Conf),
            Seed = as.integer(Seed)
        )
}

#' Normalize the game results table
#'
#' @param data A raw tournament game-results table.
#'
#' @return A cleaned game-results table with standardized column types and
#'   canonical score columns.
#' @keywords internal
normalize_game_results <- function(data) {
    rename_candidates <- c(
        Year = "Year",
        region = "region",
        round = "round",
        game_index = "game_index",
        teamA = "teamA",
        teamB = "teamB",
        teamA_seed = "teamA_seed",
        teamB_seed = "teamB_seed",
        teamA_score = "teamA_score",
        teamB_score = "teamB_score",
        total_points = "total_points",
        winner = "winner"
    )

    required_cols <- unname(rename_candidates[c("Year", "region", "round", "game_index", "teamA", "teamB", "teamA_seed", "teamB_seed", "winner")])
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
        stop_with_message(
            sprintf("Game results validation failed; missing columns: %s", paste(missing_cols, collapse = ", "))
        )
    }

    if (!"teamA_score" %in% names(data)) {
        data$teamA_score <- NA_integer_
    }
    if (!"teamB_score" %in% names(data)) {
        data$teamB_score <- NA_integer_
    }
    if (!"total_points" %in% names(data)) {
        data$total_points <- NA_integer_
    }

    data %>%
        dplyr::mutate(
            Year = as.character(Year),
            region = as.character(region),
            round = as.character(round),
            game_index = as.integer(game_index),
            teamA = canonicalize_team_name(teamA),
            teamB = canonicalize_team_name(teamB),
            teamA_seed = as.integer(teamA_seed),
            teamB_seed = as.integer(teamB_seed),
            teamA_score = as.integer(teamA_score),
            teamB_score = as.integer(teamB_score),
            total_points = dplyr::if_else(
                !is.na(total_points),
                as.integer(total_points),
                dplyr::if_else(
                    !is.na(teamA_score) & !is.na(teamB_score),
                    as.integer(teamA_score + teamB_score),
                    NA_integer_
                )
            ),
            winner = canonicalize_team_name(winner)
        )
}

#' Normalize current-year First Four rows for bracket resolution
#'
#' @param team_features A normalized team feature table.
#' @param game_results A normalized game-results table.
#' @param bracket_year The active bracket year.
#'
#' @return A tibble containing current-year First Four rows with inferred
#'   `play_in_region` and `slot_seed` columns.
#' @keywords internal
prepare_current_play_in_results <- function(team_features, game_results, bracket_year) {
    play_in_results <- game_results %>%
        dplyr::filter(
            Year == bracket_year,
            round == "First Four"
        )

    if (nrow(play_in_results) == 0) {
        return(tibble::tibble(
            Year = character(),
            region = character(),
            round = character(),
            game_index = integer(),
            teamA = character(),
            teamB = character(),
            teamA_seed = integer(),
            teamB_seed = integer(),
            teamA_score = integer(),
            teamB_score = integer(),
            total_points = integer(),
            winner = character(),
            play_in_region = character(),
            slot_seed = integer()
        ))
    }

    team_lookup <- team_features %>%
        dplyr::transmute(
            Year = as.character(Year),
            team_key = normalize_team_key(Team),
            lookup_region = Region,
            lookup_seed = Seed
        ) %>%
        dplyr::distinct()

    play_in_results %>%
        dplyr::mutate(
            teamA_key = normalize_team_key(teamA),
            teamB_key = normalize_team_key(teamB)
        ) %>%
        dplyr::left_join(
            team_lookup %>%
                dplyr::rename(
                    teamA_region = lookup_region,
                    teamA_lookup_seed = lookup_seed
                ),
            by = c("Year", "teamA_key" = "team_key")
        ) %>%
        dplyr::left_join(
            team_lookup %>%
                dplyr::rename(
                    teamB_region = lookup_region,
                    teamB_lookup_seed = lookup_seed
                ),
            by = c("Year", "teamB_key" = "team_key")
        ) %>%
        dplyr::mutate(
            play_in_region = dplyr::case_when(
                region %in% bracket_region_levels() ~ region,
                !is.na(teamA_region) & !is.na(teamB_region) & teamA_region == teamB_region ~ teamA_region,
                TRUE ~ NA_character_
            ),
            slot_seed = dplyr::coalesce(teamA_seed, teamB_seed, teamA_lookup_seed, teamB_lookup_seed)
        ) %>%
        dplyr::select(
            Year,
            region,
            round,
            game_index,
            teamA,
            teamB,
            teamA_seed,
            teamB_seed,
            teamA_score,
            teamB_score,
            total_points,
            winner,
            play_in_region,
            slot_seed
        )
}

#' Validate the team feature table
#'
#' @param data A team-level feature table.
#'
#' @return `TRUE` if validation passes.
#' @keywords internal
validate_team_features <- function(data) {
    validate_input_data(data, pre_tournament_feature_columns())

    exposed_leakage <- intersect(leakage_columns(), names(data))
    if (length(exposed_leakage) > 0) {
        stop_with_message(
            sprintf("Team feature table still exposes leakage columns: %s", paste(exposed_leakage, collapse = ", "))
        )
    }

    TRUE
}

#' Validate the game results table
#'
#' @param data A tournament game-results table.
#'
#' @return `TRUE` if validation passes.
#' @keywords internal
validate_game_results <- function(data) {
    required_cols <- c("Year", "region", "round", "game_index", "teamA", "teamB", "teamA_seed", "teamB_seed", "winner")
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
        stop_with_message(
            sprintf("Game results validation failed; missing columns: %s", paste(missing_cols, collapse = ", "))
        )
    }

    if (!all(data$winner %in% c(data$teamA, data$teamB))) {
        stop_with_message("Each game result must name either teamA or teamB as the winner")
    }

    partial_scores <- xor(is.na(data$teamA_score), is.na(data$teamB_score))
    if (any(partial_scores)) {
        stop_with_message("Game score columns must be either both populated or both missing")
    }

    scored_rows <- !is.na(data$teamA_score) & !is.na(data$teamB_score)
    if (any(scored_rows)) {
        expected_total <- data$teamA_score[scored_rows] + data$teamB_score[scored_rows]
        if (any(is.na(data$total_points[scored_rows]) | data$total_points[scored_rows] != expected_total)) {
            stop_with_message("Game total_points values must equal teamA_score + teamB_score when scores are present")
        }

        score_winner <- ifelse(data$teamA_score[scored_rows] > data$teamB_score[scored_rows], data$teamA[scored_rows], data$teamB[scored_rows])
        if (any(data$winner[scored_rows] != score_winner)) {
            stop_with_message("Game winners must match the higher score when scores are present")
        }
    }

    TRUE
}

#' Determine the active bracket year
#'
#' @param team_data Optional team feature table used to derive the newest available year.
#'
#' @return A character scalar containing the active bracket year.
#' @keywords internal
get_bracket_year <- function(team_data = NULL) {
    preferred_year <- format(Sys.Date(), "%Y")
    if (is.null(team_data)) {
        return(preferred_year)
    }

    available_years <- sort(unique(as.character(team_data$Year)))
    if (preferred_year %in% available_years) {
        preferred_year
    } else {
        max(available_years)
    }
}

#' Select historical tournament years for training
#'
#' @param team_data A team feature table.
#' @param game_results A tournament game-results table.
#' @param bracket_year The active bracket year to exclude.
#' @param history_window Number of completed historical tournaments to retain.
#'
#' @return A character vector of historical training years.
#' @keywords internal
select_historical_years <- function(team_data, game_results, bracket_year, history_window = 8L) {
    team_years <- unique(as.character(team_data$Year))
    result_years <- unique(as.character(game_results$Year))

    eligible <- intersect(team_years, result_years)
    eligible <- eligible[eligible != bracket_year]
    eligible <- eligible[eligible != "2020"]
    eligible <- sort(eligible)

    if (length(eligible) == 0) {
        stop_with_message("No completed historical tournament years are available for model training")
    }

    utils::tail(eligible, history_window)
}

#' Prepare the current bracket's team rows
#'
#' @param team_data A normalized team feature table.
#' @param bracket_year The active bracket year.
#'
#' @return A data frame containing only the current tournament teams.
#' @keywords internal
prepare_current_teams <- function(team_data, bracket_year) {
    current <- team_data %>%
        dplyr::filter(Year == bracket_year)

    if (nrow(current) == 0) {
        stop_with_message(sprintf("No current tournament teams found for bracket year %s", bracket_year))
    }

    current
}

#' Build explicit historical matchup rows
#'
#' @param team_features A historical team feature table.
#' @param game_results A historical tournament game-results table.
#'
#' @return A matchup-level training table with forward and reversed game rows.
#' @keywords internal
build_explicit_matchup_history <- function(team_features, game_results) {
    if (!"team_key" %in% names(team_features)) {
        team_features <- team_features %>%
            dplyr::mutate(team_key = normalize_team_key(Team))
    }

    team_lookup <- team_features %>%
        dplyr::distinct(Year, team_key, .keep_all = TRUE)

    game_results <- game_results %>%
        dplyr::mutate(
            teamA_key = normalize_team_key(teamA),
            teamB_key = normalize_team_key(teamB)
        )

    joined <- game_results %>%
        dplyr::left_join(team_lookup, by = c("Year", "teamA_key" = "team_key"), suffix = c("", "_teamA")) %>%
        dplyr::rename_with(~ paste0(.x, "_teamA"), .cols = c(Seed, Region, Conf, dplyr::all_of(pre_tournament_feature_columns()))) %>%
        dplyr::left_join(team_lookup, by = c("Year", "teamB_key" = "team_key"), suffix = c("", "_teamB")) %>%
        dplyr::rename_with(~ paste0(.x, "_teamB"), .cols = c(Seed, Region, Conf, dplyr::all_of(pre_tournament_feature_columns())))

    missing_team_rows <- joined %>%
        dplyr::filter(is.na(Seed_teamA) | is.na(Seed_teamB))
    if (nrow(missing_team_rows) > 0) {
        missing_teams <- dplyr::bind_rows(
            missing_team_rows %>%
                dplyr::filter(is.na(Seed_teamA)) %>%
                dplyr::transmute(Year, source_column = "teamA", Team = teamA),
            missing_team_rows %>%
                dplyr::filter(is.na(Seed_teamB)) %>%
                dplyr::transmute(Year, source_column = "teamB", Team = teamB)
        ) %>%
            dplyr::distinct() %>%
            dplyr::arrange(Year, Team, source_column)
        stop_with_message(
            sprintf(
                "Missing pre-tournament features for historical teams after aliasing: %s",
                format_missing_team_summary(missing_teams)
            )
        )
    }

    forward_rows <- purrr::pmap_dfr(
        joined,
        function(...) {
            row <- tibble::as_tibble(list(...))
            team_a <- tibble::tibble(
                Year = row$Year,
                Team = row$teamA,
                Seed = row$Seed_teamA,
                Region = row$Region_teamA,
                Conf = row$Conf_teamA
            )
            team_b <- tibble::tibble(
                Year = row$Year,
                Team = row$teamB,
                Seed = row$Seed_teamB,
                Region = row$Region_teamB,
                Conf = row$Conf_teamB
            )

            for (feature_name in pre_tournament_feature_columns()) {
                team_a[[feature_name]] <- row[[paste0(feature_name, "_teamA")]]
                team_b[[feature_name]] <- row[[paste0(feature_name, "_teamB")]]
            }

            build_matchup_feature_row(
                team_a = team_a,
                team_b = team_b,
                round_name = row$round,
                actual_outcome = as.numeric(row$winner == row$teamA),
                metadata = list(
                    Year = row$Year,
                    region = row$region,
                    game_index = row$game_index,
                    teamA = row$teamA,
                    teamB = row$teamB,
                    winner = row$winner
                )
            )
        }
    )

    reverse_rows <- forward_rows %>%
        dplyr::mutate(
            teamA_original = teamA,
            teamB_original = teamB,
            teamA = teamB_original,
            teamB = teamA_original,
            winner = dplyr::if_else(actual_outcome >= 0.5, teamB_original, teamA_original),
            actual_outcome = 1 - actual_outcome,
            same_conf = same_conf,
            seed_diff = -seed_diff,
            barthag_logit_diff = -barthag_logit_diff,
            AdjOE_diff = -AdjOE_diff,
            AdjDE_diff = -AdjDE_diff,
            WAB_diff = -WAB_diff,
            TOR_diff = -TOR_diff,
            TORD_diff = -TORD_diff,
            ORB_diff = -ORB_diff,
            DRB_diff = -DRB_diff,
            `3P%_diff` = -`3P%_diff`,
            `3P%D_diff` = -`3P%D_diff`,
            `Adj T._diff` = -`Adj T._diff`
        ) %>%
        dplyr::select(-teamA_original, -teamB_original)

    dplyr::bind_rows(forward_rows, reverse_rows) %>%
        dplyr::mutate(
            round = factor(round, levels = round_levels())
        )
}

#' Join actual game results back to team features
#'
#' @param team_features A historical team feature table.
#' @param game_results A historical tournament game-results table.
#'
#' @return A game-results table augmented with team A and team B features.
#' @keywords internal
build_actual_game_reference <- function(team_features, game_results) {
    if (!"team_key" %in% names(team_features)) {
        team_features <- team_features %>%
            dplyr::mutate(team_key = normalize_team_key(Team))
    }

    team_lookup <- team_features %>%
        dplyr::distinct(Year, team_key, .keep_all = TRUE)

    game_results %>%
        dplyr::mutate(
            teamA_key = normalize_team_key(teamA),
            teamB_key = normalize_team_key(teamB)
        ) %>%
        dplyr::left_join(team_lookup, by = c("Year", "teamA_key" = "team_key")) %>%
        dplyr::rename_with(~ paste0(.x, "_teamA"), .cols = c(Seed, Region, Conf, dplyr::all_of(pre_tournament_feature_columns()))) %>%
        dplyr::left_join(team_lookup, by = c("Year", "teamB_key" = "team_key")) %>%
        dplyr::rename_with(~ paste0(.x, "_teamB"), .cols = c(Seed, Region, Conf, dplyr::all_of(pre_tournament_feature_columns())))
}

#' Load and assemble tournament modeling inputs
#'
#' @param config A project configuration list.
#' @param include_betting_history Whether to attach historical betting features
#'   to the historical matchup training rows.
#'
#' @return A list containing historical matchup rows, historical teams, current
#'   teams, score-bearing historical results, actual historical results, and the
#'   active bracket year.
#' @export
load_tournament_data <- function(config, include_betting_history = TRUE) {
    logger::log_info("Starting data loading process")

    team_path <- config$data$team_features_path %||% config$data_path
    results_path <- config$data$game_results_path

    team_features <- normalize_team_features(read_table_file(team_path))
    game_results <- normalize_game_results(read_table_file(results_path))

    validate_team_features(team_features)
    validate_game_results(game_results)
    assert_canonical_data_quality(team_features, game_results)

    bracket_year <- get_bracket_year(team_features)
    logger::log_info("Using bracket year: {bracket_year}")

    historical_years <- select_historical_years(
        team_data = team_features,
        game_results = game_results,
        bracket_year = bracket_year,
        history_window = config$model$history_window %||% 8L
    )

    historical_teams <- team_features %>%
        dplyr::filter(Year %in% historical_years)
    current_teams <- prepare_current_teams(team_features, bracket_year)
    current_play_in_results <- prepare_current_play_in_results(team_features, game_results, bracket_year)
    historical_games <- game_results %>%
        dplyr::filter(Year %in% historical_years)

    historical_closing_lines <- load_historical_closing_lines(config$betting$history_dir %||% NULL)
    historical_betting_features <- build_historical_betting_feature_table(historical_closing_lines)

    historical_matchups <- build_explicit_matchup_history(historical_teams, historical_games)
    if (isTRUE(include_betting_history) && nrow(historical_betting_features) > 0) {
        historical_matchups <- augment_matchup_rows_with_betting_features(
            historical_matchups,
            historical_betting_features = historical_betting_features
        )
    }
    historical_actual_results <- build_actual_game_reference(historical_teams, historical_games)

    list(
        bracket_year = bracket_year,
        historical_matchups = historical_matchups,
        historical_teams = historical_teams,
        historical_games = historical_games,
        historical_actual_results = historical_actual_results,
        historical_betting_features = historical_betting_features,
        current_teams = current_teams,
        current_play_in_results = current_play_in_results,
        game_results = game_results
    )
}
