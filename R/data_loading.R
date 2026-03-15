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
            Team = as.character(Team),
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
#' @return A cleaned game-results table with standardized column types.
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
        winner = "winner"
    )

    missing_cols <- setdiff(unname(rename_candidates), names(data))
    if (length(missing_cols) > 0) {
        stop_with_message(
            sprintf("Game results validation failed; missing columns: %s", paste(missing_cols, collapse = ", "))
        )
    }

    data %>%
        dplyr::mutate(
            Year = as.character(Year),
            region = as.character(region),
            round = as.character(round),
            game_index = as.integer(game_index),
            teamA = as.character(teamA),
            teamB = as.character(teamB),
            teamA_seed = as.integer(teamA_seed),
            teamB_seed = as.integer(teamB_seed),
            winner = as.character(winner)
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
        missing_teams <- unique(c(missing_team_rows$teamA[is.na(missing_team_rows$Seed_teamA)], missing_team_rows$teamB[is.na(missing_team_rows$Seed_teamB)]))
        stop_with_message(
            sprintf("Missing pre-tournament features for historical teams: %s", paste(missing_teams, collapse = ", "))
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
#'
#' @return A list containing historical matchup rows, historical teams, current
#'   teams, actual historical results, and the active bracket year.
#' @export
load_tournament_data <- function(config) {
    logger::log_info("Starting data loading process")

    team_path <- config$data$team_features_path %||% config$data_path
    results_path <- config$data$game_results_path

    team_features <- normalize_team_features(read_table_file(team_path))
    game_results <- normalize_game_results(read_table_file(results_path))

    validate_team_features(team_features)
    validate_game_results(game_results)

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
    historical_games <- game_results %>%
        dplyr::filter(Year %in% historical_years)

    historical_matchups <- build_explicit_matchup_history(historical_teams, historical_games)
    historical_actual_results <- build_actual_game_reference(historical_teams, historical_games)

    list(
        bracket_year = bracket_year,
        historical_matchups = historical_matchups,
        historical_teams = historical_teams,
        historical_actual_results = historical_actual_results,
        current_teams = current_teams,
        game_results = game_results
    )
}
