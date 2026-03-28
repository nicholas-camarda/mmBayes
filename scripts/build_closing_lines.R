#!/usr/bin/env Rscript

#' Find the absolute path to the current script
#'
#' @return A normalized absolute path to the running script or working directory.
#' @keywords internal
find_script_path <- function() {
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("^--file=", args, value = TRUE)
    if (length(file_arg) == 0) {
        return(normalizePath(getwd()))
    }
    normalizePath(sub("^--file=", "", file_arg[1]))
}

#' Parse an optional `--year=` argument
#'
#' @param args Trailing command-line args.
#'
#' @return The parsed year (integer) or `NULL`.
#' @keywords internal
parse_year_arg <- function(args) {
    year_arg <- grep("^--year=", args, value = TRUE)
    if (length(year_arg) == 0) {
        return(NULL)
    }
    value <- sub("^--year=", "", year_arg[1])
    parsed <- suppressWarnings(as.integer(value))
    if (!is.finite(parsed) || is.na(parsed)) {
        stop_with_message(sprintf("Invalid --year value: %s", value))
    }
    parsed
}

script_path <- find_script_path()
project_root <- normalizePath(file.path(dirname(script_path), ".."))
setwd(project_root)

pkgload::load_all(project_root, export_all = TRUE, helpers = FALSE, quiet = TRUE)

load_dotenv_file(".env", override = FALSE)

args <- commandArgs(trailingOnly = TRUE)
config <- load_project_config("config.yml")

year_override <- parse_year_arg(args)

team_features <- normalize_team_features(read_table_file(config$data$team_features_path))
game_results <- normalize_game_results(read_table_file(config$data$game_results_path))
validate_team_features(team_features)
validate_game_results(game_results)
assert_canonical_data_quality(team_features, game_results)

bracket_year <- as.integer(year_override %||% get_bracket_year(team_features))

paths <- build_odds_history_paths(bracket_year, history_dir = config$betting$history_dir %||% default_runtime_history_root())
if (!file.exists(paths$lines_matchups)) {
    stop_with_message(sprintf("No odds history found at %s (capture a snapshot first).", paths$lines_matchups))
}

log_path <- file.path(config$output$path %||% default_runtime_output_root(), "logs", sprintf("closing_lines_%s.log", bracket_year))
initialize_logging(log_path)

lines_matchups <- dplyr::as_tibble(utils::read.csv(paths$lines_matchups, stringsAsFactors = FALSE))
lines_matchups <- lines_matchups %>%
    dplyr::mutate(
        snapshot_time_utc = parse_utc_timestamp(snapshot_time_utc),
        commence_time_utc = parse_utc_timestamp(commence_time)
    )

if (!has_betting_dispersion_columns(lines_matchups)) {
    if (!file.exists(paths$lines_long)) {
        stop_with_message(sprintf(
            "No dispersion columns were found in %s and the long-form odds file is missing at %s.",
            paths$lines_matchups,
            paths$lines_long
        ))
    }

    lines_long <- dplyr::as_tibble(utils::read.csv(paths$lines_long, stringsAsFactors = FALSE)) %>%
        dplyr::mutate(
            snapshot_time_utc = parse_utc_timestamp(snapshot_time_utc),
            commence_time_utc = parse_utc_timestamp(commence_time)
        )
    lines_matchups <- summarize_snapshot_matchups(lines_long)
}

results_year <- game_results %>%
    dplyr::filter(Year == as.character(bracket_year)) %>%
    dplyr::mutate(
        matchup_key = purrr::map2_chr(teamA, teamB, build_matchup_key)
    )

if (nrow(results_year) == 0) {
    stop_with_message(sprintf("No tournament results found for year %s in %s", bracket_year, config$data$game_results_path))
}

matchups_with_key <- lines_matchups %>%
    dplyr::mutate(matchup_key = key)

closing <- purrr::map_dfr(seq_len(nrow(results_year)), function(i) {
    row <- results_year[i, , drop = FALSE]
    key <- row$matchup_key[[1]]
    candidates <- matchups_with_key %>%
        dplyr::filter(matchup_key == !!key)

    if (nrow(candidates) == 0) {
        return(tibble::tibble(
            Year = as.character(bracket_year),
            region = row$region[[1]],
            round = row$round[[1]],
            game_index = row$game_index[[1]],
            teamA = row$teamA[[1]],
            teamB = row$teamB[[1]],
            winner = row$winner[[1]],
            closing_snapshot_time_utc = as.POSIXct(NA, tz = "UTC"),
            commence_time_utc = as.POSIXct(NA, tz = "UTC"),
            implied_prob_teamA = NA_real_,
            implied_prob_teamB = NA_real_,
            spread_teamA = NA_real_,
            spread_teamB = NA_real_,
            n_bookmakers = NA_integer_,
            bookmakers = NA_character_,
            prob_dispersion_a = NA_real_,
            spread_dispersion_a = NA_real_,
            closing_before_commence = NA
        ))
    }

    commence <- candidates$commence_time_utc[[1]]
    before <- candidates %>%
        dplyr::filter(!is.na(snapshot_time_utc) & !is.na(commence_time_utc) & snapshot_time_utc <= commence_time_utc) %>%
        dplyr::arrange(dplyr::desc(snapshot_time_utc))

    chosen <- if (nrow(before) > 0) {
        before[1, , drop = FALSE]
    } else {
        candidates %>% dplyr::arrange(snapshot_time_utc) %>% dplyr::slice(1)
    }

    team_a_sorted <- chosen$team_a[[1]]
    implied_a <- suppressWarnings(as.numeric(chosen$consensus_prob_a[[1]]))
    implied_b <- suppressWarnings(as.numeric(chosen$consensus_prob_b[[1]]))
    spread_a <- suppressWarnings(as.numeric(chosen$consensus_spread_a %||% NA_real_)[[1]])
    spread_b <- suppressWarnings(as.numeric(chosen$consensus_spread_b %||% NA_real_)[[1]])

    implied_teamA <- if (identical(canonicalize_team_name(row$teamA[[1]]), team_a_sorted)) implied_a else implied_b
    implied_teamB <- 1 - implied_teamA
    spread_teamA <- if (identical(canonicalize_team_name(row$teamA[[1]]), team_a_sorted)) spread_a else spread_b
    spread_teamB <- if (is.finite(spread_teamA)) -spread_teamA else NA_real_

    tibble::tibble(
        Year = as.character(bracket_year),
        region = row$region[[1]],
        round = row$round[[1]],
        game_index = row$game_index[[1]],
        teamA = row$teamA[[1]],
        teamB = row$teamB[[1]],
        winner = row$winner[[1]],
        closing_snapshot_time_utc = chosen$snapshot_time_utc[[1]],
        commence_time_utc = chosen$commence_time_utc[[1]],
        implied_prob_teamA = implied_teamA,
        implied_prob_teamB = implied_teamB,
        spread_teamA = spread_teamA,
        spread_teamB = spread_teamB,
        n_bookmakers = suppressWarnings(as.integer(chosen$n_bookmakers[[1]])),
        bookmakers = chosen$bookmakers[[1]],
        prob_dispersion_a = suppressWarnings(as.numeric(chosen$prob_dispersion_a[[1]])),
        spread_dispersion_a = suppressWarnings(as.numeric(chosen$spread_dispersion_a[[1]])),
        closing_before_commence = nrow(before) > 0
    )
})

utils::write.csv(closing, paths$closing_lines, row.names = FALSE)

cat("Built closing lines for bracket year:", bracket_year, "\n")
cat(sprintf("- Closing lines: %s\n", paths$closing_lines))
cat(sprintf("- Odds history: %s\n", paths$lines_matchups))
cat(sprintf("- Tournament results: %s\n", config$data$game_results_path))
cat(sprintf("- Matched games: %s / %s\n", sum(is.finite(closing$implied_prob_teamA)), nrow(closing)))
cat(sprintf("- Log: %s\n", log_path))
