library(dplyr)
library(httr)
library(jsonlite)
library(tibble)

#' Build the odds-history directory for a tournament year
#'
#' Odds history is intentionally stored under `data/`, which is gitignored in
#' this repository. This keeps snapshots private while enabling local
#' backtests and evaluation.
#'
#' @param year Tournament year (integer-like).
#' @param history_dir Base directory for odds history storage.
#'
#' @return A path to the year-specific odds-history directory.
#' @export
build_odds_history_year_dir <- function(year, history_dir = "data/odds_history") {
    file.path(history_dir %||% "data/odds_history", as.character(as.integer(year)))
}

#' Build canonical odds-history file paths for a tournament year
#'
#' @param year Tournament year (integer-like).
#' @param history_dir Base directory for odds history storage.
#'
#' @return A named list of paths for snapshots, long-form odds, and matchup odds.
#' @keywords internal
build_odds_history_paths <- function(year, history_dir = "data/odds_history") {
    year_dir <- build_odds_history_year_dir(year, history_dir = history_dir)
    list(
        year_dir = year_dir,
        snapshots_dir = file.path(year_dir, "snapshots"),
        lines_long = file.path(year_dir, "lines_long.csv"),
        lines_matchups = file.path(year_dir, "lines_matchups.csv"),
        latest_lines_matchups = file.path(year_dir, "latest_lines_matchups.csv"),
        closing_lines = file.path(year_dir, "closing_lines.csv")
    )
}

#' Ensure odds-history directories exist
#'
#' @param paths Paths returned by [build_odds_history_paths()].
#'
#' @return Invisibly returns `TRUE` after creating the directories.
#' @keywords internal
ensure_odds_history_dirs <- function(paths) {
    dir.create(paths$year_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(paths$snapshots_dir, recursive = TRUE, showWarnings = FALSE)
    invisible(TRUE)
}

#' Return the Odds API host used for requests
#'
#' @return The Odds API base URL.
#' @keywords internal
odds_api_host <- function() {
    "https://api.the-odds-api.com"
}

#' Convert American odds to implied probability (with vig)
#'
#' @param odds American odds (numeric). Positive odds are underdogs; negative odds
#'   are favorites.
#'
#' @return A probability between 0 and 1 (or `NA_real_` when invalid).
#' @export
american_to_implied_prob <- function(odds) {
    odds <- suppressWarnings(as.numeric(odds))
    odds[!is.finite(odds)] <- NA_real_

    implied <- rep(NA_real_, length(odds))
    positive <- !is.na(odds) & odds > 0
    negative <- !is.na(odds) & odds < 0

    implied[positive] <- 100 / (odds[positive] + 100)
    implied[negative] <- (-odds[negative]) / ((-odds[negative]) + 100)
    implied
}

#' Remove vig from a two-way implied probability market
#'
#' @param prob_a Implied probability for outcome A (with vig).
#' @param prob_b Implied probability for outcome B (with vig).
#'
#' @return A length-2 numeric vector of vig-free probabilities for outcomes A and B.
#' @export
remove_vig_two_way <- function(prob_a, prob_b) {
    prob_a <- suppressWarnings(as.numeric(prob_a))
    prob_b <- suppressWarnings(as.numeric(prob_b))
    if (!is.finite(prob_a) || !is.finite(prob_b) || is.na(prob_a) || is.na(prob_b)) {
        return(c(NA_real_, NA_real_))
    }
    total <- prob_a + prob_b
    if (!is.finite(total) || total <= 0) {
        return(c(NA_real_, NA_real_))
    }
    c(prob_a / total, prob_b / total)
}

#' Build an order-invariant matchup key for two teams
#'
#' @param team_a Team name A.
#' @param team_b Team name B.
#'
#' @return A scalar character key.
#' @export
build_matchup_key <- function(team_a, team_b) {
    a <- canonicalize_team_name(team_a)
    b <- canonicalize_team_name(team_b)
    parts <- sort(c(a, b))
    paste(parts, collapse = "|||")
}

#' Fetch raw odds JSON from The Odds API
#'
#' This function intentionally avoids logging any request URL to prevent
#' accidentally exposing an API key.
#'
#' @param api_key Odds API key.
#' @param sport_key Sport key (e.g., `basketball_ncaab`).
#' @param regions Region key string (e.g., `us`).
#' @param markets Character vector of markets (e.g., `c("h2h","spreads")`).
#' @param bookmakers Optional character vector of bookmaker keys.
#' @param odds_format Odds format (`american` or `decimal`).
#' @param date_format Date format (`iso` or `unix`).
#'
#' @return A list with `raw_json`, `parsed`, `headers`, and `retrieved_at`.
#' @export
fetch_odds_api_odds <- function(api_key,
                               sport_key,
                               regions = "us",
                               markets = c("h2h", "spreads"),
                               bookmakers = NULL,
                               odds_format = "american",
                               date_format = "iso") {
    if (!nzchar(api_key %||% "")) {
        stop_with_message("Missing Odds API key (set the configured environment variable).")
    }
    endpoint <- sprintf("%s/v4/sports/%s/odds", odds_api_host(), sport_key)

    query <- list(
        apiKey = api_key,
        regions = regions %||% "us",
        markets = paste(markets, collapse = ","),
        oddsFormat = odds_format %||% "american",
        dateFormat = date_format %||% "iso"
    )
    if (!is.null(bookmakers) && length(bookmakers) > 0) {
        query$bookmakers <- paste(bookmakers, collapse = ",")
    }

    response <- tryCatch(
        httr::GET(endpoint, query = query, httr::timeout(20)),
        error = function(e) {
            stop_with_message("Odds API request failed (network error).")
        }
    )
    headers <- httr::headers(response)
    retrieved_at <- Sys.time()

    if (httr::status_code(response) >= 400) {
        stop_with_message(
            sprintf(
                "Odds API request failed with status %s",
                httr::status_code(response)
            )
        )
    }

    raw_json <- httr::content(response, as = "text", encoding = "UTF-8")
    parsed <- jsonlite::fromJSON(raw_json, simplifyVector = FALSE)

    list(
        raw_json = raw_json,
        parsed = parsed,
        headers = headers,
        retrieved_at = retrieved_at
    )
}

#' Filter Odds API event objects down to current tournament teams
#'
#' @param events A list of Odds API event objects (from `/odds`).
#' @param current_teams A current-year team table from [load_tournament_data()].
#'
#' @return A filtered list of events likely corresponding to tournament games.
#' @export
filter_odds_events_to_tournament <- function(events, current_teams) {
    if (length(events) == 0) {
        return(list())
    }

    allowed <- unique(canonicalize_team_name(current_teams$Team))
    Filter(function(event) {
        home <- canonicalize_team_name(event$home_team %||% "")
        away <- canonicalize_team_name(event$away_team %||% "")
        nzchar(home) && nzchar(away) && home %in% allowed && away %in% allowed
    }, events)
}

#' Normalize Odds API events into a long-form odds table
#'
#' @param events Odds API event list.
#' @param bracket_year Tournament year.
#' @param snapshot_time Timestamp when the snapshot was retrieved.
#'
#' @return A tibble with one row per event × bookmaker × market × outcome.
#' @export
normalize_odds_events_long <- function(events, bracket_year, snapshot_time = Sys.time()) {
    if (length(events) == 0) {
        return(tibble::tibble())
    }

    snapshot_time_utc <- as.POSIXct(snapshot_time, tz = "UTC")

    rows <- purrr::map_dfr(events, function(event) {
        event_id <- as.character(event$id %||% "")
        commence_time <- as.character(event$commence_time %||% NA_character_)
        home_team <- canonicalize_team_name(event$home_team %||% "")
        away_team <- canonicalize_team_name(event$away_team %||% "")
        bookmakers <- event$bookmakers %||% list()

        purrr::map_dfr(bookmakers, function(book) {
            book_key <- as.character(book$key %||% NA_character_)
            book_title <- as.character(book$title %||% NA_character_)
            book_last_update <- as.character(book$last_update %||% NA_character_)
            markets <- book$markets %||% list()

            purrr::map_dfr(markets, function(market) {
                market_key <- as.character(market$key %||% NA_character_)
                market_last_update <- as.character(market$last_update %||% NA_character_)
                outcomes <- market$outcomes %||% list()

                purrr::map_dfr(outcomes, function(outcome) {
                    tibble::tibble(
                        bracket_year = as.integer(bracket_year),
                        snapshot_time_utc = snapshot_time_utc,
                        event_id = event_id,
                        commence_time = commence_time,
                        home_team = home_team,
                        away_team = away_team,
                        bookmaker_key = book_key,
                        bookmaker_title = book_title,
                        bookmaker_last_update = book_last_update,
                        market_key = market_key,
                        market_last_update = market_last_update,
                        outcome_name = canonicalize_team_name(outcome$name %||% NA_character_),
                        outcome_price = suppressWarnings(as.numeric(outcome$price %||% NA_real_)),
                        outcome_point = suppressWarnings(as.numeric(outcome$point %||% NA_real_))
                    )
                })
            })
        })
    })

    rows
}

#' Summarize a snapshot long-form odds table into matchup-level consensus lines
#'
#' Consensus is defined as the median vig-free implied win probability across
#' available bookmakers for the `h2h` market. Spread points are summarized as
#' the median point for each team in the `spreads` market when present.
#'
#' @param lines_long Long-form odds rows from [normalize_odds_events_long()].
#'
#' @return A tibble containing one row per event with consensus probabilities.
#' @export
summarize_snapshot_matchups <- function(lines_long) {
    if (nrow(lines_long) == 0) {
        return(tibble::tibble())
    }

    h2h <- lines_long %>%
        dplyr::filter(market_key == "h2h") %>%
        dplyr::filter(!is.na(outcome_price), nzchar(outcome_name))

    spreads <- lines_long %>%
        dplyr::filter(market_key == "spreads") %>%
        dplyr::filter(!is.na(outcome_point), nzchar(outcome_name))

    h2h_pairs <- h2h %>%
        dplyr::select(snapshot_time_utc, bracket_year, event_id, commence_time, home_team, away_team, bookmaker_key, outcome_name, outcome_price) %>%
        dplyr::group_by(snapshot_time_utc, bracket_year, event_id, commence_time, home_team, away_team, bookmaker_key) %>%
        dplyr::summarise(
            teams = list(outcome_name),
            prices = list(outcome_price),
            .groups = "drop"
        ) %>%
        dplyr::mutate(
            n_outcomes = vapply(teams, length, integer(1))
        ) %>%
        dplyr::filter(n_outcomes == 2L) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
            team_1 = canonicalize_team_name(teams[[1]][[1]]),
            team_2 = canonicalize_team_name(teams[[1]][[2]]),
            implied_1 = american_to_implied_prob(prices[[1]][[1]]),
            implied_2 = american_to_implied_prob(prices[[1]][[2]]),
            vig_free = list(remove_vig_two_way(implied_1, implied_2)),
            vig_free_1 = vig_free[[1]][[1]],
            vig_free_2 = vig_free[[1]][[2]],
            key = build_matchup_key(team_1, team_2),
            team_a = sort(c(team_1, team_2))[[1]],
            team_b = sort(c(team_1, team_2))[[2]],
            prob_a = if (identical(team_1, team_a)) vig_free_1 else vig_free_2,
            prob_b = 1 - prob_a
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(snapshot_time_utc, bracket_year, event_id, commence_time, home_team, away_team, bookmaker_key, key, team_a, team_b, prob_a, prob_b)

    spread_pairs <- spreads %>%
        dplyr::select(snapshot_time_utc, bracket_year, event_id, bookmaker_key, outcome_name, outcome_point) %>%
        dplyr::group_by(snapshot_time_utc, bracket_year, event_id, bookmaker_key) %>%
        dplyr::summarise(
            teams = list(outcome_name),
            points = list(outcome_point),
            .groups = "drop"
        ) %>%
        dplyr::mutate(n_outcomes = vapply(teams, length, integer(1))) %>%
        dplyr::filter(n_outcomes == 2L) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
            team_1 = canonicalize_team_name(teams[[1]][[1]]),
            team_2 = canonicalize_team_name(teams[[1]][[2]]),
            point_1 = suppressWarnings(as.numeric(points[[1]][[1]])),
            point_2 = suppressWarnings(as.numeric(points[[1]][[2]])),
            team_a = sort(c(team_1, team_2))[[1]],
            team_b = sort(c(team_1, team_2))[[2]],
            key = build_matchup_key(team_1, team_2),
            spread_a = if (identical(team_1, team_a)) point_1 else point_2,
            spread_b = if (identical(team_2, team_b)) point_2 else point_1
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(snapshot_time_utc, bracket_year, event_id, bookmaker_key, key, spread_a, spread_b)

    h2h_consensus <- h2h_pairs %>%
        dplyr::group_by(snapshot_time_utc, bracket_year, event_id, commence_time, home_team, away_team, key, team_a, team_b) %>%
        dplyr::summarise(
            n_bookmakers = dplyr::n(),
            bookmakers = paste(sort(unique(bookmaker_key)), collapse = ","),
            consensus_prob_a = stats::median(prob_a, na.rm = TRUE),
            consensus_prob_b = 1 - consensus_prob_a,
            .groups = "drop"
        )

    spread_consensus <- spread_pairs %>%
        dplyr::group_by(snapshot_time_utc, bracket_year, event_id, key) %>%
        dplyr::summarise(
            consensus_spread_a = stats::median(spread_a, na.rm = TRUE),
            consensus_spread_b = stats::median(spread_b, na.rm = TRUE),
            .groups = "drop"
        )

    h2h_consensus %>%
        dplyr::left_join(spread_consensus, by = c("snapshot_time_utc", "bracket_year", "event_id", "key")) %>%
        dplyr::arrange(snapshot_time_utc, event_id)
}

#' Append odds tables to year-level history files
#'
#' @param lines_long Long-form odds table for a snapshot.
#' @param lines_matchups Matchup-level table for a snapshot.
#' @param paths Output paths from [build_odds_history_paths()].
#'
#' @return Invisibly returns `TRUE` on success.
#' @keywords internal
append_odds_history_files <- function(lines_long, lines_matchups, paths) {
    ensure_odds_history_dirs(paths)

    if (nrow(lines_long) > 0) {
        if (!file.exists(paths$lines_long)) {
            utils::write.csv(lines_long, paths$lines_long, row.names = FALSE)
        } else {
            utils::write.table(lines_long, paths$lines_long, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
        }
    }

    if (nrow(lines_matchups) > 0) {
        if (!file.exists(paths$lines_matchups)) {
            utils::write.csv(lines_matchups, paths$lines_matchups, row.names = FALSE)
        } else {
            utils::write.table(lines_matchups, paths$lines_matchups, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
        }
        utils::write.csv(lines_matchups, paths$latest_lines_matchups, row.names = FALSE)
    }

    invisible(TRUE)
}

#' Capture and persist an Odds API snapshot for the current tournament field
#'
#' @param config Project config list.
#' @param bracket_year Active tournament year.
#' @param current_teams Current tournament teams table (from [load_tournament_data()]).
#'
#' @return A list containing paths and in-memory tables for the snapshot.
#' @export
capture_tournament_odds_snapshot <- function(config, bracket_year, current_teams) {
    betting <- config$betting %||% list()
    provider <- betting$provider %||% "odds_api"
    if (!identical(provider, "odds_api")) {
        stop_with_message(sprintf("Unsupported betting provider: %s", provider))
    }

    paths <- build_odds_history_paths(bracket_year, history_dir = betting$history_dir %||% "data/odds_history")
    ensure_odds_history_dirs(paths)

    api_key <- Sys.getenv(betting$api_key_env %||% "ODDS_API_KEY")
    fetched <- fetch_odds_api_odds(
        api_key = api_key,
        sport_key = betting$sport_key %||% "basketball_ncaab",
        regions = betting$regions %||% "us",
        markets = betting$markets %||% c("h2h", "spreads"),
        bookmakers = betting$bookmakers %||% c("draftkings", "fanduel", "betmgm", "betrivers"),
        odds_format = betting$odds_format %||% "american",
        date_format = betting$date_format %||% "iso"
    )

    events <- fetched$parsed %||% list()
    filtered <- filter_odds_events_to_tournament(events, current_teams = current_teams)
    lines_long <- normalize_odds_events_long(filtered, bracket_year = bracket_year, snapshot_time = fetched$retrieved_at)
    lines_matchups <- summarize_snapshot_matchups(lines_long)

    stamp <- gsub("\\.", "", format(as.POSIXct(fetched$retrieved_at, tz = "UTC"), "%Y%m%d_%H%M%OS6"))
    snapshot_path <- file.path(paths$snapshots_dir, sprintf("odds_api_%s.json", stamp))
    writeLines(fetched$raw_json, snapshot_path, useBytes = TRUE)

    append_odds_history_files(lines_long, lines_matchups, paths = paths)

    list(
        snapshot_path = snapshot_path,
        lines_long = lines_long,
        lines_matchups = lines_matchups,
        latest_lines_matchups_path = paths$latest_lines_matchups,
        headers = fetched$headers %||% list(),
        retrieved_at = fetched$retrieved_at
    )
}

#' Resolve the latest available matchup lines for a tournament year
#'
#' @param config Project config list.
#' @param bracket_year Active tournament year.
#' @param current_teams Current tournament teams table.
#'
#' @return A list with `lines_matchups` (may be `NULL`) and a `source_label`.
#' @export
resolve_latest_matchup_lines <- function(config, bracket_year, current_teams) {
    betting <- config$betting %||% list()
    enabled <- isTRUE(betting$enabled %||% FALSE)
    if (!isTRUE(enabled)) {
        return(list(lines_matchups = NULL, source_label = NULL, used_api_call = FALSE))
    }

    paths <- build_odds_history_paths(bracket_year, history_dir = betting$history_dir %||% "data/odds_history")
    if (file.exists(paths$latest_lines_matchups)) {
        tbl <- utils::read.csv(paths$latest_lines_matchups, stringsAsFactors = FALSE)
        return(list(
            lines_matchups = dplyr::as_tibble(tbl),
            source_label = "Local odds snapshot (latest)",
            used_api_call = FALSE,
            latest_lines_matchups_path = paths$latest_lines_matchups
        ))
    }

    fetch_policy <- betting$fetch_policy %||% "if_missing"
    if (identical(fetch_policy, "if_missing")) {
        snapshot <- capture_tournament_odds_snapshot(config, bracket_year = bracket_year, current_teams = current_teams)
        return(list(
            lines_matchups = snapshot$lines_matchups,
            source_label = "Odds API snapshot (captured)",
            used_api_call = TRUE,
            latest_lines_matchups_path = snapshot$latest_lines_matchups_path,
            snapshot_path = snapshot$snapshot_path,
            headers = snapshot$headers
        ))
    }

    list(lines_matchups = NULL, source_label = NULL, used_api_call = FALSE)
}

#' Look up a consensus line-implied probability for team A in a matchup
#'
#' @param lines_matchups Matchup-level odds table (from `latest_lines_matchups.csv`).
#' @param team_a Team A name (as used in simulations).
#' @param team_b Team B name.
#'
#' @return A scalar numeric probability for team A, or `NA_real_` when missing.
#' @export
lookup_line_prob_for_team_a <- function(lines_matchups, team_a, team_b) {
    if (is.null(lines_matchups) || nrow(lines_matchups) == 0) {
        return(NA_real_)
    }

    a <- canonicalize_team_name(team_a)
    b <- canonicalize_team_name(team_b)
    key <- build_matchup_key(a, b)
    match <- lines_matchups %>%
        dplyr::filter(key == !!key)
    if (nrow(match) == 0) {
        return(NA_real_)
    }

    team_sorted <- sort(c(a, b))
    if (identical(team_sorted[[1]], a)) {
        suppressWarnings(as.numeric(match$consensus_prob_a[[1]]))
    } else {
        suppressWarnings(as.numeric(match$consensus_prob_b[[1]]))
    }
}
