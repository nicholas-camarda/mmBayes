library(dplyr)
library(httr)
library(jsonlite)
library(tibble)

#' Build the odds-history directory for a tournament year
#'
#' Odds history is intentionally stored under the runtime root rather than the
#' code checkout. This keeps snapshots local while separating them from the
#' repository snapshot used for review.
#'
#' @param year Tournament year (integer-like).
#' @param history_dir Base directory for odds history storage.
#'
#' @return A path to the year-specific odds-history directory.
#' @export
build_odds_history_year_dir <- function(year, history_dir = default_runtime_history_root()) {
    file.path(history_dir %||% default_runtime_history_root(), as.character(as.integer(year)))
}

#' Build canonical odds-history file paths for a tournament year
#'
#' @param year Tournament year (integer-like).
#' @param history_dir Base directory for odds history storage.
#'
#' @return A named list of paths for snapshots, long-form odds, and matchup odds.
#' @keywords internal
build_odds_history_paths <- function(year, history_dir = default_runtime_history_root()) {
    year_dir <- build_odds_history_year_dir(year, history_dir = history_dir)
    list(
        year_dir = year_dir,
        snapshots_dir = file.path(year_dir, "snapshots"),
        lines_long = file.path(year_dir, "lines_long.csv"),
        lines_matchups = file.path(year_dir, "lines_matchups.csv"),
        latest_lines_matchups = file.path(year_dir, "latest_lines_matchups.csv"),
        closing_lines = file.path(year_dir, "closing_lines.csv"),
        schedule_events = file.path(year_dir, "schedule_events.csv"),
        schedule_windows = file.path(year_dir, "schedule_windows.csv"),
        collector_state = file.path(year_dir, "collector_state.json")
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

#' Find the most recent snapshot timestamp in a saved matchup-lines table
#'
#' @param path Path to `latest_lines_matchups.csv` or `lines_matchups.csv`.
#'
#' @return A POSIXct timestamp in UTC, or `NA` when unavailable.
#' @keywords internal
read_latest_snapshot_time_utc <- function(path) {
    if (!file.exists(path)) {
        return(as.POSIXct(NA, tz = "UTC"))
    }

    tbl <- tryCatch(
        utils::read.csv(path, stringsAsFactors = FALSE),
        error = function(e) NULL
    )
    if (is.null(tbl) || !"snapshot_time_utc" %in% names(tbl)) {
        return(as.POSIXct(NA, tz = "UTC"))
    }

    times <- vapply(tbl$snapshot_time_utc, function(value) {
        as.numeric(parse_utc_timestamp(value))
    }, numeric(1))
    times <- as.POSIXct(times, origin = "1970-01-01", tz = "UTC")
    times <- times[is.finite(times) & !is.na(times)]
    if (length(times) == 0) {
        return(as.POSIXct(NA, tz = "UTC"))
    }

    max(times, na.rm = TRUE)
}

#' Check whether the betting-dispersion schema is present
#'
#' @param data A betting-history or latest-lines data frame.
#'
#' @return `TRUE` when the required dispersion columns are present.
#' @keywords internal
has_betting_dispersion_columns <- function(data) {
    required <- c("prob_dispersion_a", "spread_dispersion_a")
    !is.null(data) && all(required %in% names(data))
}

#' Validate that a betting-history table has the expected schema
#'
#' @param data A betting-history or latest-lines data frame.
#' @param source_label Human-readable source name used in error messages.
#'
#' @return Invisibly returns `TRUE` when the expected columns are present.
#' @keywords internal
validate_betting_history_schema <- function(data, source_label) {
    if (is.null(data) || nrow(data) == 0) {
        return(invisible(TRUE))
    }

    required <- c("prob_dispersion_a", "spread_dispersion_a")
    missing <- setdiff(required, names(data))
    if (length(missing) > 0) {
        stop_with_message(sprintf(
            "%s is missing required columns: %s. Rebuild the configured odds-history files from the saved snapshot JSON.",
            source_label,
            paste(missing, collapse = ", ")
        ))
    }

    invisible(TRUE)
}

#' Test whether a latest-lines table is usable
#'
#' @param path Path to `latest_lines_matchups.csv`.
#'
#' @return `TRUE` when the file exists and contains at least one row.
#' @keywords internal
latest_lines_table_is_usable <- function(path) {
    if (!file.exists(path)) {
        return(FALSE)
    }

    tbl <- tryCatch(
        utils::read.csv(path, stringsAsFactors = FALSE),
        error = function(e) NULL
    )
    !is.null(tbl) &&
        nrow(tbl) > 0 &&
        has_betting_dispersion_columns(tbl)
}

#' Find the most recent saved Odds API snapshot JSON file
#'
#' @param snapshots_dir Directory containing snapshot JSON files.
#'
#' @return The path to the most recently modified snapshot JSON, or `NULL`.
#' @keywords internal
find_latest_snapshot_json <- function(snapshots_dir) {
    if (!dir.exists(snapshots_dir)) {
        return(NULL)
    }

    candidates <- list.files(snapshots_dir, pattern = "\\.json$", full.names = TRUE)
    if (length(candidates) == 0) {
        return(NULL)
    }

    info <- file.info(candidates)
    latest <- candidates[order(info$mtime, decreasing = TRUE)][[1]]
    latest %||% NULL
}

#' Parse a UTC timestamp from an Odds API snapshot filename
#'
#' Snapshot files are named like `odds_api_YYYYMMDD_HHMMSSffffff.json`, where
#' the fractional component is microseconds. When parsing fails, this returns
#' `NA`.
#'
#' @param snapshot_path Path to a snapshot JSON file.
#'
#' @return A POSIXct timestamp in UTC, or `NA`.
#' @keywords internal
parse_snapshot_time_from_filename <- function(snapshot_path) {
    stamp <- basename(snapshot_path %||% "")
    stamp <- sub("^odds_api_", "", stamp)
    stamp <- sub("\\.json$", "", stamp)
    stamp <- gsub("[^0-9_]", "", stamp)
    if (!grepl("^[0-9]{8}_[0-9]{6}", stamp)) {
        return(as.POSIXct(NA, tz = "UTC"))
    }

    date_part <- substr(stamp, 1, 8)
    time_part <- substr(stamp, 10, nchar(stamp))
    hh <- substr(time_part, 1, 2)
    mm <- substr(time_part, 3, 4)
    ss <- substr(time_part, 5, 6)
    frac <- substr(time_part, 7, nchar(time_part))
    frac <- if (nzchar(frac)) paste0(".", frac) else ""

    iso <- sprintf(
        "%s-%s-%s %s:%s:%s%s",
        substr(date_part, 1, 4),
        substr(date_part, 5, 6),
        substr(date_part, 7, 8),
        hh,
        mm,
        ss,
        frac
    )

    parsed <- suppressWarnings(as.POSIXct(iso, tz = "UTC", format = "%Y-%m-%d %H:%M:%OS"))
    if (is.na(parsed) || !is.finite(parsed)) {
        as.POSIXct(NA, tz = "UTC")
    } else {
        parsed
    }
}

#' Rebuild `latest_lines_matchups.csv` from an existing snapshot JSON
#'
#' This enables quota-sparing runs when a snapshot JSON exists but the derived
#' CSV tables were deleted or not written.
#'
#' @param bracket_year Tournament year.
#' @param current_teams Current tournament teams table.
#' @param paths Odds history paths returned by [build_odds_history_paths()].
#'
#' @return A list with `lines_matchups` and `snapshot_path`, or `NULL` when no snapshot exists.
#' @keywords internal
rebuild_latest_lines_from_snapshot_json <- function(bracket_year, current_teams, paths) {
    snapshot_path <- find_latest_snapshot_json(paths$snapshots_dir)
    if (is.null(snapshot_path)) {
        return(NULL)
    }

    snapshot_time <- parse_snapshot_time_from_filename(snapshot_path)
    if (is.na(snapshot_time) || !is.finite(snapshot_time)) {
        snapshot_time <- as.POSIXct(file.info(snapshot_path)$mtime, tz = "UTC")
    }

    parsed <- tryCatch(
        jsonlite::fromJSON(snapshot_path, simplifyVector = FALSE),
        error = function(e) list()
    )
    events <- parsed %||% list()
    filtered <- filter_odds_events_to_tournament(events, current_teams = current_teams)
    lines_long <- normalize_odds_events_long(
        filtered,
        bracket_year = bracket_year,
        snapshot_time = snapshot_time,
        allowed_team_names = current_teams$Team
    )
    lines_matchups <- summarize_snapshot_matchups(lines_long)
    write_latest_lines_matchups(lines_matchups, paths)

    list(
        snapshot_path = snapshot_path,
        lines_matchups = lines_matchups
    )
}

#' Drop the final N words from a team name candidate
#'
#' @param name Team name string.
#' @param n_words Number of trailing words to drop.
#'
#' @return A trimmed string with up to `n_words` removed.
#' @keywords internal
drop_trailing_words <- function(name, n_words = 1L) {
    name <- stringr::str_squish(as.character(name %||% ""))
    n_words <- as.integer(n_words)
    if (!nzchar(name) || !is.finite(n_words) || is.na(n_words) || n_words <= 0) {
        return(name)
    }

    parts <- unlist(strsplit(name, "\\s+"))
    if (length(parts) <= n_words) {
        return("")
    }
    stringr::str_squish(paste(parts[seq_len(length(parts) - n_words)], collapse = " "))
}

#' Match an Odds API team name to a known tournament team
#'
#' Odds API team names sometimes include mascot suffixes (e.g., "Wichita St
#' Shockers"). This helper tries progressively-stripped candidates to map the
#' name back to the canonical team list derived from `current_teams`.
#'
#' @param odds_team_name Team name from Odds API (`home_team`, `away_team`, or outcome `name`).
#' @param allowed_team_names Canonical team names allowed for the tournament year.
#'
#' @return A canonical team name when matched, otherwise `NA_character_`.
#' @keywords internal
match_odds_team_to_tournament <- function(odds_team_name, allowed_team_names) {
    allowed_team_names <- unique(canonicalize_team_name(allowed_team_names))
    allowed_keys <- raw_team_name_key(allowed_team_names)
    odds_team_name <- safe_character_scalar(odds_team_name, default = "")

    candidates <- unique(c(
        odds_team_name,
        drop_trailing_words(odds_team_name, 1),
        drop_trailing_words(odds_team_name, 2),
        drop_trailing_words(odds_team_name, 3)
    ))

    for (candidate in candidates) {
        if (!nzchar(candidate)) next
        canonical <- canonicalize_team_name(candidate)
        key <- raw_team_name_key(canonical)
        idx <- match(key, allowed_keys)
        if (!is.na(idx)) {
            return(allowed_team_names[[idx]])
        }
    }

    NA_character_
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
        home <- match_odds_team_to_tournament(event$home_team %||% "", allowed_team_names = allowed)
        away <- match_odds_team_to_tournament(event$away_team %||% "", allowed_team_names = allowed)
        nzchar(home %||% "") && nzchar(away %||% "") && home %in% allowed && away %in% allowed
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
normalize_odds_events_long <- function(events, bracket_year, snapshot_time = Sys.time(), allowed_team_names = NULL) {
    if (length(events) == 0) {
        return(tibble::tibble())
    }

    snapshot_time_utc <- parse_utc_timestamp(snapshot_time)
    allowed_team_names <- if (!is.null(allowed_team_names)) unique(canonicalize_team_name(allowed_team_names)) else NULL

    rows <- purrr::map_dfr(events, function(event) {
        event_id <- as.character(event$id %||% "")
        commence_time <- as.character(event$commence_time %||% NA_character_)
        home_team <- if (!is.null(allowed_team_names)) {
            match_odds_team_to_tournament(event$home_team %||% "", allowed_team_names = allowed_team_names)
        } else {
            canonicalize_team_name(event$home_team %||% "")
        }
        away_team <- if (!is.null(allowed_team_names)) {
            match_odds_team_to_tournament(event$away_team %||% "", allowed_team_names = allowed_team_names)
        } else {
            canonicalize_team_name(event$away_team %||% "")
        }
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
                    outcome_name <- if (!is.null(allowed_team_names)) {
                        match_odds_team_to_tournament(outcome$name %||% NA_character_, allowed_team_names = allowed_team_names)
                    } else {
                        canonicalize_team_name(outcome$name %||% NA_character_)
                    }
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
                        outcome_name = outcome_name,
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
            team_1 = canonicalize_team_name(teams[[1]]),
            team_2 = canonicalize_team_name(teams[[2]]),
            implied_1 = american_to_implied_prob(prices[[1]]),
            implied_2 = american_to_implied_prob(prices[[2]]),
            vig_free = list(remove_vig_two_way(implied_1, implied_2)),
            vig_free_1 = vig_free[[1]],
            vig_free_2 = vig_free[[2]],
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
            team_1 = canonicalize_team_name(teams[[1]]),
            team_2 = canonicalize_team_name(teams[[2]]),
            point_1 = suppressWarnings(as.numeric(points[[1]])),
            point_2 = suppressWarnings(as.numeric(points[[2]])),
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
            prob_dispersion_a = stats::sd(prob_a, na.rm = TRUE),
            .groups = "drop"
        )

    spread_consensus <- spread_pairs %>%
        dplyr::group_by(snapshot_time_utc, bracket_year, event_id, key) %>%
        dplyr::summarise(
            consensus_spread_a = stats::median(spread_a, na.rm = TRUE),
            consensus_spread_b = stats::median(spread_b, na.rm = TRUE),
            spread_dispersion_a = stats::sd(spread_a, na.rm = TRUE),
            .groups = "drop"
        )

    h2h_consensus %>%
        dplyr::left_join(spread_consensus, by = c("snapshot_time_utc", "bracket_year", "event_id", "key")) %>%
        dplyr::arrange(snapshot_time_utc, event_id)
}

#' Build an empty latest-lines table
#'
#' @return A zero-row tibble matching the latest-lines schema.
#' @keywords internal
empty_latest_lines_matchups <- function() {
    tibble::tibble(
        snapshot_time_utc = as.POSIXct(character(), tz = "UTC"),
        bracket_year = integer(),
        event_id = character(),
        commence_time = character(),
        home_team = character(),
        away_team = character(),
        key = character(),
        team_a = character(),
        team_b = character(),
        n_bookmakers = integer(),
        bookmakers = character(),
        consensus_prob_a = numeric(),
        consensus_prob_b = numeric(),
        prob_dispersion_a = numeric(),
        consensus_spread_a = numeric(),
        consensus_spread_b = numeric(),
        spread_dispersion_a = numeric()
    )
}

#' Persist the latest matchup-lines table without mutating history
#'
#' @param lines_matchups Matchup-level latest lines.
#' @param paths Output paths from [build_odds_history_paths()].
#'
#' @return Invisibly returns `TRUE` on success.
#' @keywords internal
write_latest_lines_matchups <- function(lines_matchups, paths) {
    ensure_odds_history_dirs(paths)
    latest_tbl <- if (nrow(lines_matchups) > 0) lines_matchups else empty_latest_lines_matchups()
    utils::write.csv(latest_tbl, paths$latest_lines_matchups, row.names = FALSE)
    invisible(TRUE)
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
    }
    write_latest_lines_matchups(lines_matchups, paths)

    invisible(TRUE)
}

#' Capture and persist an Odds API snapshot for the current tournament field
#'
#' @param config Project config list.
#' @param bracket_year Active tournament year.
#' @param current_teams Current tournament teams table (from [load_tournament_data()]).
#' @param force Whether to bypass the snapshot cooldown guard.
#'
#' @return A list containing paths and in-memory tables for the snapshot.
#' @export
capture_tournament_odds_snapshot <- function(config, bracket_year, current_teams, force = FALSE) {
    betting <- config$betting %||% list()
    provider <- betting$provider %||% "odds_api"
    if (!identical(provider, "odds_api")) {
        stop_with_message(sprintf("Unsupported betting provider: %s", provider))
    }

    paths <- build_odds_history_paths(bracket_year, history_dir = betting$history_dir %||% default_runtime_history_root())
    ensure_odds_history_dirs(paths)

    force <- isTRUE(force)
    cooldown_minutes <- suppressWarnings(as.numeric(betting$snapshot_cooldown_minutes %||% 10))
    cooldown_minutes <- if (is.finite(cooldown_minutes)) max(0, cooldown_minutes) else 0
    last_snapshot_time <- read_latest_snapshot_time_utc(paths$latest_lines_matchups)
    if (!is.finite(last_snapshot_time) || is.na(last_snapshot_time)) {
        last_snapshot_time <- read_latest_snapshot_time_utc(paths$lines_matchups)
    }
    if (!isTRUE(force) && cooldown_minutes > 0 && is.finite(last_snapshot_time) && !is.na(last_snapshot_time)) {
        age_minutes <- as.numeric(difftime(Sys.time(), last_snapshot_time, units = "mins"))
        if (is.finite(age_minutes) && age_minutes < cooldown_minutes) {
            latest_lines <- if (file.exists(paths$latest_lines_matchups)) {
                dplyr::as_tibble(utils::read.csv(paths$latest_lines_matchups, stringsAsFactors = FALSE))
            } else {
                tibble::tibble()
            }
            snapshot_path <- find_latest_snapshot_json(paths$snapshots_dir)
            age_minutes_rounded <- round(age_minutes, 1)
            logger::log_info("Skipping odds snapshot: most recent snapshot is {age_minutes_rounded} minutes old (cooldown={cooldown_minutes} minutes)")
            return(list(
                snapshot_path = snapshot_path,
                lines_long = tibble::tibble(),
                lines_matchups = latest_lines,
                latest_lines_matchups_path = paths$latest_lines_matchups,
                headers = list(),
                retrieved_at = last_snapshot_time,
                skipped = TRUE,
                skip_reason = "cooldown"
            ))
        }
    }

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
    lines_long <- normalize_odds_events_long(
        filtered,
        bracket_year = bracket_year,
        snapshot_time = fetched$retrieved_at,
        allowed_team_names = current_teams$Team
    )
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

    paths <- build_odds_history_paths(bracket_year, history_dir = betting$history_dir %||% default_runtime_history_root())
    if (latest_lines_table_is_usable(paths$latest_lines_matchups)) {
        tbl <- utils::read.csv(paths$latest_lines_matchups, stringsAsFactors = FALSE)
        return(list(
            lines_matchups = dplyr::as_tibble(tbl),
            source_label = "Local odds snapshot (latest)",
            used_api_call = FALSE,
            latest_lines_matchups_path = paths$latest_lines_matchups
        ))
    }

    rebuilt <- rebuild_latest_lines_from_snapshot_json(bracket_year, current_teams = current_teams, paths = paths)
    if (!is.null(rebuilt) && file.exists(paths$latest_lines_matchups)) {
        tbl <- utils::read.csv(paths$latest_lines_matchups, stringsAsFactors = FALSE)
        return(list(
            lines_matchups = dplyr::as_tibble(tbl),
            source_label = "Local odds snapshot (rebuilt)",
            used_api_call = FALSE,
            latest_lines_matchups_path = paths$latest_lines_matchups,
            snapshot_path = rebuilt$snapshot_path
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

#' Load all historical closing lines from the cloud history root
#'
#' @param history_dir Base directory for odds history storage.
#'
#' @return A tibble of combined closing lines across available years.
#' @export
load_historical_closing_lines <- function(history_dir) {
    base_dir <- path.expand(history_dir %||% default_runtime_history_root())
    if (!dir.exists(base_dir)) {
        return(tibble::tibble())
    }

    paths <- list.files(base_dir, pattern = "^closing_lines\\.csv$", recursive = TRUE, full.names = TRUE)
    if (length(paths) == 0) {
        return(tibble::tibble())
    }

    purrr::map_dfr(paths, function(path) {
        tbl <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
        if (is.null(tbl)) {
            return(tibble::tibble())
        }
        dplyr::as_tibble(tbl)
    })
}

#' Build a betting feature reference table from closing lines
#'
#' @param closing_lines A historical closing-lines table.
#'
#' @return A normalized tibble of historical betting features.
#' @export
build_historical_betting_feature_table <- function(closing_lines) {
    if (is.null(closing_lines) || nrow(closing_lines) == 0) {
        return(tibble::tibble())
    }

    validate_betting_history_schema(closing_lines, "Historical closing lines")
    closing_lines %>%
        dplyr::mutate(
            Year = as.character(Year),
            round = as.character(round),
            teamA = canonicalize_team_name(teamA),
            teamB = canonicalize_team_name(teamB),
            matchup_key = purrr::map2_chr(teamA, teamB, build_matchup_key),
            implied_prob_teamA = safe_numeric(implied_prob_teamA, default = 0.5),
            spread_teamA = safe_numeric(spread_teamA, default = 0),
            n_bookmakers = safe_numeric(n_bookmakers, default = 0),
            line_available = as.integer(is.finite(implied_prob_teamA)),
            minutes_before_commence = dplyr::if_else(
                !is.na(closing_snapshot_time_utc) & !is.na(commence_time_utc),
                as.numeric(difftime(parse_utc_timestamp(commence_time_utc), parse_utc_timestamp(closing_snapshot_time_utc), units = "mins")),
                0
            ),
            prob_dispersion = safe_numeric(prob_dispersion_a, default = 0),
            spread_dispersion = safe_numeric(spread_dispersion_a, default = 0)
        ) %>%
        dplyr::select(
            Year,
            round,
            matchup_key,
            implied_prob_teamA,
            spread_teamA,
            n_bookmakers,
            line_available,
            minutes_before_commence,
            prob_dispersion,
            spread_dispersion
        )
}

#' Build a current betting feature reference table from latest matchup lines
#'
#' @param lines_matchups A latest-lines matchup table.
#'
#' @return A normalized tibble of current betting features.
#' @export
build_current_betting_feature_table <- function(lines_matchups) {
    if (is.null(lines_matchups) || nrow(lines_matchups) == 0) {
        return(tibble::tibble())
    }

    validate_betting_history_schema(lines_matchups, "Latest matchup lines")
    dplyr::as_tibble(lines_matchups) %>%
        dplyr::mutate(
            matchup_key = as.character(key),
            implied_prob_teamA = safe_numeric(consensus_prob_a, default = 0.5),
            spread_teamA = safe_numeric(consensus_spread_a, default = 0),
            n_bookmakers = safe_numeric(n_bookmakers, default = 0),
            line_available = as.integer(n_bookmakers > 0),
            minutes_before_commence = dplyr::if_else(
                !is.na(snapshot_time_utc) & !is.na(commence_time),
                as.numeric(difftime(parse_utc_timestamp(commence_time), parse_utc_timestamp(snapshot_time_utc), units = "mins")),
                0
            ),
            prob_dispersion = safe_numeric(prob_dispersion_a, default = 0),
            spread_dispersion = safe_numeric(spread_dispersion_a, default = 0)
        ) %>%
        dplyr::select(
            matchup_key,
            implied_prob_teamA,
            spread_teamA,
            n_bookmakers,
            line_available,
            minutes_before_commence,
            prob_dispersion,
            spread_dispersion
        )
}

#' Attach betting-derived matchup features to modeling rows
#'
#' @param matchup_rows A matchup-level table.
#' @param historical_betting_features Optional historical feature table.
#' @param current_betting_features Optional current/latest feature table.
#'
#' @return `matchup_rows` with betting predictors added or overwritten.
#' @export
augment_matchup_rows_with_betting_features <- function(matchup_rows,
                                                       historical_betting_features = NULL,
                                                       current_betting_features = NULL) {
    if (is.null(matchup_rows) || nrow(matchup_rows) == 0) {
        return(tibble::tibble())
    }

    prepared <- dplyr::as_tibble(matchup_rows) %>%
        dplyr::mutate(
            Year = as.character(Year),
            round = as.character(round),
            teamA = canonicalize_team_name(teamA),
            teamB = canonicalize_team_name(teamB),
            matchup_key = purrr::map2_chr(teamA, teamB, build_matchup_key)
        )

    historical_betting_features <- historical_betting_features %||% tibble::tibble()
    current_betting_features <- current_betting_features %||% tibble::tibble()

    if (nrow(historical_betting_features) > 0) {
        prepared <- prepared %>%
            dplyr::left_join(
                historical_betting_features,
                by = c("Year", "round", "matchup_key")
            )
    }

    for (column_name in c(
        "implied_prob_teamA",
        "spread_teamA",
        "n_bookmakers",
        "line_available",
        "minutes_before_commence",
        "prob_dispersion",
        "spread_dispersion"
    )) {
        if (!column_name %in% names(prepared)) {
            prepared[[column_name]] <- NA_real_
        }
    }

    if (nrow(current_betting_features) > 0) {
        prepared <- prepared %>%
            dplyr::left_join(
                current_betting_features %>%
                    dplyr::rename_with(~ paste0(.x, "_current"), -matchup_key),
                by = "matchup_key"
            ) %>%
            dplyr::mutate(
                implied_prob_teamA = dplyr::coalesce(implied_prob_teamA, implied_prob_teamA_current),
                spread_teamA = dplyr::coalesce(spread_teamA, spread_teamA_current),
                n_bookmakers = dplyr::coalesce(n_bookmakers, n_bookmakers_current),
                line_available = dplyr::coalesce(line_available, line_available_current),
                minutes_before_commence = dplyr::coalesce(minutes_before_commence, minutes_before_commence_current),
                prob_dispersion = dplyr::coalesce(prob_dispersion, prob_dispersion_current),
                spread_dispersion = dplyr::coalesce(spread_dispersion, spread_dispersion_current)
            ) %>%
            dplyr::select(-dplyr::ends_with("_current"))
    }

    prepared %>%
        dplyr::mutate(
            implied_prob_teamA = safe_numeric(implied_prob_teamA, default = 0.5),
            spread_teamA = safe_numeric(spread_teamA, default = 0),
            n_bookmakers = safe_numeric(n_bookmakers, default = 0),
            line_available = safe_numeric(line_available, default = 0),
            minutes_before_commence = safe_numeric(minutes_before_commence, default = 0),
            prob_dispersion = safe_numeric(prob_dispersion, default = 0),
            spread_dispersion = safe_numeric(spread_dispersion, default = 0),
            betting_prob_centered = implied_prob_teamA - 0.5,
            betting_spread_teamA = spread_teamA,
            betting_bookmakers = n_bookmakers,
            betting_line_available = line_available,
            betting_minutes_before_commence = minutes_before_commence,
            betting_prob_dispersion = prob_dispersion,
            betting_spread_dispersion = spread_dispersion
        ) %>%
        dplyr::select(-dplyr::any_of(c(
            "matchup_key",
            "implied_prob_teamA",
            "spread_teamA",
            "n_bookmakers",
            "line_available",
            "minutes_before_commence",
            "prob_dispersion",
            "spread_dispersion"
        )))
}

#' Attach betting-derived total-points features to modeling rows
#'
#' @param total_rows A total-points modeling table.
#' @param historical_betting_features Optional historical feature table.
#' @param current_betting_features Optional current/latest feature table.
#'
#' @return `total_rows` with betting predictors added or overwritten.
#' @export
augment_total_points_rows_with_betting_features <- function(total_rows,
                                                            historical_betting_features = NULL,
                                                            current_betting_features = NULL) {
    if (is.null(total_rows) || nrow(total_rows) == 0) {
        return(tibble::tibble())
    }

    matchup_augmented <- augment_matchup_rows_with_betting_features(
        matchup_rows = total_rows,
        historical_betting_features = historical_betting_features,
        current_betting_features = current_betting_features
    )

    matchup_augmented %>%
        dplyr::mutate(
            betting_abs_prob_edge = abs(betting_prob_centered),
            betting_abs_spread = abs(betting_spread_teamA)
        )
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
