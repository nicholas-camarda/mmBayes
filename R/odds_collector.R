library(dplyr)
library(httr)
library(jsonlite)
library(purrr)
library(tibble)

#' Format a UTC timestamp for Odds API requests and collector state
#'
#' @param x A POSIXct timestamp or date-like value.
#'
#' @return A scalar ISO-8601 timestamp string in UTC, or `NULL`.
#' @keywords internal
format_utc_timestamp <- function(x) {
    if (is.null(x) || length(x) == 0) {
        return(NULL)
    }
    x <- suppressWarnings(as.POSIXct(x, tz = "UTC"))
    if (is.na(x) || !is.finite(x)) {
        return(NULL)
    }
    format(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

#' Parse a UTC timestamp string
#'
#' @param x An ISO-8601 timestamp string.
#'
#' @return A POSIXct timestamp in UTC, or `NA`.
#' @keywords internal
parse_utc_timestamp <- function(x) {
    if (is.null(x) || length(x) == 0 || !nzchar(as.character(x)[1])) {
        return(as.POSIXct(NA, tz = "UTC"))
    }
    if (inherits(x, "POSIXt")) {
        parsed <- as.POSIXct(x, tz = "UTC")
    } else {
        value <- as.character(x)[1]
        parsed <- suppressWarnings(as.POSIXct(strptime(value, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"), tz = "UTC"))
        if (is.na(parsed) || !is.finite(parsed)) {
            parsed <- suppressWarnings(as.POSIXct(strptime(value, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"), tz = "UTC"))
        }
    }
    if (is.na(parsed) || !is.finite(parsed)) {
        as.POSIXct(NA, tz = "UTC")
    } else {
        parsed
    }
}

#' Build an empty odds-schedule event table
#'
#' @return A zero-row tibble with the expected schedule-event schema.
#' @keywords internal
empty_odds_schedule_events <- function() {
    tibble::tibble(
        bracket_year = integer(),
        event_id = character(),
        commence_time_utc = as.POSIXct(character(), tz = "UTC"),
        home_team = character(),
        away_team = character(),
        team_a = character(),
        team_b = character(),
        matchup_key = character()
    )
}

#' Build an empty odds-schedule window table
#'
#' @return A zero-row tibble with the expected schedule-window schema.
#' @keywords internal
empty_odds_schedule_windows <- function() {
    tibble::tibble(
        bracket_year = integer(),
        slate_id = character(),
        slate_index = integer(),
        first_commence_utc = as.POSIXct(character(), tz = "UTC"),
        last_commence_utc = as.POSIXct(character(), tz = "UTC"),
        event_count = integer(),
        event_ids = character(),
        teams = character()
    )
}

#' Build the default NCAA tournament odds-collection window
#'
#' @param bracket_year Tournament year.
#'
#' @return A list with `start` and `end` UTC timestamps for the seasonal window.
#' @keywords internal
odds_collection_season_bounds <- function(bracket_year) {
    year <- as.integer(bracket_year)
    start <- as.POSIXct(sprintf("%04d-03-01 00:00:00", year), tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
    end <- as.POSIXct(sprintf("%04d-04-20 23:59:59", year), tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
    list(start = start, end = end)
}

#' Check whether the collector should be active for the current season
#'
#' @param now Current timestamp.
#' @param bracket_year Tournament year.
#'
#' @return `TRUE` when the current timestamp falls inside the seasonal window.
#' @keywords internal
is_odds_collection_season_active <- function(now = Sys.time(), bracket_year) {
    now <- as.POSIXct(now, tz = "UTC")
    bounds <- odds_collection_season_bounds(bracket_year)
    is.finite(now) && !is.na(now) && now >= bounds$start && now <= bounds$end
}

#' Compute the next seasonal start when the collector is idle
#'
#' @param now Current timestamp.
#' @param bracket_year Tournament year.
#'
#' @return A POSIXct timestamp in UTC.
#' @keywords internal
next_odds_collection_season_start <- function(now = Sys.time(), bracket_year) {
    now <- as.POSIXct(now, tz = "UTC")
    bounds <- odds_collection_season_bounds(bracket_year)
    if (is.finite(now) && !is.na(now) && now < bounds$start) {
        return(bounds$start)
    }
    odds_collection_season_bounds(as.integer(bracket_year) + 1L)$start
}

#' Fetch raw event JSON from The Odds API
#'
#' @param api_key Odds API key.
#' @param sport_key Sport key used by the Odds API.
#' @param date_format Date format (`iso` or `unix`).
#' @param commence_time_from Optional lower bound for commence time filtering.
#' @param commence_time_to Optional upper bound for commence time filtering.
#'
#' @return A list with `raw_json`, `parsed`, `headers`, and `retrieved_at`.
#' @keywords internal
fetch_odds_api_events <- function(api_key,
                                  sport_key,
                                  date_format = "iso",
                                  commence_time_from = NULL,
                                  commence_time_to = NULL) {
    if (!nzchar(api_key %||% "")) {
        stop_with_message("Missing Odds API key (set the configured environment variable).")
    }

    endpoint <- sprintf("%s/v4/sports/%s/events", odds_api_host(), sport_key)
    query <- list(
        apiKey = api_key,
        dateFormat = date_format %||% "iso"
    )
    commence_time_from <- format_utc_timestamp(commence_time_from)
    commence_time_to <- format_utc_timestamp(commence_time_to)
    if (!is.null(commence_time_from)) {
        query$commenceTimeFrom <- commence_time_from
    }
    if (!is.null(commence_time_to)) {
        query$commenceTimeTo <- commence_time_to
    }

    response <- tryCatch(
        httr::GET(endpoint, query = query, httr::timeout(20)),
        error = function(e) {
            stop_with_message("Odds API events request failed (network error).")
        }
    )
    headers <- httr::headers(response)
    retrieved_at <- Sys.time()

    if (httr::status_code(response) >= 400) {
        stop_with_message(sprintf("Odds API events request failed with status %s", httr::status_code(response)))
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

#' Normalize Odds API events into a schedule table
#'
#' @param events Odds API event list from [fetch_odds_api_events()].
#' @param bracket_year Tournament year.
#' @param current_teams Current tournament team table.
#'
#' @return A tibble of upcoming tournament events with canonical team names.
#' @keywords internal
normalize_odds_api_event_schedule <- function(events, bracket_year, current_teams) {
    if (length(events) == 0) {
        return(empty_odds_schedule_events())
    }

    allowed <- unique(canonicalize_team_name(current_teams$Team))

    purrr::map_dfr(events, function(event) {
        home <- match_odds_team_to_tournament(safe_character_scalar(event$home_team, default = ""), allowed_team_names = allowed)
        away <- match_odds_team_to_tournament(safe_character_scalar(event$away_team, default = ""), allowed_team_names = allowed)
        if (!nzchar(home) || !nzchar(away)) {
            return(tibble::tibble())
        }

        commence_time <- parse_utc_timestamp(safe_character_scalar(event$commence_time, default = NA_character_))
        teams <- sort(unique(c(home, away)))
        if (length(teams) < 2L) {
            return(tibble::tibble())
        }
        team_a <- teams[[1]]
        team_b <- teams[[2]]

        tibble::tibble(
            bracket_year = as.integer(bracket_year),
            event_id = safe_character_scalar(event$id, default = ""),
            commence_time_utc = commence_time,
            home_team = canonicalize_team_name(home),
            away_team = canonicalize_team_name(away),
            team_a = canonicalize_team_name(team_a),
            team_b = canonicalize_team_name(team_b),
            matchup_key = build_matchup_key(team_a, team_b)
        ) %>%
            dplyr::filter(nzchar(event_id), !is.na(commence_time_utc), is.finite(commence_time_utc))
    })
}

#' Group schedule events into capture slates
#'
#' @param events A normalized event schedule from [normalize_odds_api_event_schedule()].
#' @param gap_minutes Time gap that starts a new slate.
#'
#' @return A tibble with one row per capture slate.
#' @keywords internal
group_odds_schedule_into_slates <- function(events, gap_minutes = 180L) {
    if (is.null(events) || nrow(events) == 0) {
        return(empty_odds_schedule_windows())
    }

    gap_minutes <- as.integer(gap_minutes)
    if (!is.finite(gap_minutes) || is.na(gap_minutes) || gap_minutes <= 0L) {
        gap_minutes <- 180L
    }

    events %>%
        dplyr::arrange(commence_time_utc, event_id) %>%
        dplyr::mutate(
            gap_from_previous_minutes = as.numeric(difftime(commence_time_utc, dplyr::lag(commence_time_utc), units = "mins")),
            slate_break = dplyr::row_number() == 1L | gap_from_previous_minutes > gap_minutes,
            slate_index = cumsum(ifelse(is.na(slate_break), 0L, as.integer(slate_break)))
        ) %>%
        dplyr::group_by(bracket_year, slate_index) %>%
        dplyr::summarise(
            slate_id = sprintf("%s_slate_%02d", first(bracket_year), first(slate_index)),
            first_commence_utc = min(commence_time_utc, na.rm = TRUE),
            last_commence_utc = max(commence_time_utc, na.rm = TRUE),
            event_count = dplyr::n(),
            event_ids = paste(event_id, collapse = ","),
            teams = paste(sprintf("%s vs %s", team_a, team_b), collapse = " | "),
            .groups = "drop"
        ) %>%
        dplyr::select(
            bracket_year,
            slate_id,
            slate_index,
            first_commence_utc,
            last_commence_utc,
            event_count,
            event_ids,
            teams
        )
}

#' Write the current schedule cache to disk
#'
#' @param schedule_events Event-level schedule rows.
#' @param schedule_windows Slate-level schedule rows.
#' @param paths Odds-history paths from [build_odds_history_paths()].
#'
#' @return Invisibly returns `TRUE`.
#' @keywords internal
write_odds_schedule_cache <- function(schedule_events, schedule_windows, paths) {
    ensure_odds_history_dirs(paths)
    utils::write.csv(schedule_events %||% empty_odds_schedule_events(), paths$schedule_events, row.names = FALSE)
    utils::write.csv(schedule_windows %||% empty_odds_schedule_windows(), paths$schedule_windows, row.names = FALSE)
    invisible(TRUE)
}

#' Build the default odds collector state
#'
#' @param bracket_year Active tournament year.
#' @param quota_cap Maximum quota budget available for the current period.
#' @param reserve_floor Credits that should remain unused as a safety margin.
#'
#' @return A named list suitable for JSON persistence.
#' @keywords internal
default_odds_collector_state <- function(bracket_year, quota_cap = 500L, reserve_floor = 100L) {
    list(
        bracket_year = as.integer(bracket_year),
        quota_cap = as.integer(quota_cap),
        reserve_floor = as.integer(reserve_floor),
        remaining_credits = as.integer(quota_cap),
        last_schedule_refresh_at = NULL,
        last_odds_capture_at = NULL,
        next_poll_at = NULL,
        last_snapshot_path = NULL,
        last_snapshot_cost = NULL,
        last_snapshot_rows = NULL,
        last_capture_reason = NULL,
        last_window_id = NULL,
        last_error = NULL
    )
}

#' Read odds collector state from disk
#'
#' @param path Path to a JSON collector-state file.
#' @param fallback_state State to use when no file exists.
#'
#' @return A named list of collector state values.
#' @keywords internal
read_odds_collector_state <- function(path, fallback_state = list()) {
    if (!file.exists(path)) {
        return(fallback_state)
    }

    parsed <- tryCatch(
        jsonlite::read_json(path, simplifyVector = FALSE),
        error = function(e) NULL
    )
    if (is.null(parsed) || !is.list(parsed)) {
        return(fallback_state)
    }
    parsed
}

#' Write odds collector state to disk
#'
#' @param state Collector state to persist.
#' @param path Destination JSON path.
#'
#' @return Invisibly returns `TRUE` after writing the file.
#' @keywords internal
write_odds_collector_state <- function(state, path) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(state %||% list(), path, pretty = TRUE, auto_unbox = TRUE, na = "null")
    invisible(TRUE)
}

#' Select a budget-aware polling cadence
#'
#' @param remaining_credits Remaining quota credits.
#' @param reserve_floor Credits that should remain unused.
#'
#' @return A named list describing the effective cadence.
#' @keywords internal
collector_budget_tier <- function(remaining_credits, reserve_floor) {
    remaining_credits <- safe_numeric(remaining_credits, default = 0)
    reserve_floor <- safe_numeric(reserve_floor, default = 0)
    available <- remaining_credits - reserve_floor

    if (!is.finite(available) || available <= 0) {
        return(list(
            allowed = FALSE,
            budget_label = "reserve",
            lead_minutes = 0L,
            poll_interval_minutes = NA_integer_
        ))
    }

    if (available <= 50) {
        return(list(
            allowed = TRUE,
            budget_label = "tight",
            lead_minutes = 30L,
            poll_interval_minutes = 15L
        ))
    }

    if (available <= 150) {
        return(list(
            allowed = TRUE,
            budget_label = "moderate",
            lead_minutes = 60L,
            poll_interval_minutes = 20L
        ))
    }

    list(
        allowed = TRUE,
        budget_label = "ample",
        lead_minutes = 90L,
        poll_interval_minutes = 30L
    )
}

#' Compute the next odds-capture decision from schedule and state
#'
#' @param schedule_windows Slate-level schedule rows.
#' @param now Current timestamp.
#' @param state Collector state.
#' @param reserve_floor Safety margin for the remaining quota.
#'
#' @return A list describing whether to capture now and when the next poll is due.
#' @keywords internal
derive_odds_collection_plan <- function(schedule_windows,
                                        now = Sys.time(),
                                        state = list(),
                                        reserve_floor = 100L,
                                        schedule_refresh_minutes = 30L,
                                        tail_minutes = 45L) {
    now <- as.POSIXct(now, tz = "UTC")
    remaining_credits <- safe_numeric(state$remaining_credits, default = safe_numeric(state$quota_cap, default = 500))
    quota_cap <- safe_numeric(state$quota_cap, default = 500)
    budget <- collector_budget_tier(remaining_credits, reserve_floor)
    schedule_refresh_minutes <- safe_numeric(schedule_refresh_minutes, default = 30)
    if (!is.finite(schedule_refresh_minutes) || schedule_refresh_minutes <= 0) {
        schedule_refresh_minutes <- 30
    }
    tail_minutes <- safe_numeric(tail_minutes, default = 45)
    if (!is.finite(tail_minutes) || tail_minutes <= 0) {
        tail_minutes <- 45
    }
    next_refresh_at <- now + (schedule_refresh_minutes * 60)

    schedule_windows <- schedule_windows %||% empty_odds_schedule_windows()
    if (nrow(schedule_windows) == 0) {
        return(list(
            should_capture_now = FALSE,
            reason = "no_schedule",
            next_poll_at = format_utc_timestamp(next_refresh_at),
            active_window = NULL,
            budget = budget,
            remaining_credits = remaining_credits,
            quota_cap = quota_cap,
            next_refresh_at = format_utc_timestamp(next_refresh_at)
        ))
    }

    schedule_windows <- schedule_windows %>%
        dplyr::arrange(first_commence_utc, slate_index) %>%
        dplyr::mutate(
            effective_window_start_utc = first_commence_utc - (budget$lead_minutes * 60),
            effective_window_end_utc = last_commence_utc + (tail_minutes * 60)
        )

    state_next_poll_at <- parse_utc_timestamp(state$next_poll_at)
    state_last_capture_at <- parse_utc_timestamp(state$last_odds_capture_at)
    state_last_refresh_at <- parse_utc_timestamp(state$last_schedule_refresh_at)

    if (isTRUE(!budget$allowed)) {
        return(list(
            should_capture_now = FALSE,
            reason = "quota_reserve",
            next_poll_at = format_utc_timestamp(next_refresh_at),
            active_window = NULL,
            budget = budget,
            remaining_credits = remaining_credits,
            quota_cap = quota_cap,
            next_refresh_at = format_utc_timestamp(next_refresh_at)
        ))
    }

    upcoming_windows <- schedule_windows %>%
        dplyr::filter(effective_window_end_utc >= now)
    active_windows <- upcoming_windows %>%
        dplyr::filter(effective_window_start_utc <= now)
    next_window <- upcoming_windows %>% dplyr::slice_head(n = 1)

    if (nrow(active_windows) > 0) {
        active_window <- active_windows %>% dplyr::slice_head(n = 1)
        due_at <- active_window$effective_window_start_utc[[1]]
        if (is.finite(state_last_capture_at) && !is.na(state_last_capture_at)) {
            due_at <- max(due_at, state_last_capture_at + (budget$poll_interval_minutes * 60))
        }
        if (is.finite(state_next_poll_at) && !is.na(state_next_poll_at)) {
            due_at <- max(due_at, state_next_poll_at)
        }

        should_capture_now <- is.finite(due_at) && !is.na(due_at) && now >= due_at
        return(list(
            should_capture_now = should_capture_now,
            reason = if (should_capture_now) "active_window" else "wait_for_window",
            next_poll_at = format_utc_timestamp(if (should_capture_now) now + (budget$poll_interval_minutes * 60) else due_at),
            active_window = active_window,
            budget = budget,
            remaining_credits = remaining_credits,
            quota_cap = quota_cap,
            last_schedule_refresh_at = if (is.finite(state_last_refresh_at)) format_utc_timestamp(state_last_refresh_at) else NULL,
            next_refresh_at = format_utc_timestamp(next_refresh_at)
        ))
    }

    next_poll_at <- if (nrow(next_window) > 0) {
        next_window$effective_window_start_utc[[1]]
    } else {
        now + 6 * 3600
    }

    if (is.finite(state_next_poll_at) && !is.na(state_next_poll_at)) {
        next_poll_at <- max(next_poll_at, state_next_poll_at)
    }

    list(
        should_capture_now = FALSE,
        reason = "before_next_window",
        next_poll_at = format_utc_timestamp(pmin(next_poll_at, next_refresh_at, na.rm = TRUE)),
        active_window = if (nrow(next_window) > 0) next_window else NULL,
        budget = budget,
        remaining_credits = remaining_credits,
        quota_cap = quota_cap,
        last_schedule_refresh_at = if (is.finite(state_last_refresh_at)) format_utc_timestamp(state_last_refresh_at) else NULL,
        next_refresh_at = format_utc_timestamp(next_refresh_at)
    )
}

#' Refresh and persist the tournament odds schedule
#'
#' @param config Project configuration.
#' @param bracket_year Tournament year.
#' @param current_teams Current tournament team table.
#' @param now Reference timestamp for schedule windows.
#'
#' @return A list containing event- and window-level schedule tables.
#' @keywords internal
refresh_tournament_odds_schedule <- function(config,
                                             bracket_year,
                                             current_teams,
                                             now = Sys.time(),
                                             events_fetcher = fetch_odds_api_events) {
    betting <- config$betting %||% list()
    provider <- betting$provider %||% "odds_api"
    if (!identical(provider, "odds_api")) {
        stop_with_message(sprintf("Unsupported betting provider: %s", provider))
    }

    history_dir <- betting$history_dir %||% default_runtime_history_root()
    paths <- build_odds_history_paths(bracket_year, history_dir = history_dir)
    ensure_odds_history_dirs(paths)

    api_key <- Sys.getenv(betting$api_key_env %||% "ODDS_API_KEY")
    fetched <- events_fetcher(
        api_key = api_key,
        sport_key = betting$sport_key %||% "basketball_ncaab",
        date_format = betting$date_format %||% "iso",
        commence_time_from = now - (24 * 3600),
        commence_time_to = now + (14 * 24 * 3600)
    )

    events <- normalize_odds_api_event_schedule(
        events = fetched$parsed %||% list(),
        bracket_year = bracket_year,
        current_teams = current_teams
    )
    windows <- group_odds_schedule_into_slates(
        events = events,
        gap_minutes = betting$slate_gap_minutes %||% 180L
    )

    write_odds_schedule_cache(events, windows, paths)

    list(
        paths = paths,
        events = events,
        windows = windows,
        fetched = fetched
    )
}

#' Run the odds collector for the current tournament year
#'
#' @param config Optional project configuration.
#' @param force Whether to bypass the collector's cached next-poll guard.
#'
#' @return A list containing the schedule, collector state, and optional odds snapshot.
#' @export
run_tournament_odds_collector <- function(config = NULL,
                                          force = FALSE,
                                          now = Sys.time(),
                                          events_fetcher = fetch_odds_api_events,
                                          snapshot_capturer = capture_tournament_odds_snapshot) {
    config <- config %||% load_project_config()
    output_dir <- config$output$path %||% default_runtime_output_root()
    log_basename <- sprintf("odds_collector_%s.log", format(Sys.Date(), "%Y%m%d"))
    log_path <- file.path(output_dir, "logs", log_basename)
    initialize_logging(log_path)
    now <- as.POSIXct(now, tz = "UTC")

    loaded <- load_tournament_data(config, include_betting_history = FALSE)
    bracket_year <- as.integer(loaded$bracket_year)
    paths <- build_odds_history_paths(bracket_year, history_dir = config$betting$history_dir %||% default_runtime_history_root())
    reserve_floor <- config$betting$reserve_floor %||% 100L
    schedule_refresh_minutes <- config$betting$schedule_refresh_minutes %||% 30L
    poll_interval_minutes <- config$betting$poll_interval_minutes %||% 30L
    tail_minutes <- config$betting$tail_minutes %||% 45L
    cooldown_minutes <- suppressWarnings(as.numeric(config$betting$snapshot_cooldown_minutes %||% 10L))
    if (!is.finite(cooldown_minutes) || is.na(cooldown_minutes) || cooldown_minutes < 0) {
        cooldown_minutes <- 10
    }
    state <- read_odds_collector_state(paths$collector_state, fallback_state = default_odds_collector_state(
        bracket_year = bracket_year,
        quota_cap = config$betting$quota_cap %||% 500L,
        reserve_floor = reserve_floor
    ))

    if (!isTRUE(force) && !is_odds_collection_season_active(now, bracket_year)) {
        state$last_capture_reason <- "outside_tournament_window"
        state$last_error <- NULL
        state$next_poll_at <- format_utc_timestamp(next_odds_collection_season_start(now, bracket_year))
        write_odds_collector_state(state, paths$collector_state)
        return(list(
            bracket_year = bracket_year,
            schedule = list(
                paths = paths,
                events = empty_odds_schedule_events(),
                windows = empty_odds_schedule_windows(),
                fetched = NULL,
                season_active = FALSE
            ),
            state = state,
            plan = list(
                should_capture_now = FALSE,
                reason = "outside_tournament_window",
                next_poll_at = state$next_poll_at,
                active_window = NULL,
                budget = collector_budget_tier(state$remaining_credits, reserve_floor),
                remaining_credits = state$remaining_credits,
                quota_cap = state$quota_cap,
                next_refresh_at = state$next_poll_at
            ),
            snapshot = NULL,
            log_path = log_path,
            paths = paths
        ))
    }

    schedule <- refresh_tournament_odds_schedule(
        config = config,
        bracket_year = bracket_year,
        current_teams = loaded$current_teams,
        now = now,
        events_fetcher = events_fetcher
    )
    schedule$season_active <- TRUE

    state$last_schedule_refresh_at <- format_utc_timestamp(now)
    plan <- derive_odds_collection_plan(
        schedule_windows = schedule$windows,
        now = now,
        state = state,
        reserve_floor = reserve_floor,
        schedule_refresh_minutes = schedule_refresh_minutes,
        tail_minutes = tail_minutes
    )

    if (!isTRUE(force) && !isTRUE(plan$should_capture_now)) {
        state$next_poll_at <- plan$next_poll_at %||% state$next_poll_at
        state$last_capture_reason <- plan$reason %||% state$last_capture_reason
        state$last_error <- NULL
        write_odds_collector_state(state, paths$collector_state)
        return(list(
            bracket_year = bracket_year,
            schedule = schedule,
            state = state,
            plan = plan,
            snapshot = NULL,
            log_path = log_path,
            paths = paths
        ))
    }

    snapshot <- snapshot_capturer(
        config = config,
        bracket_year = bracket_year,
        current_teams = loaded$current_teams,
        force = isTRUE(force)
    )

    if (isTRUE(snapshot$skipped %||% FALSE)) {
        skipped_at <- snapshot$retrieved_at %||% now
        if (!inherits(skipped_at, "POSIXct")) {
            skipped_at <- as.POSIXct(skipped_at, tz = "UTC")
        }
        state$next_poll_at <- format_utc_timestamp(skipped_at + (cooldown_minutes * 60))
        state$last_capture_reason <- snapshot$skip_reason %||% plan$reason %||% "cooldown"
        state$last_snapshot_path <- snapshot$snapshot_path %||% state$last_snapshot_path
        state$last_snapshot_rows <- nrow(snapshot$lines_matchups %||% tibble::tibble())
        state$last_error <- NULL
        write_odds_collector_state(state, paths$collector_state)
        return(list(
            bracket_year = bracket_year,
            schedule = schedule,
            state = state,
            plan = plan,
            snapshot = snapshot,
            log_path = log_path,
            paths = paths
        ))
    }

    remaining_credits <- state$remaining_credits
    header_remaining <- snapshot$headers[["x-requests-remaining"]] %||% snapshot$headers[["X-Requests-Remaining"]] %||% NA_character_
    header_last_cost <- snapshot$headers[["x-requests-last"]] %||% snapshot$headers[["X-Requests-Last"]] %||% NA_character_
    header_remaining <- suppressWarnings(as.integer(header_remaining))
    header_last_cost <- suppressWarnings(as.integer(header_last_cost))
    if (is.finite(header_remaining) && !is.na(header_remaining)) {
        remaining_credits <- header_remaining
    } else if (is.finite(remaining_credits)) {
        remaining_credits <- max(0L, as.integer(remaining_credits) - ifelse(is.finite(header_last_cost) && !is.na(header_last_cost), header_last_cost, 2L))
    } else {
        remaining_credits <- config$betting$quota_cap %||% 500L
    }

    state$remaining_credits <- as.integer(remaining_credits)
    state$last_odds_capture_at <- format_utc_timestamp(now)
    state$next_poll_at <- format_utc_timestamp(now + ((plan$budget$poll_interval_minutes %||% poll_interval_minutes) * 60))
    state$last_snapshot_path <- snapshot$snapshot_path %||% NULL
    state$last_snapshot_cost <- if (is.finite(header_last_cost) && !is.na(header_last_cost)) as.integer(header_last_cost) else NULL
    state$last_snapshot_rows <- nrow(snapshot$lines_matchups %||% tibble::tibble())
    state$last_capture_reason <- plan$reason %||% "active_window"
    state$last_window_id <- if (!is.null(plan$active_window)) plan$active_window$slate_id[[1]] else NULL
    state$last_error <- NULL
    write_odds_collector_state(state, paths$collector_state)

    list(
        bracket_year = bracket_year,
        schedule = schedule,
        state = state,
        plan = plan,
        snapshot = snapshot,
        log_path = log_path,
        paths = paths
    )
}
