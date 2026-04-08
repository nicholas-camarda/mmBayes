library(dplyr)
library(httr)
library(jsonlite)
library(logger)
library(stringr)
library(tibble)

#' Return the verified OddsPapi bookmaker catalog used by this project
#'
#' @return A tibble containing bookmaker slugs and display names.
#' @keywords internal
oddspapi_verified_bookmakers <- function() {
    tibble::tribble(
        ~slug, ~display_name,
        "pinnacle", "Pinnacle Sports",
        "bet365", "bet365",
        "caesars", "Caesars Sportsbook",
        "draftkings", "Draftkings",
        "fanduel", "FanDuel",
        "betmgm", "BetMGM"
    )
}

#' Validate configured OddsPapi bookmaker slugs
#'
#' @param slugs Character vector of bookmaker slugs.
#'
#' @return Invisibly returns the unique validated slugs.
#' @keywords internal
validate_oddspapi_bookmakers <- function(slugs) {
    slugs <- unique(as.character(unlist(slugs %||% character(0))))
    verified <- oddspapi_verified_bookmakers()$slug
    invalid <- setdiff(slugs, verified)
    if (length(invalid) > 0) {
        stop_with_message(sprintf(
            "Unsupported OddsPapi bookmaker slug(s): %s. Use one of: %s",
            paste(invalid, collapse = ", "),
            paste(verified, collapse = ", ")
        ))
    }
    invisible(slugs)
}

#' Return the base URL for the OddsPapi v4 self-serve API
#'
#' @return A scalar character URL.
#' @keywords internal
oddspapi_v4_host <- function() {
    "https://api.oddspapi.io/v4"
}

#' Resolve the historical OddsPapi configuration block
#'
#' @param config Project configuration list.
#'
#' @return A normalized historical provider config list.
#' @keywords internal
resolve_oddspapi_historical_config <- function(config) {
    historical_override <- (config$betting %||% list())$historical %||% list()
    explicit_bookmaker_sets <- historical_override$bookmaker_sets %||% NULL
    explicit_date_windows <- historical_override$date_windows %||% NULL
    historical <- historical_override
    historical <- merge_config_lists(
        list(
            enabled = TRUE,
            provider = "oddspapi_v4",
            api_key_env = "ODDS_PAPI_API_KEY",
            sport_id = 11L,
            tournament_id = 28370L,
            target_years = integer(0),
            archive_start_year = 2026L,
            documented_retention_days = 92L,
            season_close_month_day = "04-15",
            bookmaker_sets = list(
                c("pinnacle", "bet365", "caesars"),
                c("draftkings", "fanduel", "betmgm")
            ),
            date_windows = list(
                list(from = "03-01", to = "04-15"),
                list(from = "02-15", to = "04-20"),
                list(from = "02-01", to = "04-30")
            ),
            status_id_finished = 2L,
            timeout_seconds = 20L,
            historical_request_cooldown_seconds = 5,
            cache_raw_responses = TRUE,
            continue_on_provider_gaps = TRUE,
            release_raw_cache = FALSE
        ),
        historical
    )

    # Unnamed list overrides should replace the defaults rather than disappear
    # during recursive merge. We rely on this for one-off seasonal imports.
    if (!is.null(explicit_bookmaker_sets)) {
        historical$bookmaker_sets <- explicit_bookmaker_sets
    }
    if (!is.null(explicit_date_windows)) {
        historical$date_windows <- explicit_date_windows
    }

    historical$sport_id <- as.integer(historical$sport_id %||% 11L)
    historical$tournament_id <- as.integer(historical$tournament_id %||% 28370L)
    historical$status_id_finished <- as.integer(historical$status_id_finished %||% 2L)
    historical$timeout_seconds <- as.integer(historical$timeout_seconds %||% 20L)
    historical$historical_request_cooldown_seconds <- suppressWarnings(as.numeric(historical$historical_request_cooldown_seconds %||% 5))
    historical$target_years <- as.integer(unlist(historical$target_years %||% integer(0)))
    historical$archive_start_year <- as.integer(historical$archive_start_year %||% 2026L)
    historical$documented_retention_days <- as.integer(historical$documented_retention_days %||% 92L)
    historical$season_close_month_day <- safe_character_scalar(historical$season_close_month_day %||% "04-15", default = "04-15")
    historical$bookmaker_sets <- lapply(historical$bookmaker_sets %||% list(), function(slugs) {
        validated <- unique(as.character(unlist(slugs %||% character(0))))
        validate_oddspapi_bookmakers(validated)
        validated
    })
    historical
}

#' Build a stable JSON artifact on disk
#'
#' @param object Any JSON-serializable R object.
#' @param path Destination path.
#'
#' @return Invisibly returns the written path.
#' @keywords internal
write_json_artifact <- function(object, path) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(object, path = path, auto_unbox = TRUE, pretty = TRUE, null = "null")
    invisible(path)
}

#' Infer an HTTP-like status code from a cached OddsPapi error payload
#'
#' @param parsed Parsed JSON payload.
#'
#' @return An integer status code or `NA_integer_`.
#' @keywords internal
infer_oddspapi_status_code <- function(parsed) {
    code <- toupper(safe_character_scalar(
        parsed$error$code %||% parsed$code %||% NULL,
        default = ""
    ))
    if (!nzchar(code)) {
        return(NA_integer_)
    }
    if (identical(code, "RATE_LIMITED")) {
        return(429L)
    }
    if (identical(code, "NOT_FOUND")) {
        return(404L)
    }
    NA_integer_
}

#' Respect the documented historical-odds cooldown between provider calls
#'
#' @param cooldown_seconds Minimum seconds between requests.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
respect_oddspapi_historical_cooldown <- function(cooldown_seconds = 5) {
    cooldown_seconds <- suppressWarnings(as.numeric(cooldown_seconds))
    if (!is.finite(cooldown_seconds) || is.na(cooldown_seconds) || cooldown_seconds <= 0) {
        return(invisible(NULL))
    }

    last_request <- getOption("mmBayes.oddspapi_last_hist_request_time", default = NULL)
    if (inherits(last_request, "POSIXt")) {
        elapsed <- as.numeric(difftime(Sys.time(), last_request, units = "secs"))
        wait_seconds <- cooldown_seconds - elapsed
        if (is.finite(wait_seconds) && !is.na(wait_seconds) && wait_seconds > 0) {
            Sys.sleep(wait_seconds)
        }
    }

    options(mmBayes.oddspapi_last_hist_request_time = Sys.time())
    invisible(NULL)
}

#' Extract a human-readable OddsPapi API error message
#'
#' @param parsed Parsed JSON response content.
#'
#' @return A scalar character message.
#' @keywords internal
extract_oddspapi_error_message <- function(parsed) {
    if (is.null(parsed)) {
        return("Unknown provider error")
    }

    candidates <- c(
        safe_character_scalar(parsed$message %||% NULL, default = ""),
        safe_character_scalar(parsed$error %||% NULL, default = ""),
        safe_character_scalar(parsed$code %||% NULL, default = "")
    )
    candidates <- candidates[nzchar(candidates)]
    if (length(candidates) == 0L) {
        "Unknown provider error"
    } else {
        paste(unique(candidates), collapse = " | ")
    }
}

#' Execute an OddsPapi v4 GET request
#'
#' @param endpoint Endpoint path under the v4 host.
#' @param query Named query list.
#' @param api_key OddsPapi API key.
#' @param timeout_seconds Request timeout in seconds.
#' @param allow_http_error Whether to return non-2xx responses instead of raising.
#'
#' @return A list with status code, headers, parsed JSON, and raw text.
#' @keywords internal
oddspapi_v4_get <- function(endpoint,
                            query = list(),
                            api_key,
                            timeout_seconds = 20L,
                            allow_http_error = FALSE) {
    if (!nzchar(api_key %||% "")) {
        stop_with_message("Missing OddsPapi API key (set ODDS_PAPI_API_KEY in the repo-root .env file).")
    }

    response <- tryCatch(
        httr::GET(
            paste0(oddspapi_v4_host(), endpoint),
            query = c(query, list(apiKey = api_key)),
            httr::timeout(as.numeric(timeout_seconds %||% 20L))
        ),
        error = function(e) {
            if (isTRUE(allow_http_error)) {
                return(list(`__network_error__` = e$message))
            }
            stop_with_message(sprintf("OddsPapi request failed for %s: %s", endpoint, e$message))
        }
    )

    if (is.list(response) && !inherits(response, "response") && !is.null(response$`__network_error__`)) {
        return(list(
            status_code = NA_integer_,
            headers = list(),
            parsed = list(message = response$`__network_error__`, code = "NETWORK_ERROR"),
            raw_text = ""
        ))
    }

    raw_text <- httr::content(response, as = "text", encoding = "UTF-8")
    parsed <- tryCatch(jsonlite::fromJSON(raw_text, simplifyVector = FALSE), error = function(e) NULL)
    status_code <- httr::status_code(response)

    if (!isTRUE(allow_http_error) && status_code >= 400) {
        stop_with_message(sprintf(
            "OddsPapi request failed for %s with status %s: %s",
            endpoint,
            status_code,
            extract_oddspapi_error_message(parsed)
        ))
    }

    list(
        status_code = status_code,
        headers = httr::headers(response),
        parsed = parsed,
        raw_text = raw_text
    )
}

#' Convert decimal odds into an implied probability
#'
#' @param price Decimal price.
#'
#' @return A numeric probability or `NA_real_`.
#' @keywords internal
decimal_to_implied_prob <- function(price) {
    price <- suppressWarnings(as.numeric(price))
    ifelse(is.na(price) | !is.finite(price) | price <= 1, NA_real_, 1 / price)
}

#' Resolve a quote into an implied probability
#'
#' @param price Decimal price.
#' @param price_american Optional American price.
#'
#' @return A numeric probability or `NA_real_`.
#' @keywords internal
quote_to_implied_prob <- function(price, price_american = NA_real_) {
    price_american <- suppressWarnings(as.numeric(price_american))
    price <- suppressWarnings(as.numeric(price))

    implied_from_american <- rep(NA_real_, length(price_american))
    use_american <- is.finite(price_american) & !is.na(price_american) & price_american != 0
    if (any(use_american)) {
        implied_from_american[use_american] <- american_to_implied_prob(price_american[use_american])
    }

    implied_from_decimal <- decimal_to_implied_prob(price)
    dplyr::coalesce(implied_from_american, implied_from_decimal)
}

#' Parse a provider epoch millisecond timestamp to UTC
#'
#' @param x Epoch milliseconds.
#'
#' @return A POSIXct vector in UTC.
#' @keywords internal
parse_oddspapi_changed_at <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    as.POSIXct(x / 1000, origin = "1970-01-01", tz = "UTC")
}

#' Normalize an OddsPapi timestamp-like value to epoch milliseconds
#'
#' @param x A provider timestamp, ISO-8601 string, epoch seconds, or epoch milliseconds.
#'
#' @return A numeric epoch-millisecond scalar or `NA_real_`.
#' @keywords internal
coerce_oddspapi_changed_at_ms <- function(x) {
    if (is.null(x) || length(x) == 0L) {
        return(NA_real_)
    }

    if (inherits(x, "POSIXt")) {
        return(as.numeric(as.POSIXct(x, tz = "UTC")) * 1000)
    }

    numeric_value <- suppressWarnings(as.numeric(x))
    if (is.finite(numeric_value) && !is.na(numeric_value)) {
        if (numeric_value < 1e11) {
            return(numeric_value * 1000)
        }
        return(numeric_value)
    }

    text_value <- safe_character_scalar(x, default = "")
    safe_posixct <- function(value, format = NULL) {
        tryCatch(
            suppressWarnings(as.POSIXct(value, format = format, tz = "UTC")),
            error = function(e) as.POSIXct(NA, tz = "UTC")
        )
    }

    parsed <- parse_utc_timestamp(text_value)
    if (is.na(parsed) || !is.finite(parsed)) {
        normalized_text <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", text_value)
        parsed <- safe_posixct(
            normalized_text,
            format = "%Y-%m-%dT%H:%M:%OS%z"
        )
    }
    if (is.na(parsed) || !is.finite(parsed)) {
        parsed <- safe_posixct(
            text_value,
            format = "%Y-%m-%d %H:%M:%OS"
        )
    }
    if (is.na(parsed) || !is.finite(parsed)) {
        parsed <- safe_posixct(text_value)
    }
    if (is.na(parsed) || !is.finite(parsed)) {
        return(NA_real_)
    }

    as.numeric(parsed) * 1000
}

#' Parse an OddsPapi fixture start time to UTC
#'
#' @param x A provider start time value.
#'
#' @return A POSIXct timestamp in UTC or `NA`.
#' @keywords internal
parse_oddspapi_start_time <- function(x) {
    if (is.null(x) || length(x) == 0L) {
        return(as.POSIXct(NA, tz = "UTC"))
    }

    if (inherits(x, "POSIXt")) {
        return(as.POSIXct(x, tz = "UTC"))
    }

    numeric_value <- suppressWarnings(as.numeric(x))
    if (is.finite(numeric_value) && !is.na(numeric_value)) {
        origin_value <- if (numeric_value > 1e11) numeric_value / 1000 else numeric_value
        return(as.POSIXct(origin_value, origin = "1970-01-01", tz = "UTC"))
    }

    parse_utc_timestamp(safe_character_scalar(x, default = ""))
}

#' Build the canonical raw cache path for a fixture-bookmaker pull
#'
#' @param paths Year-specific odds-history paths.
#' @param fixture_id Provider fixture ID.
#' @param bookmaker Bookmaker slug.
#'
#' @return A cache-file path.
#' @keywords internal
build_oddspapi_raw_cache_path <- function(paths, fixture_id, bookmaker) {
    safe_fixture <- gsub("[^A-Za-z0-9._-]", "_", safe_character_scalar(fixture_id, "fixture"))
    safe_bookmaker <- gsub("[^A-Za-z0-9._-]", "_", safe_character_scalar(bookmaker, "bookmaker"))
    file.path(paths$oddspapi_raw_dir, sprintf("%s__%s.json", safe_fixture, safe_bookmaker))
}

#' Return the provider's documented closing-date proxy for a tournament year
#'
#' @param year Tournament year.
#' @param historical Historical provider configuration.
#'
#' @return A Date scalar.
#' @keywords internal
oddspapi_documented_season_close_date <- function(year, historical) {
    as.Date(sprintf("%s-%s", as.integer(year), historical$season_close_month_day %||% "04-15"))
}

#' Resolve which tournament years are eligible for OddsPapi v4 import
#'
#' @param historical Historical provider configuration.
#' @param bracket_year Current bracket year from project data.
#' @param requested_years Optional explicit years requested by the caller.
#' @param reference_date Date used for retention comparisons.
#'
#' @return A list with `eligible_years` and a tibble of `skipped_years`.
#' @keywords internal
resolve_oddspapi_import_years <- function(historical,
                                          bracket_year,
                                          requested_years = NULL,
                                          reference_date = Sys.Date()) {
    bracket_year <- suppressWarnings(as.integer(bracket_year))
    archive_start_year <- suppressWarnings(as.integer(historical$archive_start_year %||% 2026L))
    retention_days <- suppressWarnings(as.integer(historical$documented_retention_days %||% 92L))
    reference_date <- as.Date(reference_date)

    candidate_years <- as.integer(requested_years %||% historical$target_years %||% integer(0))
    if (length(candidate_years) == 0L) {
        if (is.finite(bracket_year) && !is.na(bracket_year)) {
            candidate_years <- seq.int(archive_start_year, bracket_year)
        } else {
            candidate_years <- archive_start_year
        }
    }
    candidate_years <- sort(unique(candidate_years[is.finite(candidate_years) & !is.na(candidate_years)]))

    if (length(candidate_years) == 0L) {
        return(list(
            eligible_years = integer(0),
            skipped_years = tibble::tibble(year = integer(), reason = character())
        ))
    }

    skipped_rows <- tibble::tibble(year = integer(), reason = character())
    for (year in candidate_years) {
        skip_reason <- NULL
        if (!is.na(bracket_year) && year > bracket_year) {
            skip_reason <- "future_season"
        } else if (year < archive_start_year) {
            skip_reason <- "before_archive_start_year"
        } else if (oddspapi_documented_season_close_date(year, historical) < (reference_date - retention_days)) {
            skip_reason <- "outside_documented_retention_window"
        }

        if (!is.null(skip_reason)) {
            skipped_rows <- dplyr::bind_rows(
                skipped_rows,
                tibble::tibble(year = year, reason = skip_reason)
            )
        }
    }

    eligible_years <- setdiff(candidate_years, skipped_rows$year)

    list(
        eligible_years = as.integer(eligible_years),
        skipped_years = skipped_rows
    )
}

#' Flatten the OddsPapi market catalog into an outcome-level table
#'
#' @param markets Parsed `/markets` response.
#'
#' @return A tibble keyed by `market_id` and `outcome_id`.
#' @keywords internal
flatten_oddspapi_markets_catalog <- function(markets) {
    if (length(markets %||% list()) == 0L) {
        return(tibble::tibble())
    }

    purrr::map_dfr(markets, function(market) {
        market_id <- suppressWarnings(as.integer(market$marketId %||% NA_integer_))
        outcomes <- market$outcomes %||% list()
        purrr::map_dfr(outcomes, function(outcome) {
            tibble::tibble(
                market_id = market_id,
                outcome_id = suppressWarnings(as.integer(outcome$outcomeId %||% NA_integer_)),
                outcome_name = safe_character_scalar(outcome$outcomeName %||% NA_character_, default = NA_character_),
                market_type = safe_character_scalar(market$marketType %||% NA_character_, default = NA_character_),
                market_name = safe_character_scalar(market$marketName %||% NA_character_, default = NA_character_),
                market_name_short = safe_character_scalar(market$marketNameShort %||% NA_character_, default = NA_character_),
                period = safe_character_scalar(market$period %||% NA_character_, default = NA_character_),
                handicap = suppressWarnings(as.numeric(market$handicap %||% NA_real_)),
                market_length = suppressWarnings(as.integer(market$marketLength %||% NA_integer_)),
                player_prop = isTRUE(market$playerProp %||% FALSE),
                sport_id = suppressWarnings(as.integer(market$sportId %||% NA_integer_))
            )
        })
    }) %>%
        dplyr::distinct(market_id, outcome_id, .keep_all = TRUE)
}

#' Determine whether a market row represents a main spread market
#'
#' @param market_rows A market-catalog tibble for a single market.
#'
#' @return `TRUE` when the market is plausibly a team spread.
#' @keywords internal
is_oddspapi_spread_market <- function(market_rows) {
    if (nrow(market_rows) == 0L) {
        return(FALSE)
    }

    market_type <- tolower(safe_character_scalar(market_rows$market_type[[1]], default = ""))
    market_name <- tolower(safe_character_scalar(market_rows$market_name[[1]], default = ""))
    outcome_names <- tolower(unique(na.omit(market_rows$outcome_name)))

    is_team_outcome <- identical(sort(outcome_names), c("1", "2")) ||
        all(outcome_names %in% c("1", "2"))

    (grepl("spread", market_type, fixed = TRUE) ||
        grepl("handicap", market_type, fixed = TRUE) ||
        grepl("spread", market_name, fixed = TRUE) ||
        grepl("handicap", market_name, fixed = TRUE)) &&
        is_team_outcome
}

#' Flatten a v4 `historical-odds` response into quote rows
#'
#' @param response Parsed v4 `historical-odds` response.
#' @param markets_catalog Flattened market catalog.
#'
#' @return A tibble of quote rows joined to market metadata.
#' @keywords internal
flatten_oddspapi_historical_quotes <- function(response,
                                               markets_catalog,
                                               relevant_market_ids = NULL) {
    bookmakers <- response$bookmakers %||% list()

    rows <- if (length(bookmakers) > 0L) {
        purrr::imap_dfr(bookmakers, function(bookmaker_node, bookmaker_slug) {
            markets <- bookmaker_node$markets %||% list()
            if (!is.null(relevant_market_ids) && length(relevant_market_ids) > 0L) {
                keep_ids <- as.character(unique(relevant_market_ids))
                markets <- markets[names(markets) %in% keep_ids]
            }
            purrr::imap_dfr(markets, function(market_node, market_id_key) {
                outcomes <- market_node$outcomes %||% list()
                purrr::imap_dfr(outcomes, function(outcome_node, outcome_id_key) {
                    players <- outcome_node$players %||% list()
                    purrr::imap_dfr(players, function(history_rows, player_id_key) {
                        purrr::map_dfr(history_rows %||% list(), function(quote) {
                            market_id <- suppressWarnings(as.integer(quote$marketId %||% market_id_key %||% NA_integer_))
                            outcome_id <- suppressWarnings(as.integer(quote$outcomeId %||% outcome_id_key %||% NA_integer_))
                            player_id <- suppressWarnings(as.integer(quote$playerId %||% player_id_key %||% NA_integer_))

                            tibble::tibble(
                                bookmaker = safe_character_scalar(bookmaker_slug, bookmaker_slug),
                                odds_id = paste(
                                    safe_character_scalar(response$fixtureId %||% "", default = ""),
                                    bookmaker_slug,
                                    market_id,
                                    outcome_id,
                                    player_id,
                                    sep = ":"
                                ),
                                changed_at = coerce_oddspapi_changed_at_ms(quote$createdAt %||% NA_real_),
                                outcome_id = outcome_id,
                                market_id = market_id,
                                player_id = player_id,
                                price = suppressWarnings(as.numeric(quote$price %||% NA_real_)),
                                price_american = suppressWarnings(as.numeric(quote$priceAmerican %||% NA_real_)),
                                active = if (is.null(quote$active)) TRUE else isTRUE(quote$active),
                                market_active = TRUE,
                                main_line = FALSE
                            )
                        })
                    })
                })
            })
        })
    } else {
        tibble::tibble()
    }

    if (nrow(rows) == 0L) {
        return(rows)
    }

    rows %>%
        dplyr::left_join(markets_catalog, by = c("market_id", "outcome_id")) %>%
        dplyr::mutate(
            changed_at_utc = parse_oddspapi_changed_at(changed_at),
            implied_prob = quote_to_implied_prob(price, price_american)
        )
}

#' Select the latest usable two-way market snapshot before tip-off
#'
#' @param quotes Flattened quote rows for one bookmaker.
#' @param kickoff_ms Fixture start time in epoch milliseconds.
#' @param market_filter Function taking a market-catalog tibble and returning TRUE/FALSE.
#'
#' @return A one-row tibble containing the selected market, or zero rows.
#' @keywords internal
select_last_pregame_two_way_market <- function(quotes, kickoff_ms, market_filter) {
    if (nrow(quotes) == 0L) {
        return(tibble::tibble())
    }

    filtered_quotes <- quotes %>%
        dplyr::filter(active, market_active, player_id %in% c(0L, NA_integer_))

    market_candidates <- purrr::map_dfr(unique(filtered_quotes$market_id), function(market_id) {
        market_rows <- filtered_quotes %>%
            dplyr::filter(market_id == !!market_id)
        catalog_rows <- market_rows %>%
            dplyr::select(market_id, outcome_id, outcome_name, market_type, market_name, market_name_short, period, handicap, market_length, player_prop) %>%
            dplyr::distinct()
        if (!isTRUE(market_filter(catalog_rows))) {
            return(tibble::tibble())
        }

        latest_rows <- market_rows %>%
            dplyr::filter(changed_at <= kickoff_ms) %>%
            dplyr::group_by(outcome_id) %>%
            dplyr::slice_max(changed_at, n = 1, with_ties = FALSE) %>%
            dplyr::ungroup()

        if (nrow(latest_rows) < 2L) {
            return(tibble::tibble())
        }

        latest_rows %>%
            dplyr::arrange(outcome_id) %>%
            dplyr::mutate(
                selected_changed_at = max(changed_at, na.rm = TRUE),
                selected_main_line = any(main_line, na.rm = TRUE)
            )
    })

    if (nrow(market_candidates) == 0L) {
        return(tibble::tibble())
    }

    split_candidates <- split(market_candidates, market_candidates$market_id)

    purrr::map_dfr(names(split_candidates), function(market_id) {
        rows <- dplyr::as_tibble(split_candidates[[market_id]])
        tibble::tibble(
            market_id = suppressWarnings(as.integer(market_id)),
            selected_changed_at = max(rows$selected_changed_at, na.rm = TRUE),
            selected_main_line = any(rows$selected_main_line, na.rm = TRUE),
            market_distance = abs(stats::median(rows$implied_prob, na.rm = TRUE) - 0.5),
            quotes = list(rows)
        )
    }) %>%
        dplyr::arrange(dplyr::desc(selected_main_line), dplyr::desc(selected_changed_at), market_distance) %>%
        dplyr::slice(1)
}

#' Derive one bookmaker's closing line for a fixture
#'
#' @param historical_response Parsed historical-odds response.
#' @param markets_catalog Flattened market catalog.
#' @param kickoff_ms Fixture start time in epoch milliseconds.
#' @param participant_1 Canonical participant 1 name.
#' @param participant_2 Canonical participant 2 name.
#'
#' @return A one-row tibble of bookmaker-level closing-line fields.
#' @keywords internal
derive_oddspapi_bookmaker_closing_line <- function(historical_response,
                                                   markets_catalog,
                                                   kickoff_ms = NA_real_,
                                                   participant_1 = NA_character_,
                                                   participant_2 = NA_character_) {
    fixture_id <- safe_character_scalar(historical_response$fixtureId %||% NA_character_, default = NA_character_)
    kickoff_ms <- dplyr::coalesce(
        suppressWarnings(as.numeric(kickoff_ms)),
        coerce_oddspapi_changed_at_ms(historical_response$startTime %||% historical_response$trueStartTime %||% NA_real_)
    )
    participant_1 <- canonicalize_team_name(dplyr::coalesce(
        participant_1,
        historical_response$participant1Name %||% historical_response$participants$participant1Name %||% NA_character_
    ))
    participant_2 <- canonicalize_team_name(dplyr::coalesce(
        participant_2,
        historical_response$participant2Name %||% historical_response$participants$participant2Name %||% NA_character_
    ))

    bookmakers <- historical_response$bookmakers %||% list()
    bookmaker_slug <- names(bookmakers)[[1]] %||% NA_character_
    bookmaker_node <- bookmakers[[1]] %||% list()
    markets <- bookmaker_node$markets %||% list()

    if (length(markets) == 0L || !is.finite(kickoff_ms) || is.na(kickoff_ms)) {
        return(tibble::tibble(
            fixture_id = fixture_id,
            bookmaker = NA_character_,
            participant1 = participant_1,
            participant2 = participant_2,
            closing_time_utc = as.POSIXct(NA, tz = "UTC"),
            prob_participant1 = NA_real_,
            prob_participant2 = NA_real_,
            spread_participant1 = NA_real_,
            spread_participant2 = NA_real_,
            moneyline_available = FALSE,
            spread_available = FALSE
        ))
    }

    market_snapshots <- purrr::imap_dfr(markets, function(market_node, market_id_key) {
        market_id <- suppressWarnings(as.integer(market_id_key))
        if (!is.finite(market_id) || is.na(market_id)) {
            return(tibble::tibble())
        }

        catalog_rows <- markets_catalog %>%
            dplyr::filter(market_id == !!market_id)
        if (nrow(catalog_rows) == 0L) {
            return(tibble::tibble())
        }

        is_moneyline <- {
            market_type <- tolower(safe_character_scalar(catalog_rows$market_type[[1]], default = ""))
            market_name <- tolower(safe_character_scalar(catalog_rows$market_name[[1]], default = ""))
            (identical(catalog_rows$market_length[[1]], 2L) || is.na(catalog_rows$market_length[[1]])) &&
                (grepl("moneyline", market_type, fixed = TRUE) || grepl("winner", market_name, fixed = TRUE))
        }
        is_spread <- is_oddspapi_spread_market(catalog_rows)
        if (!is_moneyline && !is_spread) {
            return(tibble::tibble())
        }

        selected_quotes <- purrr::imap_dfr(market_node$outcomes %||% list(), function(outcome_node, outcome_id_key) {
            outcome_id <- suppressWarnings(as.integer(outcome_id_key))
            outcome_catalog <- catalog_rows %>%
                dplyr::filter(outcome_id == !!outcome_id) %>%
                dplyr::slice(1)
            if (nrow(outcome_catalog) == 0L) {
                return(tibble::tibble())
            }

            players <- outcome_node$players %||% list()
            if (length(players) == 0L) {
                return(tibble::tibble())
            }

            best_quote <- NULL
            best_changed_at <- -Inf
            for (player_key in names(players)) {
                player_id <- suppressWarnings(as.integer(player_key))
                if (!is.na(player_id) && player_id != 0L) {
                    next
                }
                history_rows <- players[[player_key]] %||% list()
                for (quote in history_rows) {
                    changed_at <- coerce_oddspapi_changed_at_ms(quote$createdAt %||% NA_real_)
                    if (!is.finite(changed_at) || is.na(changed_at) || changed_at > kickoff_ms) {
                        next
                    }
                    if (!isTRUE(quote$active %||% FALSE) && !is.null(quote$active)) {
                        next
                    }
                    if (changed_at >= best_changed_at) {
                        best_changed_at <- changed_at
                        best_quote <- quote
                    }
                }
            }

            if (is.null(best_quote)) {
                return(tibble::tibble())
            }

            tibble::tibble(
                market_id = market_id,
                outcome_id = outcome_id,
                outcome_name = outcome_catalog$outcome_name[[1]],
                handicap = suppressWarnings(as.numeric(outcome_catalog$handicap[[1]])),
                market_kind = if (is_moneyline) "moneyline" else "spread",
                changed_at = best_changed_at,
                changed_at_utc = parse_oddspapi_changed_at(best_changed_at),
                implied_prob = quote_to_implied_prob(best_quote$price %||% NA_real_, best_quote$priceAmerican %||% NA_real_)
            )
        })

        if (nrow(selected_quotes) < 2L) {
            return(tibble::tibble())
        }

        selected_quotes %>%
            dplyr::arrange(outcome_id) %>%
            dplyr::mutate(
                selected_changed_at = max(changed_at, na.rm = TRUE),
                market_distance = abs(stats::median(implied_prob, na.rm = TRUE) - 0.5)
            )
    })

    moneyline_market <- market_snapshots %>%
        dplyr::filter(market_kind == "moneyline") %>%
        split(.$market_id) %>%
        purrr::imap_dfr(function(rows, market_id) {
            rows <- dplyr::as_tibble(rows)
            tibble::tibble(
                market_id = suppressWarnings(as.integer(market_id)),
                selected_changed_at = max(rows$selected_changed_at, na.rm = TRUE),
                market_distance = min(rows$market_distance, na.rm = TRUE),
                quotes = list(rows)
            )
        }) %>%
        dplyr::arrange(dplyr::desc(selected_changed_at), market_distance) %>%
        dplyr::slice_head(n = 1)

    spread_market <- market_snapshots %>%
        dplyr::filter(market_kind == "spread") %>%
        split(.$market_id) %>%
        purrr::imap_dfr(function(rows, market_id) {
            rows <- dplyr::as_tibble(rows)
            tibble::tibble(
                market_id = suppressWarnings(as.integer(market_id)),
                selected_changed_at = max(rows$selected_changed_at, na.rm = TRUE),
                market_distance = min(rows$market_distance, na.rm = TRUE),
                quotes = list(rows)
            )
        }) %>%
        dplyr::arrange(dplyr::desc(selected_changed_at), market_distance) %>%
        dplyr::slice_head(n = 1)

    prob_participant1 <- NA_real_
    prob_participant2 <- NA_real_
    moneyline_available <- FALSE
    moneyline_time <- as.POSIXct(NA, tz = "UTC")

    if (nrow(moneyline_market) > 0L) {
        selected_quotes <- moneyline_market$quotes[[1]] %>%
            dplyr::arrange(outcome_id)
        implied_probs <- selected_quotes$implied_prob
        if (length(implied_probs) >= 2L && all(is.finite(implied_probs[1:2]))) {
            vig_free <- remove_vig_two_way(implied_probs[[1]], implied_probs[[2]])
            outcome_names <- toupper(selected_quotes$outcome_name[1:2] %||% c("1", "2"))
            if (identical(outcome_names[[1]], "1") || identical(outcome_names[[1]], participant_1)) {
                prob_participant1 <- vig_free[[1]]
                prob_participant2 <- vig_free[[2]]
            } else {
                prob_participant1 <- vig_free[[2]]
                prob_participant2 <- vig_free[[1]]
            }
            moneyline_available <- is.finite(prob_participant1) && is.finite(prob_participant2)
            moneyline_time <- max(selected_quotes$changed_at_utc, na.rm = TRUE)
        }
    }

    spread_participant1 <- NA_real_
    spread_participant2 <- NA_real_
    spread_available <- FALSE
    spread_time <- as.POSIXct(NA, tz = "UTC")

    if (nrow(spread_market) > 0L) {
        selected_quotes <- spread_market$quotes[[1]] %>%
            dplyr::arrange(outcome_id)
        handicap <- suppressWarnings(as.numeric(selected_quotes$handicap[[1]]))
        outcome_names <- toupper(selected_quotes$outcome_name[1:2] %||% c("1", "2"))
        if (is.finite(handicap) && !is.na(handicap)) {
            if (identical(outcome_names[[1]], "1") || identical(outcome_names[[1]], participant_1)) {
                spread_participant1 <- handicap
                spread_participant2 <- -handicap
            } else {
                spread_participant1 <- -handicap
                spread_participant2 <- handicap
            }
            spread_available <- is.finite(spread_participant1)
            spread_time <- max(selected_quotes$changed_at_utc, na.rm = TRUE)
        }
    }

    tibble::tibble(
        fixture_id = fixture_id,
        bookmaker = bookmaker_slug,
        participant1 = participant_1,
        participant2 = participant_2,
        closing_time_utc = max(c(moneyline_time, spread_time), na.rm = TRUE),
        prob_participant1 = prob_participant1,
        prob_participant2 = prob_participant2,
        spread_participant1 = spread_participant1,
        spread_participant2 = spread_participant2,
        moneyline_available = moneyline_available,
        spread_available = spread_available
    )
}

#' Normalize an OddsPapi fixture into canonical team names
#'
#' @param fixture A parsed fixture object.
#' @param allowed_team_names Allowed canonical tournament team names.
#'
#' @return A one-row tibble with canonicalized team names.
#' @keywords internal
normalize_oddspapi_fixture <- function(fixture, allowed_team_names) {
    participants <- if (is.list(fixture$participants)) fixture$participants else list()
    season <- if (is.list(fixture$season)) fixture$season else list()
    tournament <- if (is.list(fixture$tournament)) fixture$tournament else list()
    participant1 <- match_odds_team_to_tournament(
        fixture$participant1Name %||% participants$participant1Name %||% "",
        allowed_team_names = allowed_team_names
    )
    participant2 <- match_odds_team_to_tournament(
        fixture$participant2Name %||% participants$participant2Name %||% "",
        allowed_team_names = allowed_team_names
    )

    tibble::tibble(
        fixture_id = safe_character_scalar(fixture$fixtureId %||% NA_character_, default = NA_character_),
        start_time_utc = if (!is.null(fixture$trueStartTime) && !is.na(parse_oddspapi_start_time(fixture$trueStartTime))) {
            parse_oddspapi_start_time(fixture$trueStartTime)
        } else {
            parse_oddspapi_start_time(fixture$startTime %||% NA)
        },
        participant1 = participant1,
        participant2 = participant2,
        matchup_key = if (nzchar(participant1 %||% "") && nzchar(participant2 %||% "")) build_matchup_key(participant1, participant2) else NA_character_,
        has_odds = isTRUE(fixture$hasOdds %||% FALSE),
        season_id = suppressWarnings(as.integer(fixture$seasonId %||% season$seasonId %||% NA_integer_)),
        season_name = safe_character_scalar(fixture$seasonName %||% season$seasonName %||% NA_character_, default = NA_character_),
        tournament_name = safe_character_scalar(fixture$tournamentName %||% tournament$tournamentName %||% NA_character_, default = NA_character_)
    )
}

#' Read a cached or fresh historical-odds response for one fixture-bookmaker
#'
#' @param fixture_id Provider fixture ID.
#' @param bookmaker Bookmaker slug.
#' @param api_key OddsPapi API key.
#' @param paths Year-specific odds-history paths.
#' @param timeout_seconds Request timeout in seconds.
#' @param use_cache Whether existing raw cache files should be reused.
#'
#' @return A list containing `parsed`, `status_code`, `cache_path`, and `source`.
#' @keywords internal
fetch_oddspapi_fixture_historical_odds <- function(fixture_id,
                                                   bookmaker,
                                                   api_key,
                                                   paths,
                                                   timeout_seconds = 20L,
                                                   use_cache = TRUE,
                                                   cooldown_seconds = 5,
                                                   max_rate_limit_retries = 3L) {
    cache_path <- build_oddspapi_raw_cache_path(paths, fixture_id, bookmaker)
    if (isTRUE(use_cache) && file.exists(cache_path)) {
        raw_text <- readLines(cache_path, warn = FALSE, encoding = "UTF-8")
        raw_text <- paste(raw_text, collapse = "\n")
        parsed <- tryCatch(jsonlite::fromJSON(raw_text, simplifyVector = FALSE), error = function(e) NULL)
        cached_status <- if (!is.null(parsed$fixtureId)) 200L else infer_oddspapi_status_code(parsed)
        if (identical(cached_status, 429L)) {
            unlink(cache_path)
        } else {
            return(list(
                parsed = parsed,
                status_code = cached_status,
                cache_path = cache_path,
                source = "cache"
            ))
        }
    }

    attempt <- 0L
    response <- NULL
    repeat {
        attempt <- attempt + 1L
        respect_oddspapi_historical_cooldown(cooldown_seconds)
        response <- oddspapi_v4_get(
            endpoint = "/historical-odds",
            query = list(
                fixtureId = fixture_id,
                bookmakers = bookmaker
            ),
            api_key = api_key,
            timeout_seconds = timeout_seconds,
            allow_http_error = TRUE
        )
        status_code <- response$status_code %||% infer_oddspapi_status_code(response$parsed)
        if (!identical(status_code, 429L) || attempt >= max_rate_limit_retries) {
            break
        }

        retry_ms <- suppressWarnings(as.numeric(
            response$parsed$error$retryMs %||% response$parsed$retryMs %||% NA_real_
        ))
        wait_seconds <- if (is.finite(retry_ms) && !is.na(retry_ms)) {
            max(cooldown_seconds, (retry_ms / 1000) + 0.25)
        } else {
            max(cooldown_seconds, 5)
        }
        logger::log_warn(
            "OddsPapi historical odds rate limited for fixture {fixture_id} bookmaker {bookmaker}; retrying in {round(wait_seconds, 2)}s (attempt {attempt}/{max_rate_limit_retries})."
        )
        Sys.sleep(wait_seconds)
    }

    final_status <- response$status_code %||% infer_oddspapi_status_code(response$parsed)
    if (!identical(final_status, 429L)) {
        dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
        writeLines(response$raw_text %||% "", cache_path, useBytes = TRUE)
    }

    list(
        parsed = response$parsed,
        status_code = final_status,
        cache_path = cache_path,
        source = "network"
    )
}

#' Build year-specific fixture retrieval attempts
#'
#' @param year Tournament year.
#' @param historical Historical provider configuration.
#'
#' @return A list of attempt descriptors.
#' @keywords internal
build_oddspapi_recovery_attempts <- function(year, historical) {
    windows <- historical$date_windows %||% list(list(from = "03-01", to = "04-15"))
    bookmaker_sets <- historical$bookmaker_sets %||% list(c("pinnacle", "bet365", "caesars"))

    attempts <- list()
    attempt_id <- 1L
    for (window in windows) {
        for (bookmaker_set in bookmaker_sets) {
            attempts[[length(attempts) + 1L]] <- list(
                attempt_id = attempt_id,
                from = sprintf("%s-%s", year, window$from %||% "03-01"),
                to = sprintf("%s-%s", year, window$to %||% "04-15"),
                bookmakers = as.character(unlist(bookmaker_set))
            )
            attempt_id <- attempt_id + 1L
        }
    }

    attempts
}

#' Fetch fixture metadata for one recovery attempt
#'
#' @param historical Historical provider configuration.
#' @param api_key OddsPapi API key.
#' @param attempt Recovery attempt descriptor.
#'
#' @return A list with parsed fixtures and attempt metadata.
#' @keywords internal
fetch_oddspapi_fixtures_for_attempt <- function(historical, api_key, attempt) {
    response <- oddspapi_v4_get(
        endpoint = "/fixtures",
        query = list(
            tournamentId = historical$tournament_id,
            statusId = historical$status_id_finished,
            from = attempt$from,
            to = attempt$to,
            bookmakers = paste(attempt$bookmakers, collapse = ",")
        ),
        api_key = api_key,
        timeout_seconds = historical$timeout_seconds,
        allow_http_error = TRUE
    )

    list(
        attempt_id = attempt$attempt_id,
        from = attempt$from,
        to = attempt$to,
        bookmakers = attempt$bookmakers,
        status_code = response$status_code,
        parsed = response$parsed %||% list(),
        error_message = if (isTRUE(response$status_code >= 400)) extract_oddspapi_error_message(response$parsed) else ""
    )
}

#' Reconcile OddsPapi fixtures onto local tournament results
#'
#' @param fixtures Parsed fixture list.
#' @param results_year Local tournament results for one year.
#'
#' @return A list with `fixture_table`, `matched`, and `unmatched_results`.
#' @keywords internal
reconcile_oddspapi_fixtures_to_results <- function(fixtures, results_year) {
    allowed <- unique(c(results_year$teamA, results_year$teamB))
    fixture_table <- purrr::map_dfr(fixtures %||% list(), normalize_oddspapi_fixture, allowed_team_names = allowed)
    if (nrow(fixture_table) == 0L) {
        fixture_table <- tibble::tibble(
            fixture_id = character(),
            start_time_utc = as.POSIXct(character(), tz = "UTC"),
            participant1 = character(),
            participant2 = character(),
            matchup_key = character(),
            has_odds = logical(),
            season_id = integer(),
            season_name = character(),
            tournament_name = character()
        )
    }

    results_lookup <- results_year %>%
        dplyr::mutate(
            Year = as.character(Year),
            matchup_key = purrr::map2_chr(teamA, teamB, build_matchup_key)
        )

    matched <- results_lookup %>%
        dplyr::left_join(
            fixture_table %>%
                dplyr::arrange(desc(start_time_utc)) %>%
                dplyr::distinct(matchup_key, .keep_all = TRUE),
            by = "matchup_key"
        )

    unmatched_results <- matched %>%
        dplyr::filter(is.na(fixture_id) | !nzchar(fixture_id))

    list(
        fixture_table = fixture_table,
        matched = matched,
        unmatched_results = unmatched_results
    )
}

#' Build an empty closing-line row for an unmatched or unavailable game
#'
#' @param result_row One-row local tournament result tibble.
#' @param reason Provider-gap reason string.
#' @param fixture_id Optional provider fixture ID.
#' @param recovery_attempt Optional recovery attempt identifier.
#'
#' @return A one-row tibble matching the closing-lines schema.
#' @keywords internal
empty_oddspapi_closing_line_row <- function(result_row,
                                            reason,
                                            fixture_id = NA_character_,
                                            recovery_attempt = NA_integer_) {
    tibble::tibble(
        Year = as.character(result_row$Year[[1]]),
        region = result_row$region[[1]],
        round = result_row$round[[1]],
        game_index = suppressWarnings(as.integer(result_row$game_index[[1]])),
        teamA = canonicalize_team_name(result_row$teamA[[1]]),
        teamB = canonicalize_team_name(result_row$teamB[[1]]),
        winner = canonicalize_team_name(result_row$winner[[1]]),
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
        closing_before_commence = NA,
        provider_fixture_id = fixture_id,
        provider_gap_reason = reason,
        provider_recovery_attempt = recovery_attempt
    )
}

#' Derive the consensus closing line for one reconciled fixture
#'
#' @param result_row One-row local result tibble.
#' @param fixture_row One-row reconciled fixture tibble.
#' @param historical Historical provider configuration.
#' @param api_key OddsPapi API key.
#' @param paths Year-specific odds-history paths.
#' @param markets_catalog Flattened market catalog.
#' @param recovery_attempt Attempt identifier.
#'
#' @return A list with `closing_row`, `summary_row`, and `bookmaker_rows`.
#' @keywords internal
derive_oddspapi_fixture_closing_line <- function(result_row,
                                                 fixture_row,
                                                 historical,
                                                 api_key,
                                                 paths,
                                                 markets_catalog,
                                                 recovery_attempt,
                                                 bookmakers) {
    fixture_id <- fixture_row$fixture_id[[1]] %||% NA_character_
    configured_books <- unique(as.character(unlist(bookmakers %||% historical$bookmaker_sets[[1]] %||% character(0))))
    bookmaker_rows <- purrr::map_dfr(configured_books, function(bookmaker) {
        fetched <- fetch_oddspapi_fixture_historical_odds(
            fixture_id = fixture_id,
            bookmaker = bookmaker,
            api_key = api_key,
            paths = paths,
            timeout_seconds = historical$timeout_seconds,
            use_cache = isTRUE(historical$cache_raw_responses %||% TRUE),
            cooldown_seconds = historical$historical_request_cooldown_seconds %||% 5
        )

        if (!identical(fetched$status_code, 200L) || is.null(fetched$parsed$fixtureId)) {
            return(tibble::tibble(
                bookmaker = bookmaker,
                status_code = fetched$status_code,
                provider_error = extract_oddspapi_error_message(fetched$parsed),
                prob_teamA = NA_real_,
                spread_teamA = NA_real_,
                closing_time_utc = as.POSIXct(NA, tz = "UTC"),
                moneyline_available = FALSE,
                spread_available = FALSE
            ))
        }

        closing_line <- derive_oddspapi_bookmaker_closing_line(
            fetched$parsed,
            markets_catalog = markets_catalog,
            kickoff_ms = as.numeric(as.POSIXct(fixture_row$start_time_utc[[1]], tz = "UTC")) * 1000,
            participant_1 = fixture_row$participant1[[1]] %||% NA_character_,
            participant_2 = fixture_row$participant2[[1]] %||% NA_character_
        )
        team_a <- canonicalize_team_name(result_row$teamA[[1]])
        participant1 <- closing_line$participant1[[1]] %||% ""

        prob_teamA <- if (identical(team_a, participant1)) {
            closing_line$prob_participant1[[1]]
        } else {
            closing_line$prob_participant2[[1]]
        }
        spread_teamA <- if (identical(team_a, participant1)) {
            closing_line$spread_participant1[[1]]
        } else {
            closing_line$spread_participant2[[1]]
        }

        tibble::tibble(
            bookmaker = bookmaker,
            status_code = fetched$status_code,
            provider_error = NA_character_,
            prob_teamA = prob_teamA,
            spread_teamA = spread_teamA,
            closing_time_utc = closing_line$closing_time_utc[[1]],
            moneyline_available = isTRUE(closing_line$moneyline_available[[1]]),
            spread_available = isTRUE(closing_line$spread_available[[1]])
        )
    })

    usable_probs <- bookmaker_rows %>%
        dplyr::filter(moneyline_available, is.finite(prob_teamA))
    usable_spreads <- bookmaker_rows %>%
        dplyr::filter(spread_available, is.finite(spread_teamA))

    if (nrow(usable_probs) == 0L) {
        closing_row <- empty_oddspapi_closing_line_row(
            result_row = result_row,
            reason = "historical_odds_unavailable",
            fixture_id = fixture_id,
            recovery_attempt = recovery_attempt
        ) %>%
            dplyr::mutate(
                commence_time_utc = fixture_row$start_time_utc[[1]],
                provider_gap_reason = if (all(bookmaker_rows$status_code == 404, na.rm = TRUE)) {
                    "provider_not_found"
                } else if (any(bookmaker_rows$status_code >= 400, na.rm = TRUE)) {
                    "provider_error"
                } else {
                    "missing_moneyline_history"
                }
            )
    } else {
        consensus_prob <- stats::median(usable_probs$prob_teamA, na.rm = TRUE)
        consensus_spread <- if (nrow(usable_spreads) > 0L) stats::median(usable_spreads$spread_teamA, na.rm = TRUE) else NA_real_
        closing_time_utc <- suppressWarnings(max(c(usable_probs$closing_time_utc, usable_spreads$closing_time_utc), na.rm = TRUE))
        if (!is.finite(closing_time_utc) || is.na(closing_time_utc)) {
            closing_time_utc <- as.POSIXct(NA, tz = "UTC")
        }

        closing_row <- tibble::tibble(
            Year = as.character(result_row$Year[[1]]),
            region = result_row$region[[1]],
            round = result_row$round[[1]],
            game_index = suppressWarnings(as.integer(result_row$game_index[[1]])),
            teamA = canonicalize_team_name(result_row$teamA[[1]]),
            teamB = canonicalize_team_name(result_row$teamB[[1]]),
            winner = canonicalize_team_name(result_row$winner[[1]]),
            closing_snapshot_time_utc = closing_time_utc,
            commence_time_utc = fixture_row$start_time_utc[[1]],
            implied_prob_teamA = consensus_prob,
            implied_prob_teamB = 1 - consensus_prob,
            spread_teamA = consensus_spread,
            spread_teamB = if (is.finite(consensus_spread)) -consensus_spread else NA_real_,
            n_bookmakers = as.integer(nrow(usable_probs)),
            bookmakers = paste(sort(unique(usable_probs$bookmaker)), collapse = ","),
            prob_dispersion_a = if (nrow(usable_probs) > 1L) stats::sd(usable_probs$prob_teamA, na.rm = TRUE) else 0,
            spread_dispersion_a = if (nrow(usable_spreads) > 1L) stats::sd(usable_spreads$spread_teamA, na.rm = TRUE) else if (nrow(usable_spreads) == 1L) 0 else NA_real_,
            closing_before_commence = if (!is.na(closing_time_utc) && !is.na(fixture_row$start_time_utc[[1]])) closing_time_utc <= fixture_row$start_time_utc[[1]] else NA,
            provider_fixture_id = fixture_id,
            provider_gap_reason = if (nrow(usable_spreads) == 0L) "missing_spread_history" else NA_character_,
            provider_recovery_attempt = recovery_attempt
        )
    }

    summary_row <- tibble::tibble(
        Year = as.character(result_row$Year[[1]]),
        round = result_row$round[[1]],
        region = result_row$region[[1]],
        game_index = suppressWarnings(as.integer(result_row$game_index[[1]])),
        teamA = canonicalize_team_name(result_row$teamA[[1]]),
        teamB = canonicalize_team_name(result_row$teamB[[1]]),
        provider_fixture_id = fixture_id,
        recovered_moneyline = is.finite(closing_row$implied_prob_teamA[[1]]),
        recovered_spread = is.finite(closing_row$spread_teamA[[1]]),
        n_bookmakers = suppressWarnings(as.integer(closing_row$n_bookmakers[[1]])),
        bookmakers = closing_row$bookmakers[[1]],
        provider_gap_reason = closing_row$provider_gap_reason[[1]],
        recovery_attempt = recovery_attempt
    )

    list(
        closing_row = closing_row,
        summary_row = summary_row,
        bookmaker_rows = bookmaker_rows
    )
}

#' Run an authenticated OddsPapi preflight
#'
#' @param config Project configuration list.
#' @param output_path Optional destination path for the JSON report.
#'
#' @return A list describing bookmaker, tournament, and season access.
#' @export
run_oddspapi_preflight <- function(config,
                                   output_path = NULL,
                                   bracket_year = NULL,
                                   eligible_years = NULL,
                                   skipped_years = NULL) {
    historical <- resolve_oddspapi_historical_config(config)
    api_key <- Sys.getenv(historical$api_key_env %||% "ODDS_PAPI_API_KEY")
    history_dir <- path.expand((config$betting %||% list())$history_dir %||% default_runtime_history_root())
    output_path <- output_path %||% file.path(history_dir, "oddspapi_preflight.json")

    account <- oddspapi_v4_get("/account", api_key = api_key, timeout_seconds = historical$timeout_seconds, allow_http_error = TRUE)
    bookmakers <- oddspapi_v4_get(
        "/bookmakers",
        query = list(bookmakers = paste(sort(oddspapi_verified_bookmakers()$slug), collapse = ",")),
        api_key = api_key,
        timeout_seconds = historical$timeout_seconds,
        allow_http_error = TRUE
    )
    tournaments <- oddspapi_v4_get(
        "/tournaments",
        query = list(sportId = historical$sport_id),
        api_key = api_key,
        timeout_seconds = historical$timeout_seconds,
        allow_http_error = TRUE
    )
    seasons <- oddspapi_v4_get(
        "/seasons",
        query = list(tournamentId = historical$tournament_id),
        api_key = api_key,
        timeout_seconds = historical$timeout_seconds,
        allow_http_error = TRUE
    )

    accessible_slugs <- character()
    if (is.list(account$parsed) && length(account$parsed$bookmakers %||% list()) > 0L) {
        accessible_slugs <- names(account$parsed$bookmakers)
    } else if (length(bookmakers$parsed %||% list()) > 0L) {
        accessible_slugs <- vapply(bookmakers$parsed, function(row) safe_character_scalar(row$slug %||% "", ""), character(1))
    }

    report <- list(
        provider = historical$provider,
        api_key_env = historical$api_key_env,
        sport_id = historical$sport_id,
        tournament_id = historical$tournament_id,
        configured_target_years = as.integer(historical$target_years),
        archive_start_year = historical$archive_start_year,
        bracket_year = suppressWarnings(as.integer(bracket_year %||% NA_integer_)),
        eligible_import_years = as.integer(eligible_years %||% integer(0)),
        skipped_import_years = if (nrow(skipped_years %||% tibble::tibble()) > 0L) {
            split(as.data.frame(skipped_years), seq_len(nrow(skipped_years)))
        } else {
            list()
        },
        documented_historical_retention_months = 3L,
        historical_retention_note = "OddsPapi v4 docs say data older than 3 months is not returned.",
        request_limit = suppressWarnings(as.integer(account$parsed$request_limit %||% NA_integer_)),
        accessible_bookmakers = unname(sort(unique(accessible_slugs))),
        configured_bookmaker_sets = historical$bookmaker_sets,
        account_status = account$status_code,
        seasons_status = seasons$status_code,
        tournament_status = tournaments$status_code,
        seasons = purrr::map(seasons$parsed %||% list(), function(row) {
            list(
                seasonId = row$seasonId %||% NULL,
                seasonName = row$seasonName %||% NULL
            )
        }),
        tournament_candidates = purrr::map(tournaments$parsed %||% list(), function(row) {
            list(
                tournamentId = row$tournamentId %||% NULL,
                tournamentName = row$tournamentName %||% NULL,
                slug = row$slug %||% NULL
            )
        }),
        generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
    )

    write_json_artifact(report, output_path)
    report
}

#' Import historical closing lines from OddsPapi v4
#'
#' @param config Project configuration list.
#' @param years Optional integer vector overriding configured target years.
#' @param preflight Whether to write a provider preflight before importing.
#'
#' @return A list containing per-year closing lines and summary reports.
#' @export
import_historical_oddspapi_closing_lines <- function(config, years = NULL, preflight = TRUE) {
    historical <- resolve_oddspapi_historical_config(config)
    api_key <- Sys.getenv(historical$api_key_env %||% "ODDS_PAPI_API_KEY")
    history_dir <- path.expand((config$betting %||% list())$history_dir %||% default_runtime_history_root())

    team_features <- normalize_team_features(read_table_file(config$data$team_features_path))
    game_results <- normalize_game_results(read_table_file(config$data$game_results_path))
    validate_team_features(team_features)
    validate_game_results(game_results)
    assert_canonical_data_quality(team_features, game_results)
    bracket_year <- get_bracket_year(team_features)

    year_resolution <- resolve_oddspapi_import_years(
        historical = historical,
        bracket_year = bracket_year,
        requested_years = years,
        reference_date = Sys.Date()
    )
    target_years <- as.integer(year_resolution$eligible_years %||% integer(0))
    skipped_years <- year_resolution$skipped_years %||% tibble::tibble(year = integer(), reason = character())

    if (isTRUE(preflight)) {
        run_oddspapi_preflight(
            config,
            output_path = file.path(history_dir, "oddspapi_preflight.json"),
            bracket_year = bracket_year,
            eligible_years = target_years,
            skipped_years = skipped_years
        )
    }

    if (nrow(skipped_years) > 0L) {
        purrr::pwalk(skipped_years, function(year, reason) {
            logger::log_warn("Skipping OddsPapi import for {year}: {reason}")
        })
    }

    if (length(target_years) == 0L) {
        logger::log_warn("No tournament seasons are eligible for OddsPapi v4 import on {Sys.Date()}.")
        empty_result <- list()
        attr(empty_result, "eligible_years") <- target_years
        attr(empty_result, "skipped_years") <- skipped_years
        attr(empty_result, "bracket_year") <- bracket_year
        return(empty_result)
    }

    markets_response <- oddspapi_v4_get(
        "/markets",
        query = list(sportId = historical$sport_id),
        api_key = api_key,
        timeout_seconds = historical$timeout_seconds,
        allow_http_error = TRUE
    )
    markets_catalog <- flatten_oddspapi_markets_catalog(markets_response$parsed %||% list()) %>%
        dplyr::filter(is.na(sport_id) | sport_id == historical$sport_id)

    results <- list()
    for (year in target_years) {
        results_year <- game_results %>%
            dplyr::filter(Year == as.character(year)) %>%
            dplyr::mutate(
                teamA = canonicalize_team_name(teamA),
                teamB = canonicalize_team_name(teamB),
                winner = canonicalize_team_name(winner)
            )
        paths <- build_odds_history_paths(year, history_dir = history_dir)
        ensure_odds_history_dirs(paths)

        attempts <- build_oddspapi_recovery_attempts(year, historical)
        matched_table <- NULL
        fixture_table <- tibble::tibble()
        attempt_reports <- vector("list", length(attempts))

        for (index in seq_along(attempts)) {
            attempt <- attempts[[index]]
            attempt_result <- fetch_oddspapi_fixtures_for_attempt(historical, api_key, attempt)
            reconciled <- reconcile_oddspapi_fixtures_to_results(attempt_result$parsed, results_year)
            fixture_table <- dplyr::bind_rows(fixture_table, reconciled$fixture_table) %>%
                dplyr::arrange(dplyr::desc(start_time_utc)) %>%
                dplyr::distinct(fixture_id, .keep_all = TRUE)

            candidate_match <- reconciled$matched %>%
                dplyr::mutate(
                    provider_recovery_attempt = attempt$attempt_id,
                    provider_bookmakers = paste(attempt$bookmakers, collapse = ",")
                )

            if (is.null(matched_table)) {
                matched_table <- candidate_match
            } else {
                matched_table <- matched_table %>%
                    dplyr::rows_update(
                        candidate_match %>% dplyr::filter(!is.na(fixture_id) & nzchar(fixture_id)),
                        by = c("Year", "region", "round", "game_index", "teamA", "teamB", "winner", "matchup_key"),
                        unmatched = "ignore"
                    )
            }

            attempt_reports[[index]] <- list(
                attempt_id = attempt$attempt_id,
                from = attempt$from,
                to = attempt$to,
                bookmakers = attempt$bookmakers,
                status_code = attempt_result$status_code,
                fixture_count = nrow(reconciled$fixture_table),
                matched_games = sum(!is.na(candidate_match$fixture_id) & nzchar(candidate_match$fixture_id)),
                unmatched_games = sum(is.na(candidate_match$fixture_id) | !nzchar(candidate_match$fixture_id)),
                error_message = attempt_result$error_message
            )

            if (sum(!is.na(candidate_match$fixture_id) & nzchar(candidate_match$fixture_id)) >= nrow(results_year)) {
                break
            }
        }

        if (is.null(matched_table)) {
            matched_table <- results_year %>%
                dplyr::mutate(
                    matchup_key = purrr::map2_chr(teamA, teamB, build_matchup_key),
                    fixture_id = NA_character_,
                    start_time_utc = as.POSIXct(NA, tz = "UTC"),
                    provider_recovery_attempt = NA_integer_,
                    provider_bookmakers = NA_character_
                )
        }

        closing_rows <- vector("list", nrow(matched_table))
        summary_rows <- vector("list", nrow(matched_table))
        for (index in seq_len(nrow(matched_table))) {
            result_row <- matched_table[index, , drop = FALSE]
            if (is.na(result_row$fixture_id[[1]]) || !nzchar(result_row$fixture_id[[1]])) {
                closing_rows[[index]] <- empty_oddspapi_closing_line_row(
                    result_row = result_row,
                    reason = "fixture_not_found",
                    recovery_attempt = result_row$provider_recovery_attempt[[1]]
                )
                summary_rows[[index]] <- tibble::tibble(
                    Year = as.character(result_row$Year[[1]]),
                    round = result_row$round[[1]],
                    region = result_row$region[[1]],
                    game_index = suppressWarnings(as.integer(result_row$game_index[[1]])),
                    teamA = result_row$teamA[[1]],
                    teamB = result_row$teamB[[1]],
                    provider_fixture_id = NA_character_,
                    recovered_moneyline = FALSE,
                    recovered_spread = FALSE,
                    n_bookmakers = NA_integer_,
                    bookmakers = NA_character_,
                    provider_gap_reason = "fixture_not_found",
                    recovery_attempt = result_row$provider_recovery_attempt[[1]]
                )
                next
            }

            fixture_row <- matched_table[index, , drop = FALSE] %>%
                dplyr::transmute(
                    fixture_id = fixture_id,
                    start_time_utc = start_time_utc,
                    participant1 = participant1,
                    participant2 = participant2
                )
            fixture_books <- strsplit(result_row$provider_bookmakers[[1]] %||% "", ",", fixed = TRUE)[[1]]
            fixture_books <- trimws(fixture_books[nzchar(trimws(fixture_books))])
            derived <- derive_oddspapi_fixture_closing_line(
                result_row = result_row,
                fixture_row = fixture_row,
                historical = historical,
                api_key = api_key,
                paths = paths,
                markets_catalog = markets_catalog,
                recovery_attempt = result_row$provider_recovery_attempt[[1]],
                bookmakers = fixture_books
            )
            closing_rows[[index]] <- derived$closing_row
            summary_rows[[index]] <- derived$summary_row
        }

        closing_lines <- dplyr::bind_rows(closing_rows)
        import_summary <- dplyr::bind_rows(summary_rows)
        utils::write.csv(closing_lines, paths$closing_lines, row.names = FALSE)
        utils::write.csv(import_summary, paths$import_summary, row.names = FALSE)

        report <- list(
            year = year,
            target_games = nrow(results_year),
            matched_games = sum(!is.na(matched_table$fixture_id) & nzchar(matched_table$fixture_id)),
            recovered_moneyline_games = sum(import_summary$recovered_moneyline, na.rm = TRUE),
            recovered_spread_games = sum(import_summary$recovered_spread, na.rm = TRUE),
            unresolved_games = sum(!import_summary$recovered_moneyline, na.rm = TRUE),
            documented_historical_retention_months = 3L,
            requested_year_outside_documented_retention = as.Date(sprintf("%d-04-15", year)) < (Sys.Date() - 92),
            provider_gap_reasons = as.list(sort(table(import_summary$provider_gap_reason), decreasing = TRUE)),
            recovery_attempts = attempt_reports,
            generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
        )
        write_json_artifact(report, paths$import_report)

        results[[as.character(year)]] <- list(
            year = year,
            paths = paths,
            closing_lines = closing_lines,
            import_summary = import_summary,
            report = report
        )
    }

    attr(results, "eligible_years") <- target_years
    attr(results, "skipped_years") <- skipped_years
    attr(results, "bracket_year") <- bracket_year
    results
}

#' Score a candidate list against actual results
#'
#' @param candidates Candidate brackets from [generate_bracket_candidates()].
#' @param actual_results Completed current-year tournament results.
#' @param model_label A model label written into the output rows.
#'
#' @return A tibble of candidate scores.
#' @keywords internal
score_candidate_list_against_results <- function(candidates, actual_results, model_label) {
    purrr::map_dfr(candidates %||% list(), function(candidate) {
        scored <- score_bracket_against_results(candidate$matchups, actual_results)
        tibble::tibble(
            model = model_label,
            candidate_id = candidate$candidate_id %||% NA_integer_,
            candidate_type = candidate$type %||% NA_character_,
            champion = candidate$champion %||% NA_character_,
            correct_picks = scored$summary$correct_picks[[1]],
            total_games = scored$summary$total_games[[1]],
            bracket_score = scored$summary$bracket_score[[1]],
            champion_correct = isTRUE((candidate$champion %||% NA_character_) == unique(actual_results$winner[actual_results$round == "Championship"])[1])
        )
    })
}

#' Classify the betting-feature impact direction
#'
#' @param baseline_score Baseline bracket score.
#' @param betting_score Betting-feature bracket score.
#'
#' @return A scalar character label.
#' @keywords internal
classify_betting_impact <- function(baseline_score, betting_score) {
    baseline_score <- suppressWarnings(as.numeric(baseline_score))
    betting_score <- suppressWarnings(as.numeric(betting_score))
    if (!is.finite(baseline_score) || !is.finite(betting_score)) {
        return("unknown")
    }
    if (betting_score > baseline_score) {
        "improved"
    } else if (betting_score < baseline_score) {
        "worsened"
    } else {
        "matched"
    }
}

#' Evaluate historical betting features on 2026 bracket selection
#'
#' @param config Project configuration list.
#' @param draws Optional prediction draw override.
#' @param n_candidates Number of candidates per model.
#' @param n_simulations Number of stochastic simulations per model.
#'
#' @return A list containing fitted models, candidate scores, and report paths.
#' @export
evaluate_historical_betting_bracket_impact <- function(config,
                                                       draws = NULL,
                                                       n_candidates = 2L,
                                                       n_simulations = 50L) {
    loaded <- load_tournament_data(config, include_betting_history = TRUE)
    output_dir <- config$output$path %||% default_runtime_output_root()
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

    bracket_year <- loaded$bracket_year
    actual_results <- loaded$game_results %>%
        dplyr::filter(
            Year == bracket_year,
            !is.na(winner),
            !is.na(teamA_score),
            !is.na(teamB_score)
        )

    summary_path <- file.path(output_dir, sprintf("betting_bracket_impact_%s_summary.csv", bracket_year))
    differences_path <- file.path(output_dir, sprintf("betting_bracket_impact_%s_differences.csv", bracket_year))
    report_path <- file.path(output_dir, sprintf("betting_bracket_impact_%s_report.md", bracket_year))
    training_years <- unique(as.character(loaded$historical_matchups$Year %||% character(0)))
    training_betting_features <- loaded$historical_betting_features %||% tibble::tibble()
    if (nrow(training_betting_features) > 0L && "Year" %in% names(training_betting_features)) {
        training_betting_features <- training_betting_features %>%
            dplyr::filter(Year %in% training_years)
    }

    if (nrow(loaded$historical_betting_features %||% tibble::tibble()) == 0L) {
        empty_summary <- tibble::tibble(
            model = c("baseline_no_betting", "historical_betting_features"),
            candidate_id = NA_integer_,
            candidate_type = NA_character_,
            champion = NA_character_,
            correct_picks = NA_integer_,
            total_games = nrow(actual_results),
            bracket_score = NA_real_,
            champion_correct = NA,
            outcome_vs_baseline = "unknown",
            note = "No historical closing_lines.csv archive was available."
        )
        utils::write.csv(empty_summary, summary_path, row.names = FALSE)
        utils::write.csv(tibble::tibble(), differences_path, row.names = FALSE)
        writeLines(c(
            sprintf("# Betting impact for %s", bracket_year),
            "",
            "No historical betting archive was available, so the bracket-impact comparison could not be run."
        ), report_path, useBytes = TRUE)
        return(list(
            bracket_year = bracket_year,
            summary = empty_summary,
            differences = tibble::tibble(),
            output_paths = list(summary = summary_path, differences = differences_path, report = report_path)
        ))
    }

    if (nrow(training_betting_features) == 0L) {
        empty_summary <- tibble::tibble(
            model = c("baseline_no_betting", "historical_betting_features"),
            candidate_id = NA_integer_,
            candidate_type = NA_character_,
            champion = NA_character_,
            correct_picks = NA_integer_,
            total_games = nrow(actual_results),
            bracket_score = NA_real_,
            champion_correct = NA,
            outcome_vs_baseline = "unknown",
            note = sprintf("Historical closing lines were available only outside the training seasons for %s.", bracket_year)
        )
        utils::write.csv(empty_summary, summary_path, row.names = FALSE)
        utils::write.csv(tibble::tibble(), differences_path, row.names = FALSE)
        writeLines(c(
            sprintf("# Betting impact for %s", bracket_year),
            "",
            sprintf("Historical closing lines existed, but none overlapped the model training seasons before %s, so the bracket-impact comparison could not estimate a real betting-signal effect.", bracket_year)
        ), report_path, useBytes = TRUE)
        return(list(
            bracket_year = bracket_year,
            summary = empty_summary,
            differences = tibble::tibble(),
            output_paths = list(summary = summary_path, differences = differences_path, report = report_path)
        ))
    }

    engine <- config$model$engine %||% "stan_glm"
    bart_config <- config$model$bart %||% list()
    draws_budget <- as.integer(draws %||% if (identical(engine, "bart")) bart_config$n_post %||% 1000L else config$model$n_draws %||% 1000L)
    interaction_terms <- as.character(unlist(config$model$interaction_terms %||% character(0)))
    if (length(interaction_terms) == 0L) {
        interaction_terms <- NULL
    }
    core_predictors <- core_matchup_predictor_columns(config$model$required_predictors)
    betting_predictors <- unique(c(core_predictors, betting_matchup_feature_columns()))
    model_cache_dir <- config$output$model_cache_path %||% file.path(output_dir, "model_cache")
    use_model_cache <- isTRUE(config$output$use_model_cache %||% TRUE)

    baseline_model <- fit_tournament_model(
        historical_matchups = loaded$historical_matchups,
        predictor_columns = core_predictors,
        engine = engine,
        bart_config = bart_config,
        random_seed = config$model$random_seed,
        cache_dir = model_cache_dir,
        use_cache = use_model_cache,
        interaction_terms = interaction_terms,
        prior_type = config$model$prior_type %||% "normal"
    )
    enhanced_model <- fit_tournament_model(
        historical_matchups = loaded$historical_matchups,
        predictor_columns = betting_predictors,
        engine = engine,
        bart_config = bart_config,
        random_seed = config$model$random_seed,
        cache_dir = model_cache_dir,
        use_cache = use_model_cache,
        interaction_terms = interaction_terms,
        prior_type = config$model$prior_type %||% "normal"
    )

    empty_context <- list(
        current_lines_matchups = tibble::tibble(),
        current_betting_features = tibble::tibble(),
        historical_betting_features = loaded$historical_betting_features,
        source_label = "Historical closing lines only",
        used_api_call = FALSE,
        latest_lines_matchups_path = NULL,
        snapshot_path = NULL
    )
    baseline_model$betting_feature_context <- empty_context
    enhanced_model$betting_feature_context <- empty_context

    baseline_candidates <- generate_bracket_candidates(
        all_teams = loaded$current_teams,
        model_results = baseline_model,
        draws = draws_budget,
        actual_play_in_results = loaded$current_play_in_results,
        n_candidates = n_candidates,
        n_simulations = n_simulations,
        random_seed = config$model$random_seed
    )
    enhanced_candidates <- generate_bracket_candidates(
        all_teams = loaded$current_teams,
        model_results = enhanced_model,
        draws = draws_budget,
        actual_play_in_results = loaded$current_play_in_results,
        n_candidates = n_candidates,
        n_simulations = n_simulations,
        random_seed = config$model$random_seed
    )

    baseline_scores <- score_candidate_list_against_results(baseline_candidates, actual_results, "baseline_no_betting")
    enhanced_scores <- score_candidate_list_against_results(enhanced_candidates, actual_results, "historical_betting_features")
    summary_tbl <- dplyr::bind_rows(baseline_scores, enhanced_scores)

    baseline_best <- baseline_scores %>% dplyr::arrange(dplyr::desc(bracket_score), dplyr::desc(correct_picks)) %>% dplyr::slice(1)
    enhanced_best <- enhanced_scores %>% dplyr::arrange(dplyr::desc(bracket_score), dplyr::desc(correct_picks)) %>% dplyr::slice(1)
    outcome <- classify_betting_impact(baseline_best$bracket_score[[1]], enhanced_best$bracket_score[[1]])

    summary_tbl <- summary_tbl %>%
        dplyr::mutate(
            outcome_vs_baseline = ifelse(model == "baseline_no_betting", "baseline", outcome)
        )

    deterministic_diff <- compare_candidate_matchups(
        baseline_candidates[[1]]$matchups,
        enhanced_candidates[[1]]$matchups
    ) %>%
        dplyr::mutate(
            region = dplyr::if_else(
                (is.na(region) | !nzchar(as.character(region))) & round %in% c("Final Four", "Championship"),
                "National",
                as.character(region)
            )
        ) %>%
        dplyr::filter(candidate_diff_flag) %>%
        dplyr::left_join(
            actual_results %>%
                dplyr::mutate(
                    region = dplyr::if_else(
                        (is.na(region) | !nzchar(as.character(region))) & round %in% c("Final Four", "Championship"),
                        "National",
                        as.character(region)
                    )
                ) %>%
                dplyr::select(region, round, matchup_number = game_index, actual_winner = winner),
            by = c("region", "round", "matchup_number")
        ) %>%
        dplyr::mutate(
            baseline_pick = candidate_1_pick,
            betting_pick = candidate_2_pick,
            baseline_correct = baseline_pick == actual_winner,
            betting_correct = betting_pick == actual_winner
        ) %>%
        dplyr::select(
            region,
            round,
            matchup_number,
            candidate_1_matchup,
            baseline_pick,
            betting_pick,
            actual_winner,
            baseline_correct,
            betting_correct,
            confidence_tier,
            upset_leverage
        )

    utils::write.csv(summary_tbl, summary_path, row.names = FALSE)
    utils::write.csv(deterministic_diff, differences_path, row.names = FALSE)

    report_lines <- c(
        sprintf("# Betting impact for %s", bracket_year),
        "",
        sprintf("- Baseline best bracket score: %s", baseline_best$bracket_score[[1]] %||% NA),
        sprintf("- Betting-feature best bracket score: %s", enhanced_best$bracket_score[[1]] %||% NA),
        sprintf("- Outcome vs baseline: %s", outcome),
        sprintf("- Baseline champion: %s", baseline_best$champion[[1]] %||% "unknown"),
        sprintf("- Betting-feature champion: %s", enhanced_best$champion[[1]] %||% "unknown"),
        sprintf("- Deterministic bracket differences: %s", nrow(deterministic_diff))
    )
    if (nrow(deterministic_diff) > 0L) {
        example_rows <- deterministic_diff %>% dplyr::slice_head(n = min(5L, nrow(deterministic_diff)))
        report_lines <- c(
            report_lines,
            "",
            "## Changed slots",
            purrr::pmap_chr(example_rows, function(region, round, matchup_number, candidate_1_matchup, baseline_pick, betting_pick, actual_winner, baseline_correct, betting_correct, confidence_tier, upset_leverage) {
                sprintf(
                    "- %s %s #%s: baseline picked %s, betting picked %s, actual winner %s",
                    round,
                    region,
                    matchup_number,
                    baseline_pick,
                    betting_pick,
                    actual_winner %||% "unknown"
                )
            })
        )
    }
    writeLines(report_lines, report_path, useBytes = TRUE)

    list(
        bracket_year = bracket_year,
        baseline_candidates = baseline_candidates,
        enhanced_candidates = enhanced_candidates,
        summary = summary_tbl,
        differences = deterministic_diff,
        output_paths = list(summary = summary_path, differences = differences_path, report = report_path)
    )
}

#' Return the standard deprecation message for the retired live collector path
#'
#' @return A scalar character message.
#' @keywords internal
deprecated_live_betting_workflow_message <- function() {
    paste(
        "The live Odds API collector workflow is deprecated.",
        "Use `Rscript scripts/import_historical_odds_papi.R` to backfill historical closing lines",
        "and `Rscript scripts/evaluate_historical_betting_impact.R` to measure bracket impact."
    )
}
