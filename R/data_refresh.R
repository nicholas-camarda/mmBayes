library(dplyr)
library(httr)
library(logger)
library(purrr)
library(rvest)
library(stringr)
library(writexl)

#' Return an empty refresh-issues table
#'
#' @return A zero-row tibble with the canonical refresh-issue schema.
#' @keywords internal
empty_refresh_issues_table <- function() {
    tibble::tibble(
        step = character(),
        source = character(),
        severity = character(),
        message = character()
    )
}

#' Return an empty canonical team-feature table
#'
#' @return A zero-row tibble with the canonical team-feature schema.
#' @keywords internal
empty_team_feature_table <- function() {
    tibble::tibble(
        Year = character(),
        Team = character(),
        Seed = integer(),
        Region = character(),
        Conf = character(),
        ratings_cutoff = character(),
        Barthag = numeric(),
        AdjOE = numeric(),
        AdjDE = numeric(),
        WAB = numeric(),
        TOR = numeric(),
        TORD = numeric(),
        ORB = numeric(),
        DRB = numeric(),
        `3P%` = numeric(),
        `3P%D` = numeric(),
        `Adj T.` = numeric(),
        barthag_logit = numeric()
    )
}

#' Append a structured refresh issue
#'
#' @param issues Existing refresh issues tibble.
#' @param step Short internal step identifier.
#' @param source Human-readable source or subsystem label.
#' @param severity Issue severity label.
#' @param message Human-readable issue message.
#'
#' @return The refresh issues tibble with the new issue appended.
#' @keywords internal
append_refresh_issue <- function(issues,
                                 step,
                                 source,
                                 severity = "warning",
                                 message) {
    dplyr::bind_rows(
        issues %||% empty_refresh_issues_table(),
        tibble::tibble(
            step = as.character(step %||% ""),
            source = as.character(source %||% ""),
            severity = as.character(severity %||% ""),
            message = as.character(message %||% "")
        )
    )
}

#' Summarize refresh issues for operator-facing reporting
#'
#' @param issues Refresh issues tibble.
#'
#' @return A grouped summary tibble with one row per unique issue message.
#' @keywords internal
summarize_refresh_issues <- function(issues) {
    issues <- issues %||% empty_refresh_issues_table()
    if (nrow(issues) == 0) {
        return(dplyr::mutate(issues, count = integer()))
    }

    issues %>%
        dplyr::count(step, source, severity, message, name = "count", sort = TRUE)
}

#' Count rows by tournament year
#'
#' @param data A data frame with a `Year` column.
#' @param count_name Output count column name.
#'
#' @return A tibble of per-year row counts.
#' @keywords internal
count_rows_by_year <- function(data, count_name = "rows") {
    if (is.null(data) || nrow(data) == 0 || !"Year" %in% names(data)) {
        return(tibble::tibble(Year = character(), !!count_name := integer()))
    }

    data %>%
        dplyr::transmute(Year = as.character(Year)) %>%
        dplyr::count(Year, name = count_name) %>%
        dplyr::arrange(suppressWarnings(as.integer(Year)), Year)
}

#' Read per-year row counts from an existing canonical file
#'
#' @param path Path to a canonical table file.
#' @param count_name Output count column name.
#'
#' @return A tibble of per-year row counts.
#' @keywords internal
read_year_counts_from_file <- function(path, count_name = "rows") {
    if (is.null(path) || !nzchar(path) || !file.exists(path)) {
        return(tibble::tibble(Year = character(), !!count_name := integer()))
    }

    count_rows_by_year(read_table_file(path), count_name = count_name)
}

#' Format a per-year count table for logging
#'
#' @param counts A tibble from [count_rows_by_year()].
#' @param count_name Name of the count column.
#'
#' @return A compact character scalar.
#' @keywords internal
format_year_count_summary <- function(counts, count_name = names(counts)[2] %||% "rows") {
    counts <- counts %||% tibble::tibble()
    if (nrow(counts) == 0) {
        return("none")
    }

    count_name <- count_name %||% "rows"
    paste(sprintf("%s=%s", counts$Year, counts[[count_name]]), collapse = ", ")
}

#' Format a vector of year labels for operator-facing output
#'
#' @param years A character or integer vector of years.
#'
#' @return A compact character scalar.
#' @keywords internal
format_year_labels <- function(years) {
    years <- unique(as.character(years %||% character()))
    years <- years[nzchar(years)]
    if (length(years) == 0) {
        return("none")
    }

    paste(years, collapse = ", ")
}

#' Derive the configured completed historical years for a bracket year
#'
#' @param bracket_year The active bracket year.
#' @param history_window Number of completed historical tournaments to retain.
#' @param min_year Minimum supported tournament year.
#'
#' @return An integer vector of completed historical years.
#' @keywords internal
configured_completed_historical_years <- function(bracket_year, history_window = 8L, min_year = 2008L) {
    bracket_year <- suppressWarnings(as.integer(bracket_year))
    history_window <- resolve_history_window(history_window)
    min_year <- suppressWarnings(as.integer(min_year))

    if (is.na(bracket_year) || is.na(min_year)) {
        stop_with_message("Historical refresh window configuration is invalid")
    }

    eligible <- seq.int(min_year, bracket_year - 1L)
    eligible <- eligible[eligible != 2020L]
    if (length(eligible) == 0) {
        return(integer())
    }

    utils::tail(eligible, history_window)
}

#' Assess whether an existing historical year is safe to reuse
#'
#' @param existing_team_features Existing canonical team features.
#' @param existing_game_results Existing canonical game results.
#' @param year Historical year to assess.
#'
#' @return A one-row tibble describing reuse eligibility.
#' @keywords internal
assess_historical_year_cache <- function(existing_team_features, existing_game_results, year) {
    year <- as.character(year)
    year_team_features <- existing_team_features %>%
        dplyr::filter(Year == year)
    year_game_results <- existing_game_results %>%
        dplyr::filter(Year == year)

    issues <- character()

    if (nrow(year_team_features) == 0) {
        issues <- c(issues, "missing team features")
    } else {
        roster_issue <- tryCatch(
            {
                validate_tournament_roster(
                    year_team_features %>%
                        dplyr::transmute(Year, Team, Seed, Region, Conf),
                    require_seed = TRUE
                )
                NULL
            },
            error = function(err) err$message
        )
        if (!is.null(roster_issue)) {
            issues <- c(issues, roster_issue)
        }

        if (nrow(find_ratings_cutoff_issues(year_team_features)) > 0) {
            issues <- c(issues, "ratings not cut off at Selection Sunday")
        }

        missing_feature_rows <- year_team_features %>%
            dplyr::filter(dplyr::if_any(dplyr::all_of(pre_tournament_feature_columns()), is.na))
        if (nrow(missing_feature_rows) > 0) {
            issues <- c(issues, "missing pre-tournament metrics")
        }
    }

    if (nrow(year_game_results) == 0) {
        issues <- c(issues, "missing completed tournament results")
    } else {
        round_counts <- year_game_results %>%
            dplyr::count(round, name = "games")
        expected_counts <- tibble::tibble(
            round = names(expected_completed_round_counts(year)),
            expected_games = unname(expected_completed_round_counts(year))
        )
        round_issues <- expected_counts %>%
            dplyr::left_join(round_counts, by = "round") %>%
            dplyr::mutate(games = dplyr::coalesce(games, 0L)) %>%
            dplyr::filter(games != expected_games)
        if (nrow(round_issues) > 0) {
            issues <- c(issues, "unexpected completed round counts")
        }

        unresolved_teams <- if (nrow(year_team_features) > 0) {
            find_unresolved_result_teams(year_team_features, year_game_results)
        } else {
            tibble::tibble()
        }
        if (nrow(unresolved_teams) > 0) {
            issues <- c(issues, "result teams do not match team features")
        }
    }

    issues <- unique(issues)

    tibble::tibble(
        Year = year,
        reusable = length(issues) == 0,
        pre_team_feature_rows = nrow(year_team_features),
        pre_result_rows = nrow(year_game_results),
        completeness_issue = if (length(issues) == 0) NA_character_ else paste(issues, collapse = "; ")
    )
}

#' Build year-scoped refresh forensics for the current run
#'
#' @param intended_years Historical and current years requested for refresh.
#' @param bracket_year Current bracket year.
#' @param pre_team_counts Pre-refresh team-feature year counts.
#' @param pre_result_counts Pre-refresh result year counts.
#' @param ratings_counts Team-rating year counts.
#' @param roster_counts Conference-assignment year counts.
#' @param feature_counts Post-build team-feature year counts.
#' @param result_counts Post-build result year counts.
#'
#' @return A tibble summarizing year coverage across refresh stages.
#' @keywords internal
build_refresh_forensics <- function(intended_years,
                                    bracket_year,
                                    pre_team_counts,
                                    pre_result_counts,
                                    ratings_counts,
                                    roster_counts,
                                    feature_counts,
                                    result_counts) {
    all_years <- sort(unique(c(
        as.integer(intended_years),
        suppressWarnings(as.integer(pre_team_counts$Year)),
        suppressWarnings(as.integer(pre_result_counts$Year)),
        suppressWarnings(as.integer(ratings_counts$Year)),
        suppressWarnings(as.integer(roster_counts$Year)),
        suppressWarnings(as.integer(feature_counts$Year)),
        suppressWarnings(as.integer(result_counts$Year))
    )))
    all_years <- all_years[!is.na(all_years)]

    if (length(all_years) == 0) {
        return(tibble::tibble())
    }

    year_table <- tibble::tibble(Year = as.character(all_years))
    completed_years <- as.character(intended_years[intended_years < bracket_year])

    year_table %>%
        dplyr::left_join(pre_team_counts, by = "Year") %>%
        dplyr::left_join(pre_result_counts, by = "Year") %>%
        dplyr::left_join(ratings_counts, by = "Year") %>%
        dplyr::left_join(roster_counts, by = "Year") %>%
        dplyr::left_join(feature_counts, by = "Year") %>%
        dplyr::left_join(result_counts, by = "Year") %>%
        dplyr::mutate(
            dplyr::across(
                c(
                    pre_team_feature_rows,
                    pre_result_rows,
                    rating_rows,
                    roster_rows,
                    team_feature_rows,
                    post_result_rows
                ),
                ~ dplyr::coalesce(as.integer(.x), 0L)
            ),
            intended_year = Year %in% as.character(intended_years),
            completed_year = Year %in% completed_years,
            first_missing_stage = dplyr::case_when(
                intended_year & team_feature_rows == 0L & rating_rows == 0L ~ "team_ratings",
                completed_year & team_feature_rows == 0L & roster_rows == 0L ~ "historical_tournament_roster",
                intended_year & !completed_year & team_feature_rows == 0L & roster_rows == 0L ~ "conference_assignments",
                intended_year & team_feature_rows == 0L ~ "team_features",
                completed_year & post_result_rows == 0L ~ "completed_results",
                TRUE ~ NA_character_
            ),
            dropped_existing_team_features = pre_team_feature_rows > 0L & team_feature_rows == 0L,
            dropped_existing_results = pre_result_rows > 0L & post_result_rows == 0L
        ) %>%
        dplyr::arrange(suppressWarnings(as.integer(Year)), Year)
}

#' Summarize refresh forensics for operator-facing output
#'
#' @param forensics A tibble from [build_refresh_forensics()].
#'
#' @return A list of compact year summaries.
#' @keywords internal
summarize_refresh_forensics <- function(forensics) {
    forensics <- forensics %||% tibble::tibble()
    if (nrow(forensics) == 0) {
        return(list())
    }

    list(
        intended_years = forensics %>%
            dplyr::filter(intended_year) %>%
            dplyr::pull(Year),
        pre_team_feature_years = forensics %>%
            dplyr::filter(pre_team_feature_rows > 0L) %>%
            dplyr::pull(Year),
        pre_result_years = forensics %>%
            dplyr::filter(pre_result_rows > 0L) %>%
            dplyr::pull(Year),
        post_team_feature_years = forensics %>%
            dplyr::filter(team_feature_rows > 0L) %>%
            dplyr::pull(Year),
        post_result_years = forensics %>%
            dplyr::filter(post_result_rows > 0L) %>%
            dplyr::pull(Year),
        omitted_years = forensics %>%
            dplyr::filter(intended_year, !is.na(first_missing_stage)) %>%
            dplyr::transmute(label = sprintf("%s (%s)", Year, first_missing_stage)) %>%
            dplyr::pull(label),
        overwritten_team_feature_years = forensics %>%
            dplyr::filter(dropped_existing_team_features) %>%
            dplyr::transmute(label = sprintf("%s (%s)", Year, dplyr::coalesce(first_missing_stage, "unknown"))) %>%
            dplyr::pull(label),
        overwritten_result_years = forensics %>%
            dplyr::filter(dropped_existing_results) %>%
            dplyr::transmute(label = sprintf("%s (%s)", Year, dplyr::coalesce(first_missing_stage, "unknown"))) %>%
            dplyr::pull(label)
    )
}

#' Derive the overall refresh status from structured issues
#'
#' @param issues Refresh issues tibble.
#'
#' @return A scalar status string.
#' @keywords internal
refresh_status_from_issues <- function(issues) {
    issues <- issues %||% empty_refresh_issues_table()
    if (nrow(issues) == 0) {
        return("success")
    }

    "degraded_success"
}

#' Return an operator-facing label for a refresh status code
#'
#' @param status Refresh status code.
#'
#' @return A scalar human-readable status label.
#' @keywords internal
refresh_status_label <- function(status) {
    dplyr::case_when(
        identical(status, "blocked") ~ "Blocked",
        identical(status, "degraded_success") ~ "Degraded success",
        identical(status, "success") ~ "Success",
        TRUE ~ "Failure"
    )
}

#' Format the final CLI summary for `scripts/update_data.R`
#'
#' @param refresh_result Result list returned by [update_tournament_data()].
#' @param log_path Path to the refresh log file.
#'
#' @return A character vector of summary lines.
#' @keywords internal
format_refresh_status_summary <- function(refresh_result, log_path) {
    refresh_result <- refresh_result %||% list()
    status <- refresh_result$status %||% "failure"
    status_label <- refresh_status_label(status)
    warning_summary <- refresh_result$warning_summary %||% tibble::tibble()
    warning_count <- safe_numeric(refresh_result$warning_count %||% 0L, default = 0L)
    log_path <- log_path %||% refresh_result$refresh_log %||% "n/a"
    forensic_summary <- summarize_refresh_forensics(refresh_result$refresh_forensics)
    cache_decisions <- refresh_result$historical_cache_decisions %||% tibble::tibble()
    blocked_years <- refresh_result$blocked_years %||% tibble::tibble()

    headline <- if (identical(status, "blocked")) {
        "Canonical files were not updated because one or more required refresh years could not be produced and no reusable cached copy was available."
    } else if (identical(status, "degraded_success")) {
        "Canonical files were written and quality checks passed, but some optional refresh steps were skipped or downgraded."
    } else if (identical(status, "success")) {
        "Canonical files were written and all refresh steps completed cleanly."
    } else {
        "The refresh did not complete successfully."
    }

    lines <- c(
        sprintf("Refresh status: %s", status_label),
        headline,
        sprintf("- Team features: %s", refresh_result$team_features %||% "n/a"),
        sprintf("- Tournament game results: %s", refresh_result$game_results %||% "n/a"),
        sprintf("- Refresh log: %s", log_path)
    )

    if (length(forensic_summary) > 0) {
        lines <- c(
            lines,
            sprintf("- Intended years: %s", format_year_labels(forensic_summary$intended_years)),
            sprintf("- Pre-refresh team-feature years: %s", format_year_labels(forensic_summary$pre_team_feature_years)),
            sprintf("- Pre-refresh result years: %s", format_year_labels(forensic_summary$pre_result_years)),
            sprintf("- Post-refresh team-feature years: %s", format_year_labels(forensic_summary$post_team_feature_years)),
            sprintf("- Post-refresh result years: %s", format_year_labels(forensic_summary$post_result_years))
        )

        if (length(forensic_summary$omitted_years) > 0) {
            lines <- c(lines, sprintf("- Omitted years: %s", format_year_labels(forensic_summary$omitted_years)))
        }
        if (length(forensic_summary$overwritten_team_feature_years) > 0) {
            lines <- c(lines, sprintf("- Overwritten team-feature years: %s", format_year_labels(forensic_summary$overwritten_team_feature_years)))
        }
        if (length(forensic_summary$overwritten_result_years) > 0) {
            lines <- c(lines, sprintf("- Overwritten result years: %s", format_year_labels(forensic_summary$overwritten_result_years)))
        }
    }

    if (nrow(cache_decisions) > 0) {
        reused_years <- cache_decisions %>%
            dplyr::filter(cache_action == "reuse") %>%
            dplyr::pull(Year)
        refreshed_years <- cache_decisions %>%
            dplyr::filter(cache_action == "refresh") %>%
            dplyr::pull(Year)
        rejected_labels <- cache_decisions %>%
            dplyr::filter(cache_action == "refresh", !is.na(completeness_issue)) %>%
            dplyr::transmute(label = sprintf("%s (%s)", Year, completeness_issue)) %>%
            dplyr::pull(label)

        lines <- c(
            lines,
            sprintf("- Reused historical years: %s", format_year_labels(reused_years)),
            sprintf("- Refreshed historical years: %s", format_year_labels(refreshed_years))
        )
        if (length(rejected_labels) > 0) {
            lines <- c(lines, sprintf("- Refresh-required cache misses: %s", format_year_labels(rejected_labels)))
        }
    }

    if (nrow(blocked_years) > 0) {
        blocked_labels <- blocked_years %>%
            dplyr::transmute(label = sprintf("%s (%s)", Year, dplyr::coalesce(first_missing_stage, "unknown"))) %>%
            dplyr::pull(label)
        lines <- c(lines, sprintf("- Blocked required years: %s", format_year_labels(blocked_labels)))
    }

    if (warning_count > 0L && nrow(warning_summary) > 0) {
        warning_lines <- purrr::map_chr(seq_len(nrow(warning_summary)), function(index) {
            row <- warning_summary[index, , drop = FALSE]
            prefix <- sprintf("- [%s] %s", row$step[[1]], row$message[[1]])
            count_value <- safe_numeric(row$count[[1]], default = 1L)
            if (count_value > 1L) {
                paste0(prefix, sprintf(" (x%s)", as.integer(round(count_value))))
            } else {
                prefix
            }
        })
        lines <- c(
            lines,
            sprintf("- Warnings: %s", as.integer(round(warning_count))),
            "Warning summary:",
            warning_lines
        )
    }

    lines
}

#' Read HTML with a simple verification fallback
#'
#' @param url A URL to request and parse as HTML.
#'
#' @return An `xml_document` parsed from the response body.
#' @keywords internal
read_html_verified <- function(url) {
    parsed_url <- httr::parse_url(url)
    base_url <- sprintf("%s://%s", parsed_url$scheme, parsed_url$hostname)
    handle <- httr::handle(base_url)

    for (attempt in seq_len(3L)) {
        initial_response <- httr::GET(url, handle = handle, httr::user_agent("mmBayes-data-refresh/0.1"))
        httr::stop_for_status(initial_response)

        html_text <- httr::content(initial_response, as = "text", encoding = "UTF-8")
        if (stringr::str_detect(html_text, "Verifying Browser")) {
            verified_response <- httr::POST(
                url,
                body = list(js_test_submitted = "1"),
                encode = "form",
                handle = handle,
                httr::add_headers(Referer = url),
                httr::user_agent("mmBayes-data-refresh/0.1")
            )
            httr::stop_for_status(verified_response)
            html_text <- httr::content(verified_response, as = "text", encoding = "UTF-8")
        }

        if (!stringr::str_detect(html_text, "Verifying Browser")) {
            return(rvest::read_html(html_text))
        }

        Sys.sleep(0.5 * attempt)
    }

    stop_with_message(sprintf("Could not pass browser verification for %s", url))
}

#' Extract the primary HTML table from a page
#'
#' @param page An `xml_document` returned by `rvest::read_html()`.
#' @param url The source URL, used for error reporting.
#' @param expected_header_tokens Optional header tokens used to identify the
#'   intended table.
#'
#' @return A parsed data frame representing the selected HTML table.
#' @keywords internal
extract_primary_table <- function(page, url, expected_header_tokens = character()) {
    table_nodes <- rvest::html_elements(page, "table")
    if (length(table_nodes) == 0) {
        stop_with_message(sprintf("No HTML tables found at %s", url))
    }

    parsed_tables <- purrr::map(table_nodes, ~ rvest::html_table(.x, fill = TRUE))
    if (length(expected_header_tokens) == 0) {
        return(parsed_tables[[1]])
    }

    matching_table <- purrr::detect(parsed_tables, function(tbl) {
        if (nrow(tbl) == 0) {
            return(FALSE)
        }
        first_row_tokens <- as.character(tbl[1, ])
        all(expected_header_tokens %in% first_row_tokens)
    })

    if (is.null(matching_table)) {
        matching_table <- purrr::detect(parsed_tables, function(tbl) {
            all(expected_header_tokens %in% names(tbl))
        })
    }

    if (is.null(matching_table)) {
        stop_with_message(sprintf(
            "Could not find expected table headers (%s) at %s",
            paste(expected_header_tokens, collapse = ", "),
            url
        ))
    }

    matching_table
}

#' Scrape conference assignments for a tournament year
#'
#' @param year The tournament year to scrape.
#'
#' @return A data frame mapping tournament teams to conferences.
#' @keywords internal
scrape_conf_assignments <- function(year) {
    url <- sprintf(
        "https://barttorvik.com/tourneytime.php?year=%s&sort=7&conlimit=All",
        year
    )
    logger::log_info("Scraping conference assignments from {url}")

    retry_delays <- c(5, 15, 30)
    page_result <- NULL
    last_error <- NULL
    for (attempt in seq_len(length(retry_delays) + 1L)) {
        page_result <- tryCatch(
            list(
                page = read_html_verified(url),
                issues = empty_refresh_issues_table()
            ),
            error = function(error) {
                last_error <<- error
                NULL
            }
        )
        if (!is.null(page_result)) {
            break
        }

        is_verification_error <- !is.null(last_error) &&
            grepl("Could not pass browser verification", last_error$message, fixed = TRUE)
        if (!is_verification_error || attempt > length(retry_delays)) {
            break
        }

        delay_seconds <- retry_delays[[attempt]]
        logger::log_warn(
            "Conference assignments for year {year} hit browser verification on attempt {attempt}; retrying in {delay_seconds} seconds"
        )
        Sys.sleep(delay_seconds)
    }

    if (is.null(page_result)) {
        issue_message <- sprintf(
            "Skipping conference assignments for year %s: %s",
            year,
            last_error$message
        )
        logger::log_warn(issue_message)
        issue_table <- empty_refresh_issues_table() %>%
            append_refresh_issue(
                step = "conference_assignments",
                source = "Bart Torvik Tourney Time",
                severity = "warning",
                message = issue_message
            )
        page_result <- list(page = NULL, issues = issue_table)
    }
    if (is.null(page_result$page)) {
        empty_tbl <- tibble::tibble(
            Year = character(),
            Team = character(),
            Seed = integer(),
            Region = character(),
            Conf = character()
        )
        attr(empty_tbl, "refresh_issues") <- page_result$issues %||% empty_refresh_issues_table()
        return(empty_tbl)
    }
    page <- page_result$page

    table <- extract_primary_table(page, url, expected_header_tokens = c("Seed", "Region", "Team", "Conf"))

    dplyr::mutate(
        table,
        Year = as.character(year),
        Region = as.character(Region),
        Seed = as.integer(Seed),
        Team = canonicalize_team_name(stringr::str_trim(Team))
    ) %>%
        dplyr::filter(
            stringr::str_squish(Region) != "",
            stringr::str_squish(Team) != ""
        ) %>%
        dplyr::distinct(Year, Team, Region, .keep_all = TRUE) %>%
        dplyr::select(Year, Team, Seed, Region, Conf)
}

#' Selection Sunday dates by tournament year
#'
#' The day the NCAA men's bracket is announced. Team ratings use only games
#' played on or before this date. 2020 has no tournament. Add the next year's
#' date here before running the Selection Sunday refresh.
#'
#' @return A tibble with `Year` (character) and `selection_sunday` (Date).
#' @keywords internal
selection_sunday_dates <- function() {
    tibble::tribble(
        ~Year, ~selection_sunday,
        "2008", "2008-03-16",
        "2009", "2009-03-15",
        "2010", "2010-03-14",
        "2011", "2011-03-13",
        "2012", "2012-03-11",
        "2013", "2013-03-17",
        "2014", "2014-03-16",
        "2015", "2015-03-15",
        "2016", "2016-03-13",
        "2017", "2017-03-12",
        "2018", "2018-03-11",
        "2019", "2019-03-17",
        "2021", "2021-03-14",
        "2022", "2022-03-13",
        "2023", "2023-03-12",
        "2024", "2024-03-17",
        "2025", "2025-03-16",
        "2026", "2026-03-15"
    ) %>%
        dplyr::mutate(selection_sunday = as.Date(selection_sunday))
}

#' Look up the ratings cutoff date for a tournament year
#'
#' Team ratings are restricted to games played on or before Selection Sunday,
#' the day the bracket is released, so every season's features describe what
#' was known when entries were made.
#'
#' @param year A tournament year.
#'
#' @return A single Date.
#' @keywords internal
ratings_cutoff_date <- function(year) {
    year <- as.character(year)
    dates <- selection_sunday_dates()
    cutoff <- dates$selection_sunday[dates$Year == year]
    if (length(cutoff) != 1L || is.na(cutoff)) {
        stop_with_message(sprintf(
            "No Selection Sunday date for %s; add it to selection_sunday_dates() in R/data_refresh.R before refreshing data",
            year
        ))
    }
    cutoff
}

#' Build the Bart Torvik ratings URL for a season window
#'
#' @param year The tournament year.
#' @param cutoff_date Last date (inclusive) of games used in the ratings.
#'
#' @return A single URL string.
#' @keywords internal
build_bart_ratings_url <- function(year, cutoff_date = ratings_cutoff_date(year)) {
    paste0(
        "https://barttorvik.com/?year=", year,
        "&sort=&hteam=&t2value=&conlimit=All",
        "&state=All&begin=", as.integer(year) - 1L, "1101",
        "&end=", format(as.Date(cutoff_date), "%Y%m%d"),
        "&top=0&revquad=0&quad=5&venue=All&type=All&mingames=0#"
    )
}

#' Scrape Bart Torvik pre-tournament team features (deprecated)
#'
#' Deprecated: Torvik's date-filtered pages sit behind a browser-verification
#' check that this function auto-submits, and robots.txt asks for a 10-second
#' crawl delay. The refresh now uses [compute_season_team_ratings()]. Kept only
#' until Torvik grants automated access or the function is removed (#10).
#'
#' @param year The tournament year to scrape.
#' @param cutoff_date Last date (inclusive) of games used in the ratings;
#'   defaults to that year's Selection Sunday.
#'
#' @return A year-wide team-level feature table for the requested year, with a
#'   `ratings_cutoff` column recording the cutoff used.
#' @keywords internal
scrape_bart_data <- function(year, cutoff_date = ratings_cutoff_date(year)) {
    logger::log_warn("scrape_bart_data() is deprecated; use compute_season_team_ratings() (see issue #10)")
    url <- build_bart_ratings_url(year, cutoff_date)
    logger::log_info("Scraping Bart data from {url}")

    page <- read_html_verified(url)
    table <- extract_primary_table(page, url, expected_header_tokens = c("Rk", "Team", "G", "AdjOE", "AdjDE", "Barthag"))

    if (!all(c("Team", "AdjOE", "AdjDE", "Barthag") %in% names(table))) {
        column_names <- as.character(table[1, ])
        colnames(table) <- column_names
        table <- dplyr::slice(table, -1)
    }

    table %>%
        dplyr::filter(!is.na(Team), stringr::str_squish(Team) != "") %>%
        dplyr::mutate(
            Year = as.character(year),
            raw_team = as.character(Team),
            Conf = dplyr::na_if(stringr::str_squish(as.character(Conf)), ""),
            Seed = suppressWarnings(as.integer(stringr::str_match(raw_team, "\\b(\\d{1,2})\\s*seed\\b")[, 2])),
            Rk = suppressWarnings(as.numeric(Rk)),
            G = suppressWarnings(as.numeric(G)),
            dplyr::across(c(`AdjOE`, `AdjDE`, `Barthag`, `TOR`, `TORD`, `ORB`, `DRB`, `3P%`, `3P%D`, `Adj T.`, `WAB`), ~ suppressWarnings(as.numeric(.x))),
            Team = Team %>%
                stringr::str_replace(",.*$", "") %>%
                stringr::str_replace("\\s+\\d+\\s*seed$", "") %>%
                stringr::str_trim() %>%
                canonicalize_team_name(),
            team_key = normalize_team_key(Team)
        ) %>%
        dplyr::filter(
            !is.na(AdjOE),
            !is.na(AdjDE),
            !is.na(Barthag),
            !Team %in% c("Team", "")
        ) %>%
        dplyr::mutate(ratings_cutoff = format(as.Date(cutoff_date), "%Y-%m-%d")) %>%
        dplyr::select(
            Year,
            Team,
            team_key,
            Seed,
            Conf,
            G,
            ratings_cutoff,
            Barthag,
            AdjOE,
            AdjDE,
            WAB,
            TOR,
            TORD,
            ORB,
            DRB,
            `3P%`,
            `3P%D`,
            `Adj T.`
        )
}

#' Validate the scraped tournament roster
#'
#' @param conf_assignments A roster table scraped from the Bart tournament page.
#' @param require_seed Whether missing seeds should fail validation.
#'
#' @return The validated roster with canonical team keys.
#' @keywords internal
validate_tournament_roster <- function(conf_assignments, require_seed = TRUE) {
    roster <- conf_assignments %>%
        dplyr::mutate(
            Year = as.character(Year),
            Team = canonicalize_team_name(Team),
            Region = as.character(Region),
            Conf = as.character(Conf),
            Seed = as.integer(Seed),
            team_key = normalize_team_key(Team)
        )

    duplicate_team_rows <- roster %>%
        dplyr::count(Year, Region, Team, name = "n") %>%
        dplyr::filter(n > 1)
    if (nrow(duplicate_team_rows) > 0) {
        stop_with_message(
            sprintf(
                "Tournament roster contains duplicate Year/Region/Team rows: %s",
                paste(sprintf("%s %s %s", duplicate_team_rows$Year, duplicate_team_rows$Region, duplicate_team_rows$Team), collapse = "; ")
            )
        )
    }

    yearly_counts <- roster %>%
        dplyr::count(Year, name = "teams") %>%
        dplyr::filter(teams != tournament_field_size(Year))
    if (nrow(yearly_counts) > 0) {
        stop_with_message(
            sprintf(
                "Tournament roster must contain the full field (68 teams from 2011, 65 before): %s",
                paste(sprintf("%s=%s", yearly_counts$Year, yearly_counts$teams), collapse = ", ")
            )
        )
    }

    if (isTRUE(require_seed)) {
        missing_seed_rows <- roster %>%
            dplyr::filter(is.na(Seed))
        if (nrow(missing_seed_rows) > 0) {
            stop_with_message(
                sprintf(
                    "Tournament roster still has missing seeds: %s",
                    paste(sprintf("%s %s %s", missing_seed_rows$Year, missing_seed_rows$Region, missing_seed_rows$Team), collapse = "; ")
                )
            )
        }
    }

    # A region holds 16 teams plus any play-in games assigned to it (in 2014
    # three of the four First Four games were in the Midwest).
    region_counts <- roster %>%
        dplyr::count(Year, Region, name = "teams") %>%
        dplyr::filter(teams < 16L | teams > 16L + expected_play_in_games(Year))
    if (nrow(region_counts) > 0) {
        stop_with_message(
            sprintf(
                "Tournament roster has invalid region team counts: %s",
                paste(sprintf("%s %s=%s", region_counts$Year, region_counts$Region, region_counts$teams), collapse = "; ")
            )
        )
    }

    if (isTRUE(require_seed)) {
        duplicate_seed_slots <- roster %>%
            dplyr::filter(!is.na(Seed)) %>%
            dplyr::count(Year, Region, Seed, name = "n") %>%
            dplyr::filter(n > 1)

        invalid_duplicate_slots <- duplicate_seed_slots %>%
            dplyr::filter(n != 2L)
        if (nrow(invalid_duplicate_slots) > 0) {
            stop_with_message(
                sprintf(
                    "Tournament roster has invalid play-in seed duplication: %s",
                    paste(sprintf("%s %s %s=%s", invalid_duplicate_slots$Year, invalid_duplicate_slots$Region, invalid_duplicate_slots$Seed, invalid_duplicate_slots$n), collapse = "; ")
                )
            )
        }

        duplicate_slot_years <- duplicate_seed_slots %>%
            dplyr::count(Year, name = "duplicate_slots") %>%
            dplyr::filter(duplicate_slots != expected_play_in_games(Year))
        if (nrow(duplicate_slot_years) > 0) {
            stop_with_message(
                sprintf(
                    "Tournament roster must contain one play-in seed slot per play-in game (four from 2011, one before): %s",
                    paste(sprintf("%s=%s", duplicate_slot_years$Year, duplicate_slot_years$duplicate_slots), collapse = ", ")
                )
            )
        }
    }

    roster
}

#' Build the canonical team feature dataset
#'
#' @param team_ratings Year-wide team ratings (one row per team per year), for
#'   example from [compute_season_team_ratings()].
#' @param conf_assignments A conference-assignment lookup table.
#'
#' @return A normalized pre-tournament team feature table.
#' @keywords internal
build_team_feature_dataset <- function(team_ratings, conf_assignments) {
    roster <- validate_tournament_roster(conf_assignments, require_seed = FALSE)
    ratings <- team_ratings %>%
        dplyr::mutate(
            Year = as.character(Year),
            Team = canonicalize_team_name(Team),
            team_key = normalize_team_key(Team)
        )

    duplicate_rating_rows <- ratings %>%
        dplyr::count(Year, team_key, name = "n") %>%
        dplyr::filter(n > 1)
    if (nrow(duplicate_rating_rows) > 0) {
        duplicate_detail <- ratings %>%
            dplyr::inner_join(duplicate_rating_rows, by = c("Year", "team_key")) %>%
            dplyr::distinct(Year, Team)
        stop_with_message(
            sprintf(
                "Team ratings contain duplicate teams for the same year: %s",
                paste(sprintf("%s %s", duplicate_detail$Year, duplicate_detail$Team), collapse = "; ")
            )
        )
    }

    joined <- roster %>%
        dplyr::left_join(
            ratings %>%
                dplyr::select(
                    Year,
                    team_key,
                    Conf,
                    ratings_cutoff,
                    Barthag,
                    AdjOE,
                    AdjDE,
                    WAB,
                    TOR,
                    TORD,
                    ORB,
                    DRB,
                    `3P%`,
                    `3P%D`,
                    `Adj T.`
                ),
            by = c("Year", "team_key")
        ) %>%
        dplyr::mutate(
            Conf = dplyr::na_if(dplyr::coalesce(Conf.x, Conf.y), "")
        )

    unresolved_roster <- joined %>%
        dplyr::filter(dplyr::if_any(c(Conf, ratings_cutoff, Barthag, AdjOE, AdjDE, WAB, TOR, TORD, ORB, DRB, `3P%`, `3P%D`, `Adj T.`), is.na)) %>%
        dplyr::distinct(Year, Team)
    if (nrow(unresolved_roster) > 0) {
        stop_with_message(
            sprintf(
                "Year-wide team ratings did not cover every tournament team: %s",
                paste(sprintf("%s %s", unresolved_roster$Year, unresolved_roster$Team), collapse = "; ")
            )
        )
    }

    validate_tournament_roster(
        joined %>%
            dplyr::transmute(
                Year,
                Team,
                Seed,
                Region,
                Conf
            ),
        require_seed = TRUE
    )

    joined %>%
        dplyr::select(
            Year,
            Team,
            Seed = Seed,
            Region,
            Conf,
            ratings_cutoff,
            Barthag,
            AdjOE,
            AdjDE,
            WAB,
            TOR,
            TORD,
            ORB,
            DRB,
            `3P%`,
            `3P%D`,
            `Adj T.`
        ) %>%
        add_safe_pre_tournament_features()
}

#' Map bracket counters to round labels
#'
#' @param region_name The region name or `"National"`.
#' @param round_index Position of the round within the region's bracket (1 = first).
#'
#' @return A single round label.
#' @keywords internal
results_round_label <- function(region_name, round_index) {
    labels <- if (identical(region_name, "National")) {
        c("Final Four", "Championship")
    } else {
        c("Round of 64", "Round of 32", "Sweet 16", "Elite 8")
    }
    if (round_index < 1L || round_index > length(labels)) {
        return(NA_character_)
    }
    labels[[round_index]]
}

#' Parse a single tournament game line
#'
#' @param line A line of scraped tournament text.
#'
#' @return A one-row tibble describing the parsed game, or `NULL` if the line
#'   does not match the expected format.
#' @keywords internal
parse_tournament_game_line <- function(line) {
    pattern <- "^\\(?([0-9]{1,2})\\)?\\s+(.+?)\\s+(\\d+)\\s*,\\s*\\(?([0-9]{1,2})\\)?\\s+(.+?)\\s+(\\d+)(?:\\s+at .*)?$"
    matched <- stringr::str_match(line, pattern)
    if (all(is.na(matched))) {
        return(NULL)
    }

    tibble::tibble(
        teamA_seed = as.integer(matched[, 2]),
        teamA = canonicalize_team_name(stringr::str_trim(matched[, 3])),
        teamA_score = as.integer(matched[, 4]),
        teamB_seed = as.integer(matched[, 5]),
        teamB = canonicalize_team_name(stringr::str_trim(matched[, 6])),
        teamB_score = as.integer(matched[, 7])
    )
}

#' Read Sports-Reference tournament page text lines
#'
#' @param year The tournament year to read.
#' @param allow_empty Whether to allow an unavailable page to return `NULL`.
#'
#' @return A character vector of page text lines, or `NULL` when allowed and the
#'   page is unavailable.
#' @keywords internal
read_sports_reference_page_lines <- function(year, allow_empty = FALSE) {
    url <- sprintf("https://www.sports-reference.com/cbb/postseason/men/%s-ncaa.html", year)
    cache_key <- as.character(year)
    if (!is.null(sports_reference_page_cache[[cache_key]])) {
        return(sports_reference_page_cache[[cache_key]])
    }
    logger::log_info("Scraping Sports-Reference tournament page from {url}")

    page_text <- tryCatch(
        {
            # Sports-Reference allows at most 20 requests per minute.
            Sys.sleep(sports_reference_request_delay_seconds())
            response <- httr::GET(url, httr::user_agent("mmBayes/1.0"))
            httr::stop_for_status(response)
            httr::content(response, as = "text", encoding = "UTF-8")
        },
        error = function(err) {
            if (!isTRUE(allow_empty)) {
                stop(err)
            }
            logger::log_warn("Sports-Reference tournament page for {year} was unavailable: {conditionMessage(err)}")
            return(NULL)
        }
    )
    if (is.null(page_text)) {
        return(NULL)
    }

    lines <- page_text %>%
        stringr::str_split("\\n", simplify = FALSE) %>%
        purrr::pluck(1)
    assign(cache_key, lines, envir = sports_reference_page_cache)
    lines
}

#' Session cache of Sports-Reference tournament pages
#'
#' The roster and results parsers read the same page; caching it for the R
#' session halves the requests.
#' @keywords internal
sports_reference_page_cache <- new.env(parent = emptyenv())

#' Pause before each Sports-Reference request
#'
#' @return Seconds to wait (Sports-Reference allows 20 requests per minute).
#' @keywords internal
sports_reference_request_delay_seconds <- function() {
    3.5
}

#' Parse tournament bracket text into explicit game rows
#'
#' @param lines A character vector of bracket text lines.
#' @param year The tournament year represented by `lines`.
#'
#' @return A game-level table including bracket metadata, teams, scores, and
#'   winner.
#' @keywords internal
parse_tournament_bracket_games <- function(lines, year) {
    page <- rvest::read_html(paste(lines, collapse = "\n"))
    region_nodes <- rvest::html_elements(page, xpath = "//div[@id='brackets']/div[@id]")
    if (length(region_nodes) == 0) {
        stop_with_message(sprintf("No bracket regions found on the Sports-Reference page for %s", year))
    }

    region_label <- function(node_id) {
        stringr::str_to_title(stringr::str_replace_all(node_id, "-", " "))
    }
    team_from_node <- function(team_node) {
        seed <- rvest::html_text2(rvest::html_element(team_node, xpath = "./span"))
        links <- rvest::html_elements(team_node, xpath = "./a")
        list(
            seed = suppressWarnings(as.integer(stringr::str_squish(seed))),
            team = if (length(links) >= 1) canonicalize_team_name(stringr::str_squish(rvest::html_text2(links[[1]]))) else NA_character_,
            score = if (length(links) >= 2) suppressWarnings(as.integer(stringr::str_squish(rvest::html_text2(links[[2]])))) else NA_integer_
        )
    }

    first_four_rows <- list()
    main_rows <- list()
    for (region_node in region_nodes) {
        region_name <- region_label(rvest::html_attr(region_node, "id"))

        play_in_paragraphs <- rvest::html_elements(region_node, xpath = "./p[strong[contains(., 'First Four') or contains(., 'Play-In') or contains(., 'Opening Round')]]")
        for (paragraph in play_in_paragraphs) {
            play_in_lines <- rvest::html_text2(paragraph) %>%
                stringr::str_replace_all("\u00a0", " ") %>%
                stringr::str_split("\n") %>%
                purrr::pluck(1) %>%
                stringr::str_squish()
            for (line in play_in_lines) {
                parsed <- parse_tournament_game_line(line)
                if (!is.null(parsed)) {
                    first_four_rows[[length(first_four_rows) + 1L]] <- parsed %>%
                        dplyr::mutate(play_in_region = region_name)
                }
            }
        }

        round_nodes <- rvest::html_elements(region_node, xpath = ".//div[@id='bracket']/div[contains(concat(' ', normalize-space(@class), ' '), ' round ')]")
        for (round_index in seq_along(round_nodes)) {
            game_nodes <- rvest::html_elements(round_nodes[[round_index]], xpath = "./div")
            for (game_index in seq_along(game_nodes)) {
                team_nodes <- rvest::html_elements(game_nodes[[game_index]], xpath = "./div")
                if (length(team_nodes) != 2L) {
                    next
                }
                team_a <- team_from_node(team_nodes[[1]])
                team_b <- team_from_node(team_nodes[[2]])
                if (is.na(team_a$score) || is.na(team_b$score) || is.na(team_a$team) || is.na(team_b$team)) {
                    next
                }
                main_rows[[length(main_rows) + 1L]] <- tibble::tibble(
                    region = region_name,
                    round = results_round_label(region_name, round_index),
                    game_index = game_index,
                    teamA_seed = team_a$seed,
                    teamA = team_a$team,
                    teamA_score = team_a$score,
                    teamB_seed = team_b$seed,
                    teamB = team_b$team,
                    teamB_score = team_b$score
                )
            }
        }
    }

    first_four <- dplyr::bind_rows(first_four_rows)
    if (nrow(first_four) > 0) {
        first_four <- first_four %>%
            dplyr::mutate(region = "First Four", round = "First Four", game_index = dplyr::row_number())
    }
    parsed_games <- dplyr::bind_rows(first_four, dplyr::bind_rows(main_rows)) %>%
        dplyr::mutate(
            Year = as.character(year),
            play_in_region = if ("play_in_region" %in% names(.)) play_in_region else NA_character_,
            winner = ifelse(teamA_score > teamB_score, teamA, teamB)
        )

    if (nrow(parsed_games) == 0) {
        stop_with_message(sprintf("No tournament results parsed for year %s", year))
    }

    parsed_games %>%
        dplyr::select(
            Year,
            region,
            play_in_region,
            round,
            game_index,
            teamA,
            teamB,
            teamA_seed,
            teamB_seed,
            teamA_score,
            teamB_score,
            winner
        )
}

#' Parse historical Sports-Reference bracket text into canonical roster rows
#'
#' @param lines A character vector of bracket text lines.
#' @param year The tournament year represented by `lines`.
#'
#' @return A canonical roster table with year, team, seed, region, and `Conf`.
#' @keywords internal
parse_historical_tournament_roster_lines <- function(lines, year) {
    parsed_games <- parse_tournament_bracket_games(lines, year)

    unresolved_first_four <- parsed_games %>%
        dplyr::filter(region == "First Four", is.na(play_in_region))
    if (nrow(unresolved_first_four) > 0) {
        stop_with_message(
            sprintf("Historical First Four games were missing region assignments for year %s", year)
        )
    }

    roster <- dplyr::bind_rows(
        parsed_games %>%
            dplyr::transmute(
                Year,
                Team = teamA,
                Seed = teamA_seed,
                Region = dplyr::if_else(region == "First Four", play_in_region, region),
                Conf = NA_character_
            ),
        parsed_games %>%
            dplyr::transmute(
                Year,
                Team = teamB,
                Seed = teamB_seed,
                Region = dplyr::if_else(region == "First Four", play_in_region, region),
                Conf = NA_character_
            )
    ) %>%
        dplyr::filter(!Region %in% c("National", "First Four")) %>%
        dplyr::distinct(Year, Team, Seed, Region, .keep_all = TRUE) %>%
        dplyr::arrange(suppressWarnings(as.integer(Year)), Region, Seed, Team)

    validate_tournament_roster(roster, require_seed = TRUE)
}

#' Parse tournament bracket text into explicit game results
#'
#' @param lines A character vector of bracket text lines.
#' @param year The tournament year represented by `lines`.
#'
#' @return A game-level results table including region, round, teams, scores,
#'   total points, and winner.
#' @keywords internal
parse_tournament_results_lines <- function(lines, year) {
    parse_tournament_bracket_games(lines, year) %>%
        dplyr::mutate(
            total_points = teamA_score + teamB_score
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
            winner
        )
}

#' Scrape explicit tournament game results
#'
#' @param year The tournament year to scrape.
#'
#' @return A canonical historical roster table derived from the Sports-Reference
#'   postseason page. Refresh issues are attached via the `refresh_issues`
#'   attribute when the source cannot be parsed.
#' @keywords internal
scrape_historical_tournament_roster <- function(year) {
    refresh_issues <- empty_refresh_issues_table()
    roster <- tryCatch(
        {
            lines <- read_sports_reference_page_lines(year, allow_empty = FALSE)
            parse_historical_tournament_roster_lines(lines, year)
        },
        error = function(err) {
            issue_message <- sprintf(
                "Skipping historical tournament roster for year %s: %s",
                year,
                err$message
            )
            logger::log_warn(issue_message)
            refresh_issues <<- append_refresh_issue(
                refresh_issues,
                step = "historical_tournament_roster",
                source = "Sports-Reference postseason page",
                severity = "warning",
                message = issue_message
            )
            tibble::tibble(
                Year = character(),
                Team = character(),
                Seed = integer(),
                Region = character(),
                Conf = character()
            )
        }
    )

    attr(roster, "refresh_issues") <- refresh_issues
    roster
}

#' Scrape explicit tournament game results
#'
#' @param year The tournament year to scrape.
#' @param allow_empty Whether to return a zero-row result table when no
#'   completed games are available yet.
#'
#' @return A game-level results table including region, round, teams, scores,
#'   total points, and winner.
#' @keywords internal
scrape_tournament_results <- function(year, allow_empty = FALSE) {
    lines <- read_sports_reference_page_lines(year, allow_empty = allow_empty)
    if (is.null(lines)) {
        return(empty_game_results_table())
    }

    parsed_results <- tryCatch(
        parse_tournament_results_lines(lines, year),
        error = function(err) {
            if (!isTRUE(allow_empty)) {
                stop(err)
            }
            logger::log_warn("No completed tournament results parsed for {year}; returning an empty current-year result set")
            empty_game_results_table()
        }
    )

    parsed_results
}

#' Return candidate ESPN scoreboard dates for tournament fallback
#'
#' @param bracket_year The active bracket year.
#' @param today Optional current date override for testing.
#'
#' @return A character vector of `YYYYMMDD` date keys covering the First Four
#'   window.
#' @keywords internal
tournament_scoreboard_dates <- function(bracket_year, today = Sys.Date()) {
    end_date <- min(
        as.Date(sprintf("%s-04-15", bracket_year)),
        as.Date(today)
    )

    format(
        seq.Date(
            as.Date(sprintf("%s-03-15", bracket_year)),
            end_date,
            by = "day"
        ),
        "%Y%m%d"
    )
}

#' Return candidate ESPN scoreboard dates for First Four fallback
#'
#' @param bracket_year The active bracket year.
#'
#' @return A character vector of `YYYYMMDD` date keys covering the First Four
#'   window.
#' @keywords internal
first_four_scoreboard_dates <- function(bracket_year) {
    c(
        sprintf("%s0317", bracket_year),
        sprintf("%s0318", bracket_year)
    )
}

#' Extract a round label from ESPN tournament metadata
#'
#' @param headline The competition headline.
#' @param event_name The event name, when available.
#' @param short_name The event short name, when available.
#'
#' @return A canonical round label or `NA_character_`.
#' @keywords internal
extract_espn_tournament_round <- function(headline, event_name = "", short_name = "") {
    text <- paste(c(headline, event_name, short_name), collapse = " | ")
    text <- stringr::str_squish(text)
    if (is.na(text) || !nzchar(text)) {
        return(NA_character_)
    }

    round_patterns <- list(
        "First Four" = c("First Four", "Play-In"),
        "Round of 64" = c("Round of 64", "Round 1", "1st Round", "First Round"),
        "Round of 32" = c("Round of 32", "Round 2", "2nd Round", "Second Round"),
        "Sweet 16" = c("Sweet 16", "Regional Semifinal"),
        "Elite 8" = c("Elite 8", "Elite Eight", "Regional Final"),
        "Final Four" = c("Final Four", "National Semifinal", "Semifinal"),
        "Championship" = c("National Championship", "Championship Game", "Title Game")
    )

    for (round_name in names(round_patterns)) {
        if (any(vapply(round_patterns[[round_name]], function(pattern) stringr::str_detect(text, stringr::regex(pattern, ignore_case = TRUE)), logical(1)))) {
            return(round_name)
        }
    }

    NA_character_
}

#' Extract a region label from ESPN tournament metadata
#'
#' @param headline The competition headline.
#'
#' @return A canonical region label or `NA_character_`.
#' @keywords internal
extract_espn_tournament_region <- function(headline) {
    text <- stringr::str_squish(headline %||% "")
    if (is.na(text) || !nzchar(text)) {
        return(NA_character_)
    }

    if (stringr::str_detect(text, stringr::regex("National", ignore_case = TRUE))) {
        return("National")
    }

    region_match <- stringr::str_match(text, "(East|West|South|Midwest)")
    region_value <- region_match[, 2]
    if (!is.na(region_value)) {
        return(region_value)
    }

    NA_character_
}

#' Scrape completed ESPN tournament results from the scoreboard API
#'
#' @param bracket_year The active bracket year.
#' @param team_features A canonical team feature table used to recover bracket
#'   regions and seeds.
#' @param date_values Optional character vector of scoreboard dates in
#'   `YYYYMMDD` format. Defaults to the broader tournament window.
#' @param round_filter Optional character vector restricting the returned rounds.
#'
#' @return A canonical game-results table containing any completed tournament
#'   games exposed by ESPN.
#' @keywords internal
scrape_espn_tournament_results <- function(bracket_year, team_features, date_values = tournament_scoreboard_dates(bracket_year), round_filter = NULL) {
    current_teams <- team_features %>%
        dplyr::filter(Year == as.character(bracket_year)) %>%
        dplyr::transmute(
            Year = as.character(Year),
            Team = canonicalize_team_name(Team),
            team_key = normalize_team_key(Team),
            lookup_region = Region,
            lookup_seed = Seed
        ) %>%
        dplyr::distinct()

    if (nrow(current_teams) == 0) {
        return(empty_game_results_table())
    }

    # Normalize ESPN's event payload into one row per scoreboard event.
    coerce_espn_events <- function(events, scoreboard_date) {
        if (is.null(events) || length(events) == 0) {
            return(tibble::tibble())
        }

        if (inherits(events, "data.frame")) {
            return(
                tibble::as_tibble(events) %>%
                    dplyr::mutate(scoreboard_date = scoreboard_date)
            )
        }

        if (is.list(events)) {
            return(
                purrr::map_dfr(events, function(event) {
                    tibble::tibble(
                        competitions = list(event$competitions %||% list()),
                        name = as.character(event$name %||% NA_character_),
                        shortName = as.character(event$shortName %||% NA_character_),
                        date = as.character(event$date %||% NA_character_),
                        scoreboard_date = scoreboard_date
                    )
                })
            )
        }

        tibble::tibble()
    }

    events <- purrr::map_dfr(date_values, function(date_value) {
        url <- sprintf(
            "https://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/scoreboard?dates=%s&seasontype=3",
            date_value
        )
        logger::log_info("Checking ESPN scoreboard fallback for current-year tournament results at {url}")
        payload <- tryCatch(
            jsonlite::fromJSON(url),
            error = function(err) {
                logger::log_warn("ESPN scoreboard fallback unavailable for {date_value}: {conditionMessage(err)}")
                NULL
            }
        )
        if (is.null(payload) || is.null(payload$events)) {
            return(tibble::tibble())
        }
        coerce_espn_events(payload$events, scoreboard_date = date_value)
    })

    if (nrow(events) == 0) {
        return(empty_game_results_table())
    }

    parsed <- purrr::map_dfr(seq_len(nrow(events)), function(index) {
        competition <- events$competitions[[index]]
        if (is.null(competition) || nrow(competition) == 0) {
            return(tibble::tibble())
        }
        competition <- competition[1, , drop = FALSE]
        status_completed <- isTRUE(competition$status$type$completed[[1]])
        if (!status_completed) {
            return(tibble::tibble())
        }

        notes_tbl <- competition$notes[[1]] %||% tibble::tibble()
        headline <- notes_tbl$headline[[1]] %||% events$name[[index]] %||% events$shortName[[index]] %||% ""
        round_label <- extract_espn_tournament_round(headline, events$name[[index]] %||% "", events$shortName[[index]] %||% "")
        if (is.na(round_label)) {
            return(tibble::tibble())
        }
        if (!is.null(round_filter) && !round_label %in% round_filter) {
            return(tibble::tibble())
        }

        competitors <- competition$competitors[[1]]
        if (is.null(competitors) || nrow(competitors) != 2) {
            return(tibble::tibble())
        }
        if ("order" %in% names(competitors)) {
            competitors <- competitors %>% dplyr::arrange(order)
        } else if ("homeAway" %in% names(competitors)) {
            competitors <- competitors %>% dplyr::arrange(dplyr::match(homeAway, c("away", "home")))
        }

        region_value <- extract_espn_tournament_region(headline)
        team_a_name <- canonicalize_team_name(
            dplyr::coalesce(
                competitors$team$shortDisplayName[[1]],
                competitors$team$location[[1]],
                competitors$team$displayName[[1]]
            )
        )
        team_b_name <- canonicalize_team_name(
            dplyr::coalesce(
                competitors$team$shortDisplayName[[2]],
                competitors$team$location[[2]],
                competitors$team$displayName[[2]]
            )
        )
        team_a_score <- as.integer(competitors$score[[1]])
        team_b_score <- as.integer(competitors$score[[2]])

        team_a_lookup <- current_teams %>%
            dplyr::filter(team_key == normalize_team_key(team_a_name))
        team_b_lookup <- current_teams %>%
            dplyr::filter(team_key == normalize_team_key(team_b_name))

        if (nrow(team_a_lookup) != 1 || nrow(team_b_lookup) != 1) {
            return(tibble::tibble())
        }
        if (is.na(region_value)) {
            if (identical(round_label, "Final Four") || identical(round_label, "Championship")) {
                region_value <- "National"
            } else if (!is.na(team_a_lookup$lookup_region[[1]]) && !is.na(team_b_lookup$lookup_region[[1]]) && identical(team_a_lookup$lookup_region[[1]], team_b_lookup$lookup_region[[1]])) {
                region_value <- team_a_lookup$lookup_region[[1]]
            } else {
                region_value <- team_a_lookup$lookup_region[[1]] %||% team_b_lookup$lookup_region[[1]]
            }
        }

        output_region <- if (identical(round_label, "First Four")) {
            "First Four"
        } else {
            region_value %||% ifelse(round_label %in% c("Final Four", "Championship"), "National", team_a_lookup$lookup_region[[1]])
        }

        tibble::tibble(
            Year = as.character(bracket_year),
            region = output_region,
            round = round_label,
            game_index = NA_integer_,
            teamA = team_a_name,
            teamB = team_b_name,
            teamA_seed = as.integer(team_a_lookup$lookup_seed[[1]]),
            teamB_seed = as.integer(team_b_lookup$lookup_seed[[1]]),
            teamA_score = team_a_score,
            teamB_score = team_b_score,
            total_points = as.integer(team_a_score + team_b_score),
            winner = ifelse(team_a_score > team_b_score, team_a_name, team_b_name),
            completed_at = as.character(competition$date[[1]] %||% events$date[[index]] %||% NA_character_),
            source = "espn_scoreboard",
            play_in_region = dplyr::coalesce(region_value, team_a_lookup$lookup_region[[1]], team_b_lookup$lookup_region[[1]]),
            slot_seed = dplyr::coalesce(as.integer(team_a_lookup$lookup_seed[[1]]), as.integer(team_b_lookup$lookup_seed[[1]]))
        )
    })

    if (nrow(parsed) == 0) {
        return(empty_game_results_table())
    }

    parsed %>%
        dplyr::filter(!is.na(round), !is.na(region)) %>%
        dplyr::mutate(
            round = factor(round, levels = round_levels()),
            region = factor(region, levels = c(bracket_region_levels(), "National", "First Four"))
        ) %>%
        dplyr::arrange(completed_at, round, region, teamA, teamB) %>%
        dplyr::group_by(Year, region, round) %>%
        dplyr::mutate(game_index = dplyr::row_number()) %>%
        dplyr::ungroup() %>%
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
            completed_at,
            source
        )
}

#' Scrape completed First Four results from the ESPN scoreboard API
#'
#' @param bracket_year The active bracket year.
#' @param team_features A canonical team feature table used to recover bracket
#'   regions and seeds.
#' @param date_values Optional character vector of scoreboard dates in
#'   `YYYYMMDD` format.
#'
#' @return A canonical game-results table containing any completed First Four
#'   games exposed by ESPN.
#' @keywords internal
scrape_espn_first_four_results <- function(bracket_year, team_features, date_values = first_four_scoreboard_dates(bracket_year)) {
    scrape_espn_tournament_results(
        bracket_year = bracket_year,
        team_features = team_features,
        date_values = date_values,
        round_filter = "First Four"
    )
}

#' Fill missing current-year tournament rows with fallback results
#'
#' @param game_results A canonical game-results table.
#' @param team_features A canonical team feature table.
#' @param bracket_year The active bracket year.
#' @param fallback_results Optional canonical result rows to merge in place of a
#'   live fallback scrape.
#' @param round_filter Optional character vector restricting the rows that can
#'   be merged from the fallback.
#'
#' @return The game-results table with missing current-year rows filled when
#'   fallback results are available.
#' @keywords internal
merge_current_year_tournament_results <- function(game_results, team_features, bracket_year, fallback_results = NULL, round_filter = NULL) {
    bracket_year <- as.character(bracket_year)
    current_teams <- team_features %>%
        dplyr::filter(Year == bracket_year)

    # Ensure optional monitoring metadata exists before combining sources.
    normalize_monitoring_columns <- function(tbl) {
        if (!"completed_at" %in% names(tbl)) {
            tbl <- tbl %>%
                dplyr::mutate(completed_at = NA_character_)
        }
        if (!"source" %in% names(tbl)) {
            tbl <- tbl %>%
                dplyr::mutate(source = NA_character_)
        }
        tbl
    }

    if (nrow(current_teams) == 0) {
        return(game_results)
    }

    current_results <- game_results %>%
        dplyr::filter(Year == bracket_year) %>%
        normalize_monitoring_columns()
    if (!is.null(round_filter)) {
        current_results <- current_results %>%
            dplyr::filter(round %in% round_filter)
    }

    if (nrow(current_results) == 0 && is.null(fallback_results)) {
        return(game_results)
    }

    fallback_results <- fallback_results %||% tryCatch(
        scrape_espn_tournament_results(bracket_year, team_features),
        error = function(err) {
            logger::log_warn("Skipping ESPN scoreboard fallback for {bracket_year}: {err$message}")
            empty_game_results_table()
        }
    ) %>%
        normalize_monitoring_columns()
    if (!is.null(round_filter)) {
        fallback_results <- fallback_results %>%
            dplyr::filter(round %in% round_filter)
    }
    if (nrow(fallback_results) == 0) {
        return(game_results)
    }

    team_lookup <- current_teams %>%
        dplyr::transmute(
            Year = as.character(Year),
            team_key = normalize_team_key(Team),
            lookup_region = Region,
            lookup_seed = Seed
        ) %>%
        dplyr::distinct()

    # Build deterministic deduplication keys across existing and fallback rows.
    add_sort_keys <- function(tbl, source_priority) {
        if (nrow(tbl) == 0) {
            return(tbl %>% dplyr::mutate(source_priority = integer(), teamA_key = character(), teamB_key = character(), game_pair_key = character()))
        }

        tbl %>%
            dplyr::mutate(
                source_priority = source_priority,
                teamA_key = normalize_team_key(teamA),
                teamB_key = normalize_team_key(teamB),
                game_pair_key = dplyr::if_else(
                    teamA_key <= teamB_key,
                    paste(teamA_key, teamB_key, sep = "|"),
                    paste(teamB_key, teamA_key, sep = "|")
                )
            )
    }

    existing_with_slot <- add_sort_keys(current_results, 1L) %>%
        dplyr::mutate(
            play_in_region = dplyr::coalesce(
                team_lookup$lookup_region[match(teamA_key, team_lookup$team_key)],
                team_lookup$lookup_region[match(teamB_key, team_lookup$team_key)]
            ),
            slot_seed = dplyr::coalesce(
                teamA_seed,
                teamB_seed,
                team_lookup$lookup_seed[match(teamA_key, team_lookup$team_key)],
                team_lookup$lookup_seed[match(teamB_key, team_lookup$team_key)]
            )
        )

    fallback_with_slot <- add_sort_keys(fallback_results, 2L) %>%
        dplyr::mutate(
            play_in_region = team_lookup$lookup_region[match(normalize_team_key(teamA), team_lookup$team_key)],
            slot_seed = team_lookup$lookup_seed[match(normalize_team_key(teamA), team_lookup$team_key)]
        )

        merged_current <- dplyr::bind_rows(existing_with_slot, fallback_with_slot) %>%
        dplyr::filter(!is.na(region), !is.na(round), !is.na(teamA), !is.na(teamB)) %>%
        dplyr::mutate(
            region = as.character(region),
            round = as.character(round),
            completed_at = dplyr::coalesce(as.character(completed_at), NA_character_),
            source = dplyr::coalesce(as.character(source), "unknown")
        ) %>%
        dplyr::arrange(source_priority, completed_at, round, region, teamA, teamB) %>%
        dplyr::group_by(Year, round, game_pair_key) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(Year, round, region) %>%
        dplyr::mutate(game_index = dplyr::row_number()) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(round, region, game_index) %>%
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
            completed_at,
            source
        )

    historical_results <- game_results %>%
        dplyr::filter(Year != bracket_year)

    dplyr::bind_rows(historical_results, merged_current)
}

#' Fill missing current-year First Four rows with fallback results
#'
#' @param game_results A canonical game-results table.
#' @param team_features A canonical team feature table.
#' @param bracket_year The active bracket year.
#' @param fallback_results Optional canonical First Four result rows to merge in
#'   place of a live fallback scrape.
#'
#' @return The game-results table with missing current-year First Four slots
#'   filled when fallback results are available.
#' @keywords internal
fill_current_year_first_four_results <- function(game_results, team_features, bracket_year, fallback_results = NULL) {
    merge_current_year_tournament_results(
        game_results = game_results,
        team_features = team_features,
        bracket_year = bracket_year,
        fallback_results = fallback_results,
        round_filter = "First Four"
    )
}

#' Refresh canonical tournament data files
#'
#' @param start_year Optional starting year for the refresh window.
#' @param bracket_year The active bracket year.
#' @param history_window Number of completed historical tournaments to fetch by
#'   default. When omitted, the configured model history window is used.
#'
#' @param config Optional project configuration list. When omitted, the loaded
#'   project config is used.
#'
#' @return A list containing the written team-feature and game-results file
#'   paths.
#' @export
update_tournament_data <- function(config = NULL, start_year = NULL, bracket_year = as.integer(format(Sys.Date(), "%Y")), history_window = NULL) {
    config <- config %||% load_project_config()
    history_window <- history_window %||% config$model$history_window %||% 8L
    team_features_file <- config$data$team_features_path %||% file.path(default_cloud_data_root(), "pre_tournament_team_features.xlsx")
    game_results_file <- config$data$game_results_path %||% file.path(default_cloud_data_root(), "tournament_game_results.xlsx")
    refresh_log <- config$output$refresh_log_path %||% file.path(config$output$path %||% default_runtime_output_root(), "logs", "data_refresh.log")
    dir.create(dirname(team_features_file), showWarnings = FALSE, recursive = TRUE)
    dir.create(dirname(game_results_file), showWarnings = FALSE, recursive = TRUE)

    refresh_issues <- empty_refresh_issues_table()
    # Accumulate refresh warnings from per-year scrapers into the run summary.
    register_refresh_issues <- function(issues) {
        issues <- issues %||% empty_refresh_issues_table()
        if (nrow(issues) > 0) {
            refresh_issues <<- dplyr::bind_rows(refresh_issues, issues)
        }
        invisible(NULL)
    }

    explicit_start_year <- !is.null(start_year)
    if (!explicit_start_year) {
        historical_years <- c(
            configured_completed_historical_years(bracket_year, history_window = history_window),
            as.integer(bracket_year)
        )
    } else {
        start_year <- as.integer(start_year)
        historical_years <- seq.int(start_year, as.integer(bracket_year))
    }
    historical_years <- sort(unique(historical_years[historical_years != 2020L]))
    required_historical_years <- historical_years[historical_years < bracket_year]
    pre_team_counts <- read_year_counts_from_file(team_features_file, count_name = "pre_team_feature_rows")
    pre_result_counts <- read_year_counts_from_file(game_results_file, count_name = "pre_result_rows")
    existing_team_features <- if (file.exists(team_features_file)) {
        normalize_team_features(read_table_file(team_features_file))
    } else {
        empty_team_feature_table()
    }
    existing_game_results <- if (file.exists(game_results_file)) {
        normalize_game_results(read_table_file(game_results_file))
    } else {
        empty_game_results_table()
    }
    historical_cache_decisions <- if (length(required_historical_years) > 0) {
        purrr::map_dfr(required_historical_years, function(year) {
            assess_historical_year_cache(existing_team_features, existing_game_results, year)
        }) %>%
            dplyr::mutate(
                cache_action = dplyr::if_else(reusable, "reuse", "refresh")
            )
    } else {
        tibble::tibble(
            Year = character(),
            reusable = logical(),
            pre_team_feature_rows = integer(),
            pre_result_rows = integer(),
            completeness_issue = character(),
            cache_action = character()
        )
    }
    reused_historical_years <- historical_cache_decisions %>%
        dplyr::filter(cache_action == "reuse") %>%
        dplyr::pull(Year)
    refresh_historical_years <- historical_cache_decisions %>%
        dplyr::filter(cache_action == "refresh") %>%
        dplyr::pull(Year)
    scrape_years <- sort(unique(c(suppressWarnings(as.integer(refresh_historical_years)), as.integer(bracket_year))))

    logger::log_info(
        "Starting canonical data refresh for bracket year {bracket_year} across years {min(historical_years)}-{max(historical_years)}"
    )
    logger::log_info("Configured historical analysis window: {history_window}")
    logger::log_info("Canonical team-feature path for this run: {team_features_file}")
    logger::log_info("Canonical game-results path for this run: {game_results_file}")
    logger::log_info("Refresh log path for this run: {refresh_log}")
    logger::log_info("Pre-refresh team-feature year coverage: {format_year_count_summary(pre_team_counts, 'pre_team_feature_rows')}")
    logger::log_info("Pre-refresh game-result year coverage: {format_year_count_summary(pre_result_counts, 'pre_result_rows')}")
    logger::log_info("Reused historical years: {format_year_labels(reused_historical_years)}")
    logger::log_info("Refreshed historical years: {format_year_labels(refresh_historical_years)}")
    logger::log_info(
        "Historical cache misses requiring refresh: {format_year_labels(historical_cache_decisions %>% dplyr::filter(cache_action == 'refresh', !is.na(completeness_issue)) %>% dplyr::transmute(label = sprintf('%s (%s)', Year, completeness_issue)) %>% dplyr::pull(label))}"
    )

    team_ratings <- purrr::map_dfr(scrape_years, function(year) {
        logger::log_info("Computing team ratings for {year} through Selection Sunday {ratings_cutoff_date(year)}")
        compute_season_team_ratings(
            year,
            raw_dir = config$data$espn_raw_dir,
            refresh = identical(as.integer(year), as.integer(bracket_year))
        )
    })
    ratings_counts <- count_rows_by_year(team_ratings, count_name = "rating_rows")

    historical_rosters <- purrr::map_dfr(scrape_years[scrape_years < bracket_year], function(year) {
        logger::log_info("Refreshing historical tournament roster for {year}")
        roster_tbl <- scrape_historical_tournament_roster(year)
        register_refresh_issues(attr(roster_tbl, "refresh_issues", exact = TRUE))
        roster_tbl
    })
    current_roster <- purrr::map_dfr(scrape_years[scrape_years == bracket_year], function(year) {
        logger::log_info("Refreshing tournament roster for {year}")
        conf_tbl <- scrape_conf_assignments(year)
        register_refresh_issues(attr(conf_tbl, "refresh_issues", exact = TRUE))
        conf_tbl
    })
    tournament_rosters <- dplyr::bind_rows(historical_rosters, current_roster)
    roster_counts <- count_rows_by_year(tournament_rosters, count_name = "roster_rows")

    logger::log_info("Joining tournament rosters to year-wide team ratings")
    refreshed_team_features <- build_team_feature_dataset(team_ratings, tournament_rosters)
    reused_team_features <- existing_team_features %>%
        dplyr::filter(Year %in% reused_historical_years)
    team_features <- dplyr::bind_rows(reused_team_features, refreshed_team_features) %>%
        normalize_team_features() %>%
        dplyr::arrange(suppressWarnings(as.integer(Year)), Region, Seed, Team)
    logger::log_info("Built canonical team feature dataset with {nrow(team_features)} rows")
    feature_counts <- count_rows_by_year(team_features, count_name = "team_feature_rows")
    logger::log_info("Post-build team-feature year coverage: {format_year_count_summary(feature_counts, 'team_feature_rows')}")

    available_feature_years <- unique(suppressWarnings(as.integer(team_features$Year)))
    available_feature_years <- available_feature_years[!is.na(available_feature_years)]
    completed_years <- intersect(
        suppressWarnings(as.integer(refresh_historical_years)),
        available_feature_years
    )
    logger::log_info(
        "Refreshing explicit tournament game results for completed years: {paste(completed_years, collapse = ', ')}"
    )
    refreshed_historical_results <- purrr::map_dfr(completed_years, function(year) {
        logger::log_info("Refreshing tournament results for {year}")
        scrape_tournament_results(year)
    })
    reused_historical_results <- existing_game_results %>%
        dplyr::filter(Year %in% reused_historical_years)

    logger::log_info("Refreshing current-year results for {bracket_year} from the scoreboard monitoring feed")
    current_year_results <- empty_game_results_table()
    current_year_fallback <- tryCatch(
        scrape_espn_tournament_results(bracket_year, team_features),
        error = function(err) {
            issue_message <- sprintf(
                "Skipping ESPN scoreboard fallback for %s: %s",
                bracket_year,
                err$message
            )
            logger::log_warn(issue_message)
            register_refresh_issues(
                append_refresh_issue(
                    empty_refresh_issues_table(),
                    step = "current_year_scoreboard_fallback",
                    source = "ESPN scoreboard API",
                    severity = "warning",
                    message = issue_message
                )
            )
            empty_game_results_table()
        }
    )
    if (nrow(current_year_fallback) == 0 &&
        !any(refresh_issues$step == "current_year_scoreboard_fallback")) {
        register_refresh_issues(
            append_refresh_issue(
                empty_refresh_issues_table(),
                step = "current_year_scoreboard_fallback",
                source = "ESPN scoreboard API",
                severity = "warning",
                message = sprintf(
                    "No completed current-year fallback results were available for %s; current-year rows remain empty until games are completed.",
                    bracket_year
                )
            )
        )
    }
    tournament_results <- dplyr::bind_rows(reused_historical_results, refreshed_historical_results, current_year_results)
    tournament_results <- merge_current_year_tournament_results(
        game_results = tournament_results,
        team_features = team_features,
        bracket_year = bracket_year,
        fallback_results = current_year_fallback
    )
    result_counts <- count_rows_by_year(tournament_results, count_name = "post_result_rows")
    logger::log_info("Post-build game-result year coverage: {format_year_count_summary(result_counts, 'post_result_rows')}")

    refresh_forensics <- build_refresh_forensics(
        intended_years = historical_years,
        bracket_year = bracket_year,
        pre_team_counts = pre_team_counts,
        pre_result_counts = pre_result_counts,
        ratings_counts = ratings_counts,
        roster_counts = roster_counts,
        feature_counts = feature_counts,
        result_counts = result_counts
    )
    forensic_summary <- summarize_refresh_forensics(refresh_forensics)

    omitted_years <- refresh_forensics %>%
        dplyr::filter(intended_year, !is.na(first_missing_stage))
    if (nrow(omitted_years) > 0) {
        omission_message <- sprintf(
            "Historical years omitted during refresh: %s",
            paste(sprintf("%s (%s)", omitted_years$Year, omitted_years$first_missing_stage), collapse = ", ")
        )
        logger::log_warn(omission_message)
        register_refresh_issues(
            append_refresh_issue(
                empty_refresh_issues_table(),
                step = "historical_year_coverage",
                source = "Canonical refresh coverage",
                severity = "warning",
                message = omission_message
            )
        )
    }

    unrecoverable_historical_years <- refresh_forensics %>%
        dplyr::filter(
            Year %in% as.character(required_historical_years),
            team_feature_rows == 0L | post_result_rows == 0L
        )
    available_historical_years <- refresh_forensics %>%
        dplyr::filter(
            Year %in% as.character(required_historical_years),
            team_feature_rows > 0L,
            post_result_rows > 0L
        ) %>%
        dplyr::pull(Year)
    unrecoverable_current_years <- refresh_forensics %>%
        dplyr::filter(
            Year == as.character(bracket_year),
            team_feature_rows == 0L
        )
    if (!explicit_start_year && nrow(unrecoverable_historical_years) > 0 && length(available_historical_years) > 0) {
        shortfall_message <- sprintf(
            "Configured historical window could not be fully produced; proceeding with %s available historical year%s instead of %s. Omitted years: %s",
            length(available_historical_years),
            ifelse(length(available_historical_years) == 1L, "", "s"),
            history_window_label(history_window),
            paste(sprintf("%s (%s)", unrecoverable_historical_years$Year, dplyr::coalesce(unrecoverable_historical_years$first_missing_stage, "unknown")), collapse = ", ")
        )
        logger::log_warn(shortfall_message)
        register_refresh_issues(
            append_refresh_issue(
                empty_refresh_issues_table(),
                step = "historical_window_shortfall",
                source = "Canonical refresh coverage",
                severity = "warning",
                message = shortfall_message
            )
        )
    }

    blocking_historical_years <- if (explicit_start_year || length(available_historical_years) == 0L) {
        unrecoverable_historical_years
    } else {
        unrecoverable_historical_years %>% dplyr::slice(0)
    }

    if (nrow(blocking_historical_years) > 0 || nrow(unrecoverable_current_years) > 0) {
        blocked_years <- dplyr::bind_rows(blocking_historical_years, unrecoverable_current_years) %>%
            dplyr::mutate(
                label = sprintf("%s (%s)", Year, dplyr::coalesce(first_missing_stage, "unknown"))
            )
        blocked_message <- sprintf(
            "Required refresh years could not be produced; canonical files were left unchanged: %s",
            paste(blocked_years$label, collapse = ", ")
        )
        logger::log_warn(blocked_message)
        register_refresh_issues(
            append_refresh_issue(
                empty_refresh_issues_table(),
                step = "required_refresh_years_unavailable",
                source = "Canonical refresh coverage",
                severity = "warning",
                message = blocked_message
            )
        )
        warning_summary <- summarize_refresh_issues(refresh_issues)
        return(list(
            team_features = team_features_file,
            game_results = game_results_file,
            refresh_log = refresh_log,
            status = "blocked",
            warning_count = nrow(refresh_issues),
            refresh_issues = refresh_issues,
            warning_summary = warning_summary,
            historical_cache_decisions = historical_cache_decisions,
            refresh_forensics = refresh_forensics,
            refresh_forensics_summary = forensic_summary,
            blocked_years = blocked_years %>% dplyr::select(-label)
        ))
    }

    overwritten_years <- refresh_forensics %>%
        dplyr::filter(
            intended_year,
            dropped_existing_team_features | dropped_existing_results
        )
    if (nrow(overwritten_years) > 0) {
        overwrite_message <- sprintf(
            "Preexisting canonical coverage would be overwritten for years: %s",
            sprintf(
                "%s (team_features=%s, results=%s, first_missing_stage=%s)",
                overwritten_years$Year,
                overwritten_years$dropped_existing_team_features,
                overwritten_years$dropped_existing_results,
                dplyr::coalesce(overwritten_years$first_missing_stage, "unknown")
            )
            |> paste(collapse = ", ")
        )
        logger::log_warn(overwrite_message)
        register_refresh_issues(
            append_refresh_issue(
                empty_refresh_issues_table(),
                step = "historical_year_overwrite_risk",
                source = "Canonical refresh coverage",
                severity = "warning",
                message = overwrite_message
            )
        )
    }

    logger::log_info("Running canonical data quality checks")
    assert_canonical_data_quality(team_features, tournament_results)
    logger::log_info("Canonical data quality checks passed; writing refreshed files")

    writexl::write_xlsx(team_features, team_features_file)
    writexl::write_xlsx(tournament_results, game_results_file)

    logger::log_info("Wrote team features to {team_features_file}")
    logger::log_info("Wrote tournament results to {game_results_file}")

    warning_summary <- summarize_refresh_issues(refresh_issues)
    status <- refresh_status_from_issues(refresh_issues)

    list(
        team_features = team_features_file,
        game_results = game_results_file,
        refresh_log = refresh_log,
        status = status,
        warning_count = nrow(refresh_issues),
        refresh_issues = refresh_issues,
        warning_summary = warning_summary,
        historical_cache_decisions = historical_cache_decisions,
        refresh_forensics = refresh_forensics,
        refresh_forensics_summary = forensic_summary
    )
}
