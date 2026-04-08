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
    history_window <- suppressWarnings(as.integer(history_window))
    min_year <- suppressWarnings(as.integer(min_year))

    if (is.na(bracket_year) || is.na(history_window) || is.na(min_year) || history_window < 1L) {
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
#' @param bart_counts Bart rating year counts.
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
                                    bart_counts,
                                    roster_counts,
                                    feature_counts,
                                    result_counts) {
    all_years <- sort(unique(c(
        as.integer(intended_years),
        suppressWarnings(as.integer(pre_team_counts$Year)),
        suppressWarnings(as.integer(pre_result_counts$Year)),
        suppressWarnings(as.integer(bart_counts$Year)),
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
        dplyr::left_join(bart_counts, by = "Year") %>%
        dplyr::left_join(roster_counts, by = "Year") %>%
        dplyr::left_join(feature_counts, by = "Year") %>%
        dplyr::left_join(result_counts, by = "Year") %>%
        dplyr::mutate(
            dplyr::across(
                c(
                    pre_team_feature_rows,
                    pre_result_rows,
                    bart_rating_rows,
                    conference_assignment_rows,
                    team_feature_rows,
                    post_result_rows
                ),
                ~ dplyr::coalesce(as.integer(.x), 0L)
            ),
            intended_year = Year %in% as.character(intended_years),
            completed_year = Year %in% completed_years,
            first_missing_stage = dplyr::case_when(
                intended_year & team_feature_rows == 0L & bart_rating_rows == 0L ~ "bart_ratings",
                intended_year & team_feature_rows == 0L & conference_assignment_rows == 0L ~ "conference_assignments",
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

#' Scrape Bart Torvik pre-tournament team features
#'
#' @param year The tournament year to scrape.
#'
#' @return A year-wide team-level feature table for the requested year.
#' @keywords internal
scrape_bart_data <- function(year) {
    url <- paste0(
        "https://barttorvik.com/?year=", year,
        "&sort=&hteam=&t2value=&conlimit=All",
        "&state=All&begin=", year - 1, "1101",
        "&end=", year, "0501",
        "&top=0&revquad=0&quad=5&venue=All&type=All&mingames=0#"
    )
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
        dplyr::select(
            Year,
            Team,
            team_key,
            Seed,
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
        dplyr::filter(teams != 68L)
    if (nrow(yearly_counts) > 0) {
        stop_with_message(
            sprintf(
                "Tournament roster must contain exactly 68 teams per year: %s",
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

    region_counts <- roster %>%
        dplyr::count(Year, Region, name = "teams") %>%
        dplyr::filter(teams < 16L | teams > 18L)
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
            dplyr::filter(duplicate_slots != 4L)
        if (nrow(duplicate_slot_years) > 0) {
            stop_with_message(
                sprintf(
                    "Tournament roster must contain exactly four play-in seed slots per year: %s",
                    paste(sprintf("%s=%s", duplicate_slot_years$Year, duplicate_slot_years$duplicate_slots), collapse = ", ")
                )
            )
        }
    }

    roster
}

#' Build the canonical team feature dataset
#'
#' @param bart_data A pre-tournament feature table scraped from Bart Torvik.
#' @param conf_assignments A conference-assignment lookup table.
#'
#' @return A normalized pre-tournament team feature table.
#' @keywords internal
build_team_feature_dataset <- function(bart_data, conf_assignments) {
    roster <- validate_tournament_roster(conf_assignments, require_seed = FALSE)
    ratings <- bart_data %>%
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
                "Bart ratings contain duplicate teams for the same year: %s",
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
                    Seed,
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
            Seed = dplyr::coalesce(Seed.x, Seed.y)
        )

    unresolved_roster <- joined %>%
        dplyr::filter(dplyr::if_any(c(Barthag, AdjOE, AdjDE, WAB, TOR, TORD, ORB, DRB, `3P%`, `3P%D`, `Adj T.`), is.na)) %>%
        dplyr::distinct(Year, Team)
    if (nrow(unresolved_roster) > 0) {
        stop_with_message(
            sprintf(
                "Year-wide Bart ratings did not cover every tournament team: %s",
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
#' @param counter The game counter within the region section.
#'
#' @return A single round label.
#' @keywords internal
results_round_label <- function(region_name, counter) {
    if (region_name == "National") {
        if (counter <= 2) {
            return("Final Four")
        }
        return("Championship")
    }

    if (counter <= 8) {
        return("Round of 64")
    }
    if (counter <= 12) {
        return("Round of 32")
    }
    if (counter <= 14) {
        return("Sweet 16")
    }
    "Elite 8"
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

#' Parse a single-team score line in Sports-Reference bracket text
#'
#' @param line A line of scraped tournament text.
#'
#' @return A one-row tibble with `seed`, `team`, and `score`, or `NULL`.
#' @keywords internal
parse_tournament_team_result_line <- function(line) {
    pattern <- "^team\\s+\\(?([0-9]{1,2})\\)?\\s+(.+?)\\s+(\\d+)$"
    matched <- stringr::str_match(line, stringr::regex(pattern, ignore_case = TRUE))
    if (all(is.na(matched))) {
        return(NULL)
    }

    tibble::tibble(
        seed = as.integer(matched[, 2]),
        team = canonicalize_team_name(stringr::str_trim(matched[, 3])),
        score = as.integer(matched[, 4])
    )
}

#' Parse a team-only bracket line without a score
#'
#' @param line A line of scraped tournament text.
#'
#' @return A one-row tibble with `seed` and `team`, or `NULL`.
#' @keywords internal
parse_tournament_team_name_line <- function(line) {
    pattern <- "^team\\s+\\(?([0-9]{1,2})\\)?\\s+(.+?)$"
    matched <- stringr::str_match(line, stringr::regex(pattern, ignore_case = TRUE))
    if (all(is.na(matched))) {
        return(NULL)
    }

    tibble::tibble(
        seed = as.integer(matched[, 2]),
        team = canonicalize_team_name(stringr::str_trim(matched[, 3]))
    )
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
    lines <- lines %>%
        stringr::str_trim() %>%
        .[nzchar(.)]

    region_alias <- c("East", "Midwest", "South", "West", "National")
    bracket_started <- FALSE
    first_four_counter <- 0L
    main_bracket_counter <- 0L
    regional_order <- c("East", "Midwest", "South", "West")
    pending_game_team <- NULL
    pending_orphan_team <- NULL
    current_first_four_region <- NULL
    results <- list()

    for (line in lines) {
        if (line %in% region_alias) {
            bracket_started <- TRUE
            pending_game_team <- NULL
            pending_orphan_team <- NULL
            next
        }

        first_four_region <- stringr::str_match(
            line,
            stringr::regex("^(East|West|South|Midwest)\\s+First Four$", ignore_case = TRUE)
        )[, 2]
        if (!is.na(first_four_region)) {
            bracket_started <- TRUE
            pending_game_team <- NULL
            pending_orphan_team <- NULL
            current_first_four_region <- first_four_region
            next
        }

        if (!bracket_started) {
            next
        }

        is_dayton_play_in <- grepl("at Dayton, OH", line, fixed = TRUE)

        parsed <- parse_tournament_game_line(line)
        if (is.null(parsed)) {
            parsed_team <- parse_tournament_team_result_line(line)
            if (is.null(parsed_team)) {
                orphan_team <- parse_tournament_team_name_line(line)
                if (!is.null(orphan_team)) {
                    if (!is.null(pending_orphan_team)) {
                        current_first_four_region <- NULL
                        main_bracket_counter <- main_bracket_counter + 1L
                        pending_orphan_team <- NULL
                    } else {
                        pending_orphan_team <- orphan_team
                    }
                    next
                }
                next
            }

            pending_orphan_team <- NULL
            if (is.null(pending_game_team)) {
                pending_game_team <- parsed_team
                next
            }

            parsed <- tibble::tibble(
                teamA_seed = pending_game_team$seed,
                teamA = pending_game_team$team,
                teamA_score = pending_game_team$score,
                teamB_seed = parsed_team$seed,
                teamB = parsed_team$team,
                teamB_score = parsed_team$score
            )
            pending_game_team <- NULL
        } else {
            pending_game_team <- NULL
            pending_orphan_team <- NULL
        }

        is_equal_seed_matchup <- identical(parsed$teamA_seed, parsed$teamB_seed)
        is_explicit_first_four <- !is.null(current_first_four_region) && is_equal_seed_matchup
        is_dayton_first_four <- is_dayton_play_in && is_equal_seed_matchup

        if (is_explicit_first_four || is_dayton_first_four) {
            first_four_counter <- first_four_counter + 1L
            results[[length(results) + 1L]] <- parsed %>%
                dplyr::mutate(
                    Year = as.character(year),
                    region = "First Four",
                    round = "First Four",
                    game_index = first_four_counter,
                    winner = ifelse(teamA_score > teamB_score, teamA, teamB)
                )
            next
        }

        current_first_four_region <- NULL
        main_bracket_counter <- main_bracket_counter + 1L

        if (main_bracket_counter <= 60L) {
            current_region_index <- as.integer((main_bracket_counter - 1L) %/% 15L) + 1L
            current_region_counter <- ((main_bracket_counter - 1L) %% 15L) + 1L
            current_region <- regional_order[[current_region_index]]
            round_name <- results_round_label(current_region, current_region_counter)
            round_game_index <- dplyr::case_when(
                round_name == "Round of 64" ~ current_region_counter,
                round_name == "Round of 32" ~ current_region_counter - 8L,
                round_name == "Sweet 16" ~ current_region_counter - 12L,
                TRUE ~ 1L
            )

            results[[length(results) + 1L]] <- parsed %>%
                dplyr::mutate(
                    Year = as.character(year),
                    region = current_region,
                    round = round_name,
                    game_index = round_game_index,
                    winner = ifelse(teamA_score > teamB_score, teamA, teamB)
                )
            next
        }

        if (main_bracket_counter > 63L) {
            next
        }

        national_counter <- main_bracket_counter - 60L
        results[[length(results) + 1L]] <- parsed %>%
            dplyr::mutate(
                Year = as.character(year),
                region = "National",
                round = results_round_label("National", national_counter),
                game_index = ifelse(national_counter <= 2L, national_counter, 1L),
                winner = ifelse(teamA_score > teamB_score, teamA, teamB)
            )
    }

    final_results <- dplyr::bind_rows(results) %>%
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

    if (nrow(final_results) == 0) {
        stop_with_message(sprintf("No tournament results parsed for year %s", year))
    }

    final_results
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
    url <- sprintf("https://www.sports-reference.com/cbb/postseason/men/%s-ncaa.html", year)
    logger::log_info("Scraping tournament results from {url}")

    page_text <- tryCatch(
        rvest::read_html(url) %>%
            rvest::html_text2(),
        error = function(err) {
            if (!isTRUE(allow_empty)) {
                stop(err)
            }
            logger::log_warn("Current-year results page for {year} was unavailable: {conditionMessage(err)}")
            return(NULL)
        }
    )
    if (is.null(page_text)) {
        return(empty_game_results_table())
    }

    lines <- page_text %>%
        stringr::str_split("\\n", simplify = FALSE) %>%
        purrr::pluck(1)

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
    register_refresh_issues <- function(issues) {
        issues <- issues %||% empty_refresh_issues_table()
        if (nrow(issues) > 0) {
            refresh_issues <<- dplyr::bind_rows(refresh_issues, issues)
        }
        invisible(NULL)
    }

    if (is.null(start_year)) {
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

    bart_data <- purrr::map_dfr(scrape_years, function(year) {
        logger::log_info("Refreshing Bart ratings for {year}")
        scrape_bart_data(year)
    })
    bart_counts <- count_rows_by_year(bart_data, count_name = "bart_rating_rows")

    conf_assignments <- purrr::map_dfr(scrape_years, function(year) {
        logger::log_info("Refreshing tournament roster for {year}")
        conf_tbl <- scrape_conf_assignments(year)
        register_refresh_issues(attr(conf_tbl, "refresh_issues", exact = TRUE))
        conf_tbl
    })
    roster_counts <- count_rows_by_year(conf_assignments, count_name = "conference_assignment_rows")

    logger::log_info("Joining tournament rosters to year-wide Bart ratings")
    refreshed_team_features <- build_team_feature_dataset(bart_data, conf_assignments)
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
        bart_counts = bart_counts,
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
    unrecoverable_current_years <- refresh_forensics %>%
        dplyr::filter(
            Year == as.character(bracket_year),
            team_feature_rows == 0L
        )
    if (nrow(unrecoverable_historical_years) > 0 || nrow(unrecoverable_current_years) > 0) {
        blocked_years <- dplyr::bind_rows(unrecoverable_historical_years, unrecoverable_current_years) %>%
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
