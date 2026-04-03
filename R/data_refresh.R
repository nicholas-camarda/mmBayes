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

    headline <- if (identical(status, "degraded_success")) {
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
        sprintf("- Refresh log: %s", log_path %||% "n/a")
    )

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

    page_result <- tryCatch(
        list(
            page = read_html_verified(url),
            issues = empty_refresh_issues_table()
        ),
        error = function(error) {
            issue_message <- sprintf(
                "Skipping conference assignments for year %s: %s",
                year,
                error$message
            )
            logger::log_warn(issue_message)
            issue_table <- empty_refresh_issues_table() %>%
                append_refresh_issue(
                    step = "conference_assignments",
                    source = "Bart Torvik Tourney Time",
                    severity = "warning",
                    message = issue_message
                )
            list(page = NULL, issues = issue_table)
        }
    )
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
#'
#' @return A character vector of `YYYYMMDD` date keys covering the First Four
#'   window.
#' @keywords internal
tournament_scoreboard_dates <- function(bracket_year) {
    format(
        seq.Date(
            as.Date(sprintf("%s-03-15", bracket_year)),
            as.Date(sprintf("%s-04-15", bracket_year)),
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
#' @param history_window Number of historical tournaments to fetch by default.
#'
#' @param config Optional project configuration list. When omitted, the loaded
#'   project config is used.
#'
#' @return A list containing the written team-feature and game-results file paths.
#' @export
update_tournament_data <- function(config = NULL, start_year = NULL, bracket_year = as.integer(format(Sys.Date(), "%Y")), history_window = 8L) {
    config <- config %||% load_project_config()
    team_features_file <- config$data$team_features_path %||% file.path(default_cloud_data_root(), "pre_tournament_team_features.xlsx")
    game_results_file <- config$data$game_results_path %||% file.path(default_cloud_data_root(), "tournament_game_results.xlsx")
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

    start_year <- start_year %||% max(2008L, bracket_year - history_window - 2L)
    historical_years <- seq.int(start_year, bracket_year)
    historical_years <- historical_years[historical_years != 2020L]

    logger::log_info(
        "Starting canonical data refresh for bracket year {bracket_year} across years {min(historical_years)}-{max(historical_years)}"
    )

    bart_data <- purrr::map_dfr(historical_years, function(year) {
        logger::log_info("Refreshing Bart ratings for {year}")
        scrape_bart_data(year)
    })

    conf_assignments <- purrr::map_dfr(historical_years, function(year) {
        logger::log_info("Refreshing tournament roster for {year}")
        conf_tbl <- scrape_conf_assignments(year)
        register_refresh_issues(attr(conf_tbl, "refresh_issues", exact = TRUE))
        conf_tbl
    })

    logger::log_info("Joining tournament rosters to year-wide Bart ratings")
    team_features <- build_team_feature_dataset(bart_data, conf_assignments)
    logger::log_info("Built canonical team feature dataset with {nrow(team_features)} rows")

    available_feature_years <- unique(suppressWarnings(as.integer(team_features$Year)))
    available_feature_years <- available_feature_years[!is.na(available_feature_years)]
    completed_years <- intersect(
        historical_years[historical_years < bracket_year],
        available_feature_years
    )
    logger::log_info(
        "Refreshing explicit tournament game results for completed years: {paste(completed_years, collapse = ', ')}"
    )
    tournament_results <- purrr::map_dfr(completed_years, function(year) {
        logger::log_info("Refreshing tournament results for {year}")
        scrape_tournament_results(year)
    })

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
    tournament_results <- dplyr::bind_rows(tournament_results, current_year_results)
    tournament_results <- merge_current_year_tournament_results(
        game_results = tournament_results,
        team_features = team_features,
        bracket_year = bracket_year,
        fallback_results = current_year_fallback
    )

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
        status = status,
        warning_count = nrow(refresh_issues),
        refresh_issues = refresh_issues,
        warning_summary = warning_summary
    )
}
