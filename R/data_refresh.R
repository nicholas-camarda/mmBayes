library(dplyr)
library(httr)
library(logger)
library(purrr)
library(rvest)
library(stringr)
library(writexl)

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

    page <- tryCatch(
        read_html_verified(url),
        error = function(error) {
            logger::log_warn("Skipping conference assignments for year {year}: {error$message}")
            NULL
        }
    )
    if (is.null(page)) {
        return(tibble::tibble(
            Year = character(),
            Team = character(),
            Seed = integer(),
            Region = character(),
            Conf = character()
        ))
    }

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
#' @return A game-level results table including region, round, teams, and winner.
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
        dplyr::select(Year, region, round, game_index, teamA, teamB, teamA_seed, teamB_seed, winner)

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
#' @return A game-level results table including region, round, teams, and winner.
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

#' Refresh canonical tournament data files
#'
#' @param start_year Optional starting year for the refresh window.
#' @param bracket_year The active bracket year.
#' @param history_window Number of historical tournaments to fetch by default.
#'
#' @return A list containing the written team-feature and game-results file paths.
#' @export
update_tournament_data <- function(start_year = NULL, bracket_year = as.integer(format(Sys.Date(), "%Y")), history_window = 8L) {
    team_features_file <- file.path("data", "pre_tournament_team_features.xlsx")
    game_results_file <- file.path("data", "tournament_game_results.xlsx")
    dir.create("data", showWarnings = FALSE, recursive = TRUE)

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
        scrape_conf_assignments(year)
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

    logger::log_info("Refreshing current-year First Four results for {bracket_year} when available")
    current_year_results <- scrape_tournament_results(bracket_year, allow_empty = TRUE)
    tournament_results <- dplyr::bind_rows(tournament_results, current_year_results)

    logger::log_info("Running canonical data quality checks")
    assert_canonical_data_quality(team_features, tournament_results)
    logger::log_info("Canonical data quality checks passed; writing refreshed files")

    writexl::write_xlsx(team_features, team_features_file)
    writexl::write_xlsx(tournament_results, game_results_file)

    logger::log_info("Wrote team features to {team_features_file}")
    logger::log_info("Wrote tournament results to {game_results_file}")

    list(
        team_features = team_features_file,
        game_results = game_results_file
    )
}
