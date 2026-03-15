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
        Team = stringr::str_trim(Team)
    ) %>%
        dplyr::select(Year, Team, Seed, Region, Conf)
}

#' Scrape Bart Torvik pre-tournament team features
#'
#' @param year The tournament year to scrape.
#' @param region The region filter to request from Bart Torvik.
#'
#' @return A team-level feature table for the requested year and region.
#' @keywords internal
scrape_bart_data <- function(year, region) {
    url <- paste0(
        "https://barttorvik.com/?year=", year,
        "&sort=&hteam=&t2value=&conlimit=", region,
        "&state=All&begin=", year - 1, "1101",
        "&end=", year, "0501",
        "&top=0&revquad=0&quad=5&venue=All&type=All&mingames=0#"
    )
    logger::log_info("Scraping Bart data from {url}")

    page <- read_html_verified(url)
    table <- extract_primary_table(page, url, expected_header_tokens = c("Rk", "Team", "G", "AdjOE", "AdjDE", "Barthag"))

    column_names <- as.character(table[1, ])
    colnames(table) <- column_names

    table %>%
        dplyr::slice(-1) %>%
        dplyr::filter(stringr::str_detect(Team, "\\bseed\\b")) %>%
        dplyr::mutate(
            Year = as.character(year),
            Region = region,
            Rk = as.numeric(Rk),
            G = as.numeric(G),
            dplyr::across(c(`AdjOE`, `AdjDE`, `Barthag`, `TOR`, `TORD`, `ORB`, `DRB`, `3P%`, `3P%D`, `Adj T.`, `WAB`), as.numeric),
            temp = stringr::str_match(Team, "^(.*?)\\s+(\\d+)\\s*seed"),
            Team_clean = stringr::str_trim(temp[, 2]),
            Seed_clean = as.numeric(temp[, 3])
        ) %>%
        dplyr::select(-temp) %>%
        dplyr::mutate(
            Team = Team_clean,
            Seed = Seed_clean,
            .before = 5
        ) %>%
        dplyr::select(-Team_clean, -Seed_clean)
}

#' Build the canonical team feature dataset
#'
#' @param bart_data A pre-tournament feature table scraped from Bart Torvik.
#' @param conf_assignments A conference-assignment lookup table.
#'
#' @return A normalized pre-tournament team feature table.
#' @keywords internal
build_team_feature_dataset <- function(bart_data, conf_assignments) {
    bart_data %>%
        dplyr::left_join(
            conf_assignments,
            by = c("Year", "Team", "Seed", "Region"),
            suffix = c("", "_conf")
        ) %>%
        dplyr::mutate(
            Conf = dplyr::coalesce(Conf_conf, Conf)
        ) %>%
        dplyr::select(
            Year,
            Team,
            Seed,
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
        teamA = stringr::str_trim(matched[, 3]),
        teamA_score = as.integer(matched[, 4]),
        teamB_seed = as.integer(matched[, 5]),
        teamB = stringr::str_trim(matched[, 6]),
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
        team = stringr::str_trim(matched[, 3]),
        score = as.integer(matched[, 4])
    )
}

#' Scrape explicit tournament game results
#'
#' @param year The tournament year to scrape.
#'
#' @return A game-level results table including region, round, teams, and winner.
#' @keywords internal
scrape_tournament_results <- function(year) {
    url <- sprintf("https://www.sports-reference.com/cbb/postseason/men/%s-ncaa.html", year)
    logger::log_info("Scraping tournament results from {url}")

    page_text <- rvest::read_html(url) %>%
        rvest::html_text2()
    lines <- page_text %>%
        stringr::str_split("\\n", simplify = FALSE) %>%
        purrr::pluck(1) %>%
        stringr::str_trim() %>%
        .[nzchar(.)]

    region_alias <- c("East", "West", "South", "Midwest", "National")
    section <- NULL
    regional_counter <- 0L
    national_counter <- 0L
    first_four_counter <- 0L
    pending_game_team <- NULL
    next_game_is_first_four <- FALSE
    results <- list()

    for (line in lines) {
        if (line %in% region_alias) {
            section <- line
            pending_game_team <- NULL
            next_game_is_first_four <- FALSE
            if (line == "National") {
                national_counter <- 0L
            } else {
                regional_counter <- 0L
            }
            next
        }

        first_four_region <- stringr::str_match(
            line,
            stringr::regex("^(East|West|South|Midwest)\\s+First Four$", ignore_case = TRUE)
        )[, 2]
        if (!is.na(first_four_region)) {
            section <- first_four_region
            pending_game_team <- NULL
            next_game_is_first_four <- TRUE
            next
        }

        if (is.null(section)) {
            next
        }

        parsed <- parse_tournament_game_line(line)
        if (is.null(parsed)) {
            parsed_team <- parse_tournament_team_result_line(line)
            if (is.null(parsed_team)) {
                next
            }

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
        }

        if (isTRUE(next_game_is_first_four)) {
            first_four_counter <- first_four_counter + 1L
            results[[length(results) + 1L]] <- parsed %>%
                dplyr::mutate(
                    Year = as.character(year),
                    region = "First Four",
                    round = "First Four",
                    game_index = first_four_counter,
                    winner = ifelse(teamA_score > teamB_score, teamA, teamB)
                )
            next_game_is_first_four <- FALSE
            next
        }

        if (identical(section, "National")) {
            national_counter <- national_counter + 1L
            results[[length(results) + 1L]] <- parsed %>%
                dplyr::mutate(
                    Year = as.character(year),
                    region = "National",
                    round = results_round_label("National", national_counter),
                    game_index = ifelse(national_counter <= 2L, national_counter, 1L),
                    winner = ifelse(teamA_score > teamB_score, teamA, teamB)
                )
            next
        }

        if (section %in% c("East", "West", "South", "Midwest")) {
            regional_counter <- regional_counter + 1L
            round_name <- results_round_label(section, regional_counter)
            round_game_index <- dplyr::case_when(
                round_name == "Round of 64" ~ regional_counter,
                round_name == "Round of 32" ~ regional_counter - 8L,
                round_name == "Sweet 16" ~ regional_counter - 12L,
                TRUE ~ 1L
            )

            results[[length(results) + 1L]] <- parsed %>%
                dplyr::mutate(
                    Year = as.character(year),
                    region = section,
                    round = round_name,
                    game_index = round_game_index,
                    winner = ifelse(teamA_score > teamB_score, teamA, teamB)
                )
        }
    }

    final_results <- dplyr::bind_rows(results) %>%
        dplyr::select(Year, region, round, game_index, teamA, teamB, teamA_seed, teamB_seed, winner)

    if (nrow(final_results) == 0) {
        stop_with_message(sprintf("No tournament results parsed for year %s", year))
    }

    final_results
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
    regions <- c("East", "West", "Midwest", "South")

    bart_data <- purrr::map_dfr(historical_years, function(year) {
        purrr::map_dfr(regions, function(region) scrape_bart_data(year, region))
    })

    conf_assignments <- purrr::map_dfr(historical_years, scrape_conf_assignments)
    team_features <- build_team_feature_dataset(bart_data, conf_assignments)

    completed_years <- historical_years[historical_years < bracket_year]
    tournament_results <- purrr::map_dfr(completed_years, scrape_tournament_results)

    writexl::write_xlsx(team_features, team_features_file)
    writexl::write_xlsx(tournament_results, game_results_file)

    list(
        team_features = team_features_file,
        game_results = game_results_file
    )
}
