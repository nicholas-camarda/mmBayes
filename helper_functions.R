
my_get_game_ids <- function(team, season = current_season) {
    if (is.na(team)) {
        stop("team is missing with no default")
    }
    if (!"ncaahoopR" %in% .packages()) {
        ids <- create_ids_df()
    }
    base_url <- "https://www.espn.com/mens-college-basketball/team/schedule/_/id/"
    url <- paste0(
        base_url, ids$id[ids$team == team], "/season/",
        as.numeric(substring(season, 1, 4)) + 1
    )
    x <- RCurl::getURL(url)
    x <- strsplit(x, "gameId")[[1]]
    game_ids <- gsub("[^0-9]*", "", gsub("\".*", "", x))[-1]
    matches <- stringr::str_match_all(
        x,
        "<span[^>]*>((Mon|Tue|Wed|Thu|Fri|Sat|Sun),\\s+[A-Za-z]+\\s+\\d+)"
    )
    dates <- unlist(lapply(matches, function(m) m[, 2]))
    game_ids <- game_ids[!duplicated(game_ids)]
    played_dates <- purrr::map_chr(dates, dplyr::last)
    played_dates <- played_dates[!is.na(played_dates)]
    unplayed_dates <- unlist(purrr::map(dates, ~ {
        if (length(.x) > 1) {
            .x[1:(length(.x) - 1)]
        }
    }))

    # Wrap the game_ids subsetting in a tryCatch to handle edge cases
    game_ids <- tryCatch(
        {
            if (length(unplayed_dates) == 0 & length(game_ids) > length(played_dates)) {
                delta <- length(game_ids) - length(played_dates)
                indices <- 1:(length(played_dates) - delta + 1)
                if (min(indices) <= 0) {
                    warning("Calculated indices are invalid (negative or zero); returning empty game_ids")
                    character(0)
                } else {
                    game_ids[c(indices, length(game_ids))]
                }
            } else if (length(unplayed_dates) > 0) {
                unique(c(
                    game_ids[1:length(played_dates)],
                    game_ids[(length(game_ids) - length(unplayed_dates) + 1):length(game_ids)]
                ))
            } else {
                game_ids
            }
        },
        error = function(e) {
            warning("Error encountered while processing game_ids: ", e$message)
            character(0)
        }
    )

    df_id <- dplyr::tibble(
        game_id = game_ids[seq_along(c(played_dates, unplayed_dates))],
        date = c(played_dates, unplayed_dates)
    )
    df_id <- dplyr::mutate(df_id, year = ifelse(grepl("Nov|Dec", date),
        substring(season, 1, 4),
        paste0("20", substring(season, 6, 7))
    ))
    df_id <- dplyr::mutate(df_id, date = as.Date(paste(year, date), "%Y %a, %b %d"))
    df_id <- dplyr::arrange(df_id, date)
    game_ids <- df_id$game_id
    return(game_ids)
}

#' @note retry this if it returns an empty character string
#' (probably due to frying the server / network bandwidth)
get_game_ids_retry <- function(team, year, n_tries = 10) {
    game_ids <- character(0)
    for (i in 1:n_tries) {
        game_ids <- my_get_game_ids(team = team, season = year)
        if (!identical(game_ids, character(0))) {
            break
        }
        message("Retrying...")
        Sys.sleep(10) # Wait for 10 seconds before retrying
    }
    return(game_ids)
}


# Define function to scrape data for a single team and year
#' @note this function takes a team (ESPN name) and a year, and writes the season raw data for that team that year
#' @param team (ESPN name)
#' @param season in format 2019-20
#' @return nothing - writes the data to directory for later processing
scrape_data <- function(team, year, bracket_year, validated_teams_with_bpi) {
    # did they play any games?
    # team = "Arkansas"; year = "2018-19"
    # team = t; year = y

    game_ids <- get_game_ids_retry(team = team, year = year)
    if (length(game_ids) == 0) {
        # if not, return empty tibble
        message(qq("Couldn't find any data for @{team} for season @{year}"))
        return(tibble())
    }

    # DEBUG:
    # team = "Texas A&M-CC"; year = "2016-17"
    # team = "UConn"; year = "2017-18"
    # team = "Princeton"; year = "2016-17"

    library(ncaahoopR)
    data <- lapply(game_ids, FUN = function(g) {
        # print(g)
        # g = game_ids[1]
        # extract team level data for that game

        #' @note get the boxscore safely by wrapping in tryCatch
        #' @param g the game_id
        #' @return boxscore lst_df, or NULL --> which then results in an empty tibble
        #' getting returned for the loop for this g
        get_boxscore_safely <- function(g) {
            tryCatch(
                {
                    ncaahoopR::get_boxscore(game_id = g)
                },
                error = function(e) {
                    message(paste("Error while processing game_id:", g, "\nError message:", e$message))
                    message("Returning empty tibble and continuing...")
                    return(NULL) # Return NULL to indicate an error occurred
                }
            )
        }

        lst_res <- get_boxscore_safely(g)

        if (is.null(lst_res)) {
            return(tibble())
        }
        #' @note The team names are ESPN_PBP, so they need to be matched to NCAA/ESPN names
        #' @param lst lst of data frames, returned from get_boxscore function
        #' @param team the team name in question, character
        #' @param mapping_df the mapping between espn, ncaa, and espn_pbp names
        #' @return the index in lst of our team, after name matching
        find_team_index <- function(lst, team, mapping_df) {
            possible_names <- mapping_df %>%
                filter(TEAM == team) %>%
                dplyr::select(ESPN_PBP, ESPN.x, ESPN.y) %>%
                pivot_longer(c(ESPN_PBP, ESPN.x, ESPN.y), names_to = "name_type", values_to = "team_name") %>%
                stringdist_inner_join(tibble(team_name = names(lst)),
                    max_dist = 0.5,
                    method = "jw",
                    by = "team_name",
                    distance_col = "dist"
                ) %>%
                slice_min(dist, with_ties = FALSE, n = 1) %>%
                pull(team_name.y)

            # # hard code this example because it's annoying and the only exception apparently
            # if (team == "Texas A&M-CC") possible_names <- "Texas A&M-Corpus Christi"

            matching_name <- intersect(names(lst), possible_names)

            if (length(matching_name) == 1) {
                return(which(names(lst) == matching_name))
            } else {
                return(NULL)
            }
        }

        ref_team_idx <- find_team_index(
            lst = lst_res,
            team = team,
            mapping_df = validated_teams_with_bpi
        )

        # if the game_id results in no boxscore, return an empty tibble
        # to avoid breaking everything
        if (is.null(ref_team_idx)) {
            # print(g)
            return(tibble())
        }

        # otherwise continue on... get index of ref team
        ref_team <- lst_res[[ref_team_idx]]
        # get their total points
        ref_team_score <- ref_team %>%
            filter(player == "TEAM") %>%
            .$PTS
        # get index of other team
        if (ref_team_idx != 1) {
            other_idx <- 1
        } else {
            other_idx <- 2
        }
        # get opponent total points
        opp_team_score <- lst_res[[other_idx]] %>%
            filter(player == "TEAM") %>%
            .$PTS
        # check whether ref team beat the opp
        won_boolean <- ref_team_score > opp_team_score
        # return the ref team data with whether or not they won
        result <- ref_team %>%
            mutate(won_boolean = won_boolean, game_id = g)
        return(result)
    }) %>%
        bind_rows() %>%
        # also return the season in question
        mutate(season = year) %>%
        filter(player != "TEAM")

    # # extract season box score for the year, for the team
    # data <- ncaahoopR::season_boxscore(team = team, season = year, aggregate = "raw") %>%
    #     as_tibble()

    # save the year in data
    # data$season <- year

    # write the the player level statistics to file, to be compressed to team level statistics later on
    dir_to_write <- file.path("cache", team)
    dir.create(dir_to_write, showWarnings = FALSE, recursive = TRUE)
    write_tsv(data, file = file.path(dir_to_write, qq("@{team}_@{year}.tsv")))
}

#' @note run all of these functions in a loop to extract the team info
get_game_info_loop <- function(teams, years, bracket_year = 2023, validated_teams_with_bpi, num_sec = 5) {
    message("Scraping NCAA data using ncaahoopR")

    all_seasons <- years$season

    final_teams_temp <- tibble(name = validated_teams_with_bpi$TEAM) %>%
        mutate(
            num_fns = map_int(name, .f = function(f) {
                dir(file.path("cache", f), recursive = TRUE) %>% length()
            }),
            n_years = map(name, .f = function(x) {
                res <- str_extract(dir(file.path("cache", x), recursive = TRUE, full.names = TRUE), "[0-9]{4}-[0-9]{2}")
                if (length(res) == 0) {
                    return(NA)
                }
                return(res)
            })
        ) %>%
        group_by(name) %>%
        unnest(n_years) %>%
        # Create a new column that checks for each group if all seasons are present
        mutate(all_seasons_present = all(all_seasons %in% n_years)) %>%
        # Then you filter out the teams with all seasons present
        filter(!all_seasons_present) %>%
        # Optionally, you can remove the 'all_seasons_present' column if you don't need it anymore
        select(-all_seasons_present)

    final_teams <- final_teams_temp %>%
        .$name %>%
        unique()

    # pb <- progressr::progressor(steps = length(final_teams) * nrow(years))

    # final_teams <- teams

    message("Teams to analyze:")
    print(final_teams)
    print(years)

    # walk(final_teams, .f = function(t) {
    #     walk(years$season, .f = function(y) {
    #         # t <- final_teams[1]; team <- t
    #         # y <- years$season[1]; year <- y
    #         scrape_data(team = t, year = y, bracket_year, validated_teams_with_bpi)
    #     })
    # })

    # important: always future the outer loop!
    not_used_result <- foreach(idx = seq_along(final_teams)) %dopar% {
        library(tidyverse)
        library(GetoptLong)
        library(ncaahoopR)
        library(readxl)
        library(fuzzyjoin)
        library(tictoc)
        library(foreach)
        library(doParallel)

        source(file.path("helper_functions.R"))
        # idx <- 1
        t <- final_teams[idx]
        message(t)
        for (y in years$season) {
            message(qq("\nTeam = @{t} for year @{y}\n"))
            # y = years$season[1]
            # use a trycatch statement in case there's a problem with the scrape
            tryCatch(
                {
                    scrape_data(team = t, year = y, bracket_year, validated_teams_with_bpi)
                    # pb(message = qq("Processed team @{t} for season @{y}"))
                },
                error = function(e) {
                    message("Error: ", e$message, " for team: ", t, " year: ", y)
                }
            )
            # cat(".")
        }
        # wait X seconds then move to the next team so we don't fry the server
        Sys.sleep(num_sec)
    }

    message("Done!")
}


generate_initial_matchups <- function(data_summary) {
    # You may need to modify this function to match the actual tournament structure
    initial_matchups <- data.frame()

    for (i in 1:(nrow(data_summary) / 2)) {
        team_1 <- data_summary[data_summary$SEED == i, "TEAM"]
        team_2 <- data_summary[data_summary$SEED == (nrow(data_summary) + 1 - i), "TEAM"]
        matchup <- data.frame(TEAM = team_1, opponent = team_2)
        initial_matchups <- rbind(initial_matchups, matchup)
    }

    return(initial_matchups)
}

simulate_tournament <- function(post_sample, data_summary) {
    matchups <- generate_initial_matchups(data_summary)
    bracket <- list()

    for (round in 1:6) {
        probabilities <- posterior_predict(post_sample, newdata = matchups)
        winners <- simulate_round(matchups, probabilities)
        bracket[[round]] <- winners

        if (round < 6) {
            matchups <- update_matchups_for_next_round(matchups, winners)
        }
    }

    return(bracket)
}

update_matchups_for_next_round <- function(matchups, winners) {
    next_round_matchups <- data.frame()

    for (i in seq(1, length(winners), 2)) {
        winning_team_1 <- winners[i]
        winning_team_2 <- winners[i + 1]
        matchup <- data.frame(TEAM = winning_team_1, opponent = winning_team_2)
        next_round_matchups <- rbind(next_round_matchups, matchup)
    }

    return(next_round_matchups)
}

# generate teams data using the valid team ids from ncaahoopR and the teams pulled from ESPN for current tournament season
# generate_teams_data <- function(curr_teams, valid_teams_mapping) {
#     # fuzzy match the team names that are in the tournament to the valid team names in ncaahoopR
#     (vtm <- valid_teams_mapping %>% mutate(TEAM = ESPN, .before = 1))
#     teams_df_ <- stringdist_join(curr_teams, vtm,
#         by = "TEAM", # match based on team
#         mode = "inner", # use left join
#         method = "jw", # use jw distance metric
#         max_dist = 99,
#         distance_col = "dist"
#     ) %>%
#         group_by(TEAM.x) %>%
#         slice_min(order_by = dist, n = 1) %>%
#         mutate(TEAM = ESPN, .before = 4) %>%
#         dplyr::select(TEAM, BPI, BPI_RANK, NCAA, ESPN, CURR_TEAM_ID = TEAM.x, VALID_TEAM_ID_MATCH = TEAM.y) %>%
#         ungroup() %>%
#         inner_join(vtm %>% dplyr::select(TEAM), by = "TEAM")

#     return(teams_df_)
# }
