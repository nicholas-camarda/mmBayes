fixture_regions <- c("East", "West", "South", "Midwest")

#' Build synthetic team features for tests
#'
#' @param current_year Current bracket year to include.
#' @param history_years Historical seasons to include.
#'
#' @return A tibble of pre-tournament team features with deterministic values.
make_fixture_team_features <- function(current_year = 2025, history_years = 2022:2024) {
    # Build the standard 64-team regional field for one season.
    build_base_rows <- function(year) {
        expand.grid(
            Region = fixture_regions,
            Seed = 1:16,
            stringsAsFactors = FALSE
        ) %>%
            dplyr::as_tibble() %>%
            dplyr::arrange(Region, Seed) %>%
            dplyr::mutate(
                Year = as.character(year),
                Team = sprintf("%s_%02d_%s", Region, Seed, year),
                Conf = sprintf("Conf_%02d", ((Seed - 1) %% 8) + 1),
                Barthag = plogis(3.2 - (Seed / 4) + (dplyr::dense_rank(Region) / 20)),
                AdjOE = 119 - Seed + dplyr::dense_rank(Region),
                AdjDE = 89 + (Seed / 2) - (dplyr::dense_rank(Region) / 4),
                WAB = (17 - Seed) / 2 + dplyr::dense_rank(Region),
                TOR = 0.12 + (Seed / 500),
                TORD = 0.14 + ((17 - Seed) / 500),
                ORB = 0.27 + ((17 - Seed) / 400),
                DRB = 0.69 - (Seed / 500),
                `3P%` = 0.33 + ((17 - Seed) / 600),
                `3P%D` = 0.34 - ((17 - Seed) / 700),
                `Adj T.` = 67 + (Seed / 10)
            ) %>%
            add_safe_pre_tournament_features()
    }

    # Add duplicate-seed rows that exercise First Four paths.
    build_play_in_rows <- function(year) {
        dplyr::tribble(
            ~Region, ~Seed, ~Team, ~Conf, ~Barthag, ~AdjOE, ~AdjDE, ~WAB, ~TOR, ~TORD, ~ORB, ~DRB, ~`3P%`, ~`3P%D`, ~`Adj T.`,
            "East", 11L, sprintf("East_11_%s_playin", year), "Conf_04", plogis(2.1), 108, 94, 4.5, 0.132, 0.146, 0.285, 0.666, 0.347, 0.331, 68.4,
            "West", 16L, sprintf("West_16_%s_playin", year), "Conf_08", plogis(1.7), 103, 98, 2.8, 0.129, 0.141, 0.274, 0.659, 0.338, 0.336, 67.8,
            "South", 10L, sprintf("South_10_%s_playin", year), "Conf_03", plogis(2.0), 109, 95, 4.1, 0.131, 0.145, 0.282, 0.664, 0.345, 0.332, 68.1,
            "Midwest", 16L, sprintf("Midwest_16_%s_playin", year), "Conf_07", plogis(1.6), 102, 99, 2.4, 0.128, 0.140, 0.271, 0.657, 0.336, 0.337, 67.6
        ) %>%
            dplyr::mutate(Year = as.character(year)) %>%
            add_safe_pre_tournament_features()
    }

    rows <- purrr::map_dfr(c(history_years, current_year), build_base_rows)
    play_in_rows <- purrr::map_dfr(c(history_years, current_year), build_play_in_rows)

    dplyr::bind_rows(rows, play_in_rows) %>%
        dplyr::arrange(Year, Region, Seed, Team)
}

#' Score a fixture team row for deterministic winner selection
#'
#' @param team_row A one-row team feature tibble.
#'
#' @return A numeric strength score.
fixture_team_score <- function(team_row) {
    safe_numeric(team_row$barthag_logit) +
        (safe_numeric(team_row$AdjOE) / 10) -
        (safe_numeric(team_row$AdjDE) / 10) +
        (safe_numeric(team_row$WAB) / 5)
}

#' Create deterministic fixture scores for a matchup
#'
#' @param team_a A one-row team feature tibble for team A.
#' @param team_b A one-row team feature tibble for team B.
#' @param winner A one-row team feature tibble for the chosen winner.
#' @param round_name The round label for the matchup.
#' @param matchup_number The matchup number within the round.
#'
#' @return A named list with integer `teamA_score`, `teamB_score`, and
#'   `total_points` values.
make_fixture_matchup_scores <- function(team_a, team_b, winner, round_name, matchup_number) {
    round_adjustment <- c(
        "First Four" = -2,
        "Round of 64" = 0,
        "Round of 32" = 1,
        "Sweet 16" = -1,
        "Elite 8" = -2,
        "Final Four" = -4,
        "Championship" = -5
    )

    offense_sum <- safe_numeric(team_a$AdjOE[1]) + safe_numeric(team_b$AdjOE[1])
    defense_sum <- safe_numeric(team_a$AdjDE[1]) + safe_numeric(team_b$AdjDE[1])
    tempo_mean <- mean(c(safe_numeric(team_a$`Adj T.`[1]), safe_numeric(team_b$`Adj T.`[1])))
    profile_gap <- abs(safe_numeric(team_a$AdjOE[1]) - safe_numeric(team_b$AdjOE[1])) +
        abs(safe_numeric(team_a$AdjDE[1]) - safe_numeric(team_b$AdjDE[1])) +
        abs(safe_numeric(team_a$WAB[1]) - safe_numeric(team_b$WAB[1])) +
        abs(safe_numeric(team_a$`Adj T.`[1]) - safe_numeric(team_b$`Adj T.`[1]))

    base_total <- round(
        25 +
            (0.35 * offense_sum) -
            (0.08 * defense_sum) +
            (0.2 * tempo_mean) +
            (0.7 * profile_gap) +
            round_adjustment[[round_name]] +
            ((matchup_number + safe_numeric(team_a$Seed[1]) + safe_numeric(team_b$Seed[1])) %% 7)
    )
    base_total <- max(110L, min(175L, as.integer(base_total)))

    strength_gap <- abs(fixture_team_score(team_a) - fixture_team_score(team_b))
    margin <- max(1L, min(base_total - 2L, as.integer(round(4 + (strength_gap * 3) + (matchup_number %% 5)))))
    winner_score <- as.integer(floor((base_total + margin) / 2))
    loser_score <- as.integer(base_total - winner_score)

    if (winner$Team[1] == team_a$Team[1]) {
        list(teamA_score = winner_score, teamB_score = loser_score, total_points = base_total)
    } else {
        list(teamA_score = loser_score, teamB_score = winner_score, total_points = base_total)
    }
}

#' Pick a deterministic fixture winner for a matchup
#'
#' @param team_a A one-row team feature tibble for team A.
#' @param team_b A one-row team feature tibble for team B.
#' @param year Season year used to seed deterministic variation.
#' @param round_name The round label for the matchup.
#' @param region Region label used to seed deterministic variation.
#' @param matchup_number The matchup number within the round.
#'
#' @return The winning one-row team tibble.
pick_fixture_winner <- function(team_a, team_b, year, round_name, region, matchup_number) {
    score_a <- fixture_team_score(team_a)
    score_b <- fixture_team_score(team_b)

    upset_trigger <- ((as.integer(year) + nchar(region) + matchup_number + nchar(round_name)) %% 9) == 0
    if (upset_trigger && abs(score_a - score_b) < 1.5) {
        if (score_a >= score_b) team_b else team_a
    } else if (score_a >= score_b) {
        team_a
    } else {
        team_b
    }
}

#' Build deterministic historical game results for tests
#'
#' @param team_features A fixture team-feature tibble.
#' @param history_years Historical seasons to materialize.
#'
#' @return A tibble of synthetic completed tournament games.
make_fixture_game_results <- function(team_features, history_years = 2022:2024) {
    # Simulate one region from First Four through Elite 8.
    build_region_results <- function(region_teams, year, region_name) {
        results <- list()
        survivors <- region_teams

        duplicate_counts <- table(survivors$Seed)
        dup_seeds <- as.integer(names(duplicate_counts[duplicate_counts > 1]))
        if (length(dup_seeds) > 0) {
            play_in_teams <- survivors %>%
                dplyr::filter(Seed == dup_seeds[1]) %>%
                dplyr::arrange(Team)
            play_in_winner <- pick_fixture_winner(play_in_teams[1, , drop = FALSE], play_in_teams[2, , drop = FALSE], year, "First Four", region_name, 1)
            play_in_scores <- make_fixture_matchup_scores(
                play_in_teams[1, , drop = FALSE],
                play_in_teams[2, , drop = FALSE],
                play_in_winner,
                "First Four",
                1
            )
            results[[length(results) + 1L]] <- tibble::tibble(
                Year = as.character(year),
                region = region_name,
                round = "First Four",
                game_index = 1L,
                teamA = play_in_teams$Team[1],
                teamB = play_in_teams$Team[2],
                teamA_seed = play_in_teams$Seed[1],
                teamB_seed = play_in_teams$Seed[2],
                teamA_score = play_in_scores$teamA_score,
                teamB_score = play_in_scores$teamB_score,
                total_points = play_in_scores$total_points,
                winner = play_in_winner$Team[1]
            )
            survivors <- dplyr::bind_rows(
                survivors %>% dplyr::filter(Seed != dup_seeds[1]),
                play_in_winner
            )
        }

        ordered <- survivors %>%
            dplyr::arrange(match(Seed, standard_bracket_order()))
        round_names <- c("Round of 64", "Round of 32", "Sweet 16", "Elite 8")

        for (round_name in round_names) {
            next_round <- vector("list", nrow(ordered) / 2)
            for (matchup_number in seq_along(next_round)) {
                idx_a <- (matchup_number * 2) - 1
                idx_b <- matchup_number * 2
                team_a <- ordered[idx_a, , drop = FALSE]
                team_b <- ordered[idx_b, , drop = FALSE]
                winner <- pick_fixture_winner(team_a, team_b, year, round_name, region_name, matchup_number)
                matchup_scores <- make_fixture_matchup_scores(team_a, team_b, winner, round_name, matchup_number)

                results[[length(results) + 1L]] <- tibble::tibble(
                    Year = as.character(year),
                    region = region_name,
                    round = round_name,
                    game_index = matchup_number,
                    teamA = team_a$Team[1],
                    teamB = team_b$Team[1],
                    teamA_seed = team_a$Seed[1],
                    teamB_seed = team_b$Seed[1],
                    teamA_score = matchup_scores$teamA_score,
                    teamB_score = matchup_scores$teamB_score,
                    total_points = matchup_scores$total_points,
                    winner = winner$Team[1]
                )
                next_round[[matchup_number]] <- winner
            }
            ordered <- dplyr::bind_rows(next_round)
        }

        list(
            results = dplyr::bind_rows(results),
            champion = ordered
        )
    }

    # Combine the four regions with Final Four and championship results.
    build_year_results <- function(year) {
        year_teams <- team_features %>%
            dplyr::filter(Year == as.character(year))

        regional <- purrr::map(
            stats::setNames(fixture_regions, fixture_regions),
            function(region_name) {
                build_region_results(
                    region_teams = dplyr::filter(year_teams, Region == region_name),
                    year = year,
                    region_name = region_name
                )
            }
        )

        semifinal1_winner <- pick_fixture_winner(regional$South$champion, regional$West$champion, year, "Final Four", "National", 1)
        semifinal2_winner <- pick_fixture_winner(regional$East$champion, regional$Midwest$champion, year, "Final Four", "National", 2)
        champion <- pick_fixture_winner(semifinal1_winner, semifinal2_winner, year, "Championship", "National", 1)
        semifinal1_scores <- make_fixture_matchup_scores(regional$South$champion, regional$West$champion, semifinal1_winner, "Final Four", 1)
        semifinal2_scores <- make_fixture_matchup_scores(regional$East$champion, regional$Midwest$champion, semifinal2_winner, "Final Four", 2)
        championship_scores <- make_fixture_matchup_scores(semifinal1_winner, semifinal2_winner, champion, "Championship", 1)

        dplyr::bind_rows(
            purrr::map_dfr(regional, "results"),
            tibble::tibble(
                Year = as.character(year),
                region = "National",
                round = "Final Four",
                game_index = 1L,
                teamA = regional$South$champion$Team[1],
                teamB = regional$West$champion$Team[1],
                teamA_seed = regional$South$champion$Seed[1],
                teamB_seed = regional$West$champion$Seed[1],
                teamA_score = semifinal1_scores$teamA_score,
                teamB_score = semifinal1_scores$teamB_score,
                total_points = semifinal1_scores$total_points,
                winner = semifinal1_winner$Team[1]
            ),
            tibble::tibble(
                Year = as.character(year),
                region = "National",
                round = "Final Four",
                game_index = 2L,
                teamA = regional$East$champion$Team[1],
                teamB = regional$Midwest$champion$Team[1],
                teamA_seed = regional$East$champion$Seed[1],
                teamB_seed = regional$Midwest$champion$Seed[1],
                teamA_score = semifinal2_scores$teamA_score,
                teamB_score = semifinal2_scores$teamB_score,
                total_points = semifinal2_scores$total_points,
                winner = semifinal2_winner$Team[1]
            ),
            tibble::tibble(
                Year = as.character(year),
                region = "National",
                round = "Championship",
                game_index = 1L,
                teamA = semifinal1_winner$Team[1],
                teamB = semifinal2_winner$Team[1],
                teamA_seed = semifinal1_winner$Seed[1],
                teamB_seed = semifinal2_winner$Seed[1],
                teamA_score = championship_scores$teamA_score,
                teamB_score = championship_scores$teamB_score,
                total_points = championship_scores$total_points,
                winner = champion$Team[1]
            )
        )
    }

    purrr::map_dfr(history_years, build_year_results)
}

#' Build a partial current-year results table for live-scoring tests
#'
#' @param team_features A fixture team-feature tibble.
#' @param current_year Current bracket year to materialize.
#'
#' @return A tibble of current-year completed games.
make_fixture_current_year_completed_results <- function(team_features, current_year = 2025) {
    current_teams <- team_features %>%
        dplyr::filter(Year == as.character(current_year))

    # Stamp a completed-game row with deterministic winner and score data.
    build_result_row <- function(team_a, team_b, round_name, region_name, game_index, completed_at) {
        winner <- pick_fixture_winner(team_a, team_b, current_year, round_name, region_name, game_index)
        matchup_scores <- make_fixture_matchup_scores(team_a, team_b, winner, round_name, game_index)

        tibble::tibble(
            Year = as.character(current_year),
            region = region_name,
            round = round_name,
            game_index = as.integer(game_index),
            teamA = team_a$Team[1],
            teamB = team_b$Team[1],
            teamA_seed = team_a$Seed[1],
            teamB_seed = team_b$Seed[1],
            teamA_score = matchup_scores$teamA_score,
            teamB_score = matchup_scores$teamB_score,
            total_points = matchup_scores$total_points,
            winner = winner$Team[1],
            completed_at = as.character(completed_at),
            source = "fixture_live_results"
        )
    }

    first_four_team_a <- current_teams %>%
        dplyr::filter(Region == "East", Seed == 11L, !stringr::str_detect(Team, "_playin$")) %>%
        dplyr::slice(1)
    first_four_team_b <- current_teams %>%
        dplyr::filter(Region == "East", Seed == 11L, stringr::str_detect(Team, "_playin$")) %>%
        dplyr::slice(1)
    round64_team_a <- current_teams %>%
        dplyr::filter(Region == "East", Seed == 1L, !stringr::str_detect(Team, "_playin$")) %>%
        dplyr::slice(1)
    round64_team_b <- current_teams %>%
        dplyr::filter(Region == "East", Seed == 16L, !stringr::str_detect(Team, "_playin$")) %>%
        dplyr::slice(1)
    round32_team_a <- current_teams %>%
        dplyr::filter(Region == "East", Seed == 1L, !stringr::str_detect(Team, "_playin$")) %>%
        dplyr::slice(1)
    round32_team_b <- current_teams %>%
        dplyr::filter(Region == "East", Seed == 8L, !stringr::str_detect(Team, "_playin$")) %>%
        dplyr::slice(1)

    dplyr::bind_rows(
        build_result_row(first_four_team_a, first_four_team_b, "First Four", "First Four", 1L, sprintf("%s-03-18T18:00:00Z", current_year)),
        build_result_row(round64_team_a, round64_team_b, "Round of 64", "East", 1L, sprintf("%s-03-20T18:00:00Z", current_year)),
        build_result_row(round32_team_a, round32_team_b, "Round of 32", "East", 1L, sprintf("%s-03-22T18:00:00Z", current_year))
    )
}

#' Write fixture team and result workbooks for tests
#'
#' @param team_path Output path for the team workbook.
#' @param results_path Output path for the results workbook.
#' @param team_data Optional fixture team-feature tibble.
#' @param results_data Optional fixture results tibble.
#'
#' @return A named list with the written workbook paths.
write_fixture_data_files <- function(team_path, results_path, team_data = NULL, results_data = NULL) {
    team_data <- team_data %||% make_fixture_team_features()
    results_data <- results_data %||% make_fixture_game_results(team_data)

    writexl::write_xlsx(team_data, team_path)
    writexl::write_xlsx(results_data, results_path)

    list(team_path = team_path, results_path = results_path)
}

#' Build conference assignments from fixture team data
#'
#' @param team_features A fixture team-feature tibble.
#'
#' @return A compact conference-assignment tibble.
make_fixture_conf_assignments <- function(team_features) {
    team_features %>%
        dplyr::select(Year, Team, Seed, Region, Conf)
}

#' Build BART rating inputs from fixture team data
#'
#' @param team_features A fixture team-feature tibble.
#'
#' @return A tibble containing the rating columns used by BART tests.
make_fixture_bart_ratings <- function(team_features) {
    team_features %>%
        dplyr::select(
            Year,
            Team,
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

#' Apply parser alias remaps to fixture results
#'
#' @param results_data A fixture results tibble.
#'
#' @return The results tibble with parser-compatible team-name aliases.
apply_fixture_result_aliases <- function(results_data) {
    alias_map <- c(
        "North Carolina" = "UNC",
        "Connecticut" = "UConn",
        "Pittsburgh" = "Pitt",
        "Saint John's" = "St. John's (NY)",
        "Saint Peter's" = "St. Peter's",
        "UC Santa Barbara" = "UCSB",
        "Wichita State" = "Wichita State",
        "Florida State" = "Florida State",
        "Ohio State" = "Ohio State",
        "Michigan State" = "Michigan State",
        "Kansas State" = "Kansas State",
        "San Diego State" = "San Diego State",
        "Texas A&M Corpus Christi" = "Texas A&M-Corpus Christi",
        "Saint Francis" = "Saint Francis (PA)"
    )

    # Mirror the parser's preferred aliases when generating fixture winners.
    remap_name <- function(x) {
        mapped <- alias_map[x]
        dplyr::coalesce(unname(mapped), x)
    }

    results_data %>%
        dplyr::mutate(
            teamA = remap_name(teamA),
            teamB = remap_name(teamB),
            winner = remap_name(winner)
        )
}

#' Build synthetic parser input lines for a tournament year
#'
#' @param year Tournament year to emulate.
#'
#' @return A character vector of parser input lines.
make_parser_fixture_lines <- function(year = 2025L) {
    # Format one completed-game line in the parser's bracket text style.
    build_game_line <- function(seed_a, team_a, score_a, seed_b, team_b, score_b) {
        sprintf("(%s) %s %s, (%s) %s %s", seed_a, team_a, score_a, seed_b, team_b, score_b)
    }

    # Add a trailing champion line that is not attached to a box score.
    build_orphan_team_line <- function(seed, team_name) {
        sprintf("team %s %s", seed, team_name)
    }

    parser_regions <- c("East", "Midwest", "South", "West")
    play_in_specs_by_year <- list(
        `2018` = list(East = c(16L, 11L), Midwest = c(16L), South = integer(), West = c(11L)),
        `2021` = list(East = c(16L, 11L), Midwest = integer(), South = integer(), West = c(16L, 11L)),
        `2024` = list(East = integer(), Midwest = c(16L), South = c(10L), West = c(16L, 11L)),
        `2025` = list(East = c(16L), Midwest = c(11L), South = c(11L, 16L), West = integer())
    )
    play_in_specs <- play_in_specs_by_year[[as.character(year)]] %||%
        list(East = c(16L), Midwest = c(16L), South = c(11L), West = c(11L))

    regional_sections <- purrr::imap(
        parser_regions,
        function(region_name, region_index) {
            play_in_lines <- play_in_specs[[region_name]]
            counter_lines <- purrr::map_chr(seq_len(15), function(counter) {
                if (counter <= 8) {
                    seed_a <- c(1, 8, 5, 4, 6, 3, 7, 2)[counter]
                    seed_b <- c(16, 9, 12, 13, 11, 14, 10, 15)[counter]
                } else if (counter <= 12) {
                    seed_a <- c(1, 5, 6, 7)[counter - 8]
                    seed_b <- c(8, 4, 3, 2)[counter - 8]
                } else if (counter <= 14) {
                    seed_a <- c(1, 6)[counter - 12]
                    seed_b <- c(4, 2)[counter - 12]
                } else {
                    seed_a <- 1
                    seed_b <- 2
                }

                team_a <- sprintf("%s_%sA_%02d", region_name, counter, year)
                team_b <- sprintf("%s_%sB_%02d", region_name, counter, year)
                score_a <- 80 + region_index + counter
                score_b <- 60 + region_index + counter
                build_game_line(seed_a, team_a, score_a, seed_b, team_b, score_b)
            })

            first_four_block <- character()
            if (length(play_in_lines) > 0) {
                first_four_block <- c(
                    sprintf("%s First Four", region_name),
                    purrr::map_chr(seq_along(play_in_lines), function(idx) {
                        build_game_line(
                            play_in_lines[[idx]],
                            sprintf("%s_PlayIn_%sA_%s", region_name, idx, year),
                            70 + region_index + idx,
                            play_in_lines[[idx]],
                            sprintf("%s_PlayIn_%sB_%s", region_name, idx, year),
                            63 + region_index + idx
                        )
                    })
                )
            }

            c(
                first_four_block,
                counter_lines,
                build_orphan_team_line(1L, sprintf("%s_Champion_%s", region_name, year))
            )
        }
    )

    c(
        parser_regions,
        "National",
        unlist(regional_sections, use.names = FALSE),
        "National",
        build_game_line(1, sprintf("National_Semi_1A_%s", year), 71, 1, sprintf("National_Semi_1B_%s", year), 67),
        build_game_line(1, sprintf("National_Semi_2A_%s", year), 75, 1, sprintf("National_Semi_2B_%s", year), 70),
        build_game_line(1, sprintf("National_Title_A_%s", year), 69, 1, sprintf("National_Title_B_%s", year), 64)
    )
}
