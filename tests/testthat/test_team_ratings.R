#' Build a synthetic ESPN season with known team strengths
#'
#' Every pair of teams plays twice (home and away) plus once at a neutral
#' site. Possessions are fixed at 70 per team per game, so offensive
#' efficiency is exactly `mu + offense[i] + defense[j] + home * location`.
#'
#' @param n_teams Number of Division I teams.
#' @param start_date Date of the first game.
#'
#' @return A list with `team_box`, `schedule`, and the true `offense`,
#'   `defense`, `mu` and `home` values.
#' @keywords internal
make_synthetic_espn_season <- function(n_teams = 12L, start_date = as.Date("2024-11-04")) {
    set.seed(11)
    mu <- 105
    home <- 3
    offense <- seq(-6, 6, length.out = n_teams)
    defense <- rev(seq(-5, 5, length.out = n_teams))
    offense <- offense - mean(offense)
    defense <- defense - mean(defense)
    team_ids <- as.character(100 + seq_len(n_teams))

    pairs <- expand.grid(a = seq_len(n_teams), b = seq_len(n_teams))
    pairs <- pairs[pairs$a < pairs$b, ]
    games <- dplyr::bind_rows(
        dplyr::tibble(home = pairs$a, away = pairs$b, neutral = FALSE),
        dplyr::tibble(home = pairs$b, away = pairs$a, neutral = FALSE),
        dplyr::tibble(home = pairs$a, away = pairs$b, neutral = TRUE)
    ) %>%
        dplyr::mutate(
            game_id = as.character(5000 + dplyr::row_number()),
            game_date = start_date + (dplyr::row_number() %% 100)
        )

    team_row <- function(team, opponent, location, game) {
        efficiency <- mu + offense[team] + defense[opponent] + home * location
        dplyr::tibble(
            game_id = game$game_id,
            game_date = format(game$game_date),
            team_id = team_ids[team],
            team_location = sprintf("Synthetic %02d", team),
            team_home_away = if (location >= 0) "home" else "away",
            team_score = efficiency * 0.70,
            field_goals_attempted = 58.5,
            three_point_field_goals_made = 7 + team %% 3,
            three_point_field_goals_attempted = 20,
            free_throws_attempted = 20,
            offensive_rebounds = 10,
            defensive_rebounds = 25,
            total_turnovers = 12,
            opponent_team_id = team_ids[opponent],
            opponent_team_score = NA_real_
        )
    }

    box <- purrr::map_dfr(seq_len(nrow(games)), function(i) {
        game <- games[i, ]
        location <- if (game$neutral) 0 else 1
        dplyr::bind_rows(
            team_row(game$home, game$away, location, game),
            team_row(game$away, game$home, -location, game) %>%
                dplyr::mutate(team_home_away = if (game$neutral) "home" else "away")
        )
    })
    box <- box %>%
        dplyr::group_by(game_id) %>%
        dplyr::mutate(opponent_team_score = rev(team_score)) %>%
        dplyr::ungroup()

    schedule <- games %>%
        dplyr::transmute(
            game_id,
            neutral_site = ifelse(neutral, "true", "false"),
            home_id = team_ids[home],
            away_id = team_ids[away],
            home_conference_id = ifelse(home <= n_teams / 2, 2L, 23L),
            away_conference_id = ifelse(away <= n_teams / 2, 2L, 23L),
            groups_id = 2L,
            groups_short_name = "ACC",
            groups_is_conference = "true"
        )
    schedule <- dplyr::bind_rows(
        schedule,
        dplyr::tibble(
            game_id = "conference-name-row", neutral_site = "false", home_id = NA, away_id = NA,
            home_conference_id = NA, away_conference_id = NA,
            groups_id = 23L, groups_short_name = "SEC", groups_is_conference = "true"
        )
    )

    list(
        team_box = box,
        schedule = schedule,
        offense = stats::setNames(offense, team_ids),
        defense = stats::setNames(defense, team_ids),
        mu = mu,
        home = home,
        team_ids = team_ids
    )
}

test_that("ESPN release URLs point at the sportsdataverse release files", {
    expect_identical(
        espn_release_url("team_box", 2025),
        "https://github.com/sportsdataverse/sportsdataverse-data/releases/download/espn_mens_college_basketball_team_boxscores/team_box_2025.csv"
    )
    expect_identical(
        espn_release_url("schedule", 2008),
        "https://github.com/sportsdataverse/sportsdataverse-data/releases/download/espn_mens_college_basketball_schedules/mbb_schedule_2008.csv"
    )
})

test_that("team ratings recover known opponent-adjusted efficiencies", {
    season <- make_synthetic_espn_season()
    games <- build_espn_team_games(season$team_box, season$schedule, min_games = 5L)
    ratings <- compute_team_ratings(games, as.Date("2025-05-01"), espn_conference_names(), bubble_rank = 6L)

    ratings <- ratings[match(season$team_ids, ratings$espn_team_id), ]
    expect_equal(ratings$AdjOE, unname(season$mu + season$offense), tolerance = 1e-6)
    expect_equal(ratings$AdjDE, unname(season$mu + season$defense), tolerance = 1e-6)
    expect_equal(ratings$`Adj T.`, rep(70, length(season$team_ids)), tolerance = 1e-6)
    expect_equal(
        ratings$Barthag,
        ratings$AdjOE^11.5 / (ratings$AdjOE^11.5 + ratings$AdjDE^11.5)
    )
    expect_true(all(ratings$ratings_cutoff == "2025-05-01"))
    expect_setequal(unique(ratings$Conf), c("ACC", "SEC"))
})

test_that("team ratings compute four factors from possessions and rebounds", {
    season <- make_synthetic_espn_season()
    games <- build_espn_team_games(season$team_box, season$schedule, min_games = 5L)
    ratings <- compute_team_ratings(games, as.Date("2025-05-01"), bubble_rank = 6L)

    expect_equal(ratings$TOR, rep(100 * 12 / 70, nrow(ratings)), tolerance = 1e-9)
    expect_equal(ratings$TORD, rep(100 * 12 / 70, nrow(ratings)), tolerance = 1e-9)
    expect_equal(ratings$ORB, rep(100 * 10 / 35, nrow(ratings)), tolerance = 1e-9)
    expect_equal(ratings$DRB, rep(100 * 10 / 35, nrow(ratings)), tolerance = 1e-9)
    expect_true(all(ratings$`3P%` >= 35 & ratings$`3P%` <= 45))
})

test_that("games after the cutoff date never change the ratings", {
    season <- make_synthetic_espn_season()
    cutoff <- as.Date("2025-03-16")
    late_games <- season$team_box %>%
        dplyr::slice_head(n = 40) %>%
        dplyr::mutate(
            game_id = paste0("late-", game_id),
            game_date = "2025-03-25",
            team_score = team_score + 40
        )
    late_schedule <- season$schedule %>%
        dplyr::filter(game_id %in% sub("^late-", "", late_games$game_id)) %>%
        dplyr::mutate(game_id = paste0("late-", game_id))

    base_games <- build_espn_team_games(season$team_box, season$schedule, min_games = 5L)
    with_late <- build_espn_team_games(
        dplyr::bind_rows(season$team_box, late_games),
        dplyr::bind_rows(season$schedule, late_schedule),
        min_games = 5L
    )

    before <- compute_team_ratings(base_games, cutoff, bubble_rank = 6L)
    after <- compute_team_ratings(with_late, cutoff, bubble_rank = 6L)
    expect_equal(after, before)

    including_late <- compute_team_ratings(with_late, as.Date("2025-05-01"), bubble_rank = 6L)
    expect_false(isTRUE(all.equal(including_late$AdjOE, before$AdjOE)))
})

test_that("non-Division I opponents are excluded by the minimum-games rule", {
    season <- make_synthetic_espn_season()
    guest <- season$team_box %>%
        dplyr::filter(team_id == season$team_ids[[1]]) %>%
        dplyr::slice_head(n = 3) %>%
        dplyr::mutate(
            game_id = paste0("guest-", dplyr::row_number()),
            opponent_team_id = "999"
        )
    guest_rows <- dplyr::bind_rows(
        guest,
        guest %>% dplyr::mutate(team_id = "999", team_location = "Small College", opponent_team_id = season$team_ids[[1]])
    )
    guest_schedule <- dplyr::tibble(
        game_id = guest$game_id, neutral_site = "false", home_id = season$team_ids[[1]], away_id = "999",
        home_conference_id = 2L, away_conference_id = NA, groups_id = NA, groups_short_name = NA, groups_is_conference = "false"
    )

    games <- build_espn_team_games(
        dplyr::bind_rows(season$team_box, guest_rows),
        dplyr::bind_rows(season$schedule, guest_schedule),
        min_games = 5L
    )
    expect_false("999" %in% c(games$team_id, games$opponent_team_id))
})

test_that("ESPN team names resolve to the canonical tournament names", {
    expect_identical(
        canonicalize_team_name(c("Pennsylvania", "American University", "Queens University", "Penn State")),
        c("Penn", "American", "Queens", "Penn State")
    )
})

test_that("WAB needs at least as many rated teams as the bubble rank", {
    season <- make_synthetic_espn_season()
    games <- build_espn_team_games(season$team_box, season$schedule, min_games = 5L)
    expect_error(compute_team_ratings(games, as.Date("2025-05-01"), bubble_rank = 45L), "cannot use rank 45")
})
