library(dplyr)
library(logger)
library(tibble)

# Team ratings computed from ESPN game-level box scores.
#
# Source: sportsdataverse ESPN men's college basketball team box scores and
# schedules, published as GitHub release files (2005-present). Ratings use only
# games played on or before a cutoff date (Selection Sunday), so they cannot
# include NCAA tournament games.

#' Release download URL for a sportsdataverse ESPN men's basketball file
#'
#' @param kind Either `"team_box"` or `"schedule"`.
#' @param season Season year (the year the tournament is played).
#'
#' @return A single URL string.
#' @keywords internal
espn_release_url <- function(kind = c("team_box", "schedule"), season) {
    kind <- match.arg(kind)
    tag <- switch(kind,
        team_box = "espn_mens_college_basketball_team_boxscores",
        schedule = "espn_mens_college_basketball_schedules"
    )
    file_name <- espn_release_file_name(kind, season)
    sprintf("https://github.com/sportsdataverse/sportsdataverse-data/releases/download/%s/%s", tag, file_name)
}

#' File name of a sportsdataverse ESPN release file
#'
#' @param kind Either `"team_box"` or `"schedule"`.
#' @param season Season year.
#'
#' @return A single file name.
#' @keywords internal
espn_release_file_name <- function(kind = c("team_box", "schedule"), season) {
    kind <- match.arg(kind)
    switch(kind,
        team_box = sprintf("team_box_%d.csv", as.integer(season)),
        schedule = sprintf("mbb_schedule_%d.csv", as.integer(season))
    )
}

#' Download one season of ESPN box scores and schedules
#'
#' Files are kept in `dest_dir` and recorded in `manifest.csv` with their
#' SHA-256, size, source URL and download time. Completed seasons are
#' downloaded once and reused; pass `refresh = TRUE` to replace them (for
#' example the current season on Selection Sunday).
#'
#' @param season Season year.
#' @param dest_dir Directory for raw downloads.
#' @param refresh Whether to download again when the files already exist.
#'
#' @return A named character vector of local paths (`team_box`, `schedule`).
#' @keywords internal
download_espn_season_files <- function(season, dest_dir, refresh = FALSE) {
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
    paths <- c(
        team_box = file.path(dest_dir, espn_release_file_name("team_box", season)),
        schedule = file.path(dest_dir, espn_release_file_name("schedule", season))
    )

    for (kind in names(paths)) {
        path <- paths[[kind]]
        if (file.exists(path) && !isTRUE(refresh)) {
            next
        }
        url <- espn_release_url(kind, season)
        logger::log_info("Downloading {basename(path)} from {url}")
        temp_path <- paste0(path, ".part")
        response <- httr::GET(
            url,
            httr::user_agent("mmBayes-data-refresh/0.2"),
            httr::write_disk(temp_path, overwrite = TRUE)
        )
        if (httr::http_error(response)) {
            unlink(temp_path)
            stop_with_message(sprintf("Download failed (HTTP %s): %s", httr::status_code(response), url))
        }
        file.rename(temp_path, path)
        record_espn_download(dest_dir, path, url)
    }

    paths
}

#' Record a downloaded file in the raw-data manifest
#'
#' @param dest_dir Raw-data directory holding `manifest.csv`.
#' @param path Downloaded file path.
#' @param url Source URL.
#'
#' @return Invisibly, the updated manifest.
#' @keywords internal
record_espn_download <- function(dest_dir, path, url) {
    manifest_path <- file.path(dest_dir, "manifest.csv")
    entry <- tibble::tibble(
        file = basename(path),
        sha256 = digest::digest(file = path, algo = "sha256"),
        bytes = as.numeric(file.size(path)),
        source_url = url,
        downloaded_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
    )
    manifest <- if (file.exists(manifest_path)) {
        utils::read.csv(manifest_path, colClasses = "character") %>%
            tibble::as_tibble() %>%
            dplyr::mutate(bytes = as.numeric(bytes)) %>%
            dplyr::filter(file != entry$file)
    } else {
        entry[0, ]
    }
    manifest <- dplyr::bind_rows(manifest, entry) %>% dplyr::arrange(file)
    utils::write.csv(manifest, manifest_path, row.names = FALSE)
    invisible(manifest)
}

#' Box-score columns required to compute team ratings
#'
#' @return A character vector of column names.
#' @keywords internal
espn_team_box_columns <- function() {
    c(
        "game_id", "game_date", "team_id", "team_location", "team_home_away",
        "team_score", "field_goals_attempted", "three_point_field_goals_made",
        "three_point_field_goals_attempted", "free_throws_attempted",
        "offensive_rebounds", "defensive_rebounds", "total_turnovers",
        "opponent_team_id", "opponent_team_score"
    )
}

#' Schedule columns required to compute team ratings
#'
#' @return A character vector of column names.
#' @keywords internal
espn_schedule_columns <- function() {
    c(
        "game_id", "neutral_site", "home_id", "away_id", "home_conference_id",
        "away_conference_id"
    )
}

#' Read one season of ESPN box scores and schedules
#'
#' @param paths Named paths from [download_espn_season_files()].
#'
#' @return A list with `team_box` and `schedule` tibbles.
#' @keywords internal
read_espn_season_files <- function(paths) {
    read_required <- function(path, required) {
        data <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
        missing <- setdiff(required, names(data))
        if (length(missing) > 0) {
            stop_with_message(sprintf("%s is missing columns: %s", basename(path), paste(missing, collapse = ", ")))
        }
        tibble::as_tibble(data[, required])
    }
    list(
        team_box = read_required(paths[["team_box"]], espn_team_box_columns()),
        schedule = read_required(paths[["schedule"]], espn_schedule_columns())
    )
}

#' Parse an ESPN true/false column
#'
#' @param x A logical or character vector.
#'
#' @return A logical vector.
#' @keywords internal
parse_espn_flag <- function(x) {
    if (is.logical(x)) {
        return(x)
    }
    tolower(as.character(x)) %in% c("true", "t", "1")
}

#' Build team-game rows for Division I games
#'
#' Joins each team's box-score row to its opponent's row and the schedule.
#' Division I teams are those with at least `min_games` box-score rows in the
#' season; games against other opponents are dropped.
#'
#' @param team_box Team box-score rows for one season.
#' @param schedule Schedule rows for the same season.
#' @param min_games Minimum season games for a team to count as Division I.
#'
#' @return A tibble with one row per team per Division I game, carrying both
#'   teams' counting stats, possessions, location and conference IDs.
#' @keywords internal
build_espn_team_games <- function(team_box, schedule, min_games = 15L) {
    box <- team_box %>%
        dplyr::mutate(
            game_id = as.character(game_id),
            team_id = as.character(team_id),
            opponent_team_id = as.character(opponent_team_id),
            game_date = as.Date(substr(as.character(game_date), 1, 10))
        ) %>%
        dplyr::distinct(game_id, team_id, .keep_all = TRUE)

    division_one <- box %>%
        dplyr::count(team_id, name = "games") %>%
        dplyr::filter(games >= min_games) %>%
        dplyr::pull(team_id)

    opponent <- box %>%
        dplyr::select(
            game_id,
            opponent_team_id = team_id,
            opp_fga = field_goals_attempted,
            opp_3pm = three_point_field_goals_made,
            opp_3pa = three_point_field_goals_attempted,
            opp_fta = free_throws_attempted,
            opp_oreb = offensive_rebounds,
            opp_dreb = defensive_rebounds,
            opp_to = total_turnovers
        )

    sched <- schedule %>%
        dplyr::mutate(
            game_id = as.character(game_id),
            home_id = as.character(home_id),
            away_id = as.character(away_id),
            neutral_site = parse_espn_flag(neutral_site)
        ) %>%
        dplyr::distinct(game_id, .keep_all = TRUE) %>%
        dplyr::select(game_id, neutral_site, home_id, away_id, home_conference_id, away_conference_id)

    box %>%
        dplyr::filter(team_id %in% division_one, opponent_team_id %in% division_one) %>%
        dplyr::inner_join(opponent, by = c("game_id", "opponent_team_id")) %>%
        dplyr::inner_join(sched, by = "game_id") %>%
        dplyr::filter(!is.na(team_score), !is.na(opponent_team_score)) %>%
        dplyr::mutate(
            team_poss = field_goals_attempted - offensive_rebounds + total_turnovers + 0.475 * free_throws_attempted,
            opp_poss = opp_fga - opp_oreb + opp_to + 0.475 * opp_fta,
            poss = (team_poss + opp_poss) / 2,
            location = dplyr::case_when(
                neutral_site ~ 0,
                team_home_away == "home" ~ 1,
                team_home_away == "away" ~ -1,
                TRUE ~ 0
            ),
            conference_id = dplyr::if_else(team_id == home_id, home_conference_id, away_conference_id)
        )
}

#' Fit additive team effects with sum-to-zero constraints
#'
#' Fits `y = mu + a[first] + b[second] + h * location` by weighted least
#' squares, then centres `a` and `b` to mean zero across teams.
#'
#' @param y Response.
#' @param first Team index for the first effect (integer, 1..n_teams).
#' @param second Team index for the second effect, or `NULL` to fit only one
#'   effect.
#' @param n_teams Number of teams.
#' @param location Optional location covariate (+1 home, -1 away, 0 neutral).
#' @param weights Observation weights.
#' @param same_effect Whether `first` and `second` share one effect vector
#'   (used for tempo, where both teams contribute the same way).
#'
#' @return A list with `mu`, `a`, `b` and `h`.
#' @keywords internal
fit_additive_team_effects <- function(y, first, second, n_teams, location = NULL, weights = rep(1, length(y)), same_effect = FALSE) {
    n <- length(y)
    a_block <- matrix(0, n, n_teams)
    a_block[cbind(seq_len(n), first)] <- 1
    if (same_effect) {
        a_block[cbind(seq_len(n), second)] <- a_block[cbind(seq_len(n), second)] + 1
        design <- cbind(1, a_block[, -1, drop = FALSE])
    } else {
        b_block <- matrix(0, n, n_teams)
        b_block[cbind(seq_len(n), second)] <- 1
        design <- cbind(1, a_block[, -1, drop = FALSE], b_block[, -1, drop = FALSE])
    }
    if (!is.null(location)) {
        design <- cbind(design, location)
    }

    fit <- stats::lm.wfit(design, y, weights)
    coefs <- unname(fit$coefficients)
    if (anyNA(coefs)) {
        stop_with_message("Team-effect regression is not identified; the game graph may be disconnected")
    }

    mu <- coefs[[1]]
    a <- c(0, coefs[2:n_teams])
    if (same_effect) {
        shift <- mean(a)
        a <- a - shift
        mu <- mu + 2 * shift
        b <- a
    } else {
        b <- c(0, coefs[(n_teams + 1):(2 * n_teams - 1)])
        a_shift <- mean(a)
        b_shift <- mean(b)
        a <- a - a_shift
        b <- b - b_shift
        mu <- mu + a_shift + b_shift
    }
    h <- if (!is.null(location)) coefs[[length(coefs)]] else 0

    list(mu = mu, a = a, b = b, h = h)
}

#' Compute pre-tournament team ratings from team-game rows
#'
#' @param games Team-game rows from [build_espn_team_games()].
#' @param cutoff_date Last game date (inclusive) to use.
#' @param conference_names Lookup with `conference_id` and `Conf`.
#' @param bubble_rank Barthag rank of the reference "bubble" team for WAB.
#' @param pythag_exponent Exponent for Barthag and game win probabilities.
#'
#' @return A tibble with one row per Division I team: `espn_team_id`, `Team`,
#'   `Conf`, `G`, `ratings_cutoff`, `Barthag`, `AdjOE`, `AdjDE`, `WAB`, `TOR`,
#'   `TORD`, `ORB`, `DRB`, `3P%`, `3P%D`, `Adj T.`.
#' @keywords internal
compute_team_ratings <- function(games, cutoff_date, conference_names = NULL, bubble_rank = 45L, pythag_exponent = 11.5) {
    cutoff_date <- as.Date(cutoff_date)
    window <- games %>% dplyr::filter(game_date <= cutoff_date)
    if (nrow(window) == 0) {
        stop_with_message(sprintf("No Division I games on or before %s", cutoff_date))
    }

    invalid <- window %>% dplyr::filter(!is.finite(poss) | poss < 40 | poss > 110)
    if (nrow(invalid) > 0) {
        logger::log_warn("Dropping {nrow(invalid)} team-game rows with implausible possession counts (outside 40-110)")
        window <- window %>% dplyr::anti_join(invalid, by = c("game_id", "team_id"))
    }

    team_ids <- sort(unique(c(window$team_id, window$opponent_team_id)))
    n_teams <- length(team_ids)
    window <- window %>%
        dplyr::mutate(
            team_index = match(team_id, team_ids),
            opp_index = match(opponent_team_id, team_ids)
        )

    efficiency <- fit_additive_team_effects(
        y = 100 * window$team_score / window$poss,
        first = window$team_index,
        second = window$opp_index,
        n_teams = n_teams,
        location = window$location,
        weights = window$poss
    )

    one_row_per_game <- window %>% dplyr::filter(team_id < opponent_team_id)
    tempo <- fit_additive_team_effects(
        y = one_row_per_game$poss,
        first = one_row_per_game$team_index,
        second = one_row_per_game$opp_index,
        n_teams = n_teams,
        same_effect = TRUE
    )

    ratings <- tibble::tibble(
        team_id = team_ids,
        AdjOE = efficiency$mu + efficiency$a,
        AdjDE = efficiency$mu + efficiency$b,
        `Adj T.` = tempo$mu + tempo$a
    ) %>%
        dplyr::mutate(Barthag = AdjOE^pythag_exponent / (AdjOE^pythag_exponent + AdjDE^pythag_exponent))

    if (n_teams < bubble_rank) {
        stop_with_message(sprintf("Only %d teams rated; cannot use rank %d as the WAB bubble team", n_teams, bubble_rank))
    }
    bubble <- ratings %>%
        dplyr::arrange(dplyr::desc(Barthag)) %>%
        dplyr::slice(as.integer(bubble_rank))
    wab <- window %>%
        dplyr::left_join(ratings %>% dplyr::select(opponent_team_id = team_id, opp_oe = AdjOE, opp_de = AdjDE), by = "opponent_team_id") %>%
        dplyr::mutate(
            bubble_eff = bubble$AdjOE + opp_de - efficiency$mu + efficiency$h * location,
            opp_eff = opp_oe + bubble$AdjDE - efficiency$mu - efficiency$h * location,
            bubble_win_prob = bubble_eff^pythag_exponent / (bubble_eff^pythag_exponent + opp_eff^pythag_exponent),
            win = as.numeric(team_score > opponent_team_score)
        ) %>%
        dplyr::group_by(team_id) %>%
        dplyr::summarise(WAB = sum(win - bubble_win_prob), .groups = "drop")

    factors <- window %>%
        dplyr::group_by(team_id) %>%
        dplyr::summarise(
            Team = dplyr::first(team_location),
            conference_id = names(sort(table(conference_id), decreasing = TRUE))[1] %||% NA_character_,
            G = dplyr::n(),
            TOR = 100 * sum(total_turnovers) / sum(poss),
            TORD = 100 * sum(opp_to) / sum(poss),
            ORB = 100 * sum(offensive_rebounds) / (sum(offensive_rebounds) + sum(opp_dreb)),
            DRB = 100 * sum(opp_oreb) / (sum(opp_oreb) + sum(defensive_rebounds)),
            `3P%` = 100 * sum(three_point_field_goals_made) / sum(three_point_field_goals_attempted),
            `3P%D` = 100 * sum(opp_3pm) / sum(opp_3pa),
            .groups = "drop"
        )

    conference_names <- conference_names %||% espn_conference_names()
    factors %>%
        dplyr::inner_join(ratings, by = "team_id") %>%
        dplyr::left_join(wab, by = "team_id") %>%
        dplyr::mutate(conference_id = as.character(conference_id)) %>%
        dplyr::left_join(conference_names %>% dplyr::mutate(conference_id = as.character(conference_id)), by = "conference_id") %>%
        dplyr::transmute(
            espn_team_id = team_id,
            Team = canonicalize_team_name(Team),
            Conf,
            G,
            ratings_cutoff = format(cutoff_date, "%Y-%m-%d"),
            Barthag,
            AdjOE,
            AdjDE,
            WAB = dplyr::coalesce(WAB, 0),
            TOR,
            TORD,
            ORB,
            DRB,
            `3P%`,
            `3P%D`,
            `Adj T.`
        ) %>%
        dplyr::arrange(dplyr::desc(Barthag))
}

#' ESPN conference IDs and stable short names
#'
#' ESPN conference IDs are stable across seasons, but the names in each
#' season's schedule file are incomplete and vary (for example "A 10" and
#' "A-10"). Conference labels are used to identify same-conference matchups,
#' so one name per ID is used for every season. ID 43 holds Division I
#' independents (and transitional members such as the Great West).
#'
#' @return A tibble with `conference_id` (character) and `Conf`.
#' @keywords internal
espn_conference_names <- function() {
    tibble::tribble(
        ~conference_id, ~Conf,
        "1", "Am. East",
        "2", "ACC",
        "3", "A-10",
        "4", "Big East",
        "5", "Big Sky",
        "6", "Big South",
        "7", "Big Ten",
        "8", "Big 12",
        "9", "Big West",
        "10", "CAA",
        "11", "CUSA",
        "12", "Ivy",
        "13", "MAAC",
        "14", "MAC",
        "16", "MEAC",
        "18", "MVC",
        "19", "NEC",
        "20", "OVC",
        "21", "Pac-12",
        "22", "Patriot",
        "23", "SEC",
        "24", "SoCon",
        "25", "Southland",
        "26", "SWAC",
        "27", "Sun Belt",
        "29", "WCC",
        "30", "WAC/UAC",
        "43", "Ind.",
        "44", "Mountain West",
        "45", "Horizon",
        "46", "ASUN",
        "49", "Summit",
        "62", "American"
    )
}

#' Compute one season's team ratings through its Selection Sunday
#'
#' @param season Tournament year.
#' @param raw_dir Directory holding raw ESPN downloads.
#' @param cutoff_date Last game date to use; defaults to Selection Sunday.
#' @param refresh Whether to download the season files again.
#'
#' @return A team ratings tibble with a `Year` column.
#' @keywords internal
compute_season_team_ratings <- function(season, raw_dir, cutoff_date = ratings_cutoff_date(season), refresh = FALSE) {
    paths <- download_espn_season_files(season, raw_dir, refresh = refresh)
    season_data <- read_espn_season_files(paths)
    games <- build_espn_team_games(season_data$team_box, season_data$schedule)
    compute_team_ratings(games, cutoff_date, conference_names = espn_conference_names()) %>%
        dplyr::mutate(Year = as.character(season), .before = 1)
}
