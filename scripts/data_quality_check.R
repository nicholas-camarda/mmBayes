suppressPackageStartupMessages({
    library(dplyr)
    library(readxl)
    library(stringr)
})

snapshot_directory_state <- function(path) {
    if (!dir.exists(path)) {
        return(data.frame(
            file = character(),
            size = numeric(),
            mtime = numeric(),
            stringsAsFactors = FALSE
        ))
    }

    files <- list.files(path, recursive = TRUE, full.names = TRUE, include.dirs = FALSE)
    if (length(files) == 0) {
        return(data.frame(
            file = character(),
            size = numeric(),
            mtime = numeric(),
            stringsAsFactors = FALSE
        ))
    }

    info <- file.info(files)
    data.frame(
        file = normalizePath(files, winslash = "/", mustWork = FALSE),
        size = info$size,
        mtime = as.numeric(info$mtime),
        stringsAsFactors = FALSE
    )
}

archive_before <- snapshot_directory_state("archive")

team_path <- "data/pre_tournament_team_features.xlsx"
results_path <- "data/tournament_game_results.xlsx"

teams <- readxl::read_excel(team_path)
results <- readxl::read_excel(results_path)

required_team_cols <- c(
    "Year", "Team", "Seed", "Region", "Conf", "Barthag", "AdjOE", "AdjDE", "WAB",
    "TOR", "TORD", "ORB", "DRB", "3P%", "3P%D", "Adj T."
)
required_result_cols <- c(
    "Year", "region", "round", "game_index", "teamA", "teamB", "teamA_seed", "teamB_seed", "winner"
)

cat("=== Team Features ===\n")
cat("rows=", nrow(teams), " cols=", ncol(teams), "\n", sep = "")
cat("years=", paste(sort(unique(teams$Year)), collapse = ","), "\n", sep = "")

missing_team_cols <- setdiff(required_team_cols, names(teams))
cat("missing_required_columns=", if (length(missing_team_cols) == 0) "none" else paste(missing_team_cols, collapse = ","), "\n", sep = "")

cat("na_counts:\n")
for (nm in required_team_cols) {
    if (nm %in% names(teams)) {
        cat("  ", nm, ":", sum(is.na(teams[[nm]])), "\n", sep = "")
    }
}

cat("blank_team=", sum(stringr::str_trim(dplyr::coalesce(teams$Team, "")) == ""), "\n", sep = "")
cat("blank_conf=", sum(stringr::str_trim(dplyr::coalesce(teams$Conf, "")) == ""), "\n", sep = "")
cat("seed_lt1=", sum(!is.na(teams$Seed) & teams$Seed < 1), "\n", sep = "")
cat("seed_gt16=", sum(!is.na(teams$Seed) & teams$Seed > 16), "\n", sep = "")
cat("seed_na=", sum(is.na(teams$Seed)), "\n", sep = "")

key_dup_team <- teams %>%
    dplyr::count(Year, Region, Team, name = "n") %>%
    dplyr::filter(n > 1)
key_dup_seed <- teams %>%
    dplyr::count(Year, Region, Seed, name = "n") %>%
    dplyr::filter(n > 1)

cat("dup_year_region_team=", nrow(key_dup_team), "\n", sep = "")
cat("dup_year_region_seed=", nrow(key_dup_seed), "\n", sep = "")
if (nrow(key_dup_seed) > 0) {
    cat("dup_year_region_seed_sample:\n")
    print(utils::head(key_dup_seed, 12))
}

parse_failures <- teams %>%
    dplyr::filter(is.na(Seed) | stringr::str_detect(Team, "seed\\\\s*$")) %>%
    dplyr::select(Year, Region, Team, Seed)
cat("possible_parse_failures=", nrow(parse_failures), "\n", sep = "")
if (nrow(parse_failures) > 0) {
    print(utils::head(parse_failures, 12))
}

cat("\n=== Game Results ===\n")
cat("rows=", nrow(results), " cols=", ncol(results), "\n", sep = "")
cat("years=", paste(sort(unique(results$Year)), collapse = ","), "\n", sep = "")

missing_result_cols <- setdiff(required_result_cols, names(results))
cat("missing_required_columns=", if (length(missing_result_cols) == 0) "none" else paste(missing_result_cols, collapse = ","), "\n", sep = "")

cat("na_counts:\n")
for (nm in required_result_cols) {
    if (nm %in% names(results)) {
        cat("  ", nm, ":", sum(is.na(results[[nm]])), "\n", sep = "")
    }
}

bad_winner <- results %>%
    dplyr::filter(!(winner %in% c(teamA, teamB)))
cat("winner_not_in_teams=", nrow(bad_winner), "\n", sep = "")

round_counts <- results %>%
    dplyr::count(round, sort = TRUE)
cat("round_counts:\n")
print(round_counts)

dup_games <- results %>%
    dplyr::count(Year, region, round, game_index, name = "n") %>%
    dplyr::filter(n > 1)
cat("dup_year_region_round_game_index=", nrow(dup_games), "\n", sep = "")
if (nrow(dup_games) > 0) {
    print(utils::head(dup_games, 12))
}

missing_result_years_in_teams <- setdiff(unique(results$Year), unique(teams$Year))
cat("result_years_missing_in_team_features=", if (length(missing_result_years_in_teams) == 0) "none" else paste(missing_result_years_in_teams, collapse = ","), "\n", sep = "")

archive_after <- snapshot_directory_state("archive")
archive_changes <- dplyr::full_join(
    dplyr::rename(archive_before, size_before = size, mtime_before = mtime),
    dplyr::rename(archive_after, size_after = size, mtime_after = mtime),
    by = "file"
) %>%
    dplyr::filter(
        is.na(size_before) |
            is.na(size_after) |
            size_before != size_after |
            mtime_before != mtime_after
    )

if (nrow(archive_changes) > 0) {
    cat("archive_changed=true\n")
    print(utils::head(archive_changes, 20))
    quit(save = "no", status = 2)
}

cat("archive_changed=false\n")
