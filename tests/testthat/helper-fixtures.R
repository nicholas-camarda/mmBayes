make_fixture_tournament_data <- function(current_year = 2025, history_years = 2022:2024) {
    regions <- c("East", "West", "South", "Midwest")
    seeds <- 1:16

    build_year <- function(year, is_current = FALSE) {
        grid <- expand.grid(
            Region = regions,
            Seed = seeds,
            stringsAsFactors = FALSE
        )

        dplyr::as_tibble(grid) %>%
            dplyr::arrange(Region, Seed) %>%
            dplyr::mutate(
                Year = as.character(year),
                Team = sprintf("%s_%02d_%s", Region, Seed, year),
                Conf = sprintf("Conf_%02d", ((Seed - 1) %% 8) + 1),
                R64 = 1,
                R32 = as.integer(Seed <= 8),
                S16 = as.integer(Seed <= 4),
                E8 = as.integer(Seed <= 2),
                F4 = as.integer(Seed == 1),
                F2 = as.integer(Seed == 1 & Region %in% c("East", "West")),
                Champ = if (is_current) 0L else as.integer(Seed == 1 & Region == "East"),
                overall_strength = 100 - (Seed * 2) + dplyr::dense_rank(Region),
                barthag_logit = 3 - (Seed / 4),
                AdjOE = 118 - Seed,
                AdjDE = 88 + (Seed / 2),
                Clutch_Index = 2 + (17 - Seed) / 8,
                Conf_Strength = 0.35 + ((17 - Seed) / 40),
                Upset_Factor = 0.10 + (Seed / 200),
                Turnover_Edge = (17 - Seed) / 10,
                Rk = Seed,
                G = 35,
                Rec = "0-0",
                Barthag = plogis(barthag_logit),
                `EFG%` = 0.48 + ((17 - Seed) / 200),
                `EFGD%` = 0.46 - ((17 - Seed) / 300),
                TOR = 0.12 + (Seed / 500),
                TORD = 0.13 + ((17 - Seed) / 500),
                ORB = 0.28 + ((17 - Seed) / 400),
                DRB = 0.70 - (Seed / 400),
                FTR = 0.25 + ((17 - Seed) / 500),
                FTRD = 0.22 + (Seed / 500),
                `2P%` = 0.50 + ((17 - Seed) / 250),
                `2P%D` = 0.47 - ((17 - Seed) / 300),
                `3P%` = 0.34 + ((17 - Seed) / 500),
                `3P%D` = 0.33 - ((17 - Seed) / 500),
                `3PR` = 0.35 + (Seed / 1000),
                `3PRD` = 0.34 + (Seed / 1200),
                `Adj T.` = 67 + (Seed / 10),
                WAB = (17 - Seed) / 3,
                combined_metric = overall_strength,
                by = ""
            )
    }

    dplyr::bind_rows(
        purrr::map(history_years, build_year),
        build_year(current_year, is_current = TRUE)
    )
}

write_fixture_workbook <- function(path, data = make_fixture_tournament_data()) {
    writexl::write_xlsx(data, path)
    path
}
