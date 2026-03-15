library(dplyr)
library(logger)
library(purrr)
library(readxl)
library(rvest)
library(stringr)
library(writexl)

#' Scrape conference assignments from Bart Torvik
#' @export
scrape_conf_assignments <- function(year) {
    url <- sprintf(
        "https://barttorvik.com/tourneytime.php?year=%s&sort=7&conlimit=All",
        year
    )
    logger::log_info("Scraping conference assignments from {url}")

    page <- rvest::read_html(url)
    table <- page %>%
        rvest::html_element("table") %>%
        rvest::html_table(fill = TRUE)

    dplyr::mutate(
        table,
        Year = as.character(year),
        Region = factor(Region, levels = c("East", "Midwest", "South", "West"))
    ) %>%
        dplyr::arrange(Region, Seed) %>%
        dplyr::mutate(
            dplyr::across(
                c(R64, R32, S16, E8, F4, F2, Champ),
                ~ as.numeric(ifelse(as.character(.) == "✓", "1", as.character(.)))
            )
        )
}

#' Scrape team data from Bart Torvik
#' @export
scrape_bart_data <- function(year, region) {
    url <- paste0(
        "https://barttorvik.com/?year=", year,
        "&sort=&hteam=&t2value=&conlimit=", region,
        "&state=All&begin=", year - 1, "1101",
        "&end=", year, "0501",
        "&top=0&revquad=0&quad=5&venue=All&type=All&mingames=0#"
    )
    logger::log_info("Scraping Bart data from {url}")

    page <- rvest::read_html(url)
    table <- page %>%
        rvest::html_element(xpath = '//*[@id="content"]//table') %>%
        rvest::html_table()

    column_names <- as.character(table[1, ])
    colnames(table) <- column_names

    table %>%
        dplyr::slice(-1) %>%
        dplyr::mutate(
            Year = as.character(year),
            Region = region,
            Rk = as.numeric(Rk),
            G = as.numeric(G),
            dplyr::across(`AdjOE`:`WAB`, as.numeric),
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

#' Build the canonical tournament modeling dataset
#' @export
build_bayesian_dataset <- function(bart_data, conf_assignments) {
    combined <- dplyr::left_join(
        bart_data,
        conf_assignments,
        by = c("Year", "Team", "Seed", "Region", "Conf")
    )

    add_derived_features(combined)
}

#' Refresh the canonical tournament data files
#' @export
update_tournament_data <- function(start_year = 2021, bracket_year = as.integer(format(Sys.Date(), "%Y"))) {
    bart_data_file <- file.path("data", "bart_data_all_years.xlsx")
    conf_assign_file <- file.path("data", "bart_conference_assignments_all_years.xlsx")
    final_data_file <- file.path("data", "bayesian_model_data.xlsx")

    years_to_fetch <- seq.int(start_year, bracket_year)
    regions <- c("East", "West", "Midwest", "South")

    bart_data <- purrr::map_dfr(years_to_fetch, function(year) {
        purrr::map_dfr(regions, function(region) scrape_bart_data(year, region))
    })

    conf_assignments <- purrr::map_dfr(years_to_fetch, scrape_conf_assignments)
    final_data <- build_bayesian_dataset(bart_data, conf_assignments)

    writexl::write_xlsx(bart_data, bart_data_file)
    writexl::write_xlsx(conf_assignments, conf_assign_file)
    writexl::write_xlsx(final_data, final_data_file)

    list(
        bart_data = bart_data_file,
        conf_assignments = conf_assign_file,
        final_data = final_data_file
    )
}
