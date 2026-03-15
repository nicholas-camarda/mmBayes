library(logger)
library(yaml)

#' Default project configuration
#'
#' @return A nested list containing default data, model, and output settings.
#' @export
default_project_config <- function() {
    list(
        data = list(
            team_features_path = "data/pre_tournament_team_features.xlsx",
            game_results_path = "data/tournament_game_results.xlsx"
        ),
        model = list(
            history_window = 8L,
            backtest = TRUE,
            required_predictors = c(
                "round",
                "same_conf",
                "seed_diff",
                "barthag_logit_diff",
                "AdjOE_diff",
                "AdjDE_diff",
                "WAB_diff",
                "TOR_diff",
                "TORD_diff",
                "ORB_diff",
                "DRB_diff",
                "3P%_diff",
                "3P%D_diff",
                "Adj T._diff"
            ),
            random_seed = 42,
            n_draws = 1000
        ),
        output = list(
            path = "output",
            prefix = "tournament_sim",
            log_path = "output/logs/tournament_simulation.log",
            refresh_log_path = "output/logs/data_refresh.log"
        )
    )
}

#' Merge nested configuration lists
#'
#' @param base A base configuration list.
#' @param override A list of overriding values.
#'
#' @return A recursively merged configuration list.
#' @keywords internal
merge_config_lists <- function(base, override) {
    if (is.null(override)) {
        return(base)
    }

    result <- base
    for (name in names(override)) {
        if (is.list(base[[name]]) && is.list(override[[name]])) {
            result[[name]] <- merge_config_lists(base[[name]], override[[name]])
        } else {
            result[[name]] <- override[[name]]
        }
    }

    result
}

#' Load the project configuration
#'
#' @param path Path to a YAML configuration file.
#'
#' @return A configuration list combining defaults with any file-based overrides.
#' @export
load_project_config <- function(path = "config.yml") {
    config <- default_project_config()

    if (!file.exists(path)) {
        return(config)
    }

    parsed <- yaml::read_yaml(path)
    parsed_default <- if (is.list(parsed) && !is.null(parsed$default)) {
        parsed$default
    } else {
        parsed
    }

    merge_config_lists(config, parsed_default)
}

#' Initialize logging for the current run
#'
#' @param log_path Path to the log file for the current run.
#' @param tee_to_console Whether to also stream logs to the terminal.
#'
#' @return Invisibly configures the package logger appenders.
#' @export
initialize_logging <- function(log_path = "tournament_simulation.log", tee_to_console = TRUE) {
    log_dir <- dirname(log_path)
    if (!identical(log_dir, ".") && !dir.exists(log_dir)) {
        dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
    }

    logger::log_threshold(logger::INFO)
    appender <- if (isTRUE(tee_to_console)) {
        logger::appender_tee(log_path)
    } else {
        logger::appender_file(log_path)
    }

    logger::log_appender(appender)
}
