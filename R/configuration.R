library(logger)
library(yaml)

#' Default project configuration
#' @export
default_project_config <- function() {
    list(
        data_path = "data/bayesian_model_data.xlsx",
        metrics_to_use = c(
            "overall_strength", "barthag_logit", "AdjOE", "AdjDE",
            "Clutch_Index", "Conf_Strength", "Upset_Factor", "Turnover_Edge"
        ),
        model = list(
            required_metrics = c(
                "overall_strength", "barthag_logit", "AdjOE", "AdjDE",
                "Clutch_Index", "Conf_Strength", "Upset_Factor", "Turnover_Edge"
            ),
            random_seed = 42,
            n_draws = 1000
        ),
        output = list(
            path = "output",
            prefix = "tournament_sim"
        )
    )
}

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
#' @export
initialize_logging <- function(log_path = "tournament_simulation.log") {
    logger::log_threshold(logger::INFO)
    logger::log_appender(logger::appender_file(log_path))
}
