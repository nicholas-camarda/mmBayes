library(logger)
library(yaml)

#' Return the canonical project slug
#'
#' @return A scalar character project slug.
#' @keywords internal
project_slug <- function() {
    "mmBayes"
}

#' Return the default local runtime root
#'
#' @return A normalized path under the user's home directory.
#' @keywords internal
default_runtime_root <- function() {
    path.expand(file.path("~", "ProjectsRuntime", project_slug()))
}

#' Return the default local code root
#'
#' @return A normalized path to the active local checkout.
#' @keywords internal
default_code_root <- function() {
    path.expand(file.path("~", "Projects", project_slug()))
}

#' Return the canonical cloud root for shared project files
#'
#' @return A normalized path under OneDrive for shared data and outputs.
#' @keywords internal
default_cloud_root <- function() {
    path.expand(file.path("~", "Library", "CloudStorage", "OneDrive-Personal", "SideProjects", project_slug()))
}

#' Return the default OneDrive project root
#'
#' @return A normalized path to the canonical cloud project root.
#' @keywords internal
default_publish_root <- function() {
    default_cloud_root()
}

#' Return the canonical cloud data root
#'
#' @return A normalized path to the shared cloud data directory.
#' @keywords internal
default_cloud_data_root <- function() {
    file.path(default_cloud_root(), "data")
}

#' Return the local runtime data root
#'
#' @return A normalized path to the runtime data directory.
#' @keywords internal
default_runtime_data_root <- function() {
    file.path(default_runtime_root(), "data")
}

#' Return the local runtime output root
#'
#' @return A normalized path to the runtime output directory.
#' @keywords internal
default_runtime_output_root <- function() {
    file.path(default_runtime_root(), "output")
}

#' Return the canonical cloud output root
#'
#' @return A normalized path to the shared cloud output directory.
#' @keywords internal
default_cloud_output_root <- function() {
    file.path(default_cloud_root(), "output")
}

#' Return the dated publish folder for a release
#'
#' @param release_date A date or date-like value used to name the release.
#' @param publish_root Base cloud publish root.
#'
#' @return A normalized path to the dated release folder.
#' @keywords internal
project_publish_release_root <- function(release_date = Sys.Date(), publish_root = default_publish_root()) {
    file.path(path.expand(publish_root), "releases", format(as.Date(release_date), "%Y-%m-%d"))
}

#' Return the canonical project roots
#'
#' @param release_date A date or date-like value used to name the release.
#'
#' @return A named list with `code_root`, `runtime_root`, `publish_root`, and
#'   shared cloud roots.
#' @keywords internal
project_roots <- function(release_date = Sys.Date()) {
    publish_root <- default_publish_root()
    list(
        code_root = default_code_root(),
        runtime_root = default_runtime_root(),
        cloud_root = default_cloud_root(),
        data_root = default_cloud_data_root(),
        output_root = default_runtime_output_root(),
        publish_root = publish_root,
        release_root = project_publish_release_root(release_date, publish_root = publish_root)
    )
}

#' Normalize configured runtime-relative paths
#'
#' @param config A project configuration list.
#'
#' @return The input configuration with runtime-aware default paths applied.
#' @keywords internal
normalize_project_paths <- function(config) {
    runtime_root <- path.expand(config$runtime$root %||% default_runtime_root())
    cloud_data_root <- default_cloud_data_root()
    runtime_output_root <- file.path(runtime_root, "output")
    config$runtime <- merge_config_lists(
        list(
            slug = project_slug(),
            root = runtime_root,
            exports_root = file.path(runtime_root, "exports")
        ),
        config$runtime %||% list()
    )
    config$runtime$root <- path.expand(config$runtime$root %||% runtime_root)
    config$runtime$exports_root <- path.expand(config$runtime$exports_root %||% file.path(config$runtime$root, "exports"))

    config$data <- merge_config_lists(
        list(
            team_features_path = file.path(cloud_data_root, "pre_tournament_team_features.xlsx"),
            game_results_path = file.path(cloud_data_root, "tournament_game_results.xlsx")
        ),
        config$data %||% list()
    )
    config$data$team_features_path <- path.expand(config$data$team_features_path)
    config$data$game_results_path <- path.expand(config$data$game_results_path)

    default_model_cache_path <- file.path(default_runtime_output_root(), "model_cache")
    default_log_path <- file.path(default_runtime_output_root(), "logs", "tournament_simulation.log")
    default_refresh_log_path <- file.path(default_runtime_output_root(), "logs", "data_refresh.log")

    config$output$path <- path.expand(config$output$path %||% runtime_output_root)
    config$output$model_cache_path <- path.expand(
        if (is.null(config$output$model_cache_path) ||
            identical(path.expand(config$output$model_cache_path), path.expand(default_model_cache_path))) {
            file.path(config$output$path, "model_cache")
        } else {
            config$output$model_cache_path
        }
    )
    config$output$log_path <- path.expand(
        if (is.null(config$output$log_path) ||
            identical(path.expand(config$output$log_path), path.expand(default_log_path))) {
            file.path(config$output$path, "logs", "tournament_simulation.log")
        } else {
            config$output$log_path
        }
    )
    config$output$refresh_log_path <- path.expand(
        if (is.null(config$output$refresh_log_path) ||
            identical(path.expand(config$output$refresh_log_path), path.expand(default_refresh_log_path))) {
            file.path(config$output$path, "logs", "data_refresh.log")
        } else {
            config$output$refresh_log_path
        }
    )

    config
}

#' Default project configuration
#'
#' @return A nested list containing default data, model, and output settings.
#' @export
default_project_config <- function() {
    runtime_root <- default_runtime_root()
    cloud_data_root <- default_cloud_data_root()
    runtime_output_root <- default_runtime_output_root()
    list(
        runtime = list(
            slug = project_slug(),
            root = runtime_root,
            exports_root = file.path(runtime_root, "exports")
        ),
        data = list(
            team_features_path = file.path(cloud_data_root, "pre_tournament_team_features.xlsx"),
            game_results_path = file.path(cloud_data_root, "tournament_game_results.xlsx")
        ),
        model = list(
            history_window = 8L,
            backtest = TRUE,
            engine = "stan_glm",
            compare_engines = TRUE,
            allow_unavailable_comparison = FALSE,
            ensemble = default_ensemble_config(),
            bart = list(
                n_trees = 200L,
                n_burn = 500L,
                n_post = 1000L,
                k = 2,
                power = 2
            ),
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
            interaction_terms = character(0),
            prior_type = "normal",
            random_seed = 42,
            n_draws = 1000
        ),
        output = list(
            path = runtime_output_root,
            prefix = "tournament_sim",
            model_cache_path = file.path(runtime_output_root, "model_cache"),
            log_path = file.path(runtime_output_root, "logs", "tournament_simulation.log"),
            refresh_log_path = file.path(runtime_output_root, "logs", "data_refresh.log")
        )
    ) %>%
        normalize_project_paths()
}

#' Build a unique log file path for the current run
#'
#' @param log_path Base log file path to derive from.
#' @param timestamp Timestamp used to uniquify the file name.
#' @param process_id Process identifier used to reduce collisions.
#'
#' @return A unique log file path in the same directory as `log_path`.
#' @keywords internal
build_run_log_path <- function(log_path, timestamp = Sys.time(), process_id = Sys.getpid()) {
    log_path <- log_path %||% "tournament_simulation.log"
    log_dir <- dirname(log_path)
    log_name <- tools::file_path_sans_ext(basename(log_path))
    log_ext <- tools::file_ext(log_path)
    unique_stamp <- gsub("\\.", "", format(timestamp, "%Y%m%d_%H%M%OS6"))
    unique_name <- paste0(log_name, "_", unique_stamp, "_pid", process_id)
    if (nzchar(log_ext)) {
        unique_name <- paste0(unique_name, ".", log_ext)
    }

    if (identical(log_dir, ".")) {
        unique_name
    } else {
        file.path(log_dir, unique_name)
    }
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

    loaded_config <- normalize_project_paths(merge_config_lists(config, parsed_default))
    validate_ensemble_config(loaded_config)
    loaded_config
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
