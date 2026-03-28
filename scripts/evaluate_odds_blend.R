#!/usr/bin/env Rscript

#' Find the absolute path to the current script
#'
#' @return A normalized absolute path to the running script or working directory.
#' @keywords internal
find_script_path <- function() {
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("^--file=", args, value = TRUE)
    if (length(file_arg) == 0) {
        return(normalizePath(getwd()))
    }
    normalizePath(sub("^--file=", "", file_arg[1]))
}

script_path <- find_script_path()
project_root <- normalizePath(file.path(dirname(script_path), ".."))
setwd(project_root)

pkgload::load_all(project_root, export_all = TRUE, helpers = FALSE, quiet = TRUE)
load_dotenv_file(".env", override = FALSE)

config <- load_project_config("config.yml")
loaded <- load_tournament_data(config)
historical_betting_features <- loaded$historical_betting_features %||% tibble::tibble()

if (nrow(historical_betting_features) == 0) {
    stop_with_message("No historical closing lines were found under the configured odds-history root.")
}

interaction_terms <- as.character(unlist(config$model$interaction_terms %||% character(0)))
if (length(interaction_terms) == 0L) {
    interaction_terms <- NULL
}

engine <- config$model$engine %||% "stan_glm"
bart_config <- config$model$bart %||% list()
baseline_predictors <- setdiff(config$model$required_predictors, betting_matchup_feature_columns())
enhanced_predictors <- config$model$required_predictors
draws_budget <- if (identical(engine, "bart")) {
    as.integer(bart_config$n_post %||% 1000L)
} else {
    as.integer(config$model$n_draws %||% 1000L)
}

baseline_backtest <- run_rolling_backtest(
    historical_teams = loaded$historical_teams,
    historical_actual_results = loaded$historical_actual_results,
    predictor_columns = baseline_predictors,
    engine = engine,
    bart_config = bart_config,
    random_seed = config$model$random_seed,
    draws = draws_budget,
    interaction_terms = interaction_terms,
    prior_type = config$model$prior_type %||% "normal",
    historical_betting_features = historical_betting_features,
    use_cache = FALSE
)

enhanced_backtest <- run_rolling_backtest(
    historical_teams = loaded$historical_teams,
    historical_actual_results = loaded$historical_actual_results,
    predictor_columns = enhanced_predictors,
    engine = engine,
    bart_config = bart_config,
    random_seed = config$model$random_seed,
    draws = draws_budget,
    interaction_terms = interaction_terms,
    prior_type = config$model$prior_type %||% "normal",
    historical_betting_features = historical_betting_features,
    use_cache = FALSE
)

market_predictions <- enhanced_backtest$predictions %>%
    dplyr::mutate(
        market_prob = betting_prob_centered + 0.5
    ) %>%
    dplyr::filter(betting_line_available > 0)

if (nrow(market_predictions) == 0) {
    stop_with_message("Historical betting features were loaded, but none survived into backtest predictions.")
}

market_metrics <- compute_binary_metrics(market_predictions$market_prob, market_predictions$actual_outcome)
baseline_summary <- baseline_backtest$summary %>% dplyr::mutate(model = "baseline_no_betting")
enhanced_summary <- enhanced_backtest$summary %>% dplyr::mutate(model = "betting_features")
market_summary <- tibble::tibble(
    mean_log_loss = market_metrics$log_loss[[1]],
    mean_brier = market_metrics$brier[[1]],
    mean_accuracy = market_metrics$accuracy[[1]],
    mean_bracket_score = NA_real_,
    mean_correct_picks = NA_real_,
    n_holdout_games = nrow(market_predictions),
    model = "market_only"
)

comparison <- dplyr::bind_rows(
    baseline_summary %>% dplyr::select(model, dplyr::everything()),
    enhanced_summary %>% dplyr::select(model, dplyr::everything()),
    market_summary %>% dplyr::select(model, dplyr::everything())
) %>%
    dplyr::mutate(
        delta_vs_baseline_log_loss = mean_log_loss - baseline_summary$mean_log_loss[[1]],
        delta_vs_baseline_brier = mean_brier - baseline_summary$mean_brier[[1]],
        delta_vs_baseline_accuracy = mean_accuracy - baseline_summary$mean_accuracy[[1]],
        delta_vs_baseline_bracket_score = mean_bracket_score - baseline_summary$mean_bracket_score[[1]]
    )

output_dir <- config$output$path %||% default_runtime_output_root()
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
summary_path <- file.path(output_dir, "betting_feature_ablation_summary.csv")
predictions_path <- file.path(output_dir, "betting_feature_ablation_predictions.csv")
utils::write.csv(comparison, summary_path, row.names = FALSE)
utils::write.csv(
    enhanced_backtest$predictions %>%
        dplyr::mutate(
            model_prob_no_betting = baseline_backtest$predictions$predicted_prob,
            model_prob_with_betting = predicted_prob,
            market_prob = betting_prob_centered + 0.5
        ),
    predictions_path,
    row.names = FALSE
)

cat("Betting feature ablation:\n")
cat(sprintf("- Summary: %s\n", summary_path))
cat(sprintf("- Predictions: %s\n", predictions_path))
cat(sprintf(
    "- Enhanced vs baseline delta: log_loss=%.4f brier=%.4f accuracy=%.4f bracket_score=%.4f\n",
    comparison$delta_vs_baseline_log_loss[comparison$model == "betting_features"],
    comparison$delta_vs_baseline_brier[comparison$model == "betting_features"],
    comparison$delta_vs_baseline_accuracy[comparison$model == "betting_features"],
    comparison$delta_vs_baseline_bracket_score[comparison$model == "betting_features"]
))
