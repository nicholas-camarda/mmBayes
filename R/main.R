library(logger)

#' Run the tournament simulation end-to-end
#' @export
run_tournament_simulation <- function(config = NULL) {
    config <- config %||% load_project_config()
    initialize_logging()

    logger::log_info("Running tournament simulation pipeline")
    data <- load_tournament_data(config)
    model_results <- fit_tournament_model(
        historical_data = data$historical,
        metrics_to_use = config$model$required_metrics,
        random_seed = config$model$random_seed
    )
    simulation_results <- simulate_full_bracket(
        all_teams = data$current,
        model_results = model_results,
        draws = config$model$n_draws
    )
    visualization <- create_tournament_visualization(simulation_results)

    result_bundle <- list(
        bracket_year = data$bracket_year,
        data = data,
        model = model_results,
        simulations = simulation_results,
        final_four = simulation_results$final_four,
        visualization = visualization
    )

    result_bundle$output <- save_results(result_bundle, config$output)
    logger::log_info("Tournament simulation complete")

    result_bundle
}
