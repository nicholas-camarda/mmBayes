library(tidyverse)
library(here)
library(logger)
library(config)

# Initialize logging
log_threshold(INFO)
log_appender(appender_file("tournament_simulation.log"))

# Load configuration
config <- config::get()

# Main execution function
run_tournament_simulation <- function(config) {
    log_info("Starting tournament simulation")
    
    # Load and prepare data using new module
    data <- load_tournament_data(config)
    
    # Fit model with processed data
    model_results <- fit_tournament_model(
        data$historical,
        config$model$required_metrics,
        config$model$random_seed
    )
    
    # Run simulations with current year data
    simulation_results <- simulate_full_bracket(
        data$current,
        model_results$model,
        config$model$n_draws
    )
    
    # Create visualizations
    tournament_viz <- create_tournament_visualization(simulation_results)
    
    # Save results
    save_results(
        list(
            data = data,
            model = model_results,
            simulations = simulation_results,
            visualization = tournament_viz
        ),
        config$output
    )
    
    log_info("Tournament simulation completed")
    
    simulation_results
}

# Execute
results <- run_tournament_simulation(config) 