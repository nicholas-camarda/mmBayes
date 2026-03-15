#!/usr/bin/env Rscript

# Script to run the Bayesian March Madness tournament simulation
# Usage: Rscript run_simulation.R

# Load required libraries
library(tidyverse)
library(here)
library(logger)
library(rstan)
library(ggplot2)
requireNamespace("config", quietly = TRUE)

# Set Stan options to improve performance
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Source all R functions
source("R/setup.R")
source("R/data_loading.R")
source("R/model_fitting.R")
source("R/simulation_functions.R")
source("R/plotting_functions.R") 
source("R/utils.R")
source("R/main.R")

# Initialize logging
log_threshold(INFO)
log_appender(appender_file("tournament_simulation.log"))
log_info("Starting March Madness simulation")

# Run the tournament simulation
results <- run_tournament_simulation()

# Display some basic results
cat("\n=============================================\n")
cat("March Madness Tournament Prediction Results\n")
cat("=============================================\n\n")

# Show predicted champion
champion <- results$final_four$champion
cat("Predicted Champion:", champion$Team, "\n")
cat("Champion Strength:", round(champion$overall_strength, 4), "\n\n")

# Show Final Four teams
cat("Final Four Teams:\n")
for (i in 1:length(results$final_four$semifinalists)) {
  team <- results$final_four$semifinalists[[i]]
  cat(sprintf("%d. %s (Strength: %.4f)\n", i, team$Team, team$overall_strength))
}

cat("\nResults have been saved to the 'output' directory.\n")
cat("See 'output/tournament_sim_bracket.png' for the bracket visualization.\n")

log_info("Simulation complete") 