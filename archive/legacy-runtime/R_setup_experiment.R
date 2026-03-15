# setup.R - Package setup and configuration

#' @import tidyverse
#' @import readxl
#' @import logger
#' @import here
#' @import rstan
#' @import rstanarm
#' @import bayesplot
#' @import loo
#' @import config
#' @import assertthat
#' @import future
#' @import furrr
#' @import plotly
#' @import patchwork

# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Set Stan options for performance
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Initialize logging
log_threshold(INFO)
log_appender(appender_file("tournament_simulation.log"))

# Set here() as the base directory
here::i_am("R/setup.R")

# Install required packages
install.packages(c(
    "tidyverse", "rstan", "rstanarm", "bayesplot", "loo",
    "future", "furrr", "plotly", "testthat", "assertthat",
    "here", "logger", "config"
)) 