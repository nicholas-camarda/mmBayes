library(tidyverse)
library(rstan)
library(rstanarm)
library(bayesplot)
library(loo)
library(logger)

#' Configure hierarchical model priors
#' @return List of prior configurations
configure_priors <- function() {
    list(
        team_strength = normal(0, 2.5, autoscale = TRUE),
        conference_effect = student_t(3, 0, 2.5),
        global_intercept = student_t(3, 0, 2.5),
        covariance = decov(shape = 2, scale = 2)
    )
}

#' Prepare data for Bayesian modeling
#' @param data Raw input data
#' @return Processed data frame
prepare_model_data <- function(data, metrics_to_use) {
    data %>%
        # Scale numeric predictors
        mutate(across(all_of(metrics_to_use), scale)) %>%
        # Create interaction terms
        mutate(
            strength_conf = overall_strength * as.numeric(factor(Conf)),
            seed_strength = overall_strength * (17 - Seed) # Higher seeds get more weight
        )
}

#' Fit Bayesian hierarchical model
#' @export
fit_tournament_model <- function(historical_data, metrics_to_use, random_seed = 123) {
    log_info("Starting model fitting process")
    
    # Validate and prepare data
    validate_input_data(historical_data)
    model_data <- prepare_model_data(historical_data, metrics_to_use)
    
    # Configure priors
    priors <- configure_priors()
    
    # Build formula with interactions
    formula_terms <- c(
        metrics_to_use,
        "strength_conf",
        "seed_strength"
    )
    formula_str <- paste("Champ ~", 
        paste(formula_terms, collapse = " + "), 
        "+ (1 | Conf)")
    formula_input <- as.formula(formula_str)
    
    # Fit model with increased iterations and diagnostics
    model <- stan_glmer(
        formula = formula_input,
        data = model_data,
        family = binomial(),
        prior = priors$team_strength,
        prior_intercept = priors$global_intercept,
        prior_covariance = priors$covariance,
        chains = 4,
        iter = 5000,
        warmup = 1000,
        thin = 2,
        seed = random_seed,
        cores = parallel::detectCores(),
        control = list(adapt_delta = 0.95)
    )
    
    # Comprehensive diagnostics
    diagnostics <- perform_model_diagnostics(model)
    
    # Return model and diagnostics
    list(
        model = model,
        diagnostics = diagnostics,
        data = model_data
    )
}

#' Perform comprehensive model diagnostics
#' @param model Fitted stan_glmer model
#' @return List of diagnostic measures
perform_model_diagnostics <- function(model) {
    # MCMC diagnostics
    rhat_values <- summary(model)[, "Rhat"]
    neff_ratio <- summary(model)[, "n_eff"] / (4 * 2000)
    
    # Posterior predictive checks
    pp_check_plot <- pp_check(model, nreps = 50)
    
    # LOO cross-validation
    loo_results <- loo(model)
    
    # Parameter recovery plots
    posterior_plots <- list(
        traces = mcmc_trace(model),
        densities = mcmc_dens_overlay(model),
        pairs = mcmc_pairs(model, pars = c("(Intercept)", metrics_to_use[1:3]))
    )
    
    list(
        convergence = list(
            rhat = rhat_values,
            neff_ratio = neff_ratio,
            any_divergent = any(get_divergent(model))
        ),
        posterior_checks = pp_check_plot,
        loo = loo_results,
        posterior_plots = posterior_plots
    )
} 