library(dplyr)
library(logger)

require_bayesian_packages <- function() {
    required <- c("rstanarm", "bayesplot", "loo")
    missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]

    if (length(missing) > 0) {
        stop_with_message(
            sprintf(
                "Missing required Bayesian packages: %s. Install them before running mmBayes.",
                paste(missing, collapse = ", ")
            )
        )
    }
}

build_scaling_reference <- function(data, metrics_to_use) {
    purrr::map(metrics_to_use, function(metric) {
        values <- suppressWarnings(as.numeric(data[[metric]]))
        center <- mean(values, na.rm = TRUE)
        scale <- stats::sd(values, na.rm = TRUE)
        if (!is.finite(center)) {
            center <- 0
        }
        if (!is.finite(scale) || scale == 0) {
            scale <- 1
        }
        list(center = center, scale = scale)
    }) %>%
        stats::setNames(metrics_to_use)
}

apply_scaling_reference <- function(data, scaling_reference) {
    for (metric in names(scaling_reference)) {
        if (!metric %in% names(data)) {
            next
        }
        center <- scaling_reference[[metric]]$center
        scale <- scaling_reference[[metric]]$scale
        data[[metric]] <- (safe_numeric(data[[metric]]) - center) / scale
    }
    data
}

build_model_formula <- function(metrics_to_use) {
    quoted_metrics <- paste(sprintf("`%s`", metrics_to_use), collapse = " + ")

    stats::as.formula(
        paste(
            "Champ ~", quoted_metrics,
            "+ seed_strength + conf_power + historical_performance + (1 | Conf)"
        )
    )
}

#' Configure Bayesian priors
#' @export
configure_priors <- function() {
    require_bayesian_packages()

    list(
        fixed = rstanarm::normal(0, 2.5, autoscale = TRUE),
        intercept = rstanarm::normal(0, 2.5, autoscale = TRUE),
        covariance = rstanarm::decov(shape = 2, scale = 2)
    )
}

#' Prepare data for model fitting or prediction
#' @export
prepare_model_data <- function(data, metrics_to_use, scaling_reference = NULL, conf_levels = NULL) {
    validate_input_data(data, metrics_to_use)

    prepared_data <- add_derived_features(data)
    prepared_data <- impute_numeric_columns(prepared_data)
    prepared_data$Conf <- if (is.null(conf_levels)) {
        factor(prepared_data$Conf)
    } else {
        factor(prepared_data$Conf, levels = conf_levels)
    }

    if (!"seed_strength" %in% names(prepared_data)) {
        prepared_data <- dplyr::mutate(prepared_data, seed_strength = (17 - Seed) / 16)
    }
    if (!"conf_power" %in% names(prepared_data)) {
        prepared_data <- dplyr::mutate(prepared_data, conf_power = calculate_conference_power(prepared_data))
    }
    if (!"historical_performance" %in% names(prepared_data)) {
        prepared_data <- dplyr::mutate(
            prepared_data,
            historical_performance = calculate_historical_performance(prepared_data)
        )
    }

    scaling_reference <- scaling_reference %||% build_scaling_reference(prepared_data, metrics_to_use)
    prepared_data <- apply_scaling_reference(prepared_data, scaling_reference)
    prepared_data$Champ <- safe_numeric(prepared_data$Champ)

    list(
        data = prepared_data,
        scaling_reference = scaling_reference,
        conf_levels = levels(prepared_data$Conf)
    )
}

#' Fit the tournament model
#' @export
fit_tournament_model <- function(historical_data, metrics_to_use, random_seed = 123) {
    require_bayesian_packages()
    logger::log_info("Starting model fitting")
    set.seed(random_seed)

    prepared <- prepare_model_data(historical_data, metrics_to_use)
    formula_input <- build_model_formula(metrics_to_use)
    chains <- getOption("mmBayes.stan_chains", 4L)
    iter <- getOption("mmBayes.stan_iter", 2000L)
    refresh <- getOption("mmBayes.stan_refresh", 0L)
    priors <- configure_priors()

    logger::log_info("Using bayes model engine")
    logger::log_info("Stan settings: chains={chains}, iter={iter}")
    model <- rstanarm::stan_glmer(
        formula = formula_input,
        data = prepared$data,
        family = stats::binomial(link = "logit"),
        prior = priors$fixed,
        prior_intercept = priors$intercept,
        prior_covariance = priors$covariance,
        chains = chains,
        iter = iter,
        seed = random_seed,
        refresh = refresh
    )

    list(
        engine = "bayes",
        model = model,
        formula = formula_input,
        metrics_to_use = metrics_to_use,
        scaling_reference = prepared$scaling_reference,
        conf_levels = prepared$conf_levels,
        prepared_data = prepared$data,
        diagnostics = perform_model_diagnostics(model, "bayes", prepared$data)
    )
}

#' Perform model diagnostics for the active engine
#' @export
perform_model_diagnostics <- function(model, engine = "bayes", model_data = NULL) {
    posterior_draws <- nrow(as.matrix(model))
    nreps <- max(1L, min(10L, posterior_draws))

    list(
        engine = engine,
        summary = summary(model),
        loo = loo::loo(model),
        pp_check = bayesplot::pp_check(model, plotfun = "dens_overlay", nreps = nreps)
    )
}
