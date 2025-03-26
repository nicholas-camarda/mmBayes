library(tidyverse)
library(future)
library(furrr)
library(logger)

#' Simulate tournament matchup with uncertainty
#' @export
simulate_matchup <- function(teamA, teamB, round_name, matchup_number, 
                           model, draws = 1000) {
    log_info("Simulating matchup: {teamA$Team} vs {teamB$Team}")
    
    # Generate posterior predictions
    pred_data <- bind_rows(
        teamA %>% mutate(team_id = "A"),
        teamB %>% mutate(team_id = "B")
    ) %>%
        prepare_model_data(metrics_to_use)
    
    # Generate multiple posterior predictions
    posterior_preds <- posterior_predict(
        model, 
        newdata = pred_data,
        draws = draws
    )
    
    # Calculate win probabilities and uncertainties
    win_probs <- calculate_win_probabilities(posterior_preds)
    
    # Generate detailed matchup statistics
    matchup_stats <- generate_matchup_stats(teamA, teamB, win_probs)
    
    # Return comprehensive results
    create_matchup_result(
        teamA, teamB, round_name, matchup_number,
        win_probs, matchup_stats
    )
}

#' Calculate win probabilities from posterior predictions
#' @param posterior_preds Matrix of posterior predictions
#' @return List of probability metrics
calculate_win_probabilities <- function(posterior_preds) {
    team_A_wins <- posterior_preds[, 1] > posterior_preds[, 2]
    win_prob_A <- mean(team_A_wins)
    
    list(
        mean = win_prob_A,
        ci_lower = quantile(team_A_wins, 0.025),
        ci_upper = quantile(team_A_wins, 0.975),
        sd = sd(team_A_wins),
        density = density(team_A_wins)
    )
}

#' Simulate entire bracket with parallel processing
#' @export
simulate_full_bracket <- function(all_teams, model, draws = 1000) {
    log_info("Starting full bracket simulation")
    
    # Setup parallel processing
    plan(multisession)
    
    # Simulate regions in parallel
    region_results <- future_map(
        unique(all_teams$Region),
        ~simulate_region_bayesian(
            filter(all_teams, Region == .x),
            model,
            draws
        ),
        .progress = TRUE
    )
    
    # Simulate Final Four
    final_four <- simulate_final_four(
        map(region_results, "champion"),
        model,
        draws
    )
    
    list(
        region_results = region_results,
        final_four = final_four
    )
} 