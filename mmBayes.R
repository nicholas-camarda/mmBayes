# -------------------------------------------------------------------
# 1) Load and PREPARE your validated_teams_with_bpi data
# -------------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(ggrepel)
library(rstan)
library(future)
library(progressr)
library(rstanarm)
library(GetoptLong)
library(readxl)
library(writexl)
library(tictoc)
library(pROC)
library(ggplot2)

#' * Make sure to run load.R first! *

# Gets the bracket year
bracket_year <- str_split(Sys.Date(), pattern = "-", simplify = TRUE)[, 1]
# Number of bracket variations
n_addtl_brackets <- 5 # if this number is 0, then no additional brackets are simulated
random_seed <- 123
set.seed(random_seed)

# Prior metrics
metrics_to_use <- c(
    "overall_strength", "barthag_logit", "AdjOE", "AdjDE", 
    "Clutch_Index", "Conf_Strength", "Upset_Factor", "Turnover_Edge"
)

# Run regional simulations
n_draws <- 100000
sd_value <- 10
priors_file <- sprintf("priors/priors_%s.rds", bracket_year)
data_fn <- "data/bayesian_model_data.xlsx"


historical_data <- read_excel(data_fn) %>%
    filter(Year != bracket_year) %>%
    mutate(R64 = as.factor(if_else(R64 == 1, 1, 0)))

data_init <- read_excel(data_fn) %>%
    filter(Year == bracket_year, R64 > 0.5)

# -------------------------------------------------------------------
# 2) Derive explicit bayesian priors
# -------------------------------------------------------------------
derive_explicit_priors <- function(historical_data, random_seed) {
    # Scale predictors for stability
    # historical_data_scaled <- historical_data %>%
    #     mutate(across(c(overall_strength, barthag_logit, AdjOE, AdjDE), scale)) # combined_metric,

    formula_input <- as.formula(paste("Champ ~", paste(metrics_to_use, collapse = " + "), "+ (1 | Conf)"))
    model <- stan_glmer(
        formula = formula_input,
        data = historical_data,
        family = binomial(),
        prior = normal(0, 2.5),
        prior_intercept = normal(0, 5),
        chains = 4, iter = 4000, seed = random_seed
    )
    # Check convergence explicitly:
    stan_summary <- summary(model)
    rhat_values <- stan_summary[, "Rhat"]

    if (any(rhat_values > 1.01, na.rm = TRUE)) {
        warning("Rhat indicates convergence problems; consider increasing iterations or checking priors/data.")
    } else {
        message("All Rhats < 1.01: Good convergence.")
    }

    # Extract posterior summaries explicitly
    posterior_summary <- summary(model, probs = c(0.025, 0.975))
    priors <- posterior_summary[, c("mean", "sd")]
    return(priors)
}

# Check if priors already exist for this year:
if (file.exists(priors_file)) {
    message("Loading existing priors for year: ", bracket_year)
    priors <- readRDS(priors_file)
} else {
    message("Generating new priors for year: ", bracket_year)
    priors <- derive_explicit_priors(historical_data, random_seed)
    dir.create("priors", showWarnings = FALSE) # ensure directory exists
    saveRDS(priors, priors_file)
}

# -------------------------------------------------------------------
# 2) Compute a Composite Strength measure from many columns
# -------------------------------------------------------------------
# Here we take a set of “relevant” numeric columns (adjust as needed)
compute_team_strength <- function(team) {
    # List of columns to use in the composite strength. (Adjust as needed.)
    # Up in front of script for easy access
    available <- metrics_to_use[metrics_to_use %in% names(team)]
    # Compute the composite as the mean of the available metrics (after converting to numeric)
    composite <- mean(as.numeric(team[available]), na.rm = TRUE)
    return(composite)
}

# -------------------------------------------------------------------
# 3) Simulate a single matchup using a Bayesian draws approach
#    (and compute credible intervals)
# -------------------------------------------------------------------
simulate_matchup <- function(teamA, teamB, round_name, matchup_number, priors, draws = 1000, sd = 10) {

    # Compute composite strengths for each team from all available metrics.
    strength_A <- compute_team_strength(teamA)
    strength_B <- compute_team_strength(teamB)

    # # Basic deterministic win probability from the logistic function:
    # diff <- strength_A - strength_B
    # win_prob_det <- 1 / (1 + exp(-diff / sd))

    # # Now simulate draws to capture uncertainty:
    # draws_A <- rnorm(draws, mean = strength_A, sd = sd)
    # draws_B <- rnorm(draws, mean = strength_B, sd = sd)
    # win_draws <- 1 / (1 + exp(-(draws_A - draws_B) / sd))
    # win_prob_sim <- mean(win_draws)

    # # Combine the deterministic and simulated win probabilities.
    # # (For example, we take the average; you might choose a different combination rule.)
    # win_prob_final <- (win_prob_det + win_prob_sim) / 2

    # Explicit Bayesian predictive draws using priors
    # When creating beta_draws, ensure consistent naming:
    beta_names <- metrics_to_use
    beta_means <- priors[beta_names, "mean"]
    beta_sds <- priors[beta_names, "sd"]

    beta_draws <- sapply(seq_along(beta_means), function(i) rnorm(draws, beta_means[i], beta_sds[i]))
    colnames(beta_draws) <- beta_names

    # Extract explicit intercept and random effect priors
    intercept_mean <- priors["(Intercept)", "mean"]
    intercept_sd <- priors["(Intercept)", "sd"]

    conf_effect_mean_A <- priors[paste0("b[(Intercept) Conf:", teamA$Conf, "]"), "mean"]
    conf_effect_sd_A <- priors[paste0("b[(Intercept) Conf:", teamA$Conf, "]"), "sd"]

    conf_effect_mean_B <- priors[paste0("b[(Intercept) Conf:", teamB$Conf, "]"), "mean"]
    conf_effect_sd_B <- priors[paste0("b[(Intercept) Conf:", teamB$Conf, "]"), "sd"]

    # Simulate intercept and conference random effects draws explicitly
    intercept_draws <- rnorm(draws, intercept_mean, intercept_sd)
    conf_draws_A <- rnorm(draws, conf_effect_mean_A, conf_effect_sd_A)
    conf_draws_B <- rnorm(draws, conf_effect_mean_B, conf_effect_sd_B)

    # Team vectors (no scaling)
    teamA_vector <- teamA %>%
        select(all_of(beta_names)) %>%
        as.numeric()
    teamB_vector <- teamB %>%
        select(all_of(beta_names)) %>%
        as.numeric()

    # Explicit linear predictor with intercept, random effects, and fixed effects
    draws_A <- intercept_draws + conf_draws_A + rowSums(sweep(beta_draws, 2, teamA_vector, "*"))
    draws_B <- intercept_draws + conf_draws_B + rowSums(sweep(beta_draws, 2, teamB_vector, "*"))

    # Convert to probabilities explicitly:
    win_draws <- plogis(draws_A - draws_B)
    win_prob_final <- mean(win_draws)

    # Compute credible intervals from the draws:
    lower_bound <- quantile(win_draws, 0.025)
    upper_bound <- quantile(win_draws, 0.975)

    # Determine the winner using deterministic procedure:
    winner <- if (win_prob_final >= 0.5) teamA$Team else teamB$Team

    # Determine the winner using stochastic procedure, with real probability of upset:
    # winner <- if (runif(1) < win_prob_final) teamA$Team else teamB$Team

    # Return a tibble with detailed matchup information.
    res <- data.frame(
        round = round_name,
        matchup_number = matchup_number,
        teamA = teamA$Team,
        teamA_composite = strength_A,
        teamB = teamB$Team,
        teamB_composite = strength_B,
        win_prob_A = round(win_prob_final, 2),
        # win_prob_lower = round(lower_bound, 2),
        # win_prob_upper = round(upper_bound, 2),
        winner = winner
    ) %>% as_tibble()
    return(res)
}

# -------------------------------------------------------------------
# 4) Predict which team from each Region/Seed slot reaches the Round of 64.
#     Here we assume that if a given seed slot has more than one team,
#     they are playing a play-in game.
# -------------------------------------------------------------------
predict_r64_in_region <- function(data, draws = 1000, sd = 10) {
    # Keep only needed columns and rename the composite metric column
    combined <- data %>%
        select(Year, Team, Seed, Region, Champ, all_of(metrics_to_use), Conf) # need to include Conf here as (1 | Conf) is in bayesian model
    # Group by Region and Seed. For each slot:
    predictions <- combined %>%
        group_by(Region, Seed) %>%
        group_modify(~ {
            slot <- unique(.x$Seed)
            reg <- unique(.x$Region)

            if (nrow(.x) == 1) {
                # Only one team in this Region/Seed slot wins by default.
                .x <- .x %>% mutate(predicted_R64 = 1)
            } else if (nrow(.x) == 2) {
                # Two teams: simulate a play-in matchup.
                sim <- simulate_matchup(.x[1, ], .x[2, ], "Play-In", 0, priors, draws, sd)
                message("Region: ", reg, " Seed: ", slot, " - Play-In matchup: ", sim$teamA, " vs ", sim$teamB, " | Winner: ", sim$winner)
                .x <- .x %>% mutate(predicted_R64 = if_else(Team == sim$winner, 1, 0))
            } else {
                # More than two teams: choose the one with the highest composite strength.
                composite_values <- sapply(1:nrow(.x), function(i) compute_team_strength(.x[i, ]))
                winner <- .x %>%
                    slice(which.max(composite_values)) %>%
                    pull(Team)
                message("Region: ", reg, " Seed: ", slot, " - Multiple teams: predicted winner is ", winner)
                .x <- .x %>% mutate(predicted_R64 = if_else(Team == winner, 1, 0))
            }
            .x
        }) %>%
        ungroup()
    return(predictions)
}

# -------------------------------------------------------------------
# 5) Regional Bracket Simulation using Bayesian draws (with fixed seed issues handled)
# -------------------------------------------------------------------
# Here we assume the bracket order is the standard 1 vs 16, 8 vs 9, etc.
fix_region_seeds <- function(region_teams, draws = 1000, sd = 10) {
    expected <- 1:16
    actual <- region_teams$Seed
    missing <- setdiff(expected, unique(actual))
    dup_seeds <- names(which(table(actual) > 1))

    if (length(missing) == 0 && length(dup_seeds) == 0) {
        return(region_teams %>% mutate(Assigned_Seed = Seed))
    }
    if (length(missing) == 1 && length(dup_seeds) == 1) {
        dup_val <- as.numeric(dup_seeds)
        missing_val <- missing[1]
        dup_indices <- which(region_teams$Seed == dup_val)
        if (length(dup_indices) != 2) stop("Unexpected number of duplicate entries found.")
        team1 <- region_teams[dup_indices[1], , drop = FALSE]
        team2 <- region_teams[dup_indices[2], , drop = FALSE]
        play_in_result <- simulate_matchup(team1, team2, "Seed Play-In", 0, priors, draws, sd)
        if (play_in_result$win_prob_A > 0.5) {
            region_teams$Assigned_Seed <- region_teams$Seed
            region_teams$Assigned_Seed[dup_indices[1]] <- dup_val
            region_teams$Assigned_Seed[dup_indices[2]] <- missing_val
            message("Seed Play-In: ", team1$Team, " wins over ", team2$Team)
        } else {
            region_teams$Assigned_Seed <- region_teams$Seed
            region_teams$Assigned_Seed[dup_indices[1]] <- missing_val
            region_teams$Assigned_Seed[dup_indices[2]] <- dup_val
            message("Seed Play-In: ", team2$Team, " wins over ", team1$Team)
        }
        return(region_teams)
    } else {
        warning("More complex seed issues detected; manual assignment may be needed.")
        return(region_teams %>% mutate(Assigned_Seed = Seed))
    }
}

simulate_region_bayesian <- function(region_teams, draws = 1000, sd = 10) {
    # region_teams = teams_East; draws = 1000; sd = 10
    region_teams <- fix_region_seeds(region_teams, draws, sd)
    region_teams <- region_teams %>% arrange(Assigned_Seed)
    bracket_order <- c(1, 16, 8, 9, 5, 12, 4, 13, 6, 11, 3, 14, 7, 10, 2, 15)
    ordered <- region_teams[match(bracket_order, region_teams$Assigned_Seed), ]
    region_name <- unique(region_teams$Region)
    message(qq("Simulating region: @{region_name}"))

    # --- Round of 16 ---
    matchups_R16 <- list()
    winners_R16 <- list()
    matchup_number <- 1
    for (i in seq(1, 16, by = 2)) {
        teamA <- ordered[i, , drop = FALSE]
        teamB <- ordered[i + 1, , drop = FALSE]
        result <- simulate_matchup(teamA, teamB, "Round of 16", matchup_number, priors, draws, sd)
        matchups_R16[[matchup_number]] <- result
        if (result$win_prob_A > 0.5) {
            winners_R16[[matchup_number]] <- teamA
        } else {
            winners_R16[[matchup_number]] <- teamB
        }
        matchup_number <- matchup_number + 1
    }
    round16_df <- bind_rows(matchups_R16)
    winners_R16 <- bind_rows(winners_R16)

    # --- Round of 8 ---
    matchups_R8 <- list()
    winners_R8 <- list()
    matchup_number <- 1
    for (i in seq(1, nrow(winners_R16), by = 2)) {
        result <- simulate_matchup(winners_R16[i, ], winners_R16[i + 1, ], "Round of 8", matchup_number, priors, draws, sd)
        matchups_R8[[matchup_number]] <- result
        if (result$win_prob_A > 0.5) {
            winners_R8[[matchup_number]] <- winners_R16[i, ]
        } else {
            winners_R8[[matchup_number]] <- winners_R16[i + 1, ]
        }
        matchup_number <- matchup_number + 1
    }
    round8_df <- bind_rows(matchups_R8)
    winners_R8 <- bind_rows(winners_R8)

    # --- Round of 4 ---
    matchups_R4 <- list()
    winners_R4 <- list()
    matchup_number <- 1
    for (i in seq(1, nrow(winners_R8), by = 2)) {
        result <- simulate_matchup(winners_R8[i, ], winners_R8[i + 1, ], "Round of 4", matchup_number, priors, draws, sd)
        matchups_R4[[matchup_number]] <- result
        if (result$win_prob_A > 0.5) {
            winners_R4[[matchup_number]] <- winners_R8[i, ]
        } else {
            winners_R4[[matchup_number]] <- winners_R8[i + 1, ]
        }
        matchup_number <- matchup_number + 1
    }
    round4_df <- bind_rows(matchups_R4)
    winners_R4 <- bind_rows(winners_R4)

    # --- Elite 8 (Region Final) ---
    round2_df <- simulate_matchup(winners_R4[1, ], winners_R4[2, ], "Elite 8", 1, priors, draws, sd)
    region_champion <- if (round2_df$win_prob_A > 0.5) winners_R4[1, ] else winners_R4[2, ]

    list(
        round_of_16 = round16_df,
        round_of_8 = round8_df,
        round_of_4 = round4_df,
        elite_8 = round2_df,
        champion = region_champion,
        seed_assignment = unique(region_teams$Assigned_Seed)
    )
}


# Run the prediction function and then filter to 64 teams.
predicted_R64 <- predict_r64_in_region(data_init, draws = 10000, sd = 10)
data_final <- predicted_R64 %>%
    filter(predicted_R64 == 1) %>%
    left_join(data_init) %>%
    suppressMessages()

if (nrow(data_final) != 64) {
    warning("The final dataset does not contain 64 teams; found ", nrow(data_final), " teams.")
} else {
    message("Final dataset contains 64 teams.")
}

dir.create("diagnostics", showWarnings = FALSE)
team_strengths <- data_final %>%
    rowwise() %>%
    mutate(composite_strength = compute_team_strength(cur_data())) %>%
    ungroup() %>%
    arrange(desc(composite_strength))

# Save a quick summary:
write_xlsx(team_strengths, "diagnostics/team_strength_summary.xlsx")


ggplot(team_strengths, aes(x = reorder(Team, composite_strength), y = composite_strength)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(title = "Composite Team Strengths", x = "Team", y = "Composite Strength") +
    theme_minimal()

ggsave("diagnostics/composite_strengths.png", width = 8, height = 12)

# Partition teams by region:
teams_East <- data_final %>%
    filter(Region == "East") %>%
    arrange(Seed)
teams_South <- data_final %>%
    filter(Region == "South") %>%
    arrange(Seed)
teams_West <- data_final %>%
    filter(Region == "West") %>%
    arrange(Seed)
teams_Midwest <- data_final %>%
    filter(Region == "Midwest") %>%
    arrange(Seed)


res_East <- simulate_region_bayesian(region_teams = teams_East, draws = n_draws, sd = sd_value)
res_South <- simulate_region_bayesian(teams_South, draws = n_draws, sd = sd_value)
res_West <- simulate_region_bayesian(teams_West, draws = n_draws, sd = sd_value)
res_Midwest <- simulate_region_bayesian(teams_Midwest, draws = n_draws, sd = sd_value)

# Extract regional champions:
champ_East <- res_East$champion
champ_South <- res_South$champion
champ_West <- res_West$champion
champ_Midwest <- res_Midwest$champion

# -------------------------------------------------------------------
# 6) Full 64-Team Bracket: Final Four and Championship Simulation
# -------------------------------------------------------------------
final_four_matchup1 <- simulate_matchup(champ_West, champ_South, "Final Four West vs South", 1, priors, draws = n_draws, sd = sd_value)
final_four_matchup2 <- simulate_matchup(champ_East, champ_Midwest, "Final Four East vs Midwest", 2, priors, draws = n_draws, sd = sd_value)
winner_semifinal1 <- if (final_four_matchup1$win_prob_A > 0.5) champ_West else champ_South
winner_semifinal2 <- if (final_four_matchup2$win_prob_A > 0.5) champ_East else champ_Midwest
championship_matchup <- simulate_matchup(winner_semifinal1, winner_semifinal2, "Championship", 1, priors, draws = n_draws, sd = sd_value)
national_champion <- if (championship_matchup$win_prob_A > 0.5) winner_semifinal1 else winner_semifinal2

if (n_addtl_brackets > 0) {
    # Container to hold bracket results
    bracket_results <- vector("list", n_addtl_brackets)

    for (i in 1:n_addtl_brackets) {
        seed_value <- 123 + i # Different seed each time
        set.seed(seed_value)

        message("Running bracket simulation with seed: ", seed_value)

        # Predict teams reaching round of 64 (reuse prior)
        predicted_R64 <- predict_r64_in_region(data_init, draws = 10000, sd = 10)

        data_final <- predicted_R64 %>%
            filter(predicted_R64 == 1) %>%
            left_join(data_init, by = c("Year", "Team", "Seed", "Region", "Champ", metrics_to_use, "Conf")) %>%
            suppressMessages()

        # Simulate each region
        res_East <- simulate_region_bayesian(data_final %>% filter(Region == "East"), draws = n_draws, sd = sd_value)
        res_South <- simulate_region_bayesian(data_final %>% filter(Region == "South"), draws = n_draws, sd = sd_value)
        res_West <- simulate_region_bayesian(data_final %>% filter(Region == "West"), draws = n_draws, sd = sd_value)
        res_Midwest <- simulate_region_bayesian(data_final %>% filter(Region == "Midwest"), draws = n_draws, sd = sd_value)

        # Final Four and Championship
        champ_East <- res_East$champion
        champ_South <- res_South$champion
        champ_West <- res_West$champion
        champ_Midwest <- res_Midwest$champion

        semifinal1 <- simulate_matchup(champ_West, champ_South, "Final Four West vs South", 1, priors, draws = n_draws, sd = sd_value)
        semifinal2 <- simulate_matchup(champ_East, champ_Midwest, "Final Four East vs Midwest", 2, priors, draws = n_draws, sd = sd_value)

        winner1 <- if (semifinal1$win_prob_A > 0.5) champ_West else champ_South
        winner2 <- if (semifinal2$win_prob_A > 0.5) champ_East else champ_Midwest

        championship <- simulate_matchup(winner1, winner2, "Championship", 1, priors, draws = n_draws, sd = sd_value)

        champion <- if (championship$win_prob_A > 0.5) winner1 else winner2

        # Store results neatly
        bracket_results[[i]] <- list(
            seed = seed_value,
            champion = champion$Team,
            semifinalists = c(champ_East$Team, champ_South$Team, champ_West$Team, champ_Midwest$Team),
            semifinal_results = bind_rows(semifinal1, semifinal2),
            championship_result = championship
        )
    }

    # Optionally save results as RDS for easy RMarkdown reference:
    write_rds(bracket_results, "bracket_results.rds")

    champ_probs <- map_df(bracket_results, ~ {
        tibble(
            Seed = .x$seed,
            Champion = .x$champion,
            Championship_Win_Prob = .x$championship_win_prob
        )
    })

    write_xlsx(champ_probs, "diagnostics/championship_probabilities.xlsx")
}

purrr::walk(bracket_results, ~ cat("Seed:", .x$seed, " - Champion:", .x$champion, "\n"))

# -------------------------------------------------------------------
# 7) Plotting: Create and save a stacked barplot for win probabilities
#     for each team in each matchup (for all rounds)
# -------------------------------------------------------------------
# Combine matchup data from all rounds and regions.
# (You may adjust which rounds to include, here we include Round of 16, Round of 8, Round of 4 and Elite 8.)
# Combine matchups from all regions and rounds, including Final Four and Championship
all_matchups <- bind_rows(
    res_East$round_of_16 %>% mutate(Region = "East", Round = "Round of 16"),
    res_East$round_of_8 %>% mutate(Region = "East", Round = "Round of 8"),
    res_East$round_of_4 %>% mutate(Region = "East", Round = "Round of 4"),
    res_East$elite_8 %>% mutate(Region = "East", Round = "Elite 8"),
    res_South$round_of_16 %>% mutate(Region = "South", Round = "Round of 16"),
    res_South$round_of_8 %>% mutate(Region = "South", Round = "Round of 8"),
    res_South$round_of_4 %>% mutate(Region = "South", Round = "Round of 4"),
    res_South$elite_8 %>% mutate(Region = "South", Round = "Elite 8"),
    res_West$round_of_16 %>% mutate(Region = "West", Round = "Round of 16"),
    res_West$round_of_8 %>% mutate(Region = "West", Round = "Round of 8"),
    res_West$round_of_4 %>% mutate(Region = "West", Round = "Round of 4"),
    res_West$elite_8 %>% mutate(Region = "West", Round = "Elite 8"),
    res_Midwest$round_of_16 %>% mutate(Region = "Midwest", Round = "Round of 16"),
    res_Midwest$round_of_8 %>% mutate(Region = "Midwest", Round = "Round of 8"),
    res_Midwest$round_of_4 %>% mutate(Region = "Midwest", Round = "Round of 4"),
    res_Midwest$elite_8 %>% mutate(Region = "Midwest", Round = "Elite 8"),

    # Final Four matchups
    final_four_matchup1 %>% mutate(Region = "Final Four", Round = "Semifinal"),
    final_four_matchup2 %>% mutate(Region = "Final Four", Round = "Semifinal"),

    # Championship matchup
    championship_matchup %>% mutate(Region = "Final Four", Round = "Championship")
) %>%
    mutate(
        Round = factor(Round, levels = c("Round of 16", "Round of 8", "Round of 4", "Elite 8", "Semifinal", "Championship")),
        Region = factor(Region, levels = c("East", "South", "West", "Midwest", "Final Four"))
    )

# For each matchup, calculate team B’s win probability as 1 - win_prob_A.
# Then, pivot the data longer so that each matchup produces two rows: one for teamA and one for teamB.
stacked_data <- all_matchups %>%
    mutate(teamB_win_prob = 1 - win_prob_A) %>%
    pivot_longer(
        cols = c(teamA, teamB),
        names_to = "side",
        values_to = "Team"
    ) %>%
    mutate(win_prob = if_else(side == "teamA", win_prob_A, teamB_win_prob))

# List of rounds for separate plots
rounds <- unique(stacked_data$Round)


# Create directory to store results
output_dir <- "bracket_results"
dir.create(output_dir, showWarnings = FALSE)

write_xlsx(stacked_data, file.path(output_dir, "stacked_results.xlsx"))

# Save region matchups to Excel files
write_xlsx(list(
    "Round of 16" = res_East$round_of_16,
    "Round of 8" = res_East$round_of_8,
    "Round of 4" = res_East$round_of_4,
    "Elite 8" = res_East$elite_8
), file.path(output_dir, "East_Region.xlsx"))

write_xlsx(list(
    "Round of 16" = res_South$round_of_16,
    "Round of 8" = res_South$round_of_8,
    "Round of 4" = res_South$round_of_4,
    "Elite 8" = res_South$elite_8
), file.path(output_dir, "South_Region.xlsx"))

write_xlsx(list(
    "Round of 16" = res_West$round_of_16,
    "Round of 8" = res_West$round_of_8,
    "Round of 4" = res_West$round_of_4,
    "Elite 8" = res_West$elite_8
), file.path(output_dir, "West_Region.xlsx"))

write_xlsx(list(
    "Round of 16" = res_Midwest$round_of_16,
    "Round of 8" = res_Midwest$round_of_8,
    "Round of 4" = res_Midwest$round_of_4,
    "Elite 8" = res_Midwest$elite_8
), file.path(output_dir, "Midwest_Region.xlsx"))

# Save Final Four and Championship results
write_xlsx(list(
    "Final Four" = bind_rows(final_four_matchup1, final_four_matchup2),
    "Championship" = championship_matchup
), file.path(output_dir, "Final_Four_and_Championship.xlsx"))

# -------------------------------------------------------------------
# 8) Output: Detailed Bracket Results
# -------------------------------------------------------------------
cat("\n=== EAST REGION MATCHUPS ===\n")
print(res_East$round_of_16)
print(res_East$round_of_8)
print(res_East$round_of_4)
print(res_East$elite_8)
cat("East Region Champion: ", champ_East$Team, "\n\n")

cat("=== SOUTH REGION MATCHUPS ===\n")
print(res_South$round_of_16)
print(res_South$round_of_8)
print(res_South$round_of_4)
print(res_South$elite_8)
cat("South Region Champion: ", champ_South$Team, "\n\n")

cat("=== WEST REGION MATCHUPS ===\n")
print(res_West$round_of_16)
print(res_West$round_of_8)
print(res_West$round_of_4)
print(res_West$elite_8)
cat("West Region Champion: ", champ_West$Team, "\n\n")

cat("=== MIDWEST REGION MATCHUPS ===\n")
print(res_Midwest$round_of_16)
print(res_Midwest$round_of_8)
print(res_Midwest$round_of_4)
print(res_Midwest$elite_8)
cat("Midwest Region Champion: ", champ_Midwest$Team, "\n\n")

cat("=== FINAL FOUR MATCHUPS ===\n")
print(final_four_matchup1)
print(final_four_matchup2)
cat("Semifinal Winners: ", winner_semifinal1$Team, " and ", winner_semifinal2$Team, "\n\n")

cat("=== CHAMPIONSHIP MATCHUP ===\n")
print(championship_matchup)
cat("National Champion: ", national_champion$Team, "\n")

# write all of these to excel files

