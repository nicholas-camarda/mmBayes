library(tidyverse)
library(rstan)
library(future)
library(progressr)
library(rstanarm)
library(GetoptLong)
library(readxl)
library(ncaahoopR)
library(tictoc)
library(pROC)
library(fuzzyjoin)

tic()
source(file.path("helper_functions.R"))
#' General procedure:
# 1. Prepare the data: the data should include all the matchups in the tournament, with the necessary variables
# (TEAM, opponent, home, game_id, season, BPI, BPI_RANK, SEED, FGM, FGA, FG_PCT, 3PTM, 3PTA, 3PT_PCT, FTM, FTA, FT_PCT, OREB,
# DREB, REB, AST, STL, BLK, TO, PF, PTS).

# 2. Fit the model: use the stan_glm function to fit a logistic regression model to the data, with the won_boolean variable as
# the response, and the other variables as predictors. Use the binomial() family to specify the logistic regression model.
# Use the same prior specifications as in the provided code.

# 3. Generate posterior predictive samples: use the posterior_predict function to generate posterior predictive samples from the model.
# The output will be a matrix with one row per posterior predictive sample, and one column per observation (i.e., per game).

# 4. Simulate the tournament: for each posterior predictive sample, simulate the tournament by randomly assigning winners for each matchup
# based on the predicted probabilities of winning from the logistic regression model.

# 5. Calculate the tournament probabilities: for each team, calculate the proportion of posterior predictive samples in which that
# team wins the tournament.

# 6. Identify the top 5 most likely brackets: for each posterior predictive sample, identify the bracket (i.e., the set of winners for
# each matchup) that maximizes the total number of correct predictions. Keep track of the frequency of each bracket across all
# posterior predictive samples, and identify the top 5 most likely brackets.

# 7. Output the results: present the top 5 most likely brackets and their corresponding probabilities.


# progressr info
handlers(handler_pbcol(
    adjust = 1.0,
    complete = function(s) cli::bg_red(cli::col_black(s)),
    incomplete = function(s) cli::bg_cyan(cli::col_black(s))
))

# STAN options
my_cores <- 4
num_chains <- my_cores
options(mc.cores = my_cores)
rstan_options(auto_write = FALSE)
perform_variable_selection <- TRUE

# change this for different bracket years; default is current year
bracket_year <- str_split(Sys.Date(), pattern = "-", simplify = TRUE)[, 1]

all_team_cached_data_fn <- file.path("data", bracket_year, "all_team_cached_data.rds")
dir.create(file.path("data", bracket_year), showWarnings = FALSE, recursive = TRUE)

if (!file.exists(all_team_cached_data_fn)) {
    message("Loading scraped data from cached folders [ generated from load.R ]...")
    loaded_data <- tibble(name = dir(file.path("cache", bracket_year), full.names = TRUE)) %>%
        mutate(
            TEAM = dir(file.path("cache", bracket_year)),
            data = map(name, .f = function(f) {
                fns <- dir(f, full.names = TRUE, recursive = TRUE)
                dat <- map(fns, .f = function(x) {
                    res <- read_tsv(x, show_col_types = FALSE)
                    return(res)
                }) %>% bind_rows()
                return(dat)
            })
        ) %>%
        dplyr::select(-name)

    message("Writing scraped data to file...")
    write_rds(x = loaded_data, file = all_team_cached_data_fn, compress = "gz")
} else {
    message("Loading scraped data from cached file...")
    loaded_data <- read_rds(all_team_cached_data_fn)
}
message("Done!")

##
## get validated team set and merge with loaded data
validated_team_set_df <- read_tsv(file.path("data", bracket_year, qq("validated_teams_with_bpi-@{bracket_year}.tsv")), show_col_types = FALSE)
#####

merged_init_data <- inner_join(loaded_data, validated_team_set_df, by = "TEAM") %>%
    dplyr::select(TEAM, BPI, BPI_RANK, SEED, data)

#' inspect if there was something wrong with the merge and we lost teams
stopifnot(merged_init_data %>% distinct(TEAM) %>% nrow() == nrow(res))

message("Organizing full dataset...")
all_data <- merged_init_data %>%
    mutate(SEED = as.integer(SEED)) %>%
    unnest(data) %>%
    group_by(TEAM, opponent, home, game_id, season, won_boolean, BPI, BPI_RANK, SEED) %>%
    summarise(
        FGM = sum(FGM), # field goals made
        FGA = sum(FGA), # field goals attempted
        FG_PCT = mean(FGM / FGA), # FG percent
        `3PTM` = sum(`3PTM`), # 3 point made
        `3PTA` = sum(`3PTA`), # 3 point attempted
        `3PT_PCT` = mean(`3PTM` / `3PTA`), # 3 point percent
        FTM = sum(FTM), # free throws made
        FTA = sum(FTA), # free throws attempted
        FT_PCT = mean(FTM / FTA), # free throw percent
        OREB = sum(OREB), # offensive rebound
        DREB = sum(DREB), # defensive rebound
        REB = sum(REB), # total rebounds
        AST = sum(AST), # assists
        STL = sum(STL), # steals
        BLK = sum(BLK), # blocks
        TO = sum(TO), # turn overs
        PF = sum(PF), # personal fouls
        PTS = sum(PTS), # team total points
        .groups = "keep"
    ) %>%
    ungroup() %>%
    arrange(TEAM, season)

# separate the data out into training and testing data, to evaluate the performance of our model
message("Splitting into training and testing data...")
train_indices <- sample(seq_len(nrow(all_data)), size = 0.8 * nrow(all_data))
train_data <- all_data[train_indices, ]
test_data <- all_data[-train_indices, ]

set.seed(1234)
cached_model_fn <- file.path("model_cache", bracket_year, str_c(qq("training_model_cache-@{bracket_year}.rds")))
dir.create(file.path("model_cache", bracket_year), showWarnings = FALSE, recursive = TRUE)

if (!file.exists(cached_model_fn)) {
    message("Fitting training model to evaluate performance...")
    tic()
    if (perform_variable_selection) {
        message("Performing variable selection with correlation...")

        numeric_data <- all_data[, sapply(all_data, is.numeric)]
        cor_matrix <- cor(numeric_data[, !(names(numeric_data) %in% c("home", "game_id", "season", "won_boolean"))], use = "complete.obs")
        all_variables <- colnames(cor_matrix)

        #' function to identify pairs of variables with correlation higher than a given threshold and exclude one variable from each pair
        exclude_highly_correlated <- function(cor_matrix, threshold = 0.7) {
            correlated_pairs <- which(abs(cor_matrix) > threshold & cor_matrix != 1, arr.ind = TRUE)
            exclude_vars <- unique(correlated_pairs[, 1])
            return(colnames(cor_matrix)[exclude_vars])
        }

        excluded_vars <- exclude_highly_correlated(cor_matrix, 0.7)

        #' @note for help creating the formula correctly, for any variable that starts with a number
        add_backticks <- function(x) {
            sapply(x, function(element) {
                if (grepl("^[0-9]", element)) {
                    paste0("`", element, "`")
                } else {
                    element
                }
            })
        }

        # Select top variables, create a formula
        important_variables <- add_backticks(setdiff(all_variables, excluded_vars))
        formula <- as.formula(paste("won_boolean ~", paste(important_variables, collapse = " + ")))

        message("Fitting L1 LASSO regularized model to further select important variables...")
        # Run LASSO L1 penalty regularized regression to further select important variables
        model <- stan_glm(formula,
            data = train_data,
            family = binomial(),
            prior = laplace(), # regularization
            QR = FALSE,
            cores = my_cores,
            chains = num_chains
        )
    } else {
        message("Using default important variables. Fitting model on training data...")
        formula <- "won_boolean ~ TEAM + opponent + home + BPI_RANK + SEED + FG_PCT + `3PT_PCT` + FT_PCT + REB + AST + STL + BLK + TO + PF"
        model <- stan_glm(
            formula = formula,
            family = binomial(),
            data = train_data,
            prior_intercept = normal(0, 2.5),
            prior = normal(0, 2.5),
            prior_aux = normal(0, 2.5),
            QR = FALSE,
            cores = my_cores,
            chains = num_chains
        )
    }
    toc()
    write_rds(model, file = cached_model_fn, compress = "gz")
} else {
    message("Loading cached model...")
    model <- read_rds(cached_model_fn)
}
message("Done!")

# assess model performance
posterior_predict_test <- posterior_predict(model, newdata = test_data)
# calculate the predicted probabilities of winning for each game in the test dataset by taking the mean of the posterior predictive samples:
predicted_probabilities <- colMeans(posterior_predict_test)

# Create the ROC object
roc_obj <- roc(test_data$won_boolean, predicted_probabilities)
auroc <- auc(roc_obj)

# Save the plot to a PNG file
message("Writing performance file...")
dir.create(file.path("performance", bracket_year), showWarnings = FALSE, recursive = TRUE)
png(file.path("performance", bracket_year, qq("training_model-roc_curve-@{bracket_year}.png")), width = 800, height = 600)
plot(roc_obj, main = "ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate", col = "#1c61b6", lwd = 3)
text(0.7, 0.3, paste("AUC =", round(auroc, 3)), cex = 1.2, col = "black")
abline(0, 1, lty = 2, col = "gray")
dev.off()
message("Done!")

# retrain the data on the full dataset after assessing performance
cached_full_model_fn <- file.path("model_cache", bracket_year, str_c(qq("FINAL_model_cache-@{bracket_year}.rds")))
if (!file.exists(cached_full_model_fn)) {
    message("Fitting full model now...")
    model_full <- stan_glm(formula,
        data = all_data,
        family = binomial(),
        prior = laplace(),
        seed = 1234,
        QR = FALSE,
        chains = 4,
        cores = my_cores
    )
    write_rds(x = model_full, cached_full_model_fn, compress = "gz")
} else {
    message("Detected cached model. Loading full model...")
    model_full <- read_rds(cached_full_model_fn)
}
message("Done!")

# Generate posterior predictive samples
# The posterior_samples output is a matrix with one row per posterior predictive sample and one column per observation (i.e., per game).
posterior_samples <- posterior_predict(model_full, newdata = all_data)
num_samples <- dim(posterior_samples)[1]


# TODO: HELP HERE!!
seeds_df <- all_data %>% distinct(TEAM, SEED)
seeds <- seeds_df$SEED
names(seeds) <- seeds_df$TEAM
# Assuming the 'seeds' variable contains the team names and their seed placements
initial_matchups_data <- generate_initial_matchups(seeds)

brackets <- list()
for (i in 1:num_samples) {
    bracket <- simulate_tournament(posterior_samples[i, ], all_data)
    brackets[[i]] <- bracket
}

bracket_freq <- table(sapply(brackets, toString))
top_5_brackets <- head(sort(bracket_freq, decreasing = TRUE), 5)

print(top_5_brackets)
