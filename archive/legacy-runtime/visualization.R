library(ggplot2)

# Define the bracket structure
regions <- c("East", "West", "South", "Midwest")
seeds <- rep(c(1, 16, 8, 9, 5, 12, 4, 13, 6, 11, 3, 14, 7, 10, 2, 15), times = 4)
teams <- rep(NA, times = 64)
bracket <- data.frame(Region = rep(regions, each = 16), Seed = seeds, Team = teams)

# Get the top 5 brackets
top_brackets <- top_brackets %>%
    mutate(simulations = as.character(simulations))

# Add the teams to the bracket
for (i in 1:5) {
    bracket$Team[which(bracket$Seed == i)] <- top_brackets$simulations[i:64]
}

# Define the team colors
team_colors <- c(
    "#000000", "#ffffff", "#cc0000", "#0000cc", "#ff6600", "#990099",
    "#00cc00", "#ff9900", "#008080", "#ff00ff", "#000080", "#ffff00",
    "#00ffff", "#ffa500", "#a52a2a", "#00ff00"
)

# Plot the bracket
ggplot(bracket, aes(x = Region, y = Seed, fill = Team)) +
    geom_tile(color = "black", size = 0.5) +
    scale_fill_manual(values = team_colors, na.value = "gray") +
    labs(x = NULL, y = NULL, title = "Top 5 Most Likely Brackets") +
    theme_void() +
    theme(legend.position = "none")
