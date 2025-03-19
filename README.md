# mmBayes
## March Madness Bayesian Bracket Picker

### Overview
mmBayes is a Bayesian-based March Madness bracket selection tool that utilizes historical and current season data to predict NCAA tournament outcomes. It incorporates advanced basketball metrics, Bayesian modeling, and prior distributions to simulate tournament results.

### Features
- **Data Loading & Preparation**
  - Extracts and processes historical NCAA tournament data.
  - Integrates team-level statistics from multiple sources.
  - Computes advanced basketball metrics such as efficiency ratings, strength indices, and upset likelihood.
  - Handles missing or incomplete data efficiently.

- **Bayesian Model Implementation**
  - Implements a hierarchical Bayesian logistic regression model using `stan_glmer`.
  - Generates prior distributions from historical tournament data.
  - Models tournament progression from the Round of 64 to the Championship.
  - Simulates thousands of tournament outcomes to estimate team win probabilities.

- **Customizable Priors & Predictive Metrics**
  - Uses priors based on factors such as:
    - Overall team strength
    - Tournament experience index
    - Clutch performance index
    - Conference strength
    - Turnover margin
    - Offensive and defensive efficiency
    - Adjusted tempo
  - Allows dynamic adjustments of priors based on historical data.
  
- **Simulation & Prediction**
  - Simulates each tournament round using posterior probabilities.
  - Assigns win probabilities to each matchup and advances winners accordingly.
  - Predicts the most probable Final Four teams and the National Champion.

- **Visualization & Reporting**
  - Generates stacked bar plots for win probabilities at each tournament round.
  - Outputs Bayesian confidence intervals for prediction accuracy.
  - Saves tournament predictions in markdown and PDF formats.

### Code Structure
#### `load.R`
- Loads and cleans data from various NCAA datasets.
- Computes advanced statistics used in the Bayesian model.
- Prepares historical data for prior generation.

#### `mmBayes.R`
- Defines the Bayesian model for tournament simulation.
- Implements functions for matchups, simulations, and tournament advancement.
- Integrates posterior probability calculations for each round.
- Generates and saves plots for tournament outcomes.

### Getting Started
1. Clone the repository:
   ```sh
   git clone https://github.com/yourusername/mmBayes.git
   ```
2. Install dependencies:
   ```r
   install.packages(c("rstanarm", "ggplot2", "tidyverse", "pals"))
   ```
3. Run the bracket prediction:
   ```r
   source("load.R")
   source("mmBayes.R")
   ```
4. View generated plots and tournament predictions in the `plots/` and `outputs/` directories.

### Future Enhancements
- Implement player-level performance adjustments.
- Incorporate real-time updates for in-season predictions.
- Expand Bayesian priors with more granular game-level data.

### License
This project is licensed under the MIT License.
