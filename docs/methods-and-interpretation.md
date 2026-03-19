# Methods and Interpretation Guide

## Purpose

`mmBayes` is a bracket-decision tool, not just a bracket simulator. The project
fits a Bayesian matchup model on historical NCAA tournament games, turns those
posterior game probabilities into a current bracket, and then exports review
artifacts that help a human choose picks quickly.

The current project is optimized for standard round-based bracket pool scoring.
That means the safest pick is not always the mathematically most likely team to
win a single game in isolation. Round value, downstream path stability, and how
many games a bracket asks you to get right all matter.

## Data and Modeling Pipeline

### Canonical inputs

The active refresh writes exactly two canonical files:

- `data/pre_tournament_team_features.xlsx`
- `data/tournament_game_results.xlsx`

The team-features file contains pre-tournament team metrics, seeds, regions, and
conference labels. The game-results file contains explicit tournament outcomes
for completed tournaments.

### Historical training rows

The model is trained at the matchup level. Each historical NCAA tournament game
becomes one training row after joining both teams to their pre-tournament
features. Every game is also duplicated with team order reversed so the model is
approximately symmetric by construction.

### Predictor set

The active predictor set is:

- `round`
- `same_conf`
- `seed_diff`
- `barthag_logit_diff`
- `AdjOE_diff`
- `AdjDE_diff`
- `WAB_diff`
- `TOR_diff`
- `TORD_diff`
- `ORB_diff`
- `DRB_diff`
- `3P%_diff`
- `3P%D_diff`
- `Adj T._diff`

These are all derived from pre-tournament season data plus matchup context.
Leakage-prone tournament outcome columns are excluded from the active model.

### Fitting and prediction

The active model is a Bayesian logistic regression fit with
`rstanarm::stan_glm`. Continuous matchup-difference predictors are standardized
from training data only, and the same scaling reference is reused at prediction
time.

For any new game, the runtime:

1. builds one matchup feature row from the two teams
2. draws posterior expected win probabilities for team A
3. summarizes those draws into a mean, interval, and posterior standard
   deviation

### Bracket generation

The current project supports two bracket flows:

- `scripts/run_simulation.R`: full workflow with rolling backtest
- `scripts/run_bracket_candidates.R`: fast workflow focused on bracket-entry
  decisions

Both flows generate candidate brackets from the same matchup model. First Four
games are resolved first where duplicate seeds exist, then the standard bracket
is simulated from those winners.

## Technical Methods

### Candidate 1

`Candidate 1` is the primary bracket. It is built by taking the higher
posterior-mean side in each game conditional on the bracket path that has been
created so far. In the current implementation this acts as the safest
expected-value bracket under standard pool scoring.

### Candidate 2

`Candidate 2` is a bounded-risk alternate. It is not just a random second
simulation. The runtime explores stochastic bracket samples, compares them to
`Candidate 1`, and ranks alternates by:

- bracket plausibility
- simulation frequency
- whether differences are concentrated in high-leverage slots
- whether the alternate avoids too many low-value early-round flips

The chosen alternate is meant to preserve the safest core while pivoting only in
credible leverage spots.

### Decision metadata

Every predicted game is augmented with:

- `posterior_favorite`
- `win_prob_favorite`
- `win_prob_underdog`
- `ci_lower`
- `ci_upper`
- `interval_width`
- `prediction_sd`
- `round_weight`
- `decision_score`
- `upset_leverage`
- `confidence_tier`
- `rationale_short`

These drive the review outputs:

- `output/bracket_dashboard.html`
- `output/bracket_decision_sheet.csv`
- `output/bracket_candidate_1.csv`
- `output/bracket_candidate_2.csv`
- `output/bracket_candidates.txt`

### Confidence tiers

The current rules are:

- `Lock`: favorite probability is high and the credible interval is still
  comfortably above 50%
- `Lean`: favorite is clear, but not overwhelming
- `Toss-up`: favorite edge is small enough that either side is reasonable
- `Volatile`: the interval is wide enough that the matchup should be treated as
  unstable even if one team is favored

## Plain-English Methods

### What the model is doing

For every game, the model compares the two teams using their pre-tournament
profile:

- how efficient they are on offense and defense
- how strong the season résumé looks
- how good the shooting and rebounding profile looks
- how much stronger one seed appears than the other
- whether the game is in a later round or inside the same conference

It does not know the future. It is trying to answer:

> Based on what we knew before the tournament started, how often should team A
> beat team B?

### What a win probability means

If a team shows up at `72%`, that does not mean the game is safe. It means that
if a game with that profile were played many times, the model expects that team
to win about 72 out of 100 times.

### What uncertainty means

The credible interval tells you how much the posterior still moves around after
seeing the historical training data. Wide intervals usually mean one of these:

- the matchup is close
- the model does not have a clean signal for separating the teams
- the round/path context makes the prediction less stable

## Interpretation Guide

### Candidate CSV fields

The candidate CSV files still report the direct team-A probability view:

- `win_prob_A`: posterior mean probability that `teamA` wins
- `ci_lower` and `ci_upper`: lower and upper credible bounds for `teamA`
- `prediction_sd`: posterior standard deviation for `teamA`
- `winner`: the selected pick for that candidate
- `upset`: whether the selected winner is the worse numeric seed

### Decision-sheet fields

The decision sheet re-expresses games from the favored-side perspective:

- `posterior_favorite`: the side with the higher posterior mean
- `win_prob_favorite`: favorite win probability
- `win_prob_underdog`: underdog win probability
- `ci_lower` and `ci_upper`: favorite-side interval bounds
- `interval_width`: width of the favorite-side interval
- `decision_score`: urgency score for review
- `upset_leverage`: how attractive an underdog pivot is under standard round
  scoring
- `candidate_diff_flag`: whether Candidate 2 differs from Candidate 1 in that
  bracket slot

### How to use the outputs quickly

If you are filling out a bracket under time pressure:

1. start with `output/bracket_candidates.txt`
2. open `output/bracket_dashboard.html`
3. use `Candidate 1` as the default bracket
4. only review the rows marked as hard decisions or candidate differences
5. use `output/bracket_candidate_1.csv` and `output/bracket_candidate_2.csv` as
   the manual-entry backups

## How To Pick Winners

The best practical rule is not just `probability > 50%`.

Use this order of operations instead:

1. default to `Candidate 1`
2. trust `Lock` games unless you have a very specific external reason not to
3. spend your time on `Toss-up` and `Volatile` games
4. prefer late-round pivots over random early-round upset hunting
5. use `Candidate 2` if you want one alternate bracket with upside but without
   throwing away the model core

In standard scoring pools, late-round mistakes are far more expensive than
missing one noisy Round of 64 upset. That is why the decision layer emphasizes
round weight and leverage, not just single-game upset probability.

## Known Limitations

- The model is still matchup-based and does not explicitly optimize against your
  pool opponents.
- Candidate 2 is a controlled alternate, not a full game-theoretic portfolio.
- Close games remain close. The model cannot eliminate real uncertainty.
- The current HTML dashboard is a static decision console, not an interactive
  app.
- The 2021 tournament remains a special case because one Round of 64 game was a
  no-contest.

## Best Current Practice

- Use `Candidate 1` as the safe bracket.
- Use `Candidate 2` as the alternate bracket.
- If you only have a few minutes, review the diff table and the hardest-decision
  rows and leave the rest alone.
