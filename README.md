# mmBayes

`mmBayes` is an R-based NCAA tournament bracket lab. It turns pre-tournament team data and historical results into Bayesian win probabilities, bracket candidates, and review artifacts for manual entry.

## What It Produces

The repository is built to generate a small set of decision-ready outputs:

- `output/bracket_dashboard.html`: the main bracket review console
- `output/technical_dashboard.html`: the technical model-and-simulation dashboard
- `output/bracket_decision_sheet.csv`: the ranked decision sheet for manual picks
- `output/bracket_candidate_1.csv` and `output/bracket_candidate_2.csv`: the safe bracket and alternate bracket paths
- `output/bracket_candidates.txt` and `output/bracket_candidates.rds`: candidate summaries and serialized bracket candidates
- `output/tournament_sim_candidate_brackets.txt`: a text summary of the candidate bracket set
- `output/tournament_sim.rds`: the full simulation bundle
- `output/tournament_sim_model_summary.txt` and `output/tournament_sim_backtest_summary.txt`: model and backtest summaries
- `output/candidate_matchup_total_points.csv`: matchup-level total points support
- `output/championship_tiebreaker_summary.csv` and `output/championship_tiebreaker_distribution.csv`: tiebreaker outputs
- `output/model_quality/latest_model_quality.rds` plus timestamped archives in `output/model_quality/`: reusable model-quality snapshots for the dashboards

## Preview

This repo is meant to produce bracket-entry materials you can review quickly in a browser or spreadsheet. The bracket visualization is the live HTML dashboard:

[Open the bracket dashboard](output/bracket_dashboard.html)
[Open the technical dashboard](output/technical_dashboard.html)

And the main review loop usually looks like this:

1. open `output/bracket_dashboard.html` to compare the safe bracket and alternate bracket
2. scan `output/bracket_decision_sheet.csv` for the hardest decisions first
3. use `output/bracket_candidate_1.csv` and `output/bracket_candidate_2.csv` if you want the underlying pick paths
4. check the status banner to see whether First Four slots are still simulated or already final

The dashboard is designed to answer two questions fast: which picks matter most, where the bracket is still sensitive to unresolved play-in games, and whether the most recent backtest snapshot says the model is sharp and calibrated enough to trust.

## Core Workflow

The standard simulation run does the following:

1. loads `config.yml`
2. reads canonical team features and historical tournament results from `data/`
3. fits a Bayesian matchup-level winner model
4. runs a rolling held-out-tournament backtest
5. simulates the current bracket forward using posterior probabilities
6. builds the dashboard, decision sheet, and candidate bracket exports

The active runtime lives in `R/`. The supported command-line entry points live in `scripts/`.

## Supported Commands

Run the full simulation pipeline:

```sh
Rscript scripts/run_simulation.R
```

Refresh the canonical tournament data files:

```sh
Rscript scripts/update_data.R
```

Generate faster bracket-entry artifacts without the full backtest:

```sh
Rscript scripts/run_bracket_candidates.R
```

Run a data-quality check:

```sh
Rscript scripts/data_quality_check.R
```

## Modeling Design

The active modeling core is matchup-based:

- historical training data is one row per actual tournament game
- predictors are restricted to pre-tournament season features and matchup context
- leakage-prone tournament outcome features are excluded from active modeling
- bracket picks are derived from calibrated game probabilities rather than a team-level `Champ` target

The canonical data refresh writes two files:

- `data/pre_tournament_team_features.xlsx`
- `data/tournament_game_results.xlsx`

The default model uses the most recent eight completed tournaments available in the data files, skips 2020, and backtests on rolling held-out years before predicting the current bracket.

## How The Model Works

The modeling approach is intentionally simple to explain:

1. each historical tournament game becomes one training row
2. the model compares the two teams using pre-tournament metrics like efficiency, resume strength, and seed
3. a Bayesian logistic regression estimates the probability that team A wins the matchup
4. posterior draws turn those probabilities into a ranked decision sheet and candidate bracket paths

In plain English, the model does not try to guess the whole bracket directly. It estimates game-by-game win probabilities, then rolls those probabilities forward through the bracket to see which picks survive and where the leverage spots are.

The Bayesian part matters because it keeps uncertainty visible. Instead of one hard yes/no answer, the model produces a posterior distribution, which is why the dashboard can show probabilities, intervals, and alternate bracket paths rather than just a single deterministic bracket.

## Model Snapshot

The active winner model is a Bayesian logistic regression with a logit link:

$$
\Pr(\text{team A wins}) = \text{logit}^{-1}(\alpha + \beta_{round} + \beta_{same\_conf} + \beta_{seed} + \beta_{barthag} + \cdots)
$$

In practice, the model uses these matchup predictors:

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

If you want the short version of what tends to matter most, it is usually the seed and efficiency-style terms: `seed_diff`, `barthag_logit_diff`, `AdjOE_diff`, `AdjDE_diff`, and `WAB_diff`. I would treat those as the most important drivers, not because the README is trying to rank them statistically, but because they are the most directly informative inputs in the model.

That said, this is a Bayesian model, so I would avoid phrasing things as simple "significant predictors" unless you are looking at posterior intervals or coefficient summaries from a specific fit. In this setup, the more useful question is which predictors have the clearest posterior contribution and which games remain uncertain after combining all the terms.

The repo also fits a separate Bayesian total-points model for tiebreaker support, so the dashboard can surface both pick quality and championship-score estimates.

## Repository Layout

- `R/`: active package runtime
- `scripts/`: supported command-line entry points
- `tests/`: automated tests and fixtures
- `output/`: generated bracket outputs and model artifacts
- `docs/methods-and-interpretation.md`: technical methods plus plain-English interpretation guide
- `docs/reference/`: background papers and supporting reference material

## Documentation

- [Methods and Interpretation Guide](docs/methods-and-interpretation.md): the main technical reference for the active runtime, including data sources, feature engineering, Bayesian likelihoods and priors, posterior summaries, and bracket decision logic.
- [Docs Index](docs/README.md)

## Example Outputs

If you want concrete examples of what the repo generates, these are the most useful artifacts to open first:

- [Bracket dashboard](output/bracket_dashboard.html)
- [Technical dashboard](output/technical_dashboard.html)
- [Decision sheet](output/bracket_decision_sheet.csv)
- [Safe bracket path](output/bracket_candidate_1.csv)
- [Alternate bracket path](output/bracket_candidate_2.csv)

The dashboard is the best place to start if you are trying to make picks. The spreadsheet files are better if you want to review or copy entries into a pool form.

## Manual Validation

From the repository root, run:

```sh
Rscript scripts/run_simulation.R
git status --short
```

Successful validation means:

- the Bayesian simulation command completes
- the backtest summary is written
- the dashboard and decision artifacts are regenerated
- only expected runtime artifacts are created
- no legacy script is used in the active workflow
