# mmBayes

An R-based NCAA tournament bracket lab. Turns pre-tournament team data and historical results into Bayesian win probabilities, ranked bracket candidates, and review artifacts for manual entry.

## Table of Contents

- [Quick Start](#quick-start)
- [Outputs](#outputs)
- [Commands](#commands)
- [How It Works](#how-it-works)
- [Model Details](#model-details)
- [Repository Layout](#repository-layout)
- [Documentation](#documentation)

---

## Quick Start

```sh
# 1. Refresh the canonical data files
Rscript scripts/update_data.R

# 2. Run the full simulation and generate all outputs
Rscript scripts/run_simulation.R

# 3. Open the bracket dashboard in your browser
open output/bracket_dashboard.html
```

Then scan `output/bracket_decision_sheet.csv` to identify the highest-leverage picks before filling out your entry.

---

## Outputs

After a pipeline run the following files are generated in `output/`. HTML dashboards and the human-readable CSV / TXT artifacts are committed to the repo so they can be reviewed directly on GitHub.

### Dashboards (HTML)

| File | Description |
|------|-------------|
| [`bracket_dashboard.html`](output/bracket_dashboard.html) | Main bracket review console |
| [`technical_dashboard.html`](output/technical_dashboard.html) | Model diagnostics and simulation detail |

### Decision Artifacts (CSV)

| File | Description |
|------|-------------|
| [`bracket_decision_sheet.csv`](output/bracket_decision_sheet.csv) | Ranked picks sorted by leverage — start here |
| [`bracket_candidate_1.csv`](output/bracket_candidate_1.csv) | Safe bracket path |
| [`bracket_candidate_2.csv`](output/bracket_candidate_2.csv) | Alternate bracket path |
| [`candidate_matchup_total_points.csv`](output/candidate_matchup_total_points.csv) | Matchup-level total-points support |
| [`championship_tiebreaker_summary.csv`](output/championship_tiebreaker_summary.csv) | Championship tiebreaker summary |
| [`championship_tiebreaker_distribution.csv`](output/championship_tiebreaker_distribution.csv) | Full tiebreaker score distribution |

### Summaries (TXT)

| File | Description |
|------|-------------|
| [`tournament_sim_candidate_brackets.txt`](output/tournament_sim_candidate_brackets.txt) | Text summary of all candidate brackets |
| [`tournament_sim_model_summary.txt`](output/tournament_sim_model_summary.txt) | Fitted model coefficient summary |
| [`tournament_sim_backtest_summary.txt`](output/tournament_sim_backtest_summary.txt) | Rolling backtest accuracy report |
| [`bracket_candidates.txt`](output/bracket_candidates.txt) | Concise candidate bracket listing |

### Heavy Artifacts (ignored by git)

- `tournament_sim.rds` — full simulation bundle
- `bracket_candidates.rds` — serialized bracket candidates
- `model_cache/` — cached model fits
- `model_quality/` — model-quality snapshots

---

## Commands

| Command | Purpose |
|---------|---------|
| `Rscript scripts/run_simulation.R` | Full pipeline: fit, backtest, simulate, export |
| `Rscript scripts/update_data.R` | Refresh canonical data files from source |
| `Rscript scripts/run_bracket_candidates.R` | Fast bracket export without full backtest |
| `Rscript scripts/data_quality_check.R` | Data-quality validation |

---

## How It Works

The pipeline runs in six steps:

1. Load `config.yml` and read team features + historical results from `data/`
2. Build one training row per historical tournament game (matchup-level)
3. Fit a Bayesian logistic regression for game-winner probability
4. Run a rolling held-out-tournament backtest to validate calibration
5. Simulate the current bracket forward round by round using posterior draws
6. Export dashboards, decision sheet, and candidate bracket files

The model estimates game-by-game win probabilities rather than predicting the whole bracket at once. Posterior draws surface those probabilities as ranked picks, uncertainty intervals, and alternate bracket paths — so the dashboard shows where the bracket is settled and where it is still sensitive to unresolved play-in games.

Data sources:

- `data/pre_tournament_team_features.xlsx` — pre-tournament season metrics per team
- `data/tournament_game_results.xlsx` — historical tournament game results

The default configuration uses the eight most recent completed tournaments (skipping 2020) and backtests on rolling held-out years.

---

## Model Details

### Winner Model

A Bayesian logistic regression with a logit link:

$$
\Pr(\text{team A wins}) = \text{logit}^{-1}(\alpha + \beta_{round} + \beta_{same\_conf} + \beta_{seed} + \beta_{barthag} + \cdots)
$$

Matchup predictors (differences between the two teams):

| Predictor | Description |
|-----------|-------------|
| `round` | Tournament round |
| `same_conf` | Conference matchup indicator |
| `seed_diff` | Seed difference |
| `barthag_logit_diff` | Logit-transformed Barthag rating difference |
| `AdjOE_diff` | Adjusted offensive efficiency difference |
| `AdjDE_diff` | Adjusted defensive efficiency difference |
| `WAB_diff` | Wins Above Bubble difference |
| `TOR_diff` / `TORD_diff` | Turnover rate (off / def) difference |
| `ORB_diff` / `DRB_diff` | Rebounding rate (off / def) difference |
| `3P%_diff` / `3P%D_diff` | Three-point shooting / defense difference |
| `Adj T._diff` | Adjusted tempo difference |

The seed and efficiency terms (`seed_diff`, `barthag_logit_diff`, `AdjOE_diff`, `AdjDE_diff`, `WAB_diff`) tend to carry the most posterior weight.

### Tiebreaker Model

A separate Bayesian Gaussian model estimates total championship points, powering the tiebreaker outputs in the dashboard.

---

## Repository Layout

```
mmBayes/
├── R/                  # Active package runtime
├── scripts/            # Command-line entry points
├── tests/              # Automated tests and fixtures
├── data/               # Canonical team features and game results (git-ignored)
├── output/             # Generated artifacts (HTML, CSV, TXT committed; RDS ignored)
├── docs/               # Methods guide and reference material
├── archive/            # Historical material no longer in the active workflow
└── config.yml          # Pipeline configuration
```

---

## Documentation

- [Methods and Interpretation Guide](docs/methods-and-interpretation.md) — data sources, feature engineering, Bayesian likelihoods and priors, posterior summaries, bracket decision logic
- [Docs Index](docs/README.md)
