# mmBayes

An R-based NCAA tournament bracket lab. Turns pre-tournament team data, historical results, and sportsbook market data into Bayesian win probabilities, ranked bracket candidates, and review artifacts for manual entry.

## Table of Contents

- [Preview](#preview)
- [Quick Start](#quick-start)
- [Outputs](#outputs)
- [Commands](#commands)
- [How It Works](#how-it-works)
- [Model Details](#model-details)
- [Betting Data](#betting-data-default-input)
- [Repository Layout](#repository-layout)
- [Documentation](#documentation)

---

## Preview

This repo is meant to produce bracket-entry materials you can review quickly in a browser or spreadsheet. The bracket visualization is the live HTML dashboard:

[Open the bracket dashboard](output/bracket_dashboard.html)
[Open the technical dashboard](output/technical_dashboard.html)

The main review loop usually looks like this:

1. open [output/bracket_dashboard.html](output/bracket_dashboard.html) to compare the safe bracket and alternate bracket
2. scan [output/bracket_decision_sheet.csv](output/bracket_decision_sheet.csv) for the hardest decisions first
3. use [output/bracket_candidate_1.csv](output/bracket_candidate_1.csv) and [output/bracket_candidate_2.csv](output/bracket_candidate_2.csv) if you want the underlying pick paths
4. check the status banner to see whether First Four slots are still simulated or already final

The dashboard is designed to answer two questions fast: which picks matter most, where the bracket is still sensitive to unresolved play-in games, and whether the most recent backtest snapshot says the model is sharp and calibrated enough to trust.

## Working Roots

- Code checkout: `~/Projects/mmBayes`
- Runtime scratch: `~/ProjectsRuntime/mmBayes`
- Cloud project root: `~/Library/CloudStorage/OneDrive-Personal/SideProjects/mmBayes`
- Cloud data: `~/Library/CloudStorage/OneDrive-Personal/SideProjects/mmBayes/data/`
- Cloud outputs: `~/Library/CloudStorage/OneDrive-Personal/SideProjects/mmBayes/output/`
- Cloud publish drops: `~/Library/CloudStorage/OneDrive-Personal/SideProjects/mmBayes/releases/<YYYY-MM-DD>/`

The checkout is a local workspace. The cloud project root now holds the canonical inputs and shared outputs, while the runtime root stays available for local scratch artifacts.

## Quick Start

```sh
# 1. Refresh the canonical data files
Rscript scripts/update_data.R

# 2. Run the full simulation and generate all outputs
Rscript scripts/run_simulation.R

# 3. Open the bracket dashboard in your browser
open ~/Library/CloudStorage/OneDrive-Personal/SideProjects/mmBayes/output/bracket_dashboard.html
```

Then scan [output/bracket_decision_sheet.csv](output/bracket_decision_sheet.csv) to identify the highest-leverage picks before filling out your entry.
The pipeline writes its live outputs under `~/Library/CloudStorage/OneDrive-Personal/SideProjects/mmBayes/output` by default.

---

## Outputs

After a pipeline run the following files are generated in the cloud output directory, which defaults to `~/Library/CloudStorage/OneDrive-Personal/SideProjects/mmBayes/output`. The repository keeps example outputs for reference, but the active run artifacts live in the cloud output tree.

### Dashboards (HTML)

| File | Description |
|------|-------------|
| [bracket_dashboard.html](output/bracket_dashboard.html) | Main bracket review console |
| [technical_dashboard.html](output/technical_dashboard.html) | Model diagnostics and simulation detail |

### Decision Artifacts (CSV)

| File | Description |
|------|-------------|
| [bracket_decision_sheet.csv](output/bracket_decision_sheet.csv) | Ranked picks sorted by leverage - start here |
| [bracket_candidate_1.csv](output/bracket_candidate_1.csv) | Safe bracket path |
| [bracket_candidate_2.csv](output/bracket_candidate_2.csv) | Alternate bracket path |
| [candidate_matchup_total_points.csv](output/candidate_matchup_total_points.csv) | Matchup-level total-points support |
| [championship_tiebreaker_summary.csv](output/championship_tiebreaker_summary.csv) | Championship tiebreaker summary |
| [championship_tiebreaker_distribution.csv](output/championship_tiebreaker_distribution.csv) | Full tiebreaker score distribution |

### Summaries (TXT)

| File | Description |
|------|-------------|
| [tournament_sim_candidate_brackets.txt](output/tournament_sim_candidate_brackets.txt) | Text summary of all candidate brackets |
| [tournament_sim_model_summary.txt](output/tournament_sim_model_summary.txt) | Fitted model coefficient summary |
| [tournament_sim_backtest_summary.txt](output/tournament_sim_backtest_summary.txt) | Rolling backtest accuracy report |
| [bracket_candidates.txt](output/bracket_candidates.txt) | Concise candidate bracket listing |

### Heavy Artifacts (ignored by git)

- `tournament_sim.rds` - full simulation bundle
- `bracket_candidates.rds` - serialized bracket candidates
- `model_cache/` - cached model fits
- `model_quality/` - model-quality snapshots

---

## Commands

| Command | Purpose |
|---------|---------|
| `Rscript scripts/run_simulation.R` | Full pipeline: fit, backtest, simulate, export |
| `Rscript scripts/update_data.R` | Refresh canonical data files from source |
| `Rscript scripts/run_bracket_candidates.R` | Fast bracket export without full backtest |
| `Rscript scripts/data_quality_check.R` | Data-quality validation |
| `Rscript scripts/capture_odds_snapshot.R` | Capture a private Odds API snapshot |
| `Rscript scripts/build_closing_lines.R --year=2026` | Derive closing-line estimates from saved snapshots |
| `Rscript scripts/publish_release.R` | Copy approved deliverables and the odds-history snapshot into the dated OneDrive release folder |
| `Rscript scripts/evaluate_odds_blend.R --year=2026` | Compare model-only vs blended probabilities against closing lines |

Notes:

- `scripts/run_simulation.R`, `scripts/run_bracket_candidates.R`, and odds scripts load `.env` when present via `load_dotenv_file()`.
- Logs are written under `output/logs/`; full simulation logs are uniquely timestamped per run.
- Odds workflows keep local history under `data/odds_history/` (gitignored).

---

## How It Works

The pipeline runs in seven steps:

1. Load `config.yml` and read team features plus historical results from `data/`
2. Build one training row per historical tournament game at the matchup level
3. Fit a Bayesian logistic regression for game-winner probability
4. Resolve local betting lines (and optionally capture a new Odds API snapshot when enabled and missing)
5. Run a rolling held-out-tournament backtest to validate calibration
6. Simulate the current bracket forward round by round using posterior draws (optionally blending model and market probabilities by round)
7. Export dashboards, the decision sheet, and candidate bracket files

The model estimates game-by-game win probabilities rather than predicting the whole bracket at once. Posterior draws surface those probabilities as ranked picks, uncertainty intervals, and alternate bracket paths, so the dashboard shows where the bracket is settled and where it is still sensitive to unresolved play-in games.

Data sources:

- `~/Library/CloudStorage/OneDrive-Personal/SideProjects/mmBayes/data/pre_tournament_team_features.xlsx` - pre-tournament season metrics per team
- `~/Library/CloudStorage/OneDrive-Personal/SideProjects/mmBayes/data/tournament_game_results.xlsx` - historical tournament game results

The default configuration uses the eight most recent completed tournaments, skips 2020, and backtests on rolling held-out years.

---

## Model Details

### Winner Model

`mmBayes` currently supports two engines for winner prediction:

- `stan_glm` for Bayesian logistic regression with explicit priors
- `bart` for Bayesian additive regression trees

There is no neural-network engine in this repo today.

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

The seed and efficiency terms (`seed_diff`, `barthag_logit_diff`, `AdjOE_diff`, `AdjDE_diff`, `WAB_diff`) tend to carry the most posterior weight, and the default model now also includes betting-derived features such as line-implied probability, spread, bookmaker coverage, freshness, and market disagreement.

### Tiebreaker Model

A separate total-points model estimates championship points, powering the tiebreaker outputs in the dashboard. It uses the same engine family selected for the winner model and can also consume betting-derived features.

---

## Betting Data (Default Input)

Betting data is a default model input because it captures latent information the repo cannot access directly from season stats alone, including injuries, coaching adjustments, and market consensus.

- The Odds API key is read from `ODDS_API_KEY` and is never written to disk.
- Bookmakers default to `draftkings`, `fanduel`, `betmgm`, and `betrivers`.
- Markets default to `h2h` moneyline and `spreads`.
- Runtime odds history lives under `~/Library/CloudStorage/OneDrive-Personal/SideProjects/mmBayes/data/odds_history`.
- Historical training rows use time-appropriate closing or last-pre-commence lines when available.
- Current prediction rows use the latest reusable snapshot for the active tournament year.
- `Rscript scripts/evaluate_odds_blend.R` now runs a betting-feature ablation that compares a no-betting baseline, the betting-enhanced model, and a market-only benchmark.

---

## Repository Layout

```
mmBayes/
├── R/                  # Active package runtime
├── scripts/            # Command-line entry points
├── tests/              # Automated tests and fixtures
├── data/               # Canonical team features and game results
├── output/             # Checked-in example outputs; active runs use the runtime output root
├── docs/               # Methods guide and reference material
├── archive/            # Historical material no longer in the active workflow
└── config.yml          # Pipeline configuration
```

Local runtime artifacts such as transient logs and scratch caches can still live under `~/ProjectsRuntime/mmBayes`, but the canonical data and shared outputs live under the cloud project root.

---

## Documentation

- [Methods and Interpretation Guide](docs/methods-and-interpretation.md) - data sources, feature engineering, Bayesian likelihoods and priors, posterior summaries, bracket decision logic
