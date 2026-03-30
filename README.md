# mmBayes

An R-based NCAA tournament bracket lab. Turns pre-tournament team data, historical results, and a separate sportsbook odds archive into Bayesian win probabilities, ranked bracket candidates, and review artifacts for manual entry.

## Table of Contents

- [Preview](#preview)
- [Quick Start](#quick-start)
- [Outputs](#outputs)
- [Commands](#commands)
- [How It Works](#how-it-works)
- [Model Details](#model-details)
- [Betting Data](#betting-data-sidecar)
- [Repository Layout](#repository-layout)
- [Documentation](#documentation)

---

## Preview

This repo is meant to produce bracket-entry materials you can review quickly in a browser or spreadsheet.

If you only open one file, open the main bracket dashboard:

[Open the bracket dashboard](https://nicholas-camarda.github.io/mmBayes/output/bracket_dashboard.html)

The GitHub Pages hub is just a convenience landing page:

[Open the GitHub Pages dashboard hub](https://nicholas-camarda.github.io/mmBayes/)

To refresh the GitHub-linked dashboards from the latest runtime HTML, run:

`Rscript scripts/publish_github_pages.R`

If you published the latest bundle to the cloud output tree instead of the local runtime root, set `MMBAYES_PAGES_SOURCE` to that folder before running the sync.

The main review loop usually looks like this:

1. open [output/bracket_dashboard.html](output/bracket_dashboard.html) to build the two entries and review the evidence trail
2. scan [output/bracket_decision_sheet.csv](output/bracket_decision_sheet.csv) for the hardest decisions first
3. use [output/bracket_candidate_1.csv](output/bracket_candidate_1.csv) and [output/bracket_candidate_2.csv](output/bracket_candidate_2.csv) if you want the underlying pick paths
4. check the status banner to see whether First Four slots are still simulated or already final

The main dashboard is designed to answer three jobs fast: what Candidate 1 and Candidate 2 are, which evidence drives the key decisions, and which matchups deserve another look before you trust the entries.
Live tournament commentary is separate from the prediction-time inputs: First Four results can resolve the bracket path, while completed Round of 64+ games are tracked for monitoring and dashboard commentary only.

Optional diagnostics:

- [Technical dashboard](https://nicholas-camarda.github.io/mmBayes/output/technical_dashboard.html) - diagnostics and simulation detail
- [Model comparison dashboard](https://nicholas-camarda.github.io/mmBayes/output/model_comparison_dashboard.html) - engine comparison and calibration appendix

## Working Roots

- Code checkout: `~/Projects/mmBayes`
- Runtime scratch: `~/ProjectsRuntime/mmBayes`
- Cloud project root: `~/Library/CloudStorage/OneDrive-Personal/SideProjects/mmBayes`
- Cloud data: `~/Library/CloudStorage/OneDrive-Personal/SideProjects/mmBayes/data/`
- Cloud outputs: published copies under `~/Library/CloudStorage/OneDrive-Personal/SideProjects/mmBayes/output/`
- Cloud publish drops: `~/Library/CloudStorage/OneDrive-Personal/SideProjects/mmBayes/releases/<YYYY-MM-DD>/`

The checkout is a local workspace. The cloud project root holds the canonical input data and published release bundles. The runtime root stays available for local scratch artifacts and live generated outputs.

## Quick Start

```sh
# 1. Refresh the canonical data files in the cloud project root
Rscript scripts/update_data.R

# 2. Run the full simulation and generate all outputs
Rscript scripts/run_simulation.R

# 3. Open the main bracket dashboard in your browser
open ~/ProjectsRuntime/mmBayes/output/bracket_dashboard.html
```

Then scan [output/bracket_decision_sheet.csv](output/bracket_decision_sheet.csv) to identify the highest-leverage picks before filling out your entry.
The pipeline writes its live outputs under `~/ProjectsRuntime/mmBayes/output` by default. The tracked files under `output/` in this repo are reference snapshots, not the live run directory. Publish scripts can copy approved artifacts into the cloud project tree when you want a shared release bundle.

---

## Outputs

After a pipeline run the following files are generated in the runtime output directory, which defaults to `~/ProjectsRuntime/mmBayes/output`. The repository keeps example outputs for reference only. The cloud project root holds the canonical input data and dated release bundles, while the runtime root holds the active run outputs and scratch files.

### Primary Dashboard (HTML)

| File | Description |
|------|-------------|
| [bracket_dashboard.html](https://nicholas-camarda.github.io/mmBayes/output/bracket_dashboard.html) | Primary dashboard for building and reviewing the two entries |

### Optional Diagnostics (HTML)

| File | Description |
|------|-------------|
| [technical_dashboard.html](https://nicholas-camarda.github.io/mmBayes/output/technical_dashboard.html) | Secondary diagnostics and simulation detail |
| [model_comparison_dashboard.html](https://nicholas-camarda.github.io/mmBayes/output/model_comparison_dashboard.html) | Secondary calibration and engine-comparison appendix |

### Decision Artifacts (CSV)

| File | Description |
|------|-------------|
| [bracket_decision_sheet.csv](output/bracket_decision_sheet.csv) | Ranked picks sorted by leverage - start here |
| [bracket_matchup_context.csv](output/bracket_matchup_context.csv) | Enriched matchup evidence used by the dashboard |
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
| `Rscript scripts/run_odds_collector.R` | Seasonal Odds API collector for the tournament odds archive |
| `Rscript scripts/capture_odds_snapshot.R` | Capture a private Odds API snapshot |
| `Rscript scripts/build_closing_lines.R --year=2026` | Derive closing-line estimates from saved snapshots |
| `Rscript scripts/publish_release.R` | Copy approved deliverables and the odds-history snapshot into the dated OneDrive release folder |
| `Rscript scripts/evaluate_odds_blend.R --year=2026` | Compare model-only vs blended probabilities against closing lines |

Notes:

- `scripts/run_simulation.R`, `scripts/run_bracket_candidates.R`, and odds scripts load `.env` when present via `load_dotenv_file()`.
- Logs are written under `output/logs/`; full simulation logs are uniquely timestamped per run.
- Odds workflows keep local history under `data/odds_history/` (gitignored), and the seasonal collector only runs during tournament months.

---

## How It Works

The pipeline runs in seven steps:

1. Load `config.yml` and read team features plus historical results from `data/`
2. Build one training row per historical tournament game at the matchup level
3. Fit a Bayesian logistic regression for game-winner probability
4. Run the seasonal odds collector as a sidecar when you want to archive tournament market data
5. Run a rolling held-out-tournament backtest to validate calibration
6. Simulate the current bracket forward round by round using posterior draws
7. Export dashboards, the decision sheet, and candidate bracket files

The model estimates game-by-game win probabilities rather than predicting the whole bracket at once. Posterior draws surface those probabilities as ranked picks, uncertainty intervals, and alternate bracket paths, so the dashboard shows where the bracket is settled and where it is still sensitive to unresolved play-in games.

Data sources:

- `~/Library/CloudStorage/OneDrive-Personal/SideProjects/mmBayes/data/pre_tournament_team_features.xlsx` - Bart Torvik pre-tournament season metrics and tournament-field construction
- `~/Library/CloudStorage/OneDrive-Personal/SideProjects/mmBayes/data/tournament_game_results.xlsx` - historical tournament game results plus current-year monitoring rows

Current-year monitoring rows are not used to retrain the bracket model or alter the pre-tournament matchup features. They exist so the live performance panels can report how the model is doing as the tournament unfolds.

The default configuration uses the eight most recent completed tournaments, skips 2020, and backtests on rolling held-out years.
The core winner model and championship total-points model do not require betting inputs; betting is handled separately as an archive/evaluation sidecar.

To install and start the seasonal collector on this Mac:

```sh
Rscript scripts/install_odds_collector_launchd.R
```

To inspect the loaded agent:

```sh
launchctl print gui/$(id -u)/com.ncamarda.mmBayes.odds_collector
```

To remove it later:

```sh
Rscript scripts/uninstall_odds_collector_launchd.R
```

---

## Model Details

### Winner Model

`mmBayes` currently supports two engines for winner prediction:

- `stan_glm` for Bayesian logistic regression with explicit priors
- `bart` for Bayesian additive regression trees

There is no neural-network engine in this repo today.

`interaction_terms` are a `stan_glm`-only formula option in this repo. BART comparison runs use the same base predictors but omit explicit interaction terms by design, because BART learns interaction structure implicitly through tree splits.

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

The seed and efficiency terms (`seed_diff`, `barthag_logit_diff`, `AdjOE_diff`, `AdjDE_diff`, `WAB_diff`) tend to carry the most posterior weight in the core model.

### Tiebreaker Model

A separate total-points model estimates championship points, powering the tiebreaker outputs in the dashboard. It uses the same engine family selected for the winner model and stays betting-free in the core pipeline.

---

## Betting Data (Sidecar)

Betting data is now a separate archive and evaluation workflow rather than a default model input. The core prediction pipeline stays betting-free, and the odds collector is only meant to run during the tournament window so you can build a season-by-season closing-line history.

- The Odds API key is read from `ODDS_API_KEY` and is never written to disk.
- Bookmakers default to `draftkings`, `fanduel`, `betmgm`, and `betrivers`.
- Markets default to `h2h` moneyline and `spreads`.
- Runtime odds history lives under `~/Library/CloudStorage/OneDrive-Personal/SideProjects/mmBayes/data/odds_history`.
- `Rscript scripts/install_odds_collector_launchd.R` installs the seasonal collector into `~/Library/LaunchAgents` on a Mac.
- `scripts/run_odds_collector.R` is the LaunchAgent entrypoint that performs each seasonal collection pass.
- `Rscript scripts/uninstall_odds_collector_launchd.R` removes the LaunchAgent when you want to stop seasonal capture.
- Historical training rows use time-appropriate closing or last-pre-commence lines when available.
- Current prediction rows use the latest reusable snapshot only in the sidecar/evaluation workflow.
- `Rscript scripts/evaluate_odds_blend.R` now runs a betting-feature ablation that compares a no-betting baseline, the betting-enhanced model, and a market-only benchmark.

---

## Repository Layout

```
mmBayes/
├── R/                  # Active package runtime
├── scripts/            # Command-line entry points
├── tests/              # Automated tests and fixtures
├── data/               # Checked-in reference inputs; live canonical data lives in the cloud project root
├── output/             # Checked-in example outputs; active runs use the runtime output root
├── docs/               # Methods guide and reference material
├── archive/            # Historical material no longer in the active workflow
└── config.yml          # Pipeline configuration
```

Local runtime artifacts such as transient logs and scratch caches live under `~/ProjectsRuntime/mmBayes`. The canonical data live under the cloud project root, and published release bundles are written there as dated releases.

---

## Documentation

- [Methods and Interpretation Guide](docs/methods-and-interpretation.md) - data sources, feature engineering, Bayesian likelihoods and priors, posterior summaries, bracket decision logic
