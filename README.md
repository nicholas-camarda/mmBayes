# mmBayes

An R-based NCAA tournament bracket lab. Turns pre-tournament team data and historical results into Bayesian win probabilities, ranked bracket candidates, and review artifacts for manual entry.

## Table of Contents

- [Preview](#preview)
- [Quick Start](#quick-start)
- [Outputs](#outputs)
- [Commands](#commands)
- [How It Works](#how-it-works)
- [Model Details](#model-details)
- [Repository Layout](#repository-layout)
- [Documentation](#documentation)

---

## Preview

This repo is meant to produce bracket-entry materials you can review quickly in a browser or spreadsheet.

If you want the published dashboard entrypoint, start at the GitHub Pages hub:

[Open the GitHub Pages dashboard hub](https://nicholas-camarda.github.io/mmBayes/)

The hub links to the main bracket dashboard plus the technical diagnostics page.

The authoritative full run now refreshes the tracked repo dashboard snapshot automatically, so a successful `Rscript scripts/run_simulation.R` updates both the runtime HTML bundle and the GitHub Pages source files under `output/`.

If you changed only dashboard rendering code and want a fast refresh from the cached full results bundle, run:

`Rscript scripts/regenerate_and_sync_dashboards.R`

The main review loop usually looks like this:

1. open the checked-in main dashboard snapshot under `output/` to build the two entries and review the evidence trail
2. scan `bracket_decision_sheet.csv` in the configured runtime output directory for the hardest decisions first
3. use the generated `bracket_candidate_{id}.csv` files in the configured runtime output directory if you want the underlying pick paths; the default workflow currently writes Candidate 1 and Candidate 2
4. check the status banner to see whether First Four slots are still simulated or already final

The main dashboard is designed to answer three jobs fast: what Candidate 1 and Candidate 2 are, which evidence drives the key decisions, and which matchups deserve another look before you trust the entries.
Live tournament commentary is separate from the prediction-time inputs: First Four results can resolve the bracket path, while completed Round of 64+ games are tracked for monitoring and dashboard commentary only.

Optional diagnostics:

- [Technical dashboard](https://nicholas-camarda.github.io/mmBayes/output/technical_dashboard.html) - ensemble diagnostics, calibration, and simulation detail

## Working Roots

- Local source checkout
- Configured runtime output directory for live runs, logs, caches, and generated artifacts
- Configured synced project home for canonical input data and dated published releases

The checkout is the working tree. The synced project home holds the canonical input data and published release bundles. The runtime output directory stays available for live generated outputs and scratch artifacts. Maintainer-specific absolute paths belong in internal docs such as `AGENTS.md`, not in the public README.

## Quick Start

```sh
# 1. Refresh the canonical data files
Rscript scripts/update_data.R

# If the script ends with "Refresh status: Success", continue normally.
# If it ends with "Refresh status: Degraded success", you can still continue,
# but review the listed warnings and the refresh log first.
# If it fails, do not continue to simulation until the refresh error is fixed.

# 2. Run the full simulation and generate all outputs
Rscript scripts/run_simulation.R

# This writes the live dashboard bundle to the configured runtime output directory
# and syncs the tracked repo dashboard HTML snapshot under `output/`.
# Open `bracket_dashboard.html` in that directory first.
# Expect this step to take longer than the refresh step because it fits/loads models,
# runs the backtest workflow, and then renders the dashboard bundle.
```

Then open `bracket_dashboard.html` from the configured runtime output directory in your browser.

For dashboard or CSS/layout iteration after the full results bundle already exists:

```sh
# 1. Run the authoritative full simulation when model/data inputs change
Rscript scripts/run_simulation.R

# 2. Edit the dashboard rendering code

# 3. Regenerate only the dashboard HTML from the cached results bundle
Rscript scripts/regenerate_and_sync_dashboards.R
```

Then scan `bracket_decision_sheet.csv` in the configured runtime output directory to identify the highest-leverage picks before filling out your entry.
The pipeline writes its live outputs under the configured runtime output directory. The tracked files under `output/` in this repo are dashboard HTML snapshots for GitHub Pages, not the live run directory and not the CSV/TXT source of truth. `scripts/run_simulation.R` now syncs those tracked HTML snapshots automatically after a successful full run. Publish scripts can copy approved runtime deliverables into the configured synced project home when you want a shared release bundle.

---

## Outputs

After a pipeline run the following artifacts are generated in the configured runtime output directory. The repository tracks only the dashboard HTML snapshots under `output/` for GitHub Pages. CSV, TXT, RDS, cache, and log artifacts should be read from the runtime output directory or from a dated release bundle.

### Primary Dashboard (HTML)

| File | Description |
|------|-------------|
| [bracket_dashboard.html](https://nicholas-camarda.github.io/mmBayes/output/bracket_dashboard.html) | Primary dashboard for building and reviewing the two entries |

### Optional Diagnostics (HTML)

| File | Description |
|------|-------------|
| [technical_dashboard.html](https://nicholas-camarda.github.io/mmBayes/output/technical_dashboard.html) | Ensemble diagnostics, calibration, and simulation detail |

### Decision Artifacts (CSV)

| File | Description |
|------|-------------|
| `bracket_decision_sheet.csv` | Ranked picks sorted by leverage - start here |
| `bracket_matchup_context.csv` | Enriched matchup evidence used by the dashboard |
| `bracket_candidate_{id}.csv` | One CSV per retained bracket candidate |
| `candidate_matchup_total_points.csv` | Matchup-level total-points support |
| `championship_tiebreaker_summary.csv` | Championship tiebreaker summary |
| `championship_tiebreaker_distribution.csv` | Full tiebreaker score distribution |

Release publishing includes every generated `bracket_candidate_{id}.csv` file found in the runtime output directory for that run. The exact deliverable contract is defined by `save_decision_outputs()` and `release_deliverable_manifest()`.

### Summaries (TXT)

| File | Description |
|------|-------------|
| `tournament_sim_candidate_brackets.txt` | Text summary of all candidate brackets |
| `tournament_sim_model_summary.txt` | Fitted model coefficient summary |
| `tournament_sim_backtest_summary.txt` | Rolling backtest accuracy report |
| `bracket_candidates.txt` | Concise candidate bracket listing |

### Heavy Artifacts (ignored by git)

- `tournament_sim.rds` - full simulation bundle
- `bracket_candidates.rds` - serialized bracket candidates
- `model_cache/` - cached model fits
- `model_quality/` - model-quality snapshots
- other non-deliverable runtime artifacts - scratch outputs outside the supported `master` release contract

---

## Commands

| Command | Purpose |
|---------|---------|
| `Rscript scripts/run_simulation.R` | Authoritative full pipeline: fit, backtest, simulate, compare, export, and sync tracked repo dashboard HTML |
| `Rscript scripts/update_data.R` | Refresh canonical data files from source |
| `Rscript scripts/run_bracket_candidates.R` | Lighter rerun without the full backtest, but it still fits or reloads models and regenerates candidates |
| `Rscript scripts/regenerate_and_sync_dashboards.R` | Preferred dashboard-refresh command for rendering changes; rebuilds HTML from the saved full results bundle and syncs repo `output/` copies |
| `Rscript scripts/data_quality_check.R` | Data-quality validation |
| `Rscript scripts/publish_release.R` | Copy approved deliverables into the dated release folder under the configured synced project home |
| `Rscript scripts/publish_github_pages.R` | Internal plumbing helper that copies already-rendered dashboard HTML into tracked repo `output/` files; not part of the normal user workflow |

Notes:

- `scripts/run_simulation.R` and `scripts/run_bracket_candidates.R` load `.env` when present via `load_dotenv_file()`.
- Logs are written under the configured runtime output directory's `logs/` subdirectory; full simulation logs are uniquely timestamped per run.
- `scripts/run_simulation.R` syncs the tracked repo dashboard HTML snapshot after a successful full run so the committed `output/` files stay aligned with the latest runtime dashboard bundle.
- `scripts/run_bracket_candidates.R` is not the right command for CSS/layout-only iteration; it still performs model/candidate work. Use `scripts/regenerate_and_sync_dashboards.R` when the saved full results bundle already exists.
- `scripts/publish_github_pages.R` is internal plumbing. It does not regenerate dashboards and should only copy a bundle you already know is the right one.
- `scripts/publish_release.R` publishes from the runtime output directory, not the cloud output tree, and includes every generated `bracket_candidate_{id}.csv` file in that run's output.
- Dated releases contain a `deliverables/` folder and a plain-text manifest; non-deliverable runtime artifacts are not part of the release contract.
- `Rscript tests/testthat.R` is the authoritative branch-health check for `master`.

---

## How It Works

The pipeline runs in six steps:

1. Load `config.yml` and read team features plus historical results from the configured data workbook paths
2. Build ordered matchup-level training rows from historical tournament games
3. Fit Stan GLM and BART component winner models
4. Learn the constrained logit-scale ensemble combiner from rolling held-out predictions
5. Simulate the current bracket forward round by round using posterior draws
6. Export dashboards, the decision sheet, and candidate bracket files

The primary bracket picker estimates game-by-game win probabilities by combining Stan GLM and BART component probabilities on the logit scale. Posterior draws surface those probabilities as ranked picks, uncertainty intervals, and alternate bracket paths, so the dashboard shows where the bracket is settled and where it is still sensitive to unresolved play-in games.

Data sources:

- Canonical team-features workbook in the configured synced project home - Bart Torvik pre-tournament season metrics and tournament-field construction
- Canonical game-results workbook in the configured synced project home - historical tournament game results plus current-year monitoring rows

Current-year monitoring rows are not used to retrain the bracket model or alter the pre-tournament matchup features. They exist so the live performance panels can report how the model is doing as the tournament unfolds.

The default configuration requests the eight most recent completed tournaments and skips 2020. The dashboard reports the effective historical seasons actually available in the canonical workbooks; the current 2026 data state uses seven completed seasons, 2018, 2019, and 2021-2025.

---

## Model Details

### Winner Model

The supported winner workflow is an ensemble bracket picker:

- `stan_glm` supplies a Bayesian logistic-regression component with explicit priors
- `bart` supplies a Bayesian additive-regression-tree component
- the primary bracket probabilities combine the two on the logit scale with learned constrained weights

There is no neural-network engine in this repo today.

`interaction_terms` are a `stan_glm`-only formula option in this repo. BART component fits use the same base predictors but omit explicit interaction terms by design, because BART learns interaction structure implicitly through tree splits.

The ensemble combiner has the form:

$$
\Pr(\text{team A wins}) = \text{logit}^{-1}(\delta + w \cdot \text{logit}(p_{\text{Stan}}) + (1-w) \cdot \text{logit}(p_{\text{BART}}))
$$

where `0 <= w <= 1`. Current production integration requires the learned ensemble to pass a real-data proof gate before it is used as the primary bracket picker.

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

The seed and efficiency terms (`seed_diff`, `barthag_logit_diff`, `AdjOE_diff`, `AdjDE_diff`, `WAB_diff`) tend to carry the most posterior weight in the core model. Historical games are expanded into forward and reversed ordered matchup rows for prediction, so coefficient summaries are predictive diagnostics rather than independent scientific effects.

### Tiebreaker Model

A separate total-points model estimates championship points, powering the tiebreaker outputs in the dashboard. It stays betting-free in the core pipeline.

---

## Repository Layout

```
mmBayes/
├── R/                  # Active package runtime
├── scripts/            # Command-line entry points
├── tests/              # Automated tests and fixtures
├── data/               # Checked-in reference inputs; live canonical data live in the configured synced project home
├── output/             # Checked-in dashboard HTML snapshots; active runs use the configured runtime output directory
├── docs/               # Methods guide and reference material
├── archive/            # Historical material no longer in the active workflow
└── config.yml          # Pipeline configuration
```

Local runtime artifacts such as transient logs and scratch caches live under the configured runtime output directory. The canonical data live under the configured synced project home, and published release bundles are written there as dated releases.

---

## Documentation

- [Methods and Interpretation Guide](docs/methods-and-interpretation.md) - data sources, feature engineering, Bayesian likelihoods and priors, posterior summaries, bracket decision logic
