# mmBayes

Bayesian NCAA tournament bracket prediction and decision-support toolkit.

Turns pre-tournament team data and historical game results into win probabilities, ranked bracket candidates, and an interactive dashboard for reviewing the hardest decisions before you lock in your entry.

---

## Preview

**[Open the bracket dashboard](https://nicholas-camarda.github.io/mmBayes/output/app/index.html)**
&nbsp;·&nbsp;
**[Technical diagnostics](https://nicholas-camarda.github.io/mmBayes/output/app/technical.html)**

The dashboard answers three questions fast:
1. What are Candidate 1 and Candidate 2?
2. Which evidence drives the key decisions?
3. Which matchups deserve another look before you trust the entries?

---

## Quick Start

```sh
# 1. Refresh canonical data
Rscript scripts/update_data.R

# 2. Run the full pipeline (fit models, backtest, simulate, export)
Rscript scripts/run_simulation.R
```

Open `output/app/index.html` in your browser. The dashboard loads the latest simulation payload automatically.

For dashboard-only iteration when models haven't changed:

```sh
Rscript scripts/regenerate_and_sync_dashboards.R
```

Then review `bracket_decision_sheet.csv` in your runtime output directory — it ranks picks by leverage so you can focus on the highest-impact decisions first.

---

## Outputs

All artifacts are written to your configured runtime output directory. The repository tracks the dashboard app under `output/app/` for GitHub Pages.

### Dashboard

| File | Description |
|------|-------------|
| [`output/app/index.html`](https://nicholas-camarda.github.io/mmBayes/output/app/index.html) | Bracket entry workspace |
| [`output/app/technical.html`](https://nicholas-camarda.github.io/mmBayes/output/app/technical.html) | Compare boards, calibration, backtest, and ensemble diagnostics |

### Decision Artifacts (CSV)

| File | Description |
|------|-------------|
| `bracket_decision_sheet.csv` | Ranked picks by leverage — start here |
| `bracket_matchup_context.csv` | Enriched matchup evidence |
| `bracket_candidate_{id}.csv` | One CSV per bracket candidate |
| `candidate_matchup_total_points.csv` | Matchup-level total-points support |
| `championship_tiebreaker_summary.csv` | Championship tiebreaker summary |
| `championship_tiebreaker_distribution.csv` | Full tiebreaker score distribution |

### Summaries (TXT)

| File | Description |
|------|-------------|
| `tournament_sim_candidate_brackets.txt` | All candidate brackets |
| `tournament_sim_model_summary.txt` | Fitted model coefficients |
| `tournament_sim_backtest_summary.txt` | Rolling backtest accuracy |
| `bracket_candidates.txt` | Concise candidate listing |

### Runtime Artifacts

These are generated in the runtime output directory and git-ignored:
`tournament_sim.rds`, `bracket_candidates.rds`, `model_cache/`, `model_quality/`, and run logs.

---

## Commands

| Command | Purpose |
|---------|---------|
| `Rscript scripts/update_data.R` | Refresh canonical data from source |
| `Rscript scripts/run_simulation.R` | Full pipeline: fit → backtest → simulate → export |
| `Rscript scripts/regenerate_and_sync_dashboards.R` | Rebuild dashboard from cached results (no model refit) |
| `Rscript scripts/run_bracket_candidates.R` | Lighter run: model fit + candidates, no full backtest |
| `Rscript scripts/data_quality_check.R` | Validate input data |
| `Rscript scripts/publish_release.R` | Copy deliverables to dated release folder |
| `Rscript tests/testthat.R` | Run all R package tests |

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

where `0 <= w <= 1`. The primary ensemble fit runs rolling real-data validation before fitting the current tournament model; if the learned ensemble fails the configured bracket-score and calibration guardrails, the production ensemble run stops.

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
├── output/             # Checked-in React dashboard app; active runs use the configured runtime output directory
├── docs/               # Methods guide and reference material
├── archive/            # Historical material no longer in the active workflow
└── config.yml          # Pipeline configuration
```

Local runtime artifacts such as transient logs and scratch caches live under the configured runtime output directory. The canonical data live under the configured synced project home, and published release bundles are written there as dated releases.

---

## Documentation

- [Methods and Interpretation Guide](docs/methods-and-interpretation.md) - data sources, feature engineering, Bayesian likelihoods and priors, posterior summaries, bracket decision logic
