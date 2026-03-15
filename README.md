# mmBayes

`mmBayes` is a private March Madness prediction project for generating and
reviewing NCAA tournament brackets with a Bayesian workflow.

## Supported Commands

Run the current bracket simulation:

```sh
Rscript scripts/run_simulation.R
```

Refresh the canonical tournament data files:

```sh
Rscript scripts/update_data.R
```

These are the only supported entrypoints. The active runtime lives in `R/`.

## Current Workflow

The simulation command:

- loads `config.yml`
- reads canonical pre-tournament team features and explicit historical game results from `data/`
- fits a Bayesian matchup-level game model
- runs a rolling held-out-tournament backtest
- generates a deterministic max-probability bracket with posterior game probabilities
- writes outputs under `output/`

The active workflow requires `rstanarm`, `bayesplot`, and `loo`. If those
packages are unavailable, the runner should fail immediately rather than falling
back to a different modeling path.

Legacy scripts are archived under `archive/legacy-runtime/` for reference only and are not part of
the active execution path.

## Repository Layout

- `R/`: active package runtime
- `scripts/`: supported command-line entrypoints
- `tests/`: automated tests and fixtures
- `docs/reference/`: background papers and legacy writeups worth keeping
- `archive/legacy-analysis/`: old rendered reports, spreadsheets, and diagnostics
- `archive/legacy-runtime/`: superseded scripts kept only for historical reference

## Modeling Design

The active modeling core is now matchup-based:

- historical training data is one row per actual tournament game
- predictors are restricted to pre-tournament season features and matchup context
- leakage-prone tournament-outcome features are excluded from active modeling
- bracket picks are derived from calibrated game probabilities rather than a team-level `Champ` target

The canonical data refresh writes two files:

- `data/pre_tournament_team_features.xlsx`
- `data/tournament_game_results.xlsx`

The default model uses the most recent eight completed tournaments available in
the data files, skips 2020, and backtests on rolling held-out years before
predicting the current bracket.

## Manual Validation

Run these steps from the repository root:

```sh
Rscript scripts/run_simulation.R
git status --short
```

Successful validation means:

- the Bayesian simulation command completes
- the backtest summary is written
- expected output files are written
- only ignored runtime artifacts are created
- no legacy script is used in the active workflow
