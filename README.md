# mmBayes

`mmBayes` is a private March Madness prediction project for generating and
reviewing NCAA tournament brackets with a Bayesian workflow.

`master` is the canonical branch for the project.

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
- reads the canonical tournament data from `data/`
- fits the required Bayesian model
- simulates the tournament bracket
- writes outputs under `output/`

The active workflow requires `rstanarm`, `bayesplot`, and `loo`. If those
packages are unavailable, the runner should fail immediately rather than falling
back to a different modeling path.

Legacy scripts are archived under `old/` for reference only and are not part of
the active execution path.

## Project Status

The infrastructure is in good shape:

- package-style runtime in `R/`
- thin runner scripts in `scripts/`
- automated tests in `tests/testthat/`
- a working Bayesian end-to-end run

The modeling core is not yet at the point where the bracket outputs should be
treated as fully trustworthy. The current dataset and feature framing still need
a reset around real outcome labels and pre-tournament features only.

## Current Limitations

The next substantive work is not more scaffolding. It is a modeling redesign:

- rebuild targets around actual realized tournament outcomes
- restrict predictive features to information available before tournament games
- remove or redesign leakage-prone derived features
- move toward matchup-level prediction with bracket simulation layered on top

That is the path to making this a reliable personal bracket-prediction tool
rather than just a runnable modeling sandbox.

## Manual Validation

Run these steps from the repository root:

```sh
Rscript scripts/run_simulation.R
git status --short
```

Successful validation means:

- the Bayesian simulation command completes
- expected output files are written
- only ignored runtime artifacts are created
- no legacy script is used in the active workflow
