# mmBayes

`mmBayes` is a private March Madness modeling project organized like a minimal R
package with thin runner scripts. The active workflow has exactly two commands.

## Run Simulation

```sh
Rscript scripts/run_simulation.R
```

This command:

- loads `config.yml`
- reads the canonical tournament data file
- fits the required Bayesian model
- simulates the bracket
- writes outputs under `output/`

## Refresh Data

```sh
Rscript scripts/update_data.R
```

This command refreshes the canonical tournament input files under `data/`. It is
the only supported scraping/data-refresh path. Normal simulation runs never
scrape.

## Notes

- The active code path lives in `R/`.
- Legacy scripts are archived under `old/` for reference only.
- The active workflow requires `rstanarm`, `bayesplot`, and `loo`.
- The runner should fail immediately if the Bayesian package set is unavailable.

## Manual Full Regression

Run these steps from the repository root:

```sh
Rscript scripts/run_simulation.R
git status --short
```

Successful regression means:

- the simulation command completes
- output files are written
- only ignored runtime artifacts are created
- no legacy script is used in the active workflow
