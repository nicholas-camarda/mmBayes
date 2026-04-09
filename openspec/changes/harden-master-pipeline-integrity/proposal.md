## Why

`master` currently has a mismatch between its supported betting-free workflow and the code/tests/release assumptions that remain in the repo. The result is a red top-level test run, weak game-result validation that can admit malformed rows, bracket scoring helpers that can misbehave when reused outside the single-year happy path, and release publishing rules that are more brittle than the documented runtime contract.

## What Changes

- Remove or rewrite stale betting-only test coverage on `master` so the full `Rscript tests/testthat.R` path reflects the supported branch contract.
- Strengthen canonical game-result validation so each row's `winner` must match that row's `teamA` or `teamB`, and scored rows with invalid winner/score relationships are rejected explicitly.
- Tighten bracket scoring requirements so cross-year or otherwise ambiguous actual-result joins cannot silently inflate scores.
- Harden the release bundle contract so publish behavior follows the documented runtime-output boundary and does not assume a fixed candidate count unless that is an explicit supported contract.
- Add regression coverage for the validator, scorer, release manifest, and branch-level test hygiene so these failures are caught before future branch cleanup or workflow changes land on `master`.

## Capabilities

### New Capabilities
- `master-pipeline-integrity`: Keep the supported `master` workflow internally consistent across validation, scoring, release publishing, and the top-level automated test surface.

### Modified Capabilities
- None.

## Impact

- Affected R modules: [R/data_loading.R](/Users/ncamarda/Projects/mmBayes/R/data_loading.R), [R/utils.R](/Users/ncamarda/Projects/mmBayes/R/utils.R), [R/publishing.R](/Users/ncamarda/Projects/mmBayes/R/publishing.R), and [R/main.R](/Users/ncamarda/Projects/mmBayes/R/main.R).
- Affected scripts/tests: [scripts/run_simulation.R](/Users/ncamarda/Projects/mmBayes/scripts/run_simulation.R), [scripts/publish_release.R](/Users/ncamarda/Projects/mmBayes/scripts/publish_release.R), [tests/testthat.R](/Users/ncamarda/Projects/mmBayes/tests/testthat.R), [tests/testthat/test_betting_math.R](/Users/ncamarda/Projects/mmBayes/tests/testthat/test_betting_math.R), [tests/testthat/test_project_layout.R](/Users/ncamarda/Projects/mmBayes/tests/testthat/test_project_layout.R), and new or updated validator/scoring regression tests.
- Expected contract effects: no new model features or config keys; behavior changes are concentrated in validation strictness, release-manifest membership assumptions, and exported helper safety.
- Compatibility and rollback: stricter validation can surface latent bad data earlier, and release-manifest changes may alter which artifacts are considered publishable; rollback is straightforward because the change is contained to `master`’s betting-free path and does not require schema or cloud-root migration.
