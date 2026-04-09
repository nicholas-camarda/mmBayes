## 1. Branch test surface cleanup

- [x] 1.1 Remove or rewrite stale betting-only tests on `master` so `Rscript tests/testthat.R` matches the supported branch contract.
- [x] 1.2 Add a regression test or suite-level assertion that catches references to retired betting helpers on `master`.

## 2. Canonical validation hardening

- [x] 2.1 Update `validate_game_results()` in `R/data_loading.R` to enforce row-wise winner checks against the same row’s `teamA` and `teamB`.
- [x] 2.2 Add explicit rejection for tied scored rows and mismatched scored-row winner/total relationships.
- [x] 2.3 Add `testthat` coverage for malformed unscored rows, malformed scored rows, and tied scored rows.

## 3. Bracket scoring safety

- [x] 3.1 Update `score_bracket_against_results()` to require `Year` in actual-result inputs and use it as part of the join key.
- [x] 3.2 Update `score_bracket_against_results()` and any affected call sites to preserve the current single-year pipeline path while preventing silent overcounting.
- [x] 3.3 Add regression tests for single-year scoring with `Year`, missing-`Year` failures, and ambiguous actual-result inputs.

## 4. Release bundle contract hardening

- [x] 4.1 Update `R/publishing.R`, `R/main.R`, and any supporting helpers so release publishing uses runtime output as the authoritative source and candidate artifact inclusion is derived dynamically from generated `bracket_candidate_*.csv` files.
- [x] 4.3 Update release-manifest tests to cover the chosen candidate-deliverable rule and the runtime-output default source.

## 5. Documentation and verification

- [x] 5.1 Update `README.md`, `AGENTS.md`, and any relevant methods/runtime docs to reflect the hardened `master` validation, scoring, and release behavior.
- [x] 5.2 Run `Rscript tests/testthat.R` and confirm the top-level suite passes on `master`.
