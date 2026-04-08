## Why

The current historical betting spec still overemphasizes multi-year backfill and provider-retention uncertainty, even though the repo's real near-term need is much narrower: collect and preserve complete tournament closing lines starting with `2026`, and determine whether complete current-year closing-line coverage would actually improve bracket prediction. We need the spec to define success around a forward-only seasonal archive and a credible bracket-impact test, not around recovering pre-2026 seasons the user no longer cares about.

## What Changes

- Reframe success for the betting-line workflow around `2026+` forward collection, archival, and reproducibility rather than multi-year retrospective backfill.
- Clarify that the supported operational goal is to capture the complete set of closing lines for the current tournament season while the provider retention window is still open, then preserve that archive locally for future modeling use.
- Tighten the success criteria for `closing_lines.csv` so “success” means a clean, non-synthetic, repo-compatible season artifact with explicit coverage accounting, not merely a partially written file.
- Require the spec to distinguish between two questions:
  - whether the repo can operationally archive a complete current-season closing-line set going forward
  - whether having that complete set actually improves bracket prediction
- Narrow the evaluation requirement so it explicitly tests whether a complete set of `2026` closing lines would improve bracket prediction, while admitting that the answer may still be “no.”
- Clarify that incomplete or partial `2026` line coverage is operationally useful but does not satisfy the spec's primary success condition.
- Keep graceful degradation for pre-archive seasons, but stop treating `2025` and earlier historical recovery as a success gate for this change.
- Update proposal/design/tasks/spec language so docs and implementation targets align with the forward-only archive contract and the real model-evaluation question.

## Capabilities

### New Capabilities

### Modified Capabilities
- `historical-betting-closing-lines`: redefine success around complete `2026+` seasonal closing-line archiving and an explicit bracket-impact evaluation using complete current-year coverage rather than pre-2026 backfill.

## Impact

- Affected OpenSpec contract: [historical-betting-closing-lines/spec.md](/Users/ncamarda/Projects/mmBayes/openspec/specs/historical-betting-closing-lines/spec.md)
- Expected affected R modules and scripts once implemented: [R/oddspapi_historical.R](/Users/ncamarda/Projects/mmBayes/R/oddspapi_historical.R), [R/betting_lines.R](/Users/ncamarda/Projects/mmBayes/R/betting_lines.R), [R/data_loading.R](/Users/ncamarda/Projects/mmBayes/R/data_loading.R), and the historical import/evaluation entrypoints under [/Users/ncamarda/Projects/mmBayes/scripts](/Users/ncamarda/Projects/mmBayes/scripts)
- Expected affected tests: historical import, historical loading, and betting-impact evaluation coverage under [/Users/ncamarda/Projects/mmBayes/tests/testthat](/Users/ncamarda/Projects/mmBayes/tests/testthat)
- Config and artifact implications: this change should preserve the `ODDS_PAPI_API_KEY` repo-root `.env` contract and the `2026+` runtime odds-history boundary, while clarifying that success requires a complete seasonal archive rather than accepting partial `closing_lines.csv` output as sufficient
- Compatibility and rollback: this is primarily a spec-contract clarification, but it raises the bar for what counts as a successful season archive and may require implementation/task changes so partially covered runtime artifacts are no longer represented as spec-complete success
