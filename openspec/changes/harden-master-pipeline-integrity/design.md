## Context

`master` is now intended to be a betting-free branch, but the repo still shows evidence of the retired betting workflow in its test surface and in a few helper assumptions. The immediate symptoms are:

- `Rscript tests/testthat.R` fails because betting-only tests remain after the betting helpers were removed.
- `validate_game_results()` uses a table-wide winner membership check instead of a row-wise check, so malformed unscored rows can pass validation.
- `score_bracket_against_results()` joins predicted and actual games without a year key, which is safe only when callers happen to pass a single tournament year.
- `publish_release_bundle()` and `release_deliverable_manifest()` assume a fixed set of candidate outputs and can drift from the runtime output contract.

These issues are cross-cutting because they span canonical data validation, reusable exported helpers, top-level scripts, release packaging, and the test contract for the whole branch.

## Goals / Non-Goals

**Goals:**
- Make the supported `master` workflow internally consistent so the top-level test entrypoint passes.
- Enforce row-correct validation for canonical tournament results, including explicit failure on impossible scored rows.
- Make bracket scoring deterministic and safe when reused outside the current single-year pipeline path.
- Make release publishing follow the documented runtime-output boundary and actual generated candidate artifacts.
- Add regression tests that keep these guarantees from drifting again.

**Non-Goals:**
- Reintroducing betting lines, OddsPapi, or the retired live-odds workflow on `master`.
- Redesigning model structure, priors, backtest methodology, or dashboard visual language.
- Changing cloud roots, runtime roots, or the saved results bundle format beyond what is required to support the integrity fixes.

## Decisions

### Decision: Treat the full `tests/testthat.R` run as the normative branch health check

`master` currently has a supported workflow mismatch: focused tests pass, but the top-level suite fails because stale betting tests still exist. The design treats the full testthat entrypoint as the canonical branch-integrity gate and requires `master`’s tests to match only the currently supported workflow.

Alternatives considered:
- Keep the stale tests and document that only a subset should be run.
  Rejected because it makes branch health ambiguous and invites silent drift.
- Restore thin betting math helpers just to satisfy the old tests.
  Rejected because it recreates unsupported surface area on the wrong branch.

### Decision: Make result validation row-wise and explicit about ties/impossible scored states

Validation in `R/data_loading.R` should check each row’s `winner` against that row’s `teamA` and `teamB`, not against the union of team names across the table. Scored rows should fail clearly when the winner does not match the higher score, and ties should be treated as invalid unless the canonical input contract is explicitly widened later.

Alternatives considered:
- Keep the current permissive validation and rely on downstream joins or scoring to expose bad rows.
  Rejected because malformed canonical inputs can survive too far into the pipeline.
- Auto-repair winners from score columns when present.
  Rejected because that mutates canonical source claims rather than validating them.

### Decision: Make bracket scoring require an unambiguous game identity

`score_bracket_against_results()` should treat `Year` as part of the required game identity, not as an optional convenience field. The scorer should join on `Year` in addition to round, region, and matchup position, and should fail fast when callers provide results that omit `Year`. This keeps exported scoring behavior safe for retrospective or cross-year analyses and matches the canonical results contract already used by the repo.

Alternatives considered:
- Continue relying on callers to pass a single-year actual-results table.
  Rejected because the helper is exported and its current behavior can silently overcount.
- Auto-detect single-year actuals when `Year` is missing.
  Rejected because the canonical results contract already includes `Year`, so a permissive fallback would preserve ambiguity without a good reason.

### Decision: Release publishing should derive candidate deliverables from the supported runtime output contract

The release bundle should be anchored to runtime outputs, not a cloud-output fallback, and candidate CSV inclusion should be derived dynamically from the generated `bracket_candidate_*.csv` artifacts for the run being published. This keeps publishing aligned with the actual runtime output contract and avoids a second hard-coded candidate-count assumption.

Alternatives considered:
- Leave the manifest fixed at two candidate CSVs and rely on convention.
  Rejected because the current code already writes candidate files generically by candidate id.
- Publish every file in the runtime output directory.
  Rejected because the release contract is intentionally narrow and should exclude caches, logs, and `.rds` bundles.

## Risks / Trade-offs

- [Stricter validation exposes previously tolerated bad rows] → Add targeted tests and use precise failure messages so repairs are actionable rather than mysterious.
- [Scoring contract changes may break ad hoc scripts that relied on permissive joins] → Keep the single-year happy path working and fail with a message that explains how to disambiguate multi-year inputs.
- [Release-manifest hardening can change what appears in release folders] → Keep the release contract narrow and document any manifest derivation rule so operators know what to expect.
- [Removing stale tests may hide useful betting utilities] → On `master`, that is intentional; the preserved `betting-lines-spike` branch remains the rollback/reference path for retired betting work.

## Migration Plan

1. Remove or rewrite betting-only tests so `tests/testthat.R` reflects the supported `master` branch.
2. Patch `validate_game_results()` to use row-wise winner checks and explicit scored-row integrity checks.
3. Patch `score_bracket_against_results()` to join on `Year` when available or fail on ambiguous actual-result inputs.
4. Refactor release publishing so runtime output is the default source and candidate deliverables cannot drift from generation behavior.
5. Update docs/tests to reflect the hardened contracts, then rerun the top-level suite.

Rollback is straightforward: revert the change on `master`. No cloud-root migration, data-format migration, or release-folder rewrite is required.

## Open Questions

- None currently.
