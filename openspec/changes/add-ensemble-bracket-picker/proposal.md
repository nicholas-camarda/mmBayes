## Why

The current Stan GLM versus BART comparison is useful as audit evidence, but it does not directly improve the bracket-entry product. Since both engines are intended to remain part of the workflow, the primary pipeline should combine them into one validated ensemble bracket picker and make the dashboard present that integrated recommendation instead of asking the user to adjudicate separate engine scorecards.

## What Changes

- Add an ensemble matchup-probability layer that consumes Stan GLM and BART predictions and produces the primary draw-by-game probability surface used for bracket simulation, candidate generation, decision sheets, and dashboards.
- Train and validate ensemble weights from rolling historical holdout predictions only, with no current-year completed-game leakage into pre-tournament bracket generation.
- Add an explicit pre-integration proof gate that compares Stan GLM, BART, equal-weight average, and learned ensemble before changing the primary pipeline or dashboards.
- Run the proof gate as a minimal isolated evaluator against real project data and existing model helpers, without production config, saved-results schema, dashboard, publishing, release-manifest, or product-doc changes.
- Make the ensemble the supported bracket-picking path only if the proof gate shows that it improves the bracket-picking target while passing calibration guardrails.
- Add a failed-proof cleanup rule for the isolated evaluator only; most production ensemble plumbing SHALL NOT exist before the gate passes.
- Preserve Stan GLM and BART as required component engines, but demote the standalone engine-comparison dashboard from a user decision surface to audit/reference evidence.
- Retire the user-facing concept of a "preferred engine" comparison page; replace it with ensemble-first model diagnostics that explain the ensemble, its component models, learned weights, and validation evidence.
- Clean up generated dashboards so the main dashboard and candidate artifacts emphasize "what bracket should I enter?" rather than "which engine won more diagnostics?"
- If the proof gate passes and ensemble integration proceeds, update all relevant documentation surfaces so README, methods docs, repository instructions, dashboard links, and release artifact descriptions match the new ensemble-first workflow.
- Keep runtime output, tracked repo snapshots, and release bundle boundaries stable unless implementation discovers a necessary manifest addition for the ensemble audit artifact.

## Capabilities

### New Capabilities

- `ensemble-bracket-picking`: Defines how Stan GLM and BART predictions are combined, validated, persisted, and used as the primary bracket-picking probability source.

### Modified Capabilities

- `bracket-dashboard-decision-ux`: Updates the dashboard behavior so generated pages lead with ensemble bracket recommendations and move engine-comparison material into secondary audit context.

## Impact

- Affected R modules: `R/main.R`, `R/model_fitting.R`, `R/simulation_functions.R`, `R/model_quality.R`, `R/plotting_functions.R`, `R/utils.R`, and potentially `R/publishing.R` if a new ensemble audit artifact becomes release-worthy.
- Affected scripts: `scripts/run_simulation.R`, `scripts/run_bracket_candidates.R`, `scripts/regenerate_and_sync_dashboards.R`, and publish/sync scripts only if output membership changes.
- Affected tests: `tests/testthat/test_model_bart.R`, `tests/testthat/test_model_and_simulation.R`, `tests/testthat/test_pipeline_smoke.R`, dashboard interaction tests under `e2e/` or `npm test`, and any model-quality snapshot tests.
- Config impact: likely adds or revises `config.yml` model keys for ensemble behavior while preserving existing path roots and output naming defaults.
- Artifact impact: the first phase writes validation evidence only and must not alter release-facing artifacts, production config, saved-results schemas, dashboards, tracked snapshots, release manifests, or product-facing docs. If the proof gate passes, saved results bundles must carry enough ensemble provenance to regenerate dashboards from cached results; tracked repo HTML remains a mirror of runtime output; release manifests should remain narrow and avoid caches/logs/RDS bundles. The implementation should replace or rename `model_comparison_dashboard.html` as an ensemble diagnostics surface rather than preserving a misleading comparison artifact.
- Documentation impact after proof-gate success: `README.md`, `docs/methods-and-interpretation.md`, `AGENTS.md`, `config.yml` comments or examples if present, release/publish documentation, dashboard link references, and any artifact manifest descriptions must be updated in the same implementation.
- Compatibility and rollback: the rollback path is to keep Stan GLM and BART component fits intact and restore the previous primary engine selection, but implementation should not add silent fallbacks or partial degradation paths.
