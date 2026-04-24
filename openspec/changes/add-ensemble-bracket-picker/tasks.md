## 1. Pre-Integration Proof Gate

- [x] 1.1 Build a minimal isolated ensemble validation evaluator using real project data and existing model/scoring helpers; do not change production config defaults, primary simulation routing, dashboard generation, saved-results schemas, publishing, release manifests, product-facing docs, or tracked repo HTML snapshots.
- [x] 1.2 Write validation evidence only to a non-release runtime audit path, with no release-manifest or dashboard links.
- [x] 1.3 Generate aligned rolling holdout predictions for Stan GLM, BART, equal-weight average, and learned ensemble on the same historical holdout years and matchup rows.
- [x] 1.4 Fit the constrained logit-scale ensemble combiner from out-of-fold predictions only, with `w` for Stan GLM, `1 - w` for BART, `0 <= w <= 1`, and optional intercept.
- [x] 1.5 Produce a machine-readable and human-readable validation report comparing log loss, Brier score, accuracy, bracket score, and correct picks by year and in summary.
- [x] 1.6 Evaluate the promotion gate: learned ensemble mean bracket score must strictly beat Stan GLM, BART, and equal-weight average; mean correct picks must be no worse than the best component; log loss and Brier must pass configured guardrails.
- [x] 1.7 Stop implementation before production integration if the promotion gate fails, and record the failed gate conditions in the validation report.

## 2. Failed-Gate De-Implementation Path

Gate passed, so this section was verified as not applicable rather than executed.

- [x] 2.1 If the promotion gate fails, confirm that no production ensemble config keys, primary-pipeline routing, saved-results schema changes, dashboard changes, release-manifest changes, tracked snapshots, or product-facing ensemble docs were added.
- [x] 2.2 Remove the isolated evaluator and temporary validation artifacts unless the helper is intentionally retained as generic model-evaluation infrastructure.
- [x] 2.3 If any helper code remains after a failed gate, rename/scope it as generic model-evaluation infrastructure with tests and no ensemble product semantics.
- [x] 2.4 Verify after failed-gate cleanup that the current Stan-primary plus BART-audit workflow, existing dashboards, and release-facing artifacts remain intact.
- [x] 2.5 Run focused tests for the retained current workflow after cleanup.

## 3. Configuration and Contracts After Gate Passes

- [x] 3.1 Add explicit ensemble model configuration defaults in `R/configuration.R` and `config.yml`, including enabled mode, component engines, constrained logit-weight combiner type, bracket-picking primary acceptance target, and calibration guardrails.
- [x] 3.2 Add configuration validation that fails before fitting when ensemble mode requests unsupported engines, unsupported combiners, missing BART settings, or invalid acceptance targets.
- [x] 3.3 Update model overview/provenance helpers so saved bundles can distinguish primary ensemble output from Stan GLM and BART component engines.
- [x] 3.4 Add focused config tests covering valid ensemble defaults and invalid ensemble configuration failures.

## 4. Ensemble Combiner and Draw Prediction After Gate Passes

- [x] 4.1 Implement deterministic ensemble prediction for current matchup rows, returning a draw-by-game probability matrix compatible with existing simulation code.
- [x] 4.2 Define deterministic draw alignment or seed-controlled resampling when combining Stan GLM and BART posterior draws.
- [x] 4.3 Add tests for prediction shape, repeatability under seed, component engine metadata, and failure on insufficient ensemble training evidence.
- [x] 4.4 Preserve fail-closed behavior when a required component engine cannot fit, predict, or validate.

## 5. Primary Pipeline Integration After Gate Passes

- [x] 5.1 Update `R/main.R` so authoritative full simulation fits both component engines and routes bracket simulation through ensemble probabilities when ensemble mode is enabled.
- [x] 5.2 Update `scripts/run_bracket_candidates.R` so lighter candidate generation uses the saved or freshly fit ensemble probability source rather than a single component engine.
- [x] 5.3 Ensure candidate CSVs, decision sheets, matchup context, candidate summaries, and total-points outputs identify the ensemble as the primary bracket-picking model source.
- [x] 5.4 Add pipeline smoke tests proving ensemble mode produces candidates without single-engine fallback behavior.

## 6. Saved Results and Regeneration After Gate Passes

- [x] 6.1 Persist ensemble combiner metadata, component engine metadata, validation metrics, promotion-gate result, and primary model labels in `tournament_sim.rds` or the existing saved results bundle.
- [x] 6.2 Update dashboard-only regeneration helpers to restore ensemble state from saved results without refitting models.
- [x] 6.3 Add compatibility checks for saved bundles missing required ensemble metadata.
- [x] 6.4 Add tests that regenerate dashboards from a saved ensemble bundle and verify the same primary recommendation metadata is preserved.

## 7. Dashboard Cleanup After Gate Passes

- [x] 7.1 Update `create_bracket_dashboard_html()` and dashboard context helpers so ensemble bracket recommendations appear before component-engine audit material.
- [x] 7.2 Retire `model_comparison_dashboard.html` from generated release-facing dashboards, runtime links, tracked repo snapshots, release manifests, and documentation.
- [x] 7.3 Update `create_technical_dashboard_html()` so ensemble-first diagnostics replace the old standalone Stan/BART comparison surface.
- [x] 7.4 Add technical-dashboard ensemble diagnostics content showing learned weight, optional intercept, equal-weight baseline, year-level bracket-picking deltas, promotion-gate status, and log-loss/Brier guardrail status.
- [x] 7.5 Remove or rewrite dashboard copy that presents metric-count Stan-versus-BART wins as the user-facing bracket decision.
- [x] 7.6 Ensure Candidate 1, Candidate 2, review queue, evidence drawers, bracket tree, and tiebreaker sections all display ensemble primary-model provenance.
- [x] 7.7 Add or update Playwright/dashboard smoke coverage for desktop and mobile ensemble-first behavior.

## 8. Documentation and Publishing After Gate Passes

- [x] 8.1 Update `README.md` to describe the ensemble bracket picker as the supported primary workflow, list the current dashboard artifact set, and remove stale `model_comparison_dashboard.html` user-facing links or descriptions.
- [x] 8.2 Update `docs/methods-and-interpretation.md` to explain the ensemble method, proof gate, component engines, bracket-picking validation target, calibration guardrails, and monitoring-only current-year outcomes.
- [x] 8.3 Update `AGENTS.md` so future agents see the current ensemble/dashboard contract, proof-gate rule, and retired comparison-dashboard behavior.
- [x] 8.4 Update release/publish documentation, artifact manifest descriptions, and dashboard navigation references so `technical_dashboard.html` is the ensemble/component diagnostics surface.
- [x] 8.5 Keep betting-line language unchanged except where needed to clarify that ensemble mode remains betting-free on `master`.
- [x] 8.6 Update publishing and release tests so `model_comparison_dashboard.html` is no longer required as a release-facing deliverable and `technical_dashboard.html` carries the ensemble diagnostics contract.
- [x] 8.7 Run a documentation drift check with `rg` or equivalent to confirm no public/internal docs still describe Stan GLM versus BART as the user-facing model decision after integration.

## 9. End-to-End Verification

- [x] 9.1 Run focused `testthat` files for configuration, model fitting/BART, rolling backtest, simulation, saved-results regeneration, and dashboard context.
- [x] 9.2 Run the full package suite with `Rscript tests/testthat.R`.
- [x] 9.3 Run dashboard interaction tests with `npm test`.
- [x] 9.4 Run `Rscript scripts/run_simulation.R` against the canonical runtime configuration and inspect the runtime ensemble dashboard artifacts only after the proof gate passes.
- [x] 9.5 Regenerate dashboards from saved results and confirm tracked repo HTML snapshots mirror runtime output only after runtime validation passes.
- [x] 9.6 Validate the OpenSpec change with `openspec validate add-ensemble-bracket-picker --strict`.
