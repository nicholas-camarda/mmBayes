## Context

The current production workflow fits Stan GLM as the primary engine and BART as an alternate comparison engine. Both engines already produce draw-by-game matchup probabilities, and the downstream simulation, candidate generation, decision sheet, and dashboards can consume that draw-matrix shape.

The weakness is product alignment. The engine-comparison dashboard can say Stan GLM narrowly wins bracket-score metrics while BART narrowly wins probability metrics, but that does not directly produce a better bracket. The user-facing product is the bracket picker, not the diagnostics board. This change turns Stan GLM and BART into component engines for one ensemble recommendation surface.

Current-year completed games remain monitoring-only except for existing First Four path resolution. Betting-line work remains outside the supported `master` workflow.

## Goals / Non-Goals

**Goals:**

- Fit Stan GLM and BART as required component engines for the supported ensemble workflow.
- Learn an ensemble matchup-probability combiner from rolling historical holdout predictions only.
- Prove the ensemble improves bracket-picking performance in an isolated validation gate before routing any production simulation, candidate generation, saved results, release output, or dashboard workflow through it.
- Use the ensemble draw-by-game probability matrix as the primary input to bracket simulation, candidate generation, decision sheets, dashboards, and saved results bundles only after the validation gate passes.
- Preserve enough provenance to audit component engine performance, ensemble weights, validation metrics, and cached-result dashboard regeneration.
- Clean up dashboard presentation only after the validation gate passes, so users see the best bracket-entry recommendation first and engine comparison only as secondary audit evidence.
- Keep runtime/cloud/repo artifact boundaries stable and avoid new output roots.

**Non-Goals:**

- Do not introduce betting-line predictors or market overlays into the supported workflow.
- Do not add a neural network or unconstrained high-dimensional learner.
- Do not treat current-year completed Round of 64+ outcomes as pre-tournament training data.
- Do not add silent fallback behavior when an ensemble component fails.
- Do not make the dashboard a model-selection console where the user chooses Stan versus BART manually.
- Do not integrate ensemble behavior into production dashboard or publishing surfaces before the evidence gate passes.
- Do not add production ensemble config keys, saved-results schema changes, dashboard changes, release-manifest changes, or product-facing docs before the proof gate passes.

## Decisions

### Decision 1: Use a stacked probability ensemble, not winner-take-all engine selection

The ensemble SHALL combine Stan GLM and BART matchup probabilities into a single bracket-picking probability source. The first implementation should use a constrained, interpretable combiner over out-of-fold predictions:

```text
logit(p_ensemble) = alpha
                 + w * logit(p_stan)
                 + (1 - w) * logit(p_bart)
where 0 <= w <= 1
```

The fitted combiner should use a normalized Stan/BART weight and optional intercept. Equal-weight averaging should be computed as a baseline comparator, but the primary ensemble should learn its component weight from rolling holdout predictions.

Alternatives considered:

- Pure average of Stan and BART: simpler, but does not learn whether one engine is consistently better calibrated.
- Dashboard-only comparison: preserves current behavior, but does not improve bracket picking.
- More complex meta-model with many features: too easy to overfit with the current historical tournament count.

### Decision 2: Learn ensemble behavior from out-of-fold historical predictions

The ensemble training table SHALL be built from rolling holdout predictions where each holdout game is predicted only by models fit on earlier tournament years. This table is the only acceptable source for ensemble weights and ensemble validation.

The existing rolling backtest machinery should be reused and extended rather than replaced. Both component engines should be run on the same holdout splits, with Stan GLM retaining explicit configured interactions and BART using the same base predictors without explicit interaction terms.

The proof phase should be a minimal isolated evaluator: a script or testable helper that reads the current project config/data, reuses existing fit/predict/scoring helpers, writes validation evidence under a non-release runtime audit path, and does not alter production runtime contracts.

### Decision 3: Propagate ensemble draws through the existing simulation interface

The downstream simulation code should continue consuming draw-by-game matrices. The ensemble should therefore expose the same shape as `predict_matchup_rows()` results.

For each matchup and posterior draw index, combine the aligned Stan and BART draw probabilities into an ensemble draw probability. If exact draw alignment is not meaningful across engines, the implementation may resample draws by seed-controlled index before combination, but the output must remain a draw-by-game matrix with deterministic behavior under the configured random seed.

### Decision 4: Make ensemble output primary and component output audit-only

This decision is conditional on the validation gate passing. Before that gate passes, ensemble outputs SHALL remain exploratory validation artifacts and SHALL NOT become the primary simulation, dashboard, or release path.

The saved results bundle SHALL contain:

- ensemble model metadata
- component engine metadata
- ensemble weights or combiner coefficients
- rolling validation metrics for Stan, BART, and ensemble
- primary candidate brackets generated from ensemble probabilities

The main dashboard SHALL present ensemble candidates as the bracket-entry recommendation. Stan GLM and BART scorecards may remain available through the technical dashboard or an audit section, but they should not ask the user to pick an engine.

### Decision 5: Promote by bracket-picking value with calibration guardrails

The ensemble is for bracket selection, so the primary promotion target SHALL be bracket-picking performance, not metric-count wins across generic diagnostics. The acceptance rule should require the ensemble to improve or tie the best component on bracket score/correct picks while avoiding material degradation in log loss and Brier score.

The initial promotion gate SHALL be strict enough to prevent churn:

1. The learned ensemble mean bracket score must be strictly higher than Stan GLM, BART, and equal-weight average on rolling holdout validation.
2. The learned ensemble mean correct picks must be no worse than the best component engine.
3. The learned ensemble mean log loss and Brier score must stay within the configured calibration guardrail of the better component engine.
4. Year-level results must be reported before integration; if a single-year bracket-score regression is large enough to undermine confidence, the implementation should stop and bring the evidence back for review rather than continue dashboard integration.

The validation report should make failures explicit. If the ensemble improves probability scoring but worsens bracket selection, it is not automatically the product winner. If it improves bracket score by exploiting unstable probability extremes and materially worsens calibration, it also fails.

Alternatives considered:

- Primary log loss/Brier target: useful for probability honesty, but not aligned with the bracket-entry product.
- Pure bracket score target with no guardrails: aligned with the product but too vulnerable to noisy tournament outcomes and overconfident probabilities.
- Metric-count majority vote: too coarse and already produced the unhelpful Stan-versus-BART dashboard behavior.

### Decision 6: Retire the standalone model-comparison dashboard

The standalone concept behind `model_comparison_dashboard.html` should be retired. The product should not ship a page whose headline is "Stan GLM versus BART" or "preferred engine" after the pipeline moves to an ensemble. Instead, the generated diagnostics surface should be ensemble-first:

1. Primary model: ensemble.
2. Components: Stan GLM and BART.
3. Learned weight/intercept and equal-weight baseline comparison.
4. Rolling holdout bracket-picking metrics for ensemble, Stan, and BART.
5. Calibration/log-loss/Brier guardrail checks.
6. Year-level diagnostics showing where the ensemble helped or hurt.
7. Confirmation that current-year completed games are monitoring-only.

The implementation SHALL fold these diagnostics into `technical_dashboard.html` and retire `model_comparison_dashboard.html` from the generated release-facing dashboard set. This keeps the product surface cleaner:

- `bracket_dashboard.html`: the bracket-entry product.
- `technical_dashboard.html`: model, calibration, live-monitoring, and ensemble/component audit evidence.

Component model diagnostics remain necessary because the ensemble depends on Stan GLM and BART. They should be visible as secondary audit tables/sections inside the technical dashboard, not as a separate page that competes with the ensemble recommendation.

### Decision 7: Fail closed when required component engines are unavailable

BART is part of the supported ensemble contract. If ensemble mode is enabled and either Stan GLM or BART cannot fit, predict, or validate, the authoritative full simulation SHALL fail clearly. The implementation should not add `allow_unavailable` behavior, neutral weights, copied probabilities, or a single-engine substitute path.

### Decision 8: Keep artifact boundaries stable unless a specific ensemble artifact is justified

The runtime `output/` tree remains authoritative. Tracked repo `output/` remains a mirror. Release publishing remains manifest-driven and narrow.

If implementation adds an ensemble audit artifact, it must be intentionally named and either:

- included in the release manifest because it is user-facing, or
- kept as runtime scratch/audit output and omitted from release publishing.

### Decision 9: Treat documentation as part of successful integration

If the proof gate passes and ensemble integration proceeds, documentation updates are part of the implementation, not follow-up polish. The docs must reflect the live workflow at the end of the same change:

- `README.md` should describe the ensemble-first bracket picker, supported command flow, and dashboard artifact set.
- `docs/methods-and-interpretation.md` should explain the ensemble method, proof gate, component engines, calibration guardrails, and monitoring-only current-year outcomes.
- `AGENTS.md` should describe the current supported modeling/dashboard contract for future agents.
- Release/publish documentation and manifest descriptions should stop listing retired comparison artifacts and should identify `technical_dashboard.html` as the ensemble/component diagnostics surface.
- Any dashboard links or generated artifact descriptions should stop sending users to a standalone Stan-versus-BART decision page.

Docs should not be updated before the proof gate passes except to record the temporary validation report in implementation notes, because premature docs would describe behavior the product does not yet support.

### Decision 10: Implement an explicit de-implementation path if the gate fails

The first implementation phase SHALL be isolated from the production dashboard and release flow. It should not create production ensemble plumbing that would need rollback. If the validation gate fails, cleanup should be limited to removing the isolated evaluator, temporary validation artifacts, and tests that are only useful for the failed spike. The remaining repo should preserve the current Stan-primary plus BART-audit workflow, with no dormant ensemble mode and no misleading dashboard copy.

If some evidence-gathering helper is worth keeping after a failed gate, it must be renamed and scoped as a generic model-evaluation helper, not left as unused ensemble product plumbing.

## Risks / Trade-offs

- [Risk] The ensemble overfits the small number of holdout years. -> Mitigation: train only on rolling out-of-fold predictions, report year-level deltas, and require improvement on the bracket-picking target before promoting the ensemble as primary.
- [Risk] Bracket score is noisy and may reward unstable probability shifts. -> Mitigation: evaluate both bracket-score outcomes and probability calibration, but make the primary acceptance target explicit in tasks and dashboards.
- [Risk] Component draw matrices may not be naturally aligned. -> Mitigation: define deterministic draw combination or seed-controlled resampling and test repeatability.
- [Risk] Dashboard cleanup accidentally removes auditability. -> Mitigation: keep component-engine metrics in secondary technical/audit sections and persist provenance in saved results bundles.
- [Risk] Cached-result dashboard regeneration drifts from full simulation output. -> Mitigation: put all ensemble metadata needed by dashboards into the saved results bundle and test regeneration from saved results.
- [Risk] BART dependency/runtime burden increases full-run fragility. -> Mitigation: fail closed with a clear required-component error rather than silently producing a single-engine bracket.

## Migration Plan

1. Build a minimal pre-integration validation evaluator that creates rolling holdout Stan, BART, equal-weight, and learned-ensemble evidence from real project data without changing production simulation, dashboard, saved-results schema, config defaults, publish, product docs, tracked snapshots, or release behavior.
2. Produce a machine-readable and human-readable validation report with the promotion-gate result.
3. Stop if the validation gate fails; remove only the isolated evaluator/temporary artifacts unless a generic tested evaluation helper is worth keeping.
4. Only if the validation gate passes, add ensemble config defaults under the existing model configuration surface.
5. Add the ensemble prediction/draw abstraction while preserving existing component-engine fit helpers.
6. Route full simulation and bracket candidate generation through the ensemble prediction source.
7. Persist ensemble provenance in the saved results bundle.
8. Update main and technical dashboards so the ensemble recommendation is primary and component comparison is audit-only.
9. Regenerate runtime dashboards from a full run and from saved results to verify both paths.
10. Update README, methods documentation, AGENTS guidance, release/publish docs, dashboard links, and artifact descriptions so they match the ensemble-first workflow.
11. Sync tracked repo HTML snapshots only after runtime artifacts and documentation pass validation.

Rollback before promotion should be trivial because production plumbing must not exist yet: remove the isolated evaluator and temporary validation artifacts. Rollback after promotion is a deliberate code/config rollback to the previous primary-engine workflow. Neither case should be implemented as a runtime fallback branch.

## Open Questions

- None for the initial implementation. Future work can split diagnostics into a separate artifact only if `technical_dashboard.html` becomes too dense after the ensemble integration.
