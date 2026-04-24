## ADDED Requirements

### Requirement: Ensemble integration SHALL be gated by pre-integration proof
The system SHALL prove the learned ensemble improves the bracket-picking target in an isolated validation lane before any production simulation, dashboard, saved-results, publish, or release workflow is rewired to use ensemble output.

#### Scenario: Pre-integration evaluator remains isolated
- **WHEN** the ensemble proof gate is being evaluated
- **THEN** the implementation SHALL use real project data and existing model/scoring helpers where possible
- **THEN** the implementation SHALL NOT add production ensemble config defaults
- **THEN** the implementation SHALL NOT change saved-results schemas, dashboard generation, release manifests, tracked HTML snapshots, publishing behavior, or product-facing documentation
- **THEN** validation artifacts SHALL be written only to a non-release audit location

#### Scenario: Pre-integration validation passes
- **WHEN** the isolated ensemble validation lane completes
- **THEN** it SHALL compare Stan GLM, BART, equal-weight average, and learned ensemble on the same rolling holdout years
- **THEN** the learned ensemble SHALL have a strictly higher mean bracket score than Stan GLM, BART, and equal-weight average
- **THEN** the learned ensemble SHALL have mean correct picks no worse than the best component engine
- **THEN** the learned ensemble SHALL satisfy configured log loss and Brier score guardrails relative to the better component engine
- **THEN** the implementation MAY proceed to primary pipeline and dashboard integration

#### Scenario: Pre-integration validation fails
- **WHEN** the isolated ensemble validation lane does not satisfy the promotion gate
- **THEN** production simulation, dashboard, saved-results, publish, and release behavior SHALL remain on the existing workflow
- **THEN** the implementation SHALL NOT add ensemble mode as a dormant production option
- **THEN** the implementation SHALL produce a clear failed-gate report identifying which gate conditions failed

### Requirement: Failed ensemble gate SHALL have a de-implementation path
If the ensemble fails the pre-integration proof gate, implementation work SHALL remove the isolated evaluator and temporary proof artifacts unless a retained helper is generic model-evaluation infrastructure.

#### Scenario: Failed gate cleanup is performed
- **WHEN** the ensemble promotion gate fails during implementation
- **THEN** there SHOULD be no exploratory ensemble config keys, dashboard changes, release-manifest changes, primary-pipeline routing, saved-results schema changes, or product-facing documentation to remove
- **THEN** the isolated evaluator and temporary validation artifacts SHALL be removed unless explicitly retained as generic model-evaluation infrastructure
- **THEN** any retained helper code SHALL be generic model-evaluation infrastructure with tests and no ensemble product semantics
- **THEN** the current Stan-primary plus BART-audit workflow SHALL remain intact

### Requirement: Successful ensemble integration SHALL update documentation
If the ensemble passes the pre-integration proof gate and is integrated into the production workflow, all relevant documentation SHALL be updated in the same change so documented behavior matches the live workflow.

#### Scenario: Ensemble integration changes supported workflow
- **WHEN** ensemble mode becomes the supported primary bracket-picking workflow
- **THEN** `README.md` SHALL describe the ensemble-first bracket picker and current dashboard artifact set
- **THEN** `docs/methods-and-interpretation.md` SHALL explain the ensemble method, proof gate, component engines, validation target, and calibration guardrails
- **THEN** `AGENTS.md` SHALL describe the current ensemble/dashboard contract for future repository work
- **THEN** release/publish documentation and artifact descriptions SHALL remove or revise references to retired comparison artifacts

#### Scenario: Documentation still describes old engine comparison workflow
- **WHEN** implementation review finds documentation that still frames Stan GLM versus BART as the user-facing model decision
- **THEN** the ensemble integration SHALL be considered incomplete
- **THEN** implementation SHALL update or remove the stale documentation before final verification

### Requirement: Ensemble mode SHALL fit both component engines
When ensemble bracket picking is enabled, the authoritative simulation workflow SHALL fit Stan GLM and BART as required component engines before generating primary bracket candidates.

#### Scenario: Both engines are available
- **WHEN** the full simulation runs with ensemble mode enabled
- **THEN** Stan GLM SHALL be fit using the configured Stan predictors, priors, and interaction terms
- **THEN** BART SHALL be fit using the same base predictors without explicit interaction terms
- **THEN** the workflow SHALL continue only after both component engines can produce matchup probability draws

#### Scenario: A required component engine is unavailable
- **WHEN** ensemble mode is enabled and either Stan GLM or BART cannot fit or predict
- **THEN** the authoritative simulation SHALL fail with a clear required-component error
- **THEN** the workflow SHALL NOT silently substitute a single-engine bracket, neutral weights, or stale component output

### Requirement: Ensemble training SHALL use rolling out-of-fold predictions
The ensemble combiner SHALL be trained only from historical rolling holdout predictions where each held-out game is predicted by component models trained on earlier tournament years.

#### Scenario: Ensemble training rows are leakage-free
- **WHEN** the ensemble training table is built
- **THEN** each row SHALL identify the holdout year, game identity, Stan prediction, BART prediction, and observed outcome
- **THEN** each row's component predictions SHALL come from models trained without that holdout year
- **THEN** current-year completed Round of 64+ outcomes SHALL NOT contribute to ensemble training

#### Scenario: Historical validation is too sparse
- **WHEN** there are not enough completed rolling holdout predictions to fit the configured ensemble combiner
- **THEN** ensemble fitting SHALL fail clearly
- **THEN** the workflow SHALL NOT infer weights from current-year monitoring outcomes

### Requirement: Ensemble probabilities SHALL become the primary bracket simulation source
After the pre-integration proof gate passes, the primary bracket simulation SHALL consume ensemble matchup probability draws rather than choosing between Stan GLM and BART component predictions.

#### Scenario: Current bracket is simulated
- **WHEN** the current tournament bracket is simulated in ensemble mode after the proof gate has passed
- **THEN** each matchup SHALL be scored with an ensemble draw-by-game probability matrix
- **THEN** candidate brackets, decision sheets, bracket matchups, and dashboard context SHALL identify the ensemble as the primary model source

#### Scenario: Component predictions remain auditable
- **WHEN** the ensemble generates the primary matchup probabilities
- **THEN** the saved results bundle SHALL retain Stan GLM and BART component metadata sufficient to audit their predictions and validation metrics
- **THEN** component metrics SHALL NOT replace the ensemble as the primary recommendation unless ensemble mode is disabled by an explicit supported configuration

### Requirement: Ensemble validation SHALL evaluate the bracket-picking target
The ensemble validation output SHALL compare Stan GLM, BART, and ensemble performance on rolling holdout seasons using bracket-picking and probability-quality metrics.

#### Scenario: Validation metrics are produced
- **WHEN** rolling ensemble validation completes
- **THEN** the output SHALL include year-level and summary metrics for log loss, Brier score, accuracy, bracket score, and correct picks for Stan GLM, BART, equal-weight average, and learned ensemble
- **THEN** the output SHALL identify bracket score and correct picks as the primary bracket-picking acceptance target used to judge whether the ensemble improves the product
- **THEN** the output SHALL identify log loss and Brier score as calibration/probability guardrails

#### Scenario: Ensemble does not improve the configured target
- **WHEN** ensemble validation does not satisfy the configured acceptance criterion
- **THEN** the run SHALL make that result visible in the audit output
- **THEN** the implementation SHALL NOT hide the validation result behind a dashboard-only comparison

#### Scenario: Ensemble improves bracket score but harms calibration
- **WHEN** ensemble validation improves bracket score or correct picks but materially worsens log loss or Brier score beyond the configured guardrail
- **THEN** the validation output SHALL mark the ensemble promotion evidence as failing the calibration guardrail
- **THEN** the dashboard diagnostics SHALL show that the bracket-score improvement did not produce an unqualified model-quality win

### Requirement: Saved results SHALL preserve ensemble provenance
The saved results bundle SHALL contain enough ensemble metadata to regenerate dashboards and audit the run without refitting models.

#### Scenario: Dashboard regeneration uses saved results
- **WHEN** dashboards are regenerated from a saved results bundle
- **THEN** the dashboard context SHALL recover the ensemble primary model label, component engine labels, ensemble weights or coefficients, and validation summaries
- **THEN** regenerated dashboards SHALL match the ensemble recommendation state from the original full simulation run

#### Scenario: Saved bundle lacks required ensemble metadata
- **WHEN** dashboard regeneration is requested from a saved bundle that is missing required ensemble metadata
- **THEN** regeneration SHALL fail clearly or mark the bundle as incompatible
- **THEN** regeneration SHALL NOT fabricate ensemble weights or silently fall back to old engine-comparison semantics

### Requirement: Ensemble configuration SHALL be explicit
The runtime configuration SHALL expose ensemble behavior through explicit model configuration keys rather than hidden defaults or dashboard-only assumptions.

#### Scenario: Default configuration enables the supported ensemble workflow
- **WHEN** the default project configuration is loaded after this change
- **THEN** the model configuration SHALL indicate that Stan GLM and BART are required component engines for ensemble bracket picking
- **THEN** ensemble combiner settings SHALL identify a constrained logit-weight ensemble as the default combiner
- **THEN** acceptance-target settings SHALL identify bracket-picking performance as primary with log loss and Brier score guardrails

#### Scenario: Unsupported ensemble configuration is provided
- **WHEN** configuration requests an unsupported combiner, missing component engine, or invalid acceptance target
- **THEN** configuration validation SHALL fail before model fitting starts

### Requirement: Equal-weight averaging SHALL be retained as baseline evidence
The ensemble workflow SHALL evaluate a simple equal-weight Stan/BART average as baseline evidence but SHALL NOT treat it as the only ensemble implementation unless it wins the configured acceptance target.

#### Scenario: Learned ensemble is validated
- **WHEN** rolling ensemble validation runs
- **THEN** validation SHALL include the constrained learned ensemble
- **THEN** validation SHALL include an equal-weight Stan/BART average as a baseline comparator
- **THEN** the primary ensemble used for bracket generation SHALL be the configured combiner that satisfies the promotion rule
