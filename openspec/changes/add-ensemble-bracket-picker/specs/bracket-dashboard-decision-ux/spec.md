## ADDED Requirements

### Requirement: Dashboard SHALL present ensemble bracket recommendations as primary
After the ensemble proof gate passes, the main bracket dashboard SHALL treat the ensemble bracket picker as the primary recommendation surface when ensemble mode is enabled.

#### Scenario: Ensemble candidates are available
- **WHEN** `bracket_dashboard.html` is generated from an ensemble run after the proof gate has passed
- **THEN** the first decision-oriented dashboard sections SHALL identify the ensemble as the model source for Candidate 1, Candidate 2, review queues, and evidence summaries
- **THEN** the dashboard SHALL NOT ask the user to choose between Stan GLM and BART before entering a bracket

#### Scenario: Candidate artifacts are generated
- **WHEN** candidate CSV, TXT, and decision-sheet artifacts are written from an ensemble run after the proof gate has passed
- **THEN** their model-source metadata SHALL identify the ensemble recommendation
- **THEN** component-engine labels SHALL appear only as audit or provenance context

#### Scenario: Ensemble proof gate has not passed
- **WHEN** ensemble validation has not passed the pre-integration proof gate
- **THEN** `bracket_dashboard.html` SHALL remain on the current bracket-entry workflow
- **THEN** dashboard cleanup SHALL NOT remove current production surfaces or replace model provenance with ensemble labels

### Requirement: Engine comparison SHALL be secondary audit evidence
Generated dashboards SHALL keep Stan GLM and BART comparison material available for review without making it the main user decision workflow.

#### Scenario: Technical dashboard includes component audit
- **WHEN** the technical dashboard is generated from an ensemble run
- **THEN** Stan GLM, BART, and ensemble validation metrics SHALL be available in an audit or technical section
- **THEN** the section SHALL explain that component-engine comparison is diagnostic evidence and not the bracket-entry recommendation

#### Scenario: Model comparison page is retired
- **WHEN** implementation updates generated dashboard artifacts after the ensemble proof gate passes
- **THEN** `model_comparison_dashboard.html` SHALL be removed from the release-facing dashboard set
- **THEN** no generated dashboard link SHALL send users to a standalone Stan-versus-BART decision page
- **THEN** runtime output links, tracked repo snapshots, release manifests, and documentation SHALL be updated in the same implementation

#### Scenario: Dashboard documentation links are updated
- **WHEN** dashboard artifacts are regenerated after the ensemble proof gate passes
- **THEN** public README links, methods-guide artifact tables, release-manifest descriptions, and any dashboard navigation references SHALL identify `bracket_dashboard.html` as the bracket-entry product and `technical_dashboard.html` as the ensemble/component diagnostics surface
- **THEN** no documentation SHALL list `model_comparison_dashboard.html` as a supported release-facing dashboard

#### Scenario: Technical dashboard carries model diagnostics
- **WHEN** `technical_dashboard.html` is generated from an ensemble run after the proof gate passes
- **THEN** it SHALL include ensemble-first model diagnostics
- **THEN** it SHALL identify the ensemble as the primary bracket picker
- **THEN** Stan GLM and BART scorecards SHALL appear only as component audit sections for the ensemble

### Requirement: Technical dashboard SHALL explain ensemble quality explicitly
The technical dashboard SHALL explain whether the ensemble improves bracket picking and whether probability-quality guardrails remain acceptable.

#### Scenario: Ensemble diagnostics are shown
- **WHEN** technical dashboard diagnostics are rendered for an ensemble run
- **THEN** the diagnostics SHALL show learned Stan/BART weight, optional intercept, and equal-weight baseline comparison
- **THEN** the diagnostics SHALL show rolling holdout bracket score and correct-pick metrics for ensemble, Stan GLM, BART, and equal-weight baseline
- **THEN** the diagnostics SHALL show log loss and Brier guardrail status
- **THEN** the diagnostics SHALL show year-level rows or summaries identifying where the ensemble helped or hurt

#### Scenario: Current-year live results are shown
- **WHEN** current-year completed-game monitoring appears in technical dashboard diagnostics
- **THEN** it SHALL be labeled as monitoring-only
- **THEN** it SHALL NOT be presented as evidence used to train ensemble weights or select the pre-tournament bracket recommendation

### Requirement: Dashboard copy SHALL align with bracket-picking objective
Dashboard text and labels SHALL describe the ensemble output as the bracket picker and SHALL avoid presenting metric-count engine wins as the product decision.

#### Scenario: Backtest summary is displayed
- **WHEN** ensemble validation metrics are shown in dashboard summary cards
- **THEN** the summary SHALL distinguish bracket-picking metrics from calibration/probability metrics
- **THEN** the summary SHALL state the configured primary acceptance target used for ensemble evaluation
- **THEN** the summary SHALL NOT use metric-count wins as the decisive dashboard headline

#### Scenario: Close component comparison occurs
- **WHEN** Stan GLM and BART have mixed component-engine validation results
- **THEN** the dashboard SHALL show that as audit context
- **THEN** the main recommendation SHALL continue to be the ensemble candidates generated from the integrated probability source

### Requirement: Dashboard smoke tests SHALL cover ensemble-first behavior
Dashboard verification SHALL assert that generated HTML surfaces the ensemble recommendation first and keeps component comparison secondary.

#### Scenario: Desktop dashboard smoke test validates ensemble-first flow
- **WHEN** fixture or runtime dashboard HTML is tested in a desktop browser viewport
- **THEN** the test SHALL verify that ensemble candidate guidance appears before component-engine comparison material
- **THEN** candidate toggles, evidence jumps, and graphical surfaces SHALL remain interactive

#### Scenario: Mobile dashboard smoke test validates ensemble-first flow
- **WHEN** dashboard HTML is tested in a mobile browser viewport
- **THEN** the first usable workflow SHALL remain bracket entry and review guidance
- **THEN** component-engine comparison material SHALL not displace the ensemble recommendation from the initial mobile workflow
