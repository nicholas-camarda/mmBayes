## Context

The archived OddsPapi change established a forward-only `2026+` v4 workflow, but the current spec still leaves too much room for ambiguous success. In practice, we learned three things:

1. The user does not care whether `2025` and earlier can be backfilled.
2. A partially populated `closing_lines.csv` is operationally useful, but it is not the same thing as a successful season archive.
3. The repo needs two different answers that should not be conflated:
   - whether we can reliably archive complete tournament closing lines going forward
   - whether complete current-year closing lines would have improved bracket prediction

The current runtime state already demonstrates why the distinction matters. We were able to repair [closing_lines.csv](/Users/ncamarda/ProjectsRuntime/mmBayes/data/odds_history/2026/closing_lines.csv) into a non-flat, real file for part of the `2026` tournament, but that does not mean the forward archive contract is done, and it does not answer whether complete closing-line coverage would improve bracket decisions. The updated design therefore needs to define success around season completeness and an honest bracket-impact study, not around partial provider retrieval or unsupported pre-2026 backfill.

## Goals / Non-Goals

**Goals:**
- Define operational success as a complete, clean, repo-compatible closing-line archive for the active target season, measured against the local tournament results workbook for that season.
- Keep the supported provider contract forward-only (`2026+`) and stop treating pre-2026 recovery as a success criterion.
- Require importer/reporting logic to distinguish `complete success`, `partial archive`, and `provider-limited archive` explicitly.
- Add a clear retrospective evaluation that answers the specific question: if we had the complete set of `2026` closing lines for every completed tournament game, would those lines have improved bracket prediction?
- Require that retrospective evaluation to include both direct line-driven matchup substitution and candidate-ranking overlay analyses.
- Require season reporting to expose separate moneyline and spread completeness rates in addition to the overall season status.
- Preserve graceful degradation for missing historical seasons so the model still runs when betting archives are absent outside the supported window.

**Non-Goals:**
- Reopening `2025` and earlier as required backfill targets.
- Reinstating the launchd/live collector as the supported workflow.
- Treating partial current-year coverage as spec-complete success.
- Pretending a retrospective complete-current-year closing-line test is the same thing as a deployable pre-tournament workflow.

## Decisions

### 1. Operational success is season completeness, not partial artifact existence

Decision:
- The spec will define operational success as a `closing_lines.csv` archive that covers every completed game in the local results workbook for the target season and contains both moneyline and spread values for those rows.
- Moneyline completeness and spread completeness will also be reported separately for the season.

Rationale:
- A file can exist and still be useless, as we saw with the stale all-`NA` runtime artifact.
- A partially populated file can be helpful for debugging or limited analyses, but it does not satisfy the repo's forward-collection goal.
- Measuring completeness against the local results workbook avoids hard-coding tournament game counts and keeps the success metric aligned with the repo's own canonical data.
- Separate moneyline/spread rates make it possible to see near-success states without diluting the full feature-contract definition of success.

Alternatives considered:
- Treat any non-empty `closing_lines.csv` as success.
  Rejected because it lets incomplete or mostly missing artifacts masquerade as completed work.
- Use a fixed NCAA tournament game count as the success gate.
  Rejected because the authoritative target should be the repo's local results workbook, not a magic number embedded in the spec.

### 2. `2025` and earlier are explicitly out of the success path

Decision:
- The revised spec will state that pre-2026 seasons may remain missing without jeopardizing success, as long as the importer and model pipeline behave correctly for `2026+`.

Rationale:
- This matches the user's clarified priority.
- It keeps the spec aligned with the public v4 retention reality instead of preserving an outdated pseudo-backfill goal.

Alternatives considered:
- Keep pre-2026 backfill as a soft success objective.
  Rejected because it dilutes the implementation target and muddies operator expectations.

### 3. The spec will separate deployable archive value from retrospective “would it help?” analysis

Decision:
- The revised contract will split bracket-impact evaluation into two interpretations:
  - a deployable historical-training evaluation that only becomes meaningful once archived prior seasons exist
  - a retrospective current-year upper-bound evaluation that asks whether complete `2026` closing lines would have improved bracket prediction if they had been available for every actual `2026` matchup
- The retrospective current-year evaluation will include both:
  - a direct line-driven matchup substitution analysis that measures the informational value of complete `2026` closing lines on realized matchups
  - a candidate-ranking overlay analysis that measures whether the existing mmBayes bracket-ranking workflow would have selected a better bracket from its candidate pool using that same complete `2026` line signal

Rationale:
- The user specifically wants to know whether complete current-year closing lines would help.
- That is a valid retrospective research question even though it is not a real pre-submission bracket workflow.
- Keeping the two analyses separate prevents us from overclaiming deployable predictive benefit from a same-year retrospective study.
- The two retrospective views answer related but different questions: whether the information itself helps, and whether the repo's bracket-ranking machinery exploits that information well enough to matter.

Alternatives considered:
- Keep only the historical-training evaluation.
  Rejected because it does not answer the user's immediate question while the archive is still young.
- Treat the current-year retrospective result as deployable evidence.
  Rejected because closing lines are not available upfront for bracket submission.

### 4. Partial current-year coverage is a documented fallback state, not success

Decision:
- The importer may still write a partial current-year archive when some completed games are missing, but the resulting season status must be labeled as partial or provider-limited rather than successful.

Rationale:
- Partial archives are still useful for operational continuity and debugging.
- The repo must not blur the line between “usable artifact” and “complete archive.”

Alternatives considered:
- Refuse to write any artifact unless the year is complete.
  Rejected because partial files still provide signal and preserve recoverable work.

### 5. Repo docs and reporting should speak in terms of season status

Decision:
- Documentation, summaries, and import reports will use explicit season-level statuses such as `complete`, `partial`, and `provider-limited`, tied to completed-game coverage and non-missing line values.
- `complete` will require both moneyline and spread for every completed game, while reporting will also include separate moneyline-completeness and spread-completeness rates.

Rationale:
- The core problem we just worked through was ambiguous operational truth.
- Explicit statuses make success auditable and easier to reason about than loosely worded logs.

Alternatives considered:
- Leave status implicit in counts or raw CSV inspection.
  Rejected because operators should not need manual forensic work to know whether the spec target was met.

## Risks / Trade-offs

- [Risk] A full current-season archive may still be operationally hard to obtain within the provider retention window. → Mitigation: keep partial archives, but classify them explicitly as non-success states and preserve detailed gap reports.
- [Risk] The retrospective current-year closing-line study could show apparent benefit that is not deployable for pre-tournament brackets. → Mitigation: label it as an upper-bound or counterfactual analysis, not a real deployment evaluation.
- [Risk] Tightening the success definition may make the current implementation look less “done” than before. → Mitigation: that is intentional; the revised spec should reflect truthful success, not optimistic file existence.
- [Risk] Local results-workbook completeness becomes the season success denominator. → Mitigation: this is preferable to a hard-coded game count because it keeps the denominator aligned with the repo's own canonical tournament record.

## Migration Plan

1. Update the spec so the capability contract defines forward-only archive success around complete current-season coverage and the retrospective `2026` bracket-impact study.
2. Update implementation tasks so importer summaries/reporting compute season status from local-results coverage and usable line counts.
3. Update docs and evaluation guidance so partial current-year archives are described as partial/provider-limited, not successful completion.
4. Preserve graceful handling of missing pre-2026 betting history and document that those seasons are outside the success contract.

## Open Questions

- None at the design-contract level for this clarification change.
