## 1. Success Contract

- [x] 1.1 Update the historical OddsPapi import/reporting path to compute season completeness against the completed-game rows in the local results workbook for each supported target year.
- [x] 1.2 Define full season success as both moneyline and spread coverage for every completed game, and add explicit season-level statuses such as `complete`, `partial`, and `provider-limited` to the import report and operator-facing summaries.
- [x] 1.3 Add separate moneyline-completeness and spread-completeness rates to season summaries without relaxing the stricter full-season success rule.
- [x] 1.4 Ensure partial or provider-limited `closing_lines.csv` artifacts remain usable but are never reported as successful season completion.

## 2. Forward-Only Archive Behavior

- [x] 2.1 Remove any remaining implementation assumptions that treat `2025` and earlier seasons as required success targets for the supported betting workflow.
- [x] 2.2 Verify that the importer, config defaults, and docs consistently define the supported archive contract as `2026+` forward collection inside the v4 retention window.
- [x] 2.3 Add or update `testthat` coverage proving that missing pre-2026 betting seasons do not block the pipeline and do not count against the new success contract.

## 3. Complete-Season Current-Year Evaluation

- [x] 3.1 Implement a retrospective `2026` complete-closing-lines evaluation that explicitly answers whether full current-year closing-line information would have improved bracket prediction for the completed `2026` tournament.
- [x] 3.2 Implement the required direct line-driven matchup substitution analysis for that retrospective `2026` study.
- [x] 3.3 Implement the required candidate-ranking overlay analysis for that retrospective `2026` study.
- [x] 3.4 Make the evaluation output distinguish between deployable historical-training evidence and same-year retrospective upper-bound evidence, and report results for both retrospective analysis types.
- [x] 3.5 Add or update `testthat` coverage for the new evaluation-state handling: complete current-year coverage, partial current-year coverage, no prior archived training seasons, and both retrospective analysis modes.

## 4. Documentation And Operator Guidance

- [x] 4.1 Update betting-related docs and script guidance so they define success as complete current-season archive coverage rather than partial artifact generation.
- [x] 4.2 Document that pre-2026 recovery is outside the supported success path and that the `2026` retrospective complete-lines study is explicitly non-deployable hindsight analysis.
- [x] 4.3 Update any reporting or release-facing notes that still imply a non-empty `closing_lines.csv` means the seasonal archive workflow is complete.
