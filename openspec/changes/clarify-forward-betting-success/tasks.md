## 1. Success Contract

- [ ] 1.1 Update the historical OddsPapi import/reporting path to compute season completeness against the completed-game rows in the local results workbook for each supported target year.
- [ ] 1.2 Add explicit season-level statuses such as `complete`, `partial`, and `provider-limited` to the import report and operator-facing summaries.
- [ ] 1.3 Ensure partial or provider-limited `closing_lines.csv` artifacts remain usable but are never reported as successful season completion.

## 2. Forward-Only Archive Behavior

- [ ] 2.1 Remove any remaining implementation assumptions that treat `2025` and earlier seasons as required success targets for the supported betting workflow.
- [ ] 2.2 Verify that the importer, config defaults, and docs consistently define the supported archive contract as `2026+` forward collection inside the v4 retention window.
- [ ] 2.3 Add or update `testthat` coverage proving that missing pre-2026 betting seasons do not block the pipeline and do not count against the new success contract.

## 3. Complete-Season Current-Year Evaluation

- [ ] 3.1 Implement a retrospective `2026` complete-closing-lines evaluation that explicitly answers whether full current-year closing-line information would have improved bracket prediction for the completed `2026` tournament.
- [ ] 3.2 Make the evaluation output distinguish between deployable historical-training evidence and same-year retrospective upper-bound evidence.
- [ ] 3.3 Add or update `testthat` coverage for the new evaluation-state handling: complete current-year coverage, partial current-year coverage, and no prior archived training seasons.

## 4. Documentation And Operator Guidance

- [ ] 4.1 Update betting-related docs and script guidance so they define success as complete current-season archive coverage rather than partial artifact generation.
- [ ] 4.2 Document that pre-2026 recovery is outside the supported success path and that the `2026` retrospective complete-lines study is explicitly non-deployable hindsight analysis.
- [ ] 4.3 Update any reporting or release-facing notes that still imply a non-empty `closing_lines.csv` means the seasonal archive workflow is complete.
