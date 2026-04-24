## Why

The bracket dashboard already exposes the right decision workflow, but the current visual hierarchy makes too many surfaces feel equally important. A dark system palette and more explicit bracket-entry organization should make the dashboard faster to scan, easier to use on mobile, and clearer about which winners to enter, which calls to review, and what is only reference material.

## What Changes

- Redesign the generated `bracket_dashboard.html` layout around four visible workflow zones: decide now, enter bracket, inspect why, and reference.
- Add a bracket fill-in surface that follows how March Madness brackets are actually entered: by candidate, region, round, matchup, selected winner, champion, Final Four, and championship tiebreaker.
- Make the main workflow help the user copy or transcribe a complete bracket with minimal context switching, while using evidence only for the handful of uncertain or bracket-changing calls.
- Replace the warm-paper visual theme with a dark system palette that uses color semantically: action state, candidate identity, confidence tier, and reference state must remain visually distinct.
- Add a compact next-action summary that tells the user what to review first, what bracket choice remains, and what entry values are ready.
- Redesign the candidate divergence summary into a more graphical, bracket-like heatmap or route map that preserves click-through behavior to evidence and exact diff reference material.
- Promote Candidate 1 versus Candidate 2 into a direct comparison panel before the detailed candidate cards.
- Surface compact graphical aids where they help the entry workflow: route-difference cues on the bracket tree, probability or interval mini-bars in review cards, and championship tiebreaker distribution summaries in candidate recommendations when available.
- Improve mobile ergonomics with a compact navigation pattern and a dashboard order that prioritizes the review queue and evidence over wide tree inspection.
- Preserve the static generated HTML architecture and existing dashboard-only regeneration path; do not introduce a frontend framework or duplicate rendering path.
- Add or update Playwright coverage and fixture/dashboard generation checks for desktop and mobile layout, navigation, evidence jumps, bracket-tree interactions, and nonblank graphical elements.

## Capabilities

### New Capabilities
- `bracket-dashboard-decision-ux`: Covers the generated bracket dashboard's bracket fill-in workflow, dark visual system, graphical review aids, candidate comparison, and responsive usability requirements.

### Modified Capabilities
- `matchup-evidence-visuals`: Existing evidence panels and matchup visualizations must remain compatible with the redesigned dashboard shell and dark system palette.

## Impact

- Affected R modules: `R/plotting_functions.R` for generated dashboard structure, CSS, inline SVG styling, candidate summaries, divergence map rendering, bracket tree styling, evidence cards, and client-side interaction wiring; `R/utils.R` only if existing bracket tree data shaping needs additional route-difference metadata.
- Affected scripts: `scripts/regenerate_dashboards.R`, `scripts/regenerate_and_sync_dashboards.R`, and `scripts/run_bracket_candidates.R` should continue to produce the same artifact family through existing render helpers.
- Affected tests: `tests/testthat/test_bracket_tree_interactions.R`, dashboard-related `testthat` coverage, `e2e/bracket_tree.spec.cjs`, and `e2e/generate_fixture_dashboard.R`.
- Runtime and artifact boundaries: no `config.yml` key changes, no project-root changes, no saved-results bundle contract changes, no output filename renames, no tracked repo snapshot boundary changes, and no release-manifest membership changes are intended.
- Compatibility: existing links to `output/bracket_dashboard.html`, `technical_dashboard.html`, and `model_comparison_dashboard.html` should remain valid. Rollback should be limited to reverting dashboard generator and test changes because the change does not alter upstream model outputs or publish manifests.
