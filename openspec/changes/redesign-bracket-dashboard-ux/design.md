## Context

`bracket_dashboard.html` is a static generated dashboard produced by the R rendering path in `R/plotting_functions.R`. It already has the correct workflow ingredients: a review queue, divergence map, candidate recommendation cards, bracket tree, evidence drawers, exact diff reference, candidate paths, and links to the technical and model comparison dashboards. The redesign must treat those ingredients as support for filling out a real March Madness bracket, where the user needs to enter winners by region and round, confirm a champion and title-game tiebreaker, and pause only for picks that deserve manual review.

The authoritative full simulation and dashboard-only regeneration flows both need to keep using the saved results bundle and existing rendering helpers. Runtime output remains under the runtime `output/` tree, tracked repo `output/` files remain publishable snapshots, and dated release folders continue to receive the approved release deliverables only.

## Open Questions Resolved Up Front

- Palette direction: use a dark system palette as the primary dashboard theme.
- Primary workflow: optimize for filling in one or two complete brackets, not for exploring analytics first.
- Entry order: provide a candidate-specific fill-in surface organized by region and round so picks can be copied into common bracket-entry forms without forcing the user to reconstruct the path from evidence drawers.
- Architecture: keep the dashboard as static generated HTML with inline CSS/JS from the existing R renderer.
- Dependencies: do not introduce a frontend framework, CSS build step, or runtime JavaScript package.
- Artifact contract: keep current dashboard filenames, links, publish behavior, saved-results bundle assumptions, and release-manifest membership.
- Verification standard: use both R/testthat checks and Playwright desktop/mobile screenshot or DOM checks for the redesigned interactive surface.

## Goals / Non-Goals

**Goals:**
- Make the dashboard read as a bracket-entry assistant with distinct zones for entering the bracket, deciding the few uncertain calls, inspecting evidence, and opening reference material.
- Provide a complete candidate-specific entry surface that lists the exact winners to enter by region and round, plus champion, Final Four, title matchup, and tiebreaker when available.
- Apply a dark system palette with semantic tokens for page surface, elevated cards, action state, confidence tier, candidate identity, warning/attention, and reference-only material.
- Make Candidate 1 versus Candidate 2 tradeoffs directly comparable before detailed candidate cards.
- Convert the divergence summary into a graphical route/heatmap surface that makes changed rounds and regions obvious while preserving click-through behavior.
- Add compact graphical cues where they reduce interpretation time: review-card probability/interval tracks, candidate tiebreaker distribution summaries, and bracket-tree route-difference cues.
- Improve mobile usability by reducing top navigation height and prioritizing review/evidence workflows over wide tree inspection.

**Non-Goals:**
- Do not change model fitting, simulation, backtest, odds, or total-points methods.
- Do not add new config keys, change project roots, rename output files, or alter release-manifest membership.
- Do not create a second dashboard renderer or a framework-backed web application.
- Do not move calibration, model comparison, or full technical diagnostics back into the main bracket workflow.

## Decisions

### Decision: Use a dark-first semantic token system
Use named CSS token groups inside the generated dashboard style string rather than scattered one-off hex colors. The token groups should cover background, panel, elevated panel, text, muted text, border, action, candidate 1, candidate 2, confidence tiers, reference, and status states.

Alternative considered: only adjust the existing warm-paper colors. That keeps the implementation smaller, but it does not solve the current hierarchy problem because the page would still rely on many similar pale cards.

### Decision: Reorganize visible sections around bracket fill-in zones
Keep the same anchor targets and underlying data, but make the visual order explicit:

```text
Enter bracket   candidate selector, fill-in checklist, champion/Final Four/tiebreaker
Decide now      next-action strip, divergence route map, review cards
Inspect why     bracket tree, evidence drawers
Reference       exact diff, full paths, technical dashboard links
```

Alternative considered: lead with the review queue because it is analytically important. That is useful for investigation, but it makes bracket entry harder because the user first needs a complete thing to enter and then a short list of places where they may choose to deviate.

### Decision: Preserve generated static HTML
Implement the redesign inside `create_bracket_dashboard_html()` and nearby helper functions. Keep the current inline JavaScript interaction model for filters, divergence jumps, evidence opening, and bracket-tree candidate toggles.

Alternative considered: split the dashboard into separate CSS/JS assets or a small frontend app. That adds build and publish complexity without changing the data contract or user workflow.

### Decision: Make the divergence surface graphical but still actionable
Replace the current card-grid divergence map with a compact route map or heatmap organized by round and region. Each active cell must continue to jump to either surfaced evidence or the exact diff reference, using the existing target metadata where possible.

Alternative considered: keep the card grid and increase contrast. That helps scan individual buckets but still does not reveal the bracket-wide pattern as quickly.

### Decision: Add graphical aids only where they answer an entry question
Review cards should show compact probability/interval tracks. Candidate cards should show compact championship total distribution summaries when available. The bracket tree should visually distinguish shared path, Candidate 2-only route differences, and confidence tiers without requiring hover.

Alternative considered: add more charts broadly. That risks turning the main dashboard into another technical dashboard, which conflicts with the existing separation between bracket workflow and diagnostics.

## Risks / Trade-offs

- Dark palette can reduce readability if contrast is not tested -> Mitigate with explicit token choices, Playwright screenshots, and DOM checks for text, cards, active states, and SVG visibility.
- Too many color encodings can confuse the user -> Mitigate by assigning one job per color family: candidate identity, confidence, action, and reference state must not reuse the same accent.
- Mobile tree inspection remains inherently constrained by a wide bracket -> Mitigate by making review/evidence the primary mobile workflow and treating the tree as horizontally scrollable secondary evidence.
- A visually rich dashboard can slow down bracket entry -> Mitigate by requiring a complete fill-in checklist that is more prominent than exploratory charts and by keeping evidence behind targeted jumps.
- Reworking generated HTML can break existing evidence jump targets -> Mitigate with tests for divergence cell clicks, review-card evidence jumps, bracket-tree evidence jumps, and candidate toggles.
- SVG route-difference styling may become visually dense -> Mitigate with a small legend and a minimal overlay that emphasizes changed route segments rather than every bracket line.

## Migration Plan

1. Update dashboard helper structure and CSS tokens in `R/plotting_functions.R` while preserving existing anchors and filenames.
2. Regenerate fixture dashboards and tracked dashboard snapshots through the existing dashboard generation scripts.
3. Run targeted `testthat` coverage for dashboard HTML contracts and bracket-tree behavior.
4. Run Playwright desktop and mobile checks against a generated fixture dashboard.
5. Roll back by reverting the dashboard generator and regenerated snapshot artifacts; no upstream model, config, saved bundle, or publish-manifest migration is required.
