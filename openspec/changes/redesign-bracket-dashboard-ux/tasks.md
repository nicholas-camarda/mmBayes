## 1. Dashboard Structure

- [x] 1.1 Refactor `create_bracket_dashboard_html()` section assembly so the generated page has distinct bracket-entry, decision, inspection, and reference zones while preserving existing anchors and filenames.
- [x] 1.2 Add a candidate-specific fill-in surface organized by region and round with matchup, seeds, selected winner, confidence tier, champion, Final Four, title-game matchup, and tiebreaker when available.
- [x] 1.3 Add a next-action summary surface above detailed review material using existing candidate summary, watchlist, and tiebreaker data.
- [x] 1.4 Add a direct Candidate 1 versus Candidate 2 comparison panel that helps decide which complete bracket to enter before detailed candidate cards.
- [x] 1.5 Keep technical dashboard and model comparison links in reference-only areas without changing output names or release-manifest membership.

## 2. Dark Visual System

- [x] 2.1 Replace scattered dashboard colors in `R/plotting_functions.R` with dark semantic CSS tokens for background, panels, elevated cards, text, borders, actions, candidates, confidence tiers, status states, and reference material.
- [x] 2.2 Update generated cards, disclosures, controls, tables, probability tracks, and status panels to use the dark token system.
- [x] 2.3 Update matchup evidence panel styling so favorite/underdog cards, directional advantage charts, probability tracks, and raw-detail disclosures remain readable in the dark dashboard.
- [x] 2.4 Verify color roles remain distinct for candidate identity, confidence tiers, action controls, and reference-only material.

## 3. Graphical Decision Aids

- [x] 3.1 Redesign the candidate divergence surface as a compact round-by-region route map or heatmap while preserving click-through metadata.
- [x] 3.2 Preserve divergence click behavior for surfaced evidence, filtered review rows, and exact diff reference targets.
- [x] 3.3 Add compact probability or credible-interval tracks to review cards when posterior summary values are available.
- [x] 3.4 Add compact championship tiebreaker distribution summaries to candidate recommendations when total-points prediction distributions are available.
- [x] 3.5 Add bracket-tree route-difference cues and legend entries that distinguish shared path, Candidate 2-only route changes, and confidence tiers without relying on hover.

## 4. Responsive UX

- [x] 4.1 Replace the current mobile full-width stacked jump buttons with a compact mobile navigation pattern.
- [x] 4.2 Ensure the first mobile viewport communicates dashboard title, status, selected candidate entry, and next useful action without hiding the workflow behind oversized controls.
- [x] 4.3 Keep the candidate fill-in surface, review queue, and surfaced evidence usable on mobile without requiring horizontal bracket-tree inspection.
- [x] 4.4 Keep bracket-tree content horizontally scrollable and nonblank on mobile.

## 5. Tests and Verification

- [x] 5.1 Update `testthat` coverage for generated dashboard HTML contracts: section anchors, candidate fill-in surface, next-action summary, candidate comparison, divergence map metadata, and evidence panel compatibility.
- [x] 5.2 Update bracket-tree tests for candidate toggles, route-difference cues, evidence jump targets, and legend content.
- [x] 5.3 Update `e2e/generate_fixture_dashboard.R` if fixture data need additional divergence, probability, or tiebreaker coverage.
- [x] 5.4 Update Playwright coverage to verify desktop workflow zones, divergence clicks, evidence jumps, candidate toggles, and nonblank graphical elements.
- [x] 5.5 Add mobile Playwright checks for compact navigation, readable candidate fill-in guidance, review-card accessibility, and evidence jumps.

## 6. Artifact Refresh

- [x] 6.1 Regenerate fixture dashboards through the existing dashboard generation path.
- [x] 6.2 Regenerate tracked dashboard snapshots only through existing scripts that mirror runtime outputs.
- [x] 6.3 Run `Rscript tests/testthat.R`.
- [x] 6.4 Run `npm test`.
- [x] 6.5 Inspect desktop and mobile screenshots before marking the dashboard redesign complete.
