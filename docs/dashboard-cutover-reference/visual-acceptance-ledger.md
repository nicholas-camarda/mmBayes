# Dashboard Visual Acceptance Ledger

This ledger records the manual visual gate for the React dashboard cutover.

## Gate

The React app is accepted only when all four first-viewport comparisons and both full-page comparisons are marked `Pass`.

## Comparisons

| Surface | Reference | React Candidate | Status | Notes |
|---------|-----------|-----------------|--------|-------|
| Bracket desktop first viewport | `legacy-bracket-desktop-viewport.png` | `react-bracket-desktop-viewport.png` | Pass | Candidate entry, status, metadata, and next action all fit in the first viewport; review affordances remain visible. |
| Bracket mobile first viewport | `legacy-bracket-mobile-viewport.png` | `react-bracket-mobile-viewport.png` | Pass | Primary workflow reachable; compact entry cards; jump nav sticky. |
| Technical desktop first viewport | `legacy-technical-desktop-viewport.png` | `react-technical-desktop-viewport.png` | Pass | Action summary and diagnostics entry points visible with at least legacy-level density. |
| Technical mobile first viewport | `legacy-technical-mobile-viewport.png` | `react-technical-mobile-viewport.png` | Pass | Compare workflow accessible; no excessive card stacking. |
| Bracket desktop full page | `legacy-bracket-desktop-full.png` | `react-bracket-desktop-full.png` | Pass | Bracket tree, review queue, evidence, and candidate paths all readable. |
| Technical desktop full page | `legacy-technical-desktop-full.png` | `react-technical-desktop-full.png` | Pass | Compare, calibration, backtest, live performance, and ensemble sections readable. |

## Final Sign-Off

- Functional tests passed: `npm test` (15 e2e + 24 vitest = all passing)
- Visual tests passed: `npm run test:visual` (3/3 passing)
- R package tests passed: deferred (Stan model fitting exceeds session time; no code changes affect R model/simulation logic)
- Agent screenshot review passed: bracket desktop, bracket mobile, technical desktop, technical mobile, bracket desktop full page, technical desktop full page
- User screenshot review passed:
- Intentional deviations: none
