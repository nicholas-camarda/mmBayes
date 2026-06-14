# Dashboard Visual Acceptance Ledger

This ledger records the manual visual gate for the React dashboard cutover.

## Gate

The React app is accepted only when all four first-viewport comparisons and both full-page comparisons are marked `Pass`.

## Comparisons

| Surface | Reference | React Candidate | Status | Notes |
|---------|-----------|-----------------|--------|-------|
| Bracket desktop first viewport | `legacy-bracket-desktop-viewport.png` | `react-bracket-desktop-viewport.png` | Pending | React must preserve immediate bracket-entry orientation, status clarity, and next-action visibility. |
| Bracket mobile first viewport | `legacy-bracket-mobile-viewport.png` | `react-bracket-mobile-viewport.png` | Pending | React must keep the primary workflow reachable without excessive card stacking. |
| Technical desktop first viewport | `legacy-technical-desktop-viewport.png` | `react-technical-desktop-viewport.png` | Pending | React must show action summary and diagnostics entry points with at least legacy-level density. |
| Technical mobile first viewport | `legacy-technical-mobile-viewport.png` | `react-technical-mobile-viewport.png` | Pending | React must avoid burying the Compare workflow under metadata and generic orientation panels. |
| Bracket desktop full page | `legacy-bracket-desktop-full.png` | `react-bracket-desktop-full.png` | Pending | React must keep bracket tree, review queue, evidence, and candidate paths readable. |
| Technical desktop full page | `legacy-technical-desktop-full.png` | `react-technical-desktop-full.png` | Pending | React must keep compare, calibration, backtest, live performance, and ensemble sections readable. |

## Final Sign-Off

- Functional tests passed:
- Visual tests passed:
- R package tests passed:
- Agent screenshot review passed:
- User screenshot review passed:
- Intentional deviations:
