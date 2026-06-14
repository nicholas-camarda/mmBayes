# Dashboard Cutover Reference

These screenshots capture the legacy monolith dashboards and the React app during the React-only cutover. They are the repair reference for deciding whether the React app can replace the old root-level HTML dashboards.

Required review:

- `legacy-bracket-desktop-viewport.png` vs `react-bracket-desktop-viewport.png`
- `legacy-bracket-mobile-viewport.png` vs `react-bracket-mobile-viewport.png`
- `legacy-technical-desktop-viewport.png` vs `react-technical-desktop-viewport.png`
- `legacy-technical-mobile-viewport.png` vs `react-technical-mobile-viewport.png`
- full-page screenshots for deeper reference sections
- `parity-matrix.md`
- `visual-acceptance-ledger.md`

React must be at least as usable and visually coherent for bracket entry, review queue, evidence inspection, bracket tree reading, compare boards, live monitoring, calibration, backtest, championship totals, and ensemble diagnostics before legacy deletion is accepted.

The legacy HTML files are not a runtime fallback. They are only the local comparison target until the React app passes this gate.
