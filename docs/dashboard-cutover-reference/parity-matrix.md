# Dashboard Cutover Parity Matrix

Each row must be filled with concrete React evidence before legacy deletion starts. "Automated evidence" should name the exact test. "Manual evidence" should name the screenshot or browser path reviewed.

| Legacy job | React surface | Automated evidence | Manual evidence | Status |
|------------|---------------|--------------------|-----------------|--------|
| Candidate 1 and Candidate 2 bracket entry | `output/app/index.html#build` | `e2e/frontend_app.spec.cjs`: bracket workflow parity | `react-bracket-desktop-viewport.png`, `react-bracket-mobile-viewport.png`; compared against legacy first viewports | Pending visual repair |
| Candidate comparison and route divergence | `output/app/index.html#build`, `#bracket-tree` | `e2e/frontend_app.spec.cjs`: candidate toggles and route-diff checks | React vs legacy bracket full-page screenshots | Pending visual repair |
| Ranked review queue | `output/app/index.html#review-queue` | `e2e/frontend_app.spec.cjs`: review queue and evidence jumps | React first viewport, mobile, and full-page bracket screenshots | Pending visual repair |
| Matchup evidence drilldown | `output/app/index.html#evidence` | `e2e/frontend_app.spec.cjs`: `[data-open-evidence]` opens matching details | Full-page bracket screenshot review plus E2E opened-details assertion | Pending visual repair |
| Bracket tree readability | `output/app/index.html#bracket-tree` | `e2e/frontend_app.spec.cjs`: SVG content and route-diff checks | Full-page bracket screenshot review | Pending visual repair |
| Full candidate paths/reference | `output/app/index.html#paths` | `frontend/src/bracket/BracketApp.test.tsx` | Full-page bracket screenshot review | Pending visual repair |
| Live performance monitoring | `output/app/technical.html` | `e2e/frontend_app.spec.cjs`: live performance panel visible | Technical first-viewport and full-page screenshot review | Pending visual repair |
| Calibration diagnostics | `output/app/technical.html` | `e2e/frontend_app.spec.cjs`: calibration chart visible | Technical full-page screenshot review | Pending visual repair |
| Backtest diagnostics | `output/app/technical.html` | `e2e/frontend_app.spec.cjs`: backtest panel visible | Technical full-page screenshot review | Pending visual repair |
| Ensemble diagnostics | `output/app/technical.html` | `frontend/src/technical/TechnicalApp.test.tsx` and E2E heading assertion | Technical full-page screenshot review | Pending visual repair |
| Championship totals | `output/app/technical.html` | `e2e/frontend_app.spec.cjs`: championship totals visible | Technical full-page screenshot review | Pending visual repair |
| Mobile bracket review flow | `output/app/index.html` at 390px | `e2e/frontend_app.spec.cjs`: mobile workflow tests | `react-bracket-mobile-viewport.png` compared with `legacy-bracket-mobile-viewport.png` | Pending visual repair |
| Payload load failure is visible, not blank | Bracket and technical app roots | `e2e/frontend_app.spec.cjs`: malformed payload test | Failure alert contract covered by browser test | Automated coverage present; pending final visual gate |
| Keyboard operation for primary toggles | Bracket app controls | `e2e/frontend_app.spec.cjs`: keyboard toggle test | Focus path covered by keyboard E2E | Automated coverage present; pending final visual gate |
| Real synced payload renders | checked-in `output/app/` | `e2e/frontend_app.spec.cjs`: real output app smoke test | Local `output/app/index.html` and `technical.html` render through checked-in payload bundle | Automated coverage present; pending final visual gate |
