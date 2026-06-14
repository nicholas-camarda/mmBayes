# React Dashboard Cutover Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make `output/app/` the only generated, published, documented dashboard surface for mmBayes, with React bracket and technical apps matching or improving the legacy monolith dashboard workflows and visual quality.

**Architecture:** R remains the source of truth for tournament data, decision sheets, model diagnostics, payload generation, and schema validation. React/Vite renders the only dashboard HTML files from the versioned `dashboard_payloads.js` bundle; root-level monolith dashboard files and legacy R HTML renderers are removed only after parity tests, visual reference captures, and human review prove the React workflows cover the same user jobs at equal or better quality.

**Tech Stack:** R/testthat, React 19, TypeScript, Vite, Vitest, Playwright, static `file://` dashboard delivery, JSON Schema v1.1.0 payload contracts

---

## File map

| File | Responsibility in this plan |
|------|-----------------------------|
| `frontend/src/bracket/BracketApp.tsx` | Bracket-entry workflow shell: candidate entry, review queue, evidence, tree, reference sections |
| `frontend/src/technical/TechnicalApp.tsx` | Diagnostic workflow shell: compare boards, live monitoring, backtest, calibration, ensemble diagnostics |
| `frontend/src/components/bracket/*.tsx` | Bracket panels that must replace legacy monolith user jobs |
| `frontend/src/components/technical/*.tsx` | Technical panels that must replace legacy monolith user jobs |
| `frontend/src/styles/**/*.css` | Visual quality, spacing, responsive layout, and print/screenshot stability |
| `frontend/src/**/*.test.tsx` | Component tests for React parity and missing-section behavior |
| `e2e/frontend_app.spec.cjs` | Offline interaction parity for bracket and technical dashboards |
| `e2e/frontend_visual.spec.cjs` | Visual regression gates for bracket and technical dashboards |
| `R/utils.R` | Dashboard payload writing, React app sync, saved-result regeneration, return object contracts |
| `R/publishing.R` | Release deliverable manifest, with `app/` as the only dashboard deliverable |
| `scripts/publish_github_pages.R` | Copy-only helper for the React app bundle, not root monolith HTML |
| `scripts/regenerate_and_sync_dashboards.R` | Cached-result dashboard regeneration and React app sync |
| `scripts/run_simulation.R` | Full pipeline dashboard generation and React app sync |
| `tests/testthat/test_dashboard_payload_schema.R` | Payload and app sync contract tests |
| `tests/testthat/test_pipeline_smoke.R` | Pipeline output contract: no root monolith dashboards |
| `tests/testthat/test_project_layout.R` | Release manifest and saved-result regeneration contract tests |
| `tests/testthat/test_documentation_links.R` | README, docs, and GitHub Pages hub contract tests |
| `tests/testthat/test_model_and_simulation.R` | Remove legacy HTML substring assertions; keep model/simulation assertions |
| `tests/testthat/test_bracket_tree_interactions.R` | Remove or replace root-monolith interaction expectations with React app coverage |
| `e2e/bracket_tree.spec.cjs` | Delete after equivalent React tree/evidence E2E coverage exists |
| `e2e/generate_fixture_dashboard.R` | Delete after root monolith fixture HTML is no longer tested |
| `README.md` | Current-state dashboard docs: `output/app/` only |
| `docs/dashboard-frontend-architecture.md` | Current architecture after cutover, with no fallback language |
| `docs/dashboard-cutover-reference/parity-matrix.md` | Explicit legacy-to-React workflow parity checklist and evidence |
| `docs/methods-and-interpretation.md` | Operational entrypoint descriptions after cutover |
| `index.html` | GitHub Pages hub links only to React app dashboard surfaces |
| `output/app/**` | Tracked publishable React dashboard snapshot |
| `output/bracket_dashboard.html` | Delete tracked legacy monolith snapshot |
| `output/technical_dashboard.html` | Delete tracked legacy monolith snapshot |

## Legacy protection rule

Do not delete legacy monolith files, tests, or renderer code until Task 4's quality acceptance gate is complete. Before that gate, the legacy HTML is the reference product, not dead code. The final state still has no duplicate dashboard surface, but deletion happens only after evidence shows the React app is good enough to move forward without keeping the old HTML as a safety net. Immediately before deletion begins, create a local recovery marker so the pre-cutover state can be inspected later without keeping duplicate public dashboard paths.

## Current-state audit before edits

The checkout currently has dashboard-related modifications in progress. Before executing this plan, inspect and preserve user work:

```sh
git status --short
git diff -- R/utils.R R/plotting_functions.R frontend/src e2e tests/testthat README.md docs index.html
```

Expected: the executor understands which hunks are pre-existing and does not revert them. If a required file has unrelated user changes, edit around them and call that out in the implementation report.

## Open decisions

The plan assumes these defaults unless the user says otherwise:

| Decision | Recommended default | Why |
|----------|---------------------|-----|
| Public URLs for retired monoliths | Remove links and files; do not create redirect HTML | The stated goal says no fallback or duplicate dashboard paths |
| `frontend/dist` missing during R pipeline | Fail clearly when dashboard publication is requested | A soft skip preserves the old dual-surface end state |
| Legacy R renderer code | Delete live root monolith renderers only after React parity tests, reference screenshots, and user review pass | Avoid removing the comparison target before it has been used to judge the replacement |
| Visual bar | React must pass current visual baselines, side-by-side legacy comparison, and manual desktop/mobile inspection | The goal is legacy quality or better, not exact recreation |

---

### Task 1: Capture the legacy reference and parity matrix before changing cutover contracts

**Files:**
- Create: `docs/dashboard-cutover-reference/README.md`
- Create: `docs/dashboard-cutover-reference/parity-matrix.md`
- Create: `docs/dashboard-cutover-reference/*.png`
- Read: `output/bracket_dashboard.html`
- Read: `output/technical_dashboard.html`
- Read: `output/app/index.html`
- Read: `output/app/technical.html`

- [ ] **Step 1: Verify legacy reference files still exist**

```sh
test -e output/bracket_dashboard.html
test -e output/technical_dashboard.html
test -e output/app/index.html
test -e output/app/technical.html
```

Expected: all commands exit 0. If a legacy file is already missing, stop and restore it from git before continuing so there is a real comparison target.

- [ ] **Step 2: Capture desktop and mobile screenshots**

Use Playwright to capture both legacy and React surfaces at the same viewport sizes. Capture both full-page and first-viewport screenshots; the first viewport is the fastest way to catch weak hierarchy, bad density, or action areas pushed below the fold.

```sh
node - <<'NODE'
const { chromium } = require("@playwright/test");
const fs = require("node:fs");
const path = require("node:path");

(async () => {
  const out = path.resolve("docs/dashboard-cutover-reference");
  fs.mkdirSync(out, { recursive: true });
  const browser = await chromium.launch();
  const pages = [
    ["legacy-bracket", "output/bracket_dashboard.html"],
    ["legacy-technical", "output/technical_dashboard.html"],
    ["react-bracket", "output/app/index.html"],
    ["react-technical", "output/app/technical.html"],
  ];
  const sizes = [
    ["desktop", { width: 1320, height: 960 }],
    ["mobile", { width: 390, height: 900 }],
  ];
  for (const [pageName, file] of pages) {
    for (const [sizeName, viewport] of sizes) {
      const page = await browser.newPage({ viewport });
      await page.goto(`file://${path.resolve(file)}`, { waitUntil: "domcontentloaded" });
      await page.screenshot({
        path: path.join(out, `${pageName}-${sizeName}-full.png`),
        fullPage: true,
      });
      await page.screenshot({
        path: path.join(out, `${pageName}-${sizeName}-viewport.png`),
        fullPage: false,
      });
      await page.close();
    }
  }
  await browser.close();
})();
NODE
```

Expected: sixteen PNG files are written under `docs/dashboard-cutover-reference/`.

- [ ] **Step 3: Write the parity matrix**

Create `docs/dashboard-cutover-reference/parity-matrix.md`:

```md
# Dashboard Cutover Parity Matrix

Each row must be filled with concrete React evidence before legacy deletion starts. "Automated evidence" should name the exact test. "Manual evidence" should name the screenshot or browser path reviewed.

| Legacy job | React surface | Automated evidence | Manual evidence | Status |
|------------|---------------|--------------------|-----------------|--------|
| Candidate 1 and Candidate 2 bracket entry | `output/app/index.html#build` | `e2e/frontend_app.spec.cjs`: bracket workflow parity | `react-bracket-desktop-viewport.png`, `react-bracket-mobile-viewport.png` | Pending |
| Candidate comparison and route divergence | `output/app/index.html#build`, `#bracket-tree` | `e2e/frontend_app.spec.cjs`: candidate toggles and route-diff checks | React vs legacy bracket screenshots | Pending |
| Ranked review queue | `output/app/index.html#review-queue` | `e2e/frontend_app.spec.cjs`: review queue and evidence jumps | React first viewport and mobile screenshots | Pending |
| Matchup evidence drilldown | `output/app/index.html#evidence` | `e2e/frontend_app.spec.cjs`: `[data-open-evidence]` opens matching details | Evidence panel screenshot review | Pending |
| Bracket tree readability | `output/app/index.html#bracket-tree` | `e2e/frontend_app.spec.cjs`: SVG content and route-diff checks | Bracket tree screenshot review | Pending |
| Full candidate paths/reference | `output/app/index.html#paths` | `frontend/src/bracket/BracketApp.test.tsx` | Full-page bracket screenshot review | Pending |
| Live performance monitoring | `output/app/technical.html` | `e2e/frontend_app.spec.cjs`: live performance panel visible | Technical screenshot review | Pending |
| Calibration diagnostics | `output/app/technical.html` | `e2e/frontend_app.spec.cjs`: calibration chart visible | Technical screenshot review | Pending |
| Backtest diagnostics | `output/app/technical.html` | `e2e/frontend_app.spec.cjs`: backtest panel visible | Technical screenshot review | Pending |
| Ensemble diagnostics | `output/app/technical.html` | `frontend/src/technical/TechnicalApp.test.tsx` and E2E heading assertion | Technical screenshot review | Pending |
| Championship totals | `output/app/technical.html` | `e2e/frontend_app.spec.cjs`: championship totals visible | Technical screenshot review | Pending |
| Mobile bracket review flow | `output/app/index.html` at 390px | `e2e/frontend_app.spec.cjs`: mobile workflow tests | `react-bracket-mobile-viewport.png` | Pending |
| Payload load failure is visible, not blank | Bracket and technical app roots | `e2e/frontend_app.spec.cjs`: malformed payload test | Browser alert text if payload fails | Pending |
| Keyboard operation for primary toggles | Bracket app controls | `e2e/frontend_app.spec.cjs`: keyboard toggle test | Manual tab/focus spot check | Pending |
| Real synced payload renders | checked-in `output/app/` | `e2e/frontend_app.spec.cjs`: real output app smoke test | Local `output/app/index.html` and `technical.html` | Pending |
```

- [ ] **Step 4: Write the reference README**

Create `docs/dashboard-cutover-reference/README.md`:

```md
# Dashboard Cutover Reference

These screenshots capture the legacy monolith dashboards and the React app before the React-only cutover. They are the quality reference for deciding whether the React app can replace the old root-level HTML dashboards.

Required review:

- `legacy-bracket-desktop-viewport.png` vs `react-bracket-desktop-viewport.png`
- `legacy-bracket-mobile-viewport.png` vs `react-bracket-mobile-viewport.png`
- `legacy-technical-desktop-viewport.png` vs `react-technical-desktop-viewport.png`
- `legacy-technical-mobile-viewport.png` vs `react-technical-mobile-viewport.png`
- full-page screenshots for deeper reference sections
- `parity-matrix.md`

React must be at least as usable and visually coherent for bracket entry, review queue, evidence inspection, bracket tree reading, compare boards, live monitoring, calibration, backtest, championship totals, and ensemble diagnostics before legacy deletion starts.
```

- [ ] **Step 5: Commit the reference capture**

```sh
git add docs/dashboard-cutover-reference
git commit -m "docs: capture dashboard cutover reference screenshots"
```

### Task 2: Add explicit React parity assertions

**Files:**
- Modify: `e2e/frontend_app.spec.cjs`
- Modify: `frontend/src/bracket/BracketApp.test.tsx`
- Modify: `frontend/src/technical/TechnicalApp.test.tsx`

- [ ] **Step 1: Write failing bracket workflow E2E assertions**

Add this test to `e2e/frontend_app.spec.cjs` inside `test.describe("static frontend dashboards", () => { ... })`:

```js
test("bracket React app covers the legacy entry and evidence workflow jobs", async ({ page }) => {
  const errors = collectBrowserErrors(page);
  await page.setViewportSize({ width: 1320, height: 960 });
  const stageDir = stageApp();
  await page.goto(`file://${path.join(stageDir, "index.html")}`, { waitUntil: "domcontentloaded" });

  await expect(page.getByRole("heading", { name: /bracket entry workspace/i })).toBeVisible();
  await expect(page.locator("#build .entry-workspace")).toBeVisible();
  await expect(page.locator("#review-queue")).toBeVisible();
  await expect(page.locator("#bracket-tree svg.btree-svg").first()).toBeVisible();
  await expect(page.locator("#evidence details").first()).toBeVisible();
  await expect(page.locator("#paths")).toBeVisible();
  await expect(page.locator("#technical-appendix")).toBeVisible();

  await expect(page.getByText(/Candidate 2 changes from the baseline/i)).toBeVisible();
  await expect(page.getByText(/Favorite probability/i)).toBeVisible();
  await expect(page.getByText(/Overall strength rating/i)).toBeVisible();
  await expect(page.getByText(/Review priority score/i)).toBeVisible();

  const evidenceJump = page.locator("[data-open-evidence]").first();
  const evidenceId = await evidenceJump.getAttribute("data-open-evidence");
  expect(evidenceId).toBeTruthy();
  await evidenceJump.click();
  await expect(page.locator(`details[id="${evidenceId}"]`)).toBeVisible();

  expect(errors).toEqual([]);
});
```

- [ ] **Step 2: Write failing technical workflow E2E assertions**

Add this test to the same file:

```js
test("technical React app covers compare, calibration, backtest, and ensemble diagnostics", async ({ page }) => {
  const errors = collectBrowserErrors(page);
  await page.setViewportSize({ width: 1320, height: 960 });
  const stageDir = stageApp();
  await page.goto(`file://${path.join(stageDir, "technical.html")}`, { waitUntil: "domcontentloaded" });

  await expect(page.getByRole("heading", { name: /technical bracket dashboard/i })).toBeVisible();
  await expect(page.getByTestId("compare-workspace")).toBeVisible();
  await expect(page.getByTestId("ranked-decisions-board")).toBeVisible();
  await expect(page.getByTestId("candidate-differences-table")).toBeVisible();
  await expect(page.getByTestId("live-performance-panel")).toBeVisible();
  await expect(page.getByTestId("backtest-panel")).toBeVisible();
  await expect(page.getByTestId("calibration-chart")).toBeVisible();
  await expect(page.getByText(/Ensemble diagnostics/i)).toBeVisible();
  await expect(page.getByText(/Championship totals/i)).toBeVisible();

  expect(errors).toEqual([]);
});
```

- [ ] **Step 3: Write a real synced payload smoke test**

Add this test to `e2e/frontend_app.spec.cjs`. It must load the checked-in `output/app/` bundle and its real `dashboard_payloads.js`, not the fixture staging directory:

```js
test("synced output app renders the real checked-in payload bundle", async ({ page }) => {
  const outputAppDir = path.join(repoRoot, "output", "app");
  for (const requiredFile of ["index.html", "technical.html", "dashboard_payloads.js"]) {
    expect(fs.existsSync(path.join(outputAppDir, requiredFile))).toBeTruthy();
  }

  const errors = collectBrowserErrors(page);
  await page.goto(`file://${path.join(outputAppDir, "index.html")}`, { waitUntil: "domcontentloaded" });
  await expect(page.getByRole("heading", { name: /bracket entry workspace/i })).toBeVisible();
  await expect(page.locator("#build .entry-workspace")).toBeVisible();
  await expect(page.locator("#review-queue")).toBeVisible();

  await page.goto(`file://${path.join(outputAppDir, "technical.html")}`, { waitUntil: "domcontentloaded" });
  await expect(page.getByRole("heading", { name: /technical bracket dashboard/i })).toBeVisible();
  await expect(page.getByTestId("compare-workspace")).toBeVisible();

  expect(errors).toEqual([]);
});
```

- [ ] **Step 4: Write malformed payload tests so the app never fails blank**

Add this test to `e2e/frontend_app.spec.cjs`:

```js
test("payload load failures render visible errors instead of a blank app", async ({ page }) => {
  const stageDir = fs.mkdtempSync(path.join(os.tmpdir(), "mmbayes-bad-payload-"));
  fs.cpSync(distDir, stageDir, { recursive: true });
  fs.writeFileSync(
    path.join(stageDir, "dashboard_payloads.js"),
    'window.__MMBAYES_PAYLOADS__ = {"bracket":{"dashboard_schema_version":"9.0.0"},"technical":{"dashboard_schema_version":"9.0.0"}};',
  );

  await page.goto(`file://${path.join(stageDir, "index.html")}`, { waitUntil: "domcontentloaded" });
  await expect(page.getByRole("alert")).toContainText(/Failed to load dashboard payload/i);
  await expect(page.getByRole("heading", { name: /bracket dashboard/i })).toBeVisible();

  await page.goto(`file://${path.join(stageDir, "technical.html")}`, { waitUntil: "domcontentloaded" });
  await expect(page.getByRole("alert")).toContainText(/Failed to load dashboard payload/i);
  await expect(page.getByRole("heading", { name: /technical dashboard/i })).toBeVisible();
});
```

- [ ] **Step 5: Write keyboard and focus sanity checks**

Add this test to `e2e/frontend_app.spec.cjs`:

```js
test("primary bracket controls are keyboard-operable and keep visible focus", async ({ page }) => {
  const stageDir = stageApp();
  await page.setViewportSize({ width: 1320, height: 960 });
  await page.goto(`file://${path.join(stageDir, "index.html")}`, { waitUntil: "domcontentloaded" });

  const candidateTwoToggle = page.locator(".entry-toggle[data-entry-target='candidate-2']");
  await candidateTwoToggle.focus();
  await expect(candidateTwoToggle).toBeFocused();
  await page.keyboard.press("Enter");
  await expect(page.locator("[data-entry-panel='candidate-2']")).toBeVisible();

  const firstEvidenceJump = page.locator("[data-open-evidence]").first();
  await firstEvidenceJump.focus();
  await expect(firstEvidenceJump).toBeFocused();
  await page.keyboard.press("Enter");
  const evidenceId = await firstEvidenceJump.getAttribute("data-open-evidence");
  expect(evidenceId).toBeTruthy();
  await expect(page.locator(`details[id="${evidenceId}"]`)).toBeVisible();
});
```

- [ ] **Step 6: Strengthen component tests for missing optional sections**

Add assertions to `frontend/src/bracket/BracketApp.test.tsx`:

```tsx
expect(screen.getByRole("heading", { name: /bracket entry workspace/i })).toBeInTheDocument();
expect(screen.getByText(/Candidate Recommendations/i)).toBeInTheDocument();
expect(screen.getByText(/Review Queue/i)).toBeInTheDocument();
expect(screen.getByText(/Matchup Evidence/i)).toBeInTheDocument();
expect(screen.getByText(/Full candidate paths/i)).toBeInTheDocument();
```

Add assertions to `frontend/src/technical/TechnicalApp.test.tsx`:

```tsx
expect(screen.getByRole("heading", { name: /technical bracket dashboard/i })).toBeInTheDocument();
expect(screen.getByTestId("compare-workspace")).toBeInTheDocument();
expect(screen.getByTestId("backtest-panel")).toBeInTheDocument();
expect(screen.getByTestId("calibration-chart")).toBeInTheDocument();
expect(screen.getByText(/Ensemble diagnostics/i)).toBeInTheDocument();
```

- [ ] **Step 7: Run tests to verify failures or coverage gaps**

```sh
cd frontend && npm test -- src/bracket/BracketApp.test.tsx src/technical/TechnicalApp.test.tsx
cd .. && npm test -- e2e/frontend_app.spec.cjs
```

Expected: tests either fail on missing React parity behavior or pass and document existing parity. If they pass immediately, record that these user jobs were already covered before code changes.

- [ ] **Step 8: Commit parity tests**

```sh
git add e2e/frontend_app.spec.cjs frontend/src/bracket/BracketApp.test.tsx frontend/src/technical/TechnicalApp.test.tsx
git commit -m "test: assert React dashboard parity workflows"
```

### Task 3: Fix React parity and visual quality gaps exposed by Task 2

**Files:**
- Modify: `frontend/src/bracket/BracketApp.tsx`
- Modify: `frontend/src/technical/TechnicalApp.tsx`
- Modify: `frontend/src/components/bracket/*.tsx`
- Modify: `frontend/src/components/technical/*.tsx`
- Modify: `frontend/src/styles/**/*.css`
- Test: `frontend/src/**/*.test.tsx`
- Test: `e2e/frontend_app.spec.cjs`
- Test: `e2e/frontend_visual.spec.cjs`

- [ ] **Step 1: Run the focused failing test again**

```sh
cd frontend && npm test -- src/bracket/BracketApp.test.tsx src/technical/TechnicalApp.test.tsx
cd .. && npm test -- e2e/frontend_app.spec.cjs
```

Expected: the output identifies the first missing or degraded React parity behavior.

- [ ] **Step 2: Implement only the missing behavior**

Use existing components and payload fields first. Do not add new payload fields unless a required legacy user job cannot be represented from the current v1.1.0 payload. When editing CSS, keep cards at 8px radius or less unless the existing component already uses a smaller local token.

For example, if the technical app lacks a stable selector around ensemble diagnostics, wrap the existing diagnostics block in `frontend/src/components/technical/EnsembleDiagnosticsPanel.tsx` with a visible heading and test id:

```tsx
<section className="technical-panel" data-testid="ensemble-diagnostics-panel">
  <h2>Ensemble diagnostics</h2>
  {/* existing diagnostics content */}
</section>
```

For example, if bracket evidence jumps are hard to use on mobile, adjust the existing jump button styles in `frontend/src/styles/components/evidence.css`:

```css
.jump-button {
  min-height: 36px;
  padding: 7px 10px;
  border-radius: 6px;
}

@media (max-width: 640px) {
  .jump-button {
    width: 100%;
    justify-content: center;
  }
}
```

- [ ] **Step 3: Verify focused tests pass**

```sh
cd frontend && npm test -- src/bracket/BracketApp.test.tsx src/technical/TechnicalApp.test.tsx
cd .. && npm test -- e2e/frontend_app.spec.cjs
```

Expected: focused component and interaction tests pass with no browser console errors.

- [ ] **Step 4: Run visual regression after UI changes**

```sh
npm run test:visual
```

Expected: screenshots pass or show intentional visual improvements. If the screenshots fail because the UI intentionally improved, inspect the Playwright diff artifacts and update snapshots with:

```sh
npm test -- e2e/frontend_visual.spec.cjs --update-snapshots
```

- [ ] **Step 5: Regenerate fixtures if payload shape changed**

Only run this if Task 3 required changing payload builders, schemas, or TypeScript payload types:

```sh
Rscript scripts/generate_dashboard_payload_fixtures.R
cd frontend && npm test -- src/lib/loadPayloads.test.ts src/bracket/BracketApp.test.tsx src/technical/TechnicalApp.test.tsx && cd ..
```

Expected: regenerated fixture JSON validates against `inst/schemas/` and React tests still pass. Commit fixture changes with the payload/schema change, not separately.

- [ ] **Step 6: Commit React parity fixes**

```sh
git add frontend/src e2e/frontend_app.spec.cjs e2e/frontend_visual.spec.cjs
git commit -m "feat: close React dashboard parity gaps"
```

### Task 4: Quality acceptance gate before any legacy deletion

**Files:**
- Read: `docs/dashboard-cutover-reference/*.png`
- Modify: `docs/dashboard-cutover-reference/parity-matrix.md`
- Read: `output/app/index.html`
- Read: `output/app/technical.html`
- Test: `frontend/src/**/*.test.tsx`
- Test: `e2e/frontend_app.spec.cjs`
- Test: `e2e/frontend_visual.spec.cjs`

- [ ] **Step 1: Run all React quality gates**

```sh
cd frontend && npm run build && npm test && cd ..
npm test -- e2e/frontend_app.spec.cjs
npm run test:visual
```

Expected: all pass. Do not proceed to deletion with skipped or failing parity/visual tests.

- [ ] **Step 2: Open the side-by-side reference screenshots**

```sh
open docs/dashboard-cutover-reference
open output/app/index.html
open output/app/technical.html
```

Review against the captured legacy screenshots. The React app must be clearly acceptable for:

- visual hierarchy and density
- bracket entry speed
- candidate comparison
- evidence drilldown
- bracket tree readability
- mobile review flow
- technical compare boards
- calibration/backtest diagnostics
- ensemble diagnostics
- championship totals

- [ ] **Step 3: Record the acceptance result**

Fill every `Status` cell in `docs/dashboard-cutover-reference/parity-matrix.md` with `Accepted` or a concrete rejection such as `Rejected: bracket tree labels overlap at 390px`. Do not proceed if any row is rejected.

Append this section to `docs/dashboard-cutover-reference/README.md` only when the review passes:

```md
## Acceptance Result

The React dashboard app passed the cutover review. The legacy monolith dashboards were retained as reference through the parity and visual review, and deletion can now proceed because `output/app/` is acceptable as the sole dashboard surface.
```

If review fails, append this instead and stop:

```md
## Rejection Result

The React dashboard app did not pass the cutover review. Legacy monolith deletion is blocked.

Defects to fix before cutover:

- Bracket tree labels overlap at the 390px mobile viewport.
- Review queue evidence jumps are not keyboard-operable.
- Technical calibration chart is less readable than the legacy desktop reference.
```

Do not delete legacy files after a rejection result. The bullets above show the required specificity; replace them with the actual observed defects before committing a rejection note, then add those defects to Task 3 and fix them first.

- [ ] **Step 4: Commit the acceptance note**

```sh
git add docs/dashboard-cutover-reference/README.md
git commit -m "docs: record React dashboard cutover acceptance"
```

- [ ] **Step 5: User review checkpoint**

Stop and show the user:

- the local React dashboard paths
- the reference screenshot directory
- the test results
- the known remaining visual or workflow risks

Do not start Task 5 unless the user agrees that the React app is good enough to replace the legacy HTML. This is the point where the project decides it is moving completely forward and can forget about the monolith dashboards after deletion.

- [ ] **Step 6: Create a pre-deletion recovery marker**

After user acceptance and immediately before Task 5, create a local marker that points to the last commit with the legacy monolith dashboards still intact:

```sh
git tag pre-react-dashboard-cutover-$(date +%Y%m%d-%H%M%S)
```

Expected: `git tag --list 'pre-react-dashboard-cutover-*'` shows the new marker. This marker is for inspection/recovery only; it is not a public fallback dashboard path.

### Task 5: Make the R dashboard contract React-only

**Files:**
- Modify: `R/utils.R`
- Modify: `tests/testthat/test_dashboard_payload_schema.R`
- Modify: `tests/testthat/test_pipeline_smoke.R`
- Modify: `tests/testthat/test_project_layout.R`

- [ ] **Step 1: Write failing tests for React-only dashboard generation**

In `tests/testthat/test_dashboard_payload_schema.R`, replace the test name and expectations for `write_dashboard_outputs` with:

```r
test_that("write_dashboard_outputs writes validated React app payload artifacts only", {
    output_dir <- file.path(tempdir(), paste0("payload_test_", as.integer(Sys.time())))
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)
    candidates <- make_payload_test_candidates()
    result <- write_dashboard_outputs(
        bracket_year = 2026L,
        candidates = candidates,
        output_dir = output_dir,
        backtest = list(
            summary = tibble::tibble(
                mean_log_loss = 0.401,
                mean_brier = 0.188,
                mean_accuracy = 0.713,
                mean_bracket_score = 85.4,
                mean_correct_picks = 42.7
            )
        )
    )

    expect_false(file.exists(file.path(output_dir, "bracket_dashboard.html")))
    expect_false(file.exists(file.path(output_dir, "technical_dashboard.html")))
    expect_true(file.exists(file.path(output_dir, "bracket_dashboard_payload.json")))
    expect_true(file.exists(file.path(output_dir, "technical_dashboard_payload.json")))
    expect_true(file.exists(file.path(output_dir, "dashboard_payloads.js")))
    expect_null(result$dashboard)
    expect_null(result$technical_dashboard)
    expect_equal(result$bracket_payload, file.path(output_dir, "bracket_dashboard_payload.json"))
})
```

In `tests/testthat/test_pipeline_smoke.R`, replace monolith expectations with:

```r
expect_false(file.exists(file.path(output_dir, "bracket_dashboard.html")))
expect_false(file.exists(file.path(output_dir, "technical_dashboard.html")))
expect_true(file.exists(file.path(output_dir, "bracket_dashboard_payload.json")))
expect_true(file.exists(file.path(output_dir, "technical_dashboard_payload.json")))
expect_true(file.exists(file.path(output_dir, "dashboard_payloads.js")))
expect_null(results$output$dashboard)
expect_null(results$output$technical_dashboard)
```

In the regeneration section of the same test, do not unlink root dashboard HTML. Assert app/payload sync instead:

```r
regenerated <- regenerate_dashboard_outputs_from_results(
    results = results,
    output_dir = output_dir,
    repo_output_dir = repo_output_dir,
    dashboard_build_metadata = dashboard_build_metadata
)

expect_null(regenerated$dashboard)
expect_null(regenerated$technical_dashboard)
expect_null(regenerated$model_comparison_dashboard)
expect_true(file.exists(regenerated$bracket_payload))
expect_true(file.exists(regenerated$technical_payload))
expect_true(file.exists(regenerated$payload_js))
expect_true(dir.exists(file.path(output_dir, "app")) || is.null(regenerated$app_dirs))
```

- [ ] **Step 2: Run tests to verify they fail on current monolith contract**

```sh
Rscript -e 'pkgload::load_all(".", export_all = TRUE, helpers = TRUE, quiet = TRUE); testthat::test_file("tests/testthat/test_dashboard_payload_schema.R")'
Rscript -e 'pkgload::load_all(".", export_all = TRUE, helpers = TRUE, quiet = TRUE); testthat::test_file("tests/testthat/test_pipeline_smoke.R")'
```

Expected before implementation: failures show `bracket_dashboard.html` and `technical_dashboard.html` still exist or returned paths are still non-null.

- [ ] **Step 3: Remove root monolith writes from `write_dashboard_outputs()`**

In `R/utils.R`, inside `write_dashboard_outputs()`, delete the `dashboard_path`, `technical_dashboard_path`, and both `writeLines(create_*_dashboard_html(...))` blocks. Keep model-comparison cleanup and payload writing. Return `dashboard = NULL` and `technical_dashboard = NULL`:

```r
    retired_dashboard_paths <- file.path(output_dir, c(
        "bracket_dashboard.html",
        "technical_dashboard.html",
        "model_comparison_dashboard.html"
    ))
    existing_retired_paths <- retired_dashboard_paths[file.exists(retired_dashboard_paths)]
    if (length(existing_retired_paths) > 0L) {
        unlink(existing_retired_paths)
    }
```

Use this return contract:

```r
    list(
        dashboard = NULL,
        technical_dashboard = NULL,
        model_comparison_dashboard = NULL,
        bracket_payload = payload_paths$bracket,
        technical_payload = payload_paths$technical,
        payload_js = payload_paths$js,
        model_quality_source_label = model_quality_context$source_label %||% NULL,
        model_quality_source_path = model_quality_context$source_path %||% NULL,
        model_quality_used_cached_quality = isTRUE(model_quality_context$used_cached_quality %||% model_quality_context$used_fallback %||% FALSE),
        model_quality_used_fallback = isTRUE(model_quality_context$used_cached_quality %||% model_quality_context$used_fallback %||% FALSE)
    )
```

- [ ] **Step 4: Make `dashboard_html_manifest()` React-only or remove it**

If `dashboard_html_manifest()` is still called by scripts/tests, change it to return the app HTML entry names relative to `app/`:

```r
dashboard_html_manifest <- function() {
    c(
        file.path("app", "index.html"),
        file.path("app", "technical.html")
    )
}
```

If all callers are simpler without it, delete the function and update tests to assert `app/index.html` and `app/technical.html` directly.

- [ ] **Step 5: Make `sync_frontend_app()` fail clearly when app publication is required**

Change the missing-build branch in `sync_frontend_app()` from soft skip to fail:

```r
    if (!dir.exists(dist_dir)) {
        stop_with_message(
            paste(
                "Frontend app build not found at frontend/dist.",
                "Run `cd frontend && npm install && npm run build` before dashboard publication."
            )
        )
    }
```

Update `tests/testthat/test_dashboard_payload_schema.R`:

```r
test_that("sync_frontend_app fails clearly when dist is missing", {
    project_root <- file.path(tempdir(), paste0("no_dist_", as.integer(Sys.time())))
    runtime_dir <- file.path(project_root, "runtime_output")
    dir.create(runtime_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(project_root, recursive = TRUE), add = TRUE)
    expect_error(
        sync_frontend_app(project_root, runtime_output_dir = runtime_dir),
        "Frontend app build not found"
    )
})
```

- [ ] **Step 6: Run focused R tests**

```sh
Rscript -e 'pkgload::load_all(".", export_all = TRUE, helpers = TRUE, quiet = TRUE); testthat::test_file("tests/testthat/test_dashboard_payload_schema.R")'
Rscript -e 'pkgload::load_all(".", export_all = TRUE, helpers = TRUE, quiet = TRUE); testthat::test_file("tests/testthat/test_pipeline_smoke.R")'
Rscript -e 'pkgload::load_all(".", export_all = TRUE, helpers = TRUE, quiet = TRUE); testthat::test_file("tests/testthat/test_project_layout.R")'
```

Expected: focused R tests pass and no test expects root-level dashboard HTML.

- [ ] **Step 7: Commit R dashboard contract change**

```sh
git add R/utils.R tests/testthat/test_dashboard_payload_schema.R tests/testthat/test_pipeline_smoke.R tests/testthat/test_project_layout.R
git commit -m "refactor: make React app the only generated dashboard"
```

### Task 6: Make publishing and GitHub Pages sync React-only

**Files:**
- Modify: `R/publishing.R`
- Modify: `scripts/publish_github_pages.R`
- Modify: `tests/testthat/test_project_layout.R`
- Modify: `tests/testthat/test_documentation_links.R`

- [ ] **Step 1: Write failing release manifest expectations**

In `tests/testthat/test_project_layout.R`, assert the release manifest contains `app` and does not contain root dashboard HTML:

```r
test_that("release deliverable manifest publishes only the React dashboard app", {
    manifest <- release_deliverable_manifest()
    expect_true("app" %in% manifest)
    expect_false("bracket_dashboard.html" %in% manifest)
    expect_false("technical_dashboard.html" %in% manifest)
    expect_false("model_comparison_dashboard.html" %in% manifest)
})
```

- [ ] **Step 2: Write failing release publish integration expectations**

In `tests/testthat/test_project_layout.R`, add a test that proves `publish_release_bundle()` copies the app directory recursively and fails if app files are missing:

```r
test_that("publish_release_bundle copies the React app directory as the dashboard deliverable", {
    runtime_root <- tempfile(pattern = "mmBayes-runtime-")
    output_dir <- file.path(runtime_root, "output")
    app_dir <- file.path(output_dir, "app")
    dir.create(app_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(runtime_root, recursive = TRUE), add = TRUE)

    config <- default_project_config()
    config$runtime$root <- runtime_root
    config$output$path <- output_dir
    config <- normalize_project_paths(config)

    for (filename in setdiff(release_deliverable_manifest(), "app")) {
        writeLines(sprintf("fixture:%s", filename), file.path(output_dir, filename))
    }
    writeLines("candidate", file.path(output_dir, "bracket_candidate_1.csv"))
    writeLines("<html>bracket app</html>", file.path(app_dir, "index.html"))
    writeLines("<html>technical app</html>", file.path(app_dir, "technical.html"))
    writeLines("window.__MMBAYES_PAYLOADS__ = {};", file.path(app_dir, "dashboard_payloads.js"))

    publish_root <- tempfile(pattern = "mmBayes-publish-")
    result <- publish_release_bundle(
        config = config,
        release_date = as.Date("2026-03-30"),
        publish_root = publish_root
    )

    copied_app <- file.path(result$deliverables_dir, "app")
    expect_true(dir.exists(copied_app))
    expect_true(file.exists(file.path(copied_app, "index.html")))
    expect_true(file.exists(file.path(copied_app, "technical.html")))
    expect_true(file.exists(file.path(copied_app, "dashboard_payloads.js")))
    expect_false(file.exists(file.path(result$deliverables_dir, "bracket_dashboard.html")))
    expect_false(file.exists(file.path(result$deliverables_dir, "technical_dashboard.html")))
})

test_that("publish_release_bundle fails clearly when the React app deliverable is incomplete", {
    runtime_root <- tempfile(pattern = "mmBayes-runtime-")
    output_dir <- file.path(runtime_root, "output")
    app_dir <- file.path(output_dir, "app")
    dir.create(app_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(runtime_root, recursive = TRUE), add = TRUE)

    config <- default_project_config()
    config$runtime$root <- runtime_root
    config$output$path <- output_dir
    config <- normalize_project_paths(config)

    for (filename in setdiff(release_deliverable_manifest(), "app")) {
        writeLines(sprintf("fixture:%s", filename), file.path(output_dir, filename))
    }
    writeLines("candidate", file.path(output_dir, "bracket_candidate_1.csv"))
    writeLines("<html>bracket app</html>", file.path(app_dir, "index.html"))

    expect_error(
        publish_release_bundle(
            config = config,
            release_date = as.Date("2026-03-31"),
            publish_root = tempfile(pattern = "mmBayes-publish-")
        ),
        regexp = "Missing release artifact|dashboard_payloads|technical"
    )
})
```

If this test fails because `copy_release_artifact()` only checks the directory and not required files inside `app/`, add an `assert_dashboard_app_deliverable()` helper in `R/publishing.R` and call it before copying `app`.

- [ ] **Step 3: Write failing publish helper expectations**

In `tests/testthat/test_documentation_links.R`, update the publish helper test:

```r
test_that("publish_github_pages script syncs only the React app bundle", {
    repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
    script_text <- paste(readLines(file.path(repo_root, "scripts", "publish_github_pages.R"), warn = FALSE), collapse = "\n")

    expect_match(script_text, "Internal helper")
    expect_match(script_text, "output/app")
    expect_no_match(script_text, "dashboard_html_manifest\\(\\)")
    expect_no_match(script_text, "bracket_dashboard\\.html")
    expect_no_match(script_text, "technical_dashboard\\.html")
    expect_no_match(script_text, "which.max\\(bundle_mtime\\)")
    expect_no_match(script_text, "candidate_roots")
})
```

- [ ] **Step 4: Run tests to verify failure**

```sh
Rscript -e 'pkgload::load_all(".", export_all = TRUE, helpers = TRUE, quiet = TRUE); testthat::test_file("tests/testthat/test_project_layout.R")'
Rscript -e 'pkgload::load_all(".", export_all = TRUE, helpers = TRUE, quiet = TRUE); testthat::test_file("tests/testthat/test_documentation_links.R")'
```

Expected before implementation: failures identify legacy root HTML still in release or sync contracts.

- [ ] **Step 5: Update `release_deliverable_manifest()`**

In `R/publishing.R`, remove root dashboard HTML from the manifest:

```r
    manifest <- c(
        "app",
        "bracket_decision_sheet.csv",
        "bracket_matchup_context.csv",
        "candidate_matchup_total_points.csv",
        "championship_tiebreaker_summary.csv",
        "championship_tiebreaker_distribution.csv",
        "tournament_sim_candidate_brackets.txt",
        "tournament_sim_model_summary.txt",
        "tournament_sim_backtest_summary.txt",
        "bracket_candidates.txt"
    )
```

- [ ] **Step 6: Add app deliverable validation in `R/publishing.R`**

Add this helper near `copy_release_artifact()`:

```r
assert_dashboard_app_deliverable <- function(app_dir) {
    required_files <- c("index.html", "technical.html", "dashboard_payloads.js")
    missing_files <- required_files[!file.exists(file.path(app_dir, required_files))]
    if (length(missing_files) > 0L) {
        stop_with_message(sprintf(
            "React dashboard app deliverable is incomplete at %s. Missing: %s",
            app_dir,
            paste(missing_files, collapse = ", ")
        ))
    }
    invisible(TRUE)
}
```

In `publish_release_bundle()`, call it before copying `app`:

```r
    deliverable_paths <- vapply(deliverable_manifest, function(filename) {
        source <- file.path(output_dir, filename)
        if (identical(filename, "app")) {
            assert_dashboard_app_deliverable(source)
        }
        if (!file.exists(source)) {
            stop_with_message(sprintf("Missing release artifact: %s", source))
        }
        copy_release_artifact(source, deliverables_dir)
    }, character(1))
```

- [ ] **Step 7: Rewrite `scripts/publish_github_pages.R` to copy only `app/`**

Replace root dashboard file requirements with:

```r
dashboard_app_dir <- "app"
runtime_output_root <- path.expand(Sys.getenv(
    "MMBAYES_PAGES_SOURCE",
    unset = default_runtime_output_root()
))
runtime_app_dir <- file.path(runtime_output_root, dashboard_app_dir)
required_paths <- c(
    file.path(runtime_app_dir, "index.html"),
    file.path(runtime_app_dir, "technical.html"),
    file.path(runtime_app_dir, "dashboard_payloads.js")
)
if (!dir.exists(runtime_app_dir) || !all(file.exists(required_paths))) {
    stop_with_message(sprintf(
        paste(
            "Internal helper scripts/publish_github_pages.R requires an already-rendered React app bundle at %s.",
            "Use `Rscript scripts/run_simulation.R` for the authoritative full workflow or",
            "`Rscript scripts/regenerate_and_sync_dashboards.R` to rebuild from the saved results bundle."
        ),
        runtime_app_dir
    ))
}
```

Use `sync_dashboard_html_files()` only for the contents of `runtime_app_dir` into `file.path(project_root, "output", "app")`, or replace it with a direct directory copy if `sync_dashboard_html_files()` was removed.

- [ ] **Step 8: Run focused tests**

```sh
Rscript -e 'pkgload::load_all(".", export_all = TRUE, helpers = TRUE, quiet = TRUE); testthat::test_file("tests/testthat/test_project_layout.R")'
Rscript -e 'pkgload::load_all(".", export_all = TRUE, helpers = TRUE, quiet = TRUE); testthat::test_file("tests/testthat/test_documentation_links.R")'
```

Expected: release and publish tests pass with no root monolith references.

- [ ] **Step 9: Commit publishing cutover**

```sh
git add R/publishing.R scripts/publish_github_pages.R tests/testthat/test_project_layout.R tests/testthat/test_documentation_links.R
git commit -m "refactor: publish only the React dashboard app"
```

### Task 7: Remove legacy monolith tests, fixture generation, and tracked files

**Files:**
- Modify: `tests/testthat/test_model_and_simulation.R`
- Modify or delete: `tests/testthat/test_bracket_tree_interactions.R`
- Delete: `e2e/bracket_tree.spec.cjs`
- Delete: `e2e/generate_fixture_dashboard.R`
- Delete: `output/bracket_dashboard.html`
- Delete: `output/technical_dashboard.html`

- [ ] **Step 1: Write a negative search test for retired monolith outputs**

Add this test to `tests/testthat/test_documentation_links.R`:

```r
test_that("repository docs and tests do not link retired root dashboard HTML files", {
    repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
    searchable_files <- list.files(
        repo_root,
        pattern = "\\.(R|md|html|cjs|json|yml|yaml)$",
        recursive = TRUE,
        full.names = TRUE
    )
    searchable_files <- searchable_files[!grepl("node_modules|frontend/dist|output/app/assets", searchable_files)]
    text <- paste(vapply(searchable_files, function(path) {
        paste(readLines(path, warn = FALSE), collapse = "\n")
    }, character(1)), collapse = "\n")

    expect_no_match(text, "output/bracket_dashboard\\.html")
    expect_no_match(text, "output/technical_dashboard\\.html")
    expect_no_match(text, "bracket_dashboard\\.html")
    expect_no_match(text, "technical_dashboard\\.html")
})
```

If this test is too broad because source functions still contain retired names during Task 6, keep it skipped with a clear reason until Task 6, then unskip it:

```r
skip("Enable after legacy renderer code is removed in Task 6.")
```

- [ ] **Step 2: Remove legacy HTML substring assertions**

In `tests/testthat/test_model_and_simulation.R`, remove assertions that parse `dashboard_html` for monolith layout, CSS, and `technical_dashboard.html` links. Keep model, simulation, decision-sheet, candidate, scoring, and payload tests.

- [ ] **Step 3: Delete root monolith E2E fixture path**

Delete `e2e/bracket_tree.spec.cjs` and `e2e/generate_fixture_dashboard.R` after confirming equivalent React coverage exists in `e2e/frontend_app.spec.cjs`.

- [ ] **Step 4: Delete tracked monolith HTML files**

```sh
git rm output/bracket_dashboard.html output/technical_dashboard.html
```

- [ ] **Step 5: Run tests that should no longer reference monoliths**

```sh
Rscript -e 'pkgload::load_all(".", export_all = TRUE, helpers = TRUE, quiet = TRUE); testthat::test_file("tests/testthat/test_documentation_links.R")'
Rscript -e 'pkgload::load_all(".", export_all = TRUE, helpers = TRUE, quiet = TRUE); testthat::test_file("tests/testthat/test_model_and_simulation.R")'
npm test -- e2e/frontend_app.spec.cjs
```

Expected: tests pass; no remaining test depends on `bracket_dashboard.html` or `technical_dashboard.html`.

- [ ] **Step 6: Commit legacy test/file removal**

```sh
git add tests/testthat/test_model_and_simulation.R tests/testthat/test_bracket_tree_interactions.R tests/testthat/test_documentation_links.R e2e frontend output
git commit -m "test: remove legacy monolith dashboard coverage"
```

### Task 8: Remove legacy R monolith renderer code

**Files:**
- Modify: `R/plotting_functions.R`
- Modify: `R/utils.R`
- Test: `tests/testthat/*.R`

- [ ] **Step 1: Confirm no live callers remain**

```sh
rg -n "create_bracket_dashboard_html|create_technical_dashboard_html|deprecated_create_bracket_dashboard_html|Open technical_dashboard|dashboard_preview_url\\(\"technical_dashboard\\.html\"\\)" R tests scripts e2e README.md docs index.html
```

Expected before deletion: definitions remain in `R/plotting_functions.R`; no required caller remains outside deleted legacy tests.

- [ ] **Step 2: Delete legacy renderer definitions**

From `R/plotting_functions.R`, delete:

- `.deprecated_create_bracket_dashboard_html`
- `create_bracket_dashboard_html`
- `create_technical_dashboard_html`
- helper functions used only by those renderers

Use `rg` after each deletion group to avoid deleting payload builders or shared formatting helpers used by `R/dashboard_payloads.R`.

- [ ] **Step 3: Remove obsolete helper links**

If no callers remain, delete `dashboard_preview_url()` from `R/plotting_functions.R`.

- [ ] **Step 4: Run R tests**

```sh
Rscript tests/testthat.R
```

Expected: all R tests pass; any failure from a deleted helper means the helper was shared and should be restored or moved to the active payload path.

- [ ] **Step 5: Commit renderer removal**

```sh
git add R/plotting_functions.R R/utils.R tests
git commit -m "refactor: remove legacy dashboard HTML renderers"
```

### Task 9: Update documentation to current React-only state

**Files:**
- Modify: `README.md`
- Modify: `docs/dashboard-frontend-architecture.md`
- Modify: `docs/methods-and-interpretation.md`
- Modify: `docs/runtime-roots.md`
- Modify: `index.html`
- Modify: `tests/testthat/test_documentation_links.R`

- [ ] **Step 1: Write failing documentation contract tests**

In `tests/testthat/test_documentation_links.R`, update the GitHub Pages test:

```r
test_that("GitHub Pages landing page links only to the React dashboards", {
    repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
    index_path <- file.path(repo_root, "index.html")
    expect_true(file.exists(index_path))
    expect_true(file.exists(file.path(repo_root, ".nojekyll")))

    index_text <- paste(readLines(index_path, warn = FALSE), collapse = "\n")
    expect_match(index_text, "output/app/index\\.html")
    expect_match(index_text, "output/app/technical\\.html")
    expect_no_match(index_text, "output/bracket_dashboard\\.html")
    expect_no_match(index_text, "output/technical_dashboard\\.html")
    expect_no_match(index_text, "Legacy HTML")
})
```

Update README workflow expectations:

```r
expect_match(readme_text, "output/app/")
expect_no_match(readme_text, "legacy R-rendered")
expect_no_match(readme_text, "backward compatibility")
expect_no_match(readme_text, "bracket_dashboard\\.html")
expect_no_match(readme_text, "technical_dashboard\\.html")
```

- [ ] **Step 2: Run docs tests to verify failure**

```sh
Rscript -e 'pkgload::load_all(".", export_all = TRUE, helpers = TRUE, quiet = TRUE); testthat::test_file("tests/testthat/test_documentation_links.R")'
```

Expected before docs edits: failures point to legacy/backward-compatible language.

- [ ] **Step 3: Rewrite README current-state sections**

Use direct current-state wording:

```md
### Dashboard App

| File | Description |
|------|-------------|
| [output/app/index.html](https://nicholas-camarda.github.io/mmBayes/output/app/index.html) | Primary bracket entry workspace |
| [output/app/technical.html](https://nicholas-camarda.github.io/mmBayes/output/app/technical.html) | Compare boards, live monitoring, calibration, and backtest detail |
```

Remove the `Legacy HTML snapshots` section entirely.

- [ ] **Step 4: Rewrite GitHub Pages hub**

In `index.html`, remove the legacy card and keep only the app links:

```html
<a class="button primary" href="output/app/index.html">Open bracket dashboard</a>
<a class="button" href="output/app/technical.html">Open technical dashboard</a>
```

- [ ] **Step 5: Rewrite architecture docs**

In `docs/dashboard-frontend-architecture.md`, replace additive/fallback/rollback language with current-state architecture:

```md
The dashboard surface is the static React app synced to `output/app/`. The R pipeline writes versioned payload JSON and a `dashboard_payloads.js` shim; the React bundle renders both the bracket-entry and technical diagnostic workspaces from that payload. Root-level monolith HTML dashboards are not generated or published.
```

- [ ] **Step 6: Run docs tests**

```sh
Rscript -e 'pkgload::load_all(".", export_all = TRUE, helpers = TRUE, quiet = TRUE); testthat::test_file("tests/testthat/test_documentation_links.R")'
```

Expected: docs tests pass and no docs describe legacy fallback as current behavior.

- [ ] **Step 7: Commit docs cutover**

```sh
git add README.md docs index.html tests/testthat/test_documentation_links.R
git commit -m "docs: document React-only dashboard surface"
```

### Task 10: Rebuild, sync, and verify publishable `output/app/`

**Files:**
- Modify: `output/app/**`

- [ ] **Step 1: Build frontend**

```sh
cd frontend && npm run build && cd ..
```

Expected: `frontend/dist/index.html`, `frontend/dist/technical.html`, and deterministic assets exist.

- [ ] **Step 2: Regenerate and sync dashboard app**

```sh
Rscript scripts/regenerate_and_sync_dashboards.R
```

Expected when the saved results bundle exists: runtime payloads are regenerated, runtime `app/` is synced, and tracked `output/app/` is updated. If the saved bundle is absent, run the authoritative full pipeline:

```sh
Rscript scripts/run_simulation.R
```

- [ ] **Step 3: Confirm root monolith files are absent**

```sh
test ! -e output/bracket_dashboard.html
test ! -e output/technical_dashboard.html
test -e output/app/index.html
test -e output/app/technical.html
test -e output/app/dashboard_payloads.js
```

Expected: all commands exit 0.

- [ ] **Step 4: Run full verification gates from fast to slow**

```sh
cd frontend && npm test && npm run build && cd ..
npm test -- e2e/frontend_app.spec.cjs
Rscript -e 'pkgload::load_all(".", export_all = TRUE, helpers = TRUE, quiet = TRUE); testthat::test_file("tests/testthat/test_dashboard_payload_schema.R")'
Rscript -e 'pkgload::load_all(".", export_all = TRUE, helpers = TRUE, quiet = TRUE); testthat::test_file("tests/testthat/test_project_layout.R")'
Rscript -e 'pkgload::load_all(".", export_all = TRUE, helpers = TRUE, quiet = TRUE); testthat::test_file("tests/testthat/test_documentation_links.R")'
Rscript tests/testthat.R
npm test
npm run test:visual
```

Expected: all gates pass with no skipped dashboard parity tests except intentional environment skips documented in the final report. The ordering is intentional: component and focused app failures surface quickly before the expensive full R and visual gates.

- [ ] **Step 5: Inspect dashboard manually**

Open the local files in a browser:

```sh
open output/app/index.html
open output/app/technical.html
```

Manual checks:

- bracket entry workspace is visible above deep reference material
- candidate toggles work
- bracket tree has visible labels and evidence jumps
- review queue is usable at 390px and 1320px widths
- technical dashboard shows compare, live monitoring, calibration, backtest, championship totals, and ensemble diagnostics
- no visual regression looks materially worse than the deleted monolith snapshots

- [ ] **Step 6: Commit synced React app**

```sh
git add output/app
git commit -m "chore: sync React-only dashboard app snapshot"
```

### Task 11: Final completion audit

**Files:**
- Read-only verification across repo

- [ ] **Step 1: Search active surfaces for retired dashboard paths**

```sh
rg -n "bracket_dashboard\\.html|technical_dashboard\\.html|model_comparison_dashboard\\.html|legacy R-rendered|backward compatibility|Legacy HTML" README.md docs/*.md index.html R scripts tests e2e frontend output
```

Expected: no matches. This active-surface search intentionally excludes `docs/superpowers/plans/` so old implementation plans can retain history without masking stale product docs or live code.

- [ ] **Step 2: Search old plans separately for only historical references**

```sh
rg -n "bracket_dashboard\\.html|technical_dashboard\\.html|legacy R-rendered|backward compatibility|Legacy HTML" docs/superpowers/plans || true
```

Expected: matches are allowed only as historical planning notes. If any old plan is being used as current docs, update that plan or remove the stale wording.

- [ ] **Step 3: Verify release manifest**

```sh
Rscript -e 'pkgload::load_all(".", export_all = TRUE, helpers = FALSE, quiet = TRUE); print(release_deliverable_manifest())'
```

Expected: output includes `app` and excludes `bracket_dashboard.html`, `technical_dashboard.html`, and `model_comparison_dashboard.html`.

- [ ] **Step 4: Verify checked-in app is not fixture data**

```sh
Rscript -e 'pkgload::load_all(".", export_all = TRUE, helpers = TRUE, quiet = TRUE); testthat::test_file("tests/testthat/test_dashboard_payload_schema.R")'
```

Expected: the existing tracked-output payload test passes, proving `output/app/dashboard_payloads.js` is not the Vitest fixture shim.

- [ ] **Step 5: Verify git status**

```sh
git status --short
```

Expected: only intentional changes remain.

- [ ] **Step 6: Final report**

Report:

- React parity behaviors covered
- legacy monolith generation paths removed
- publication and release manifest now React-only
- docs updated with current-state wording
- tracked `output/app/` synced
- tests run and exact results
- any manual visual inspection notes
- any remaining risk that could not be verified

## Self-review

| Requirement | Plan coverage |
|-------------|---------------|
| `output/app/` sole dashboard surface | Tasks 5, 6, 7, 9, 10, 11 |
| React bracket app covers bracket-entry workflow | Tasks 2, 3, 4, 10 |
| React technical app covers diagnostic workflow | Tasks 2, 3, 4, 10 |
| Match or improve visual quality | Tasks 1, 3, 4, 10 |
| Pass component, interaction, visual, schema, and R tests | Tasks 2, 3, 5, 10, 11 |
| Replace root monolith HTML generation | Tasks 5, 8 |
| Replace publication/release manifests | Task 6 |
| Replace documentation and tracked outputs | Tasks 7, 9, 10 |
| No fallback or duplicate dashboard paths | Tasks 5, 6, 7, 9, 11 |
| Real checked-in payload renders | Tasks 2, 10, 11 |
| Blank app failure prevented | Task 2 |
| Release publish copies `app/` recursively | Task 6 |
| Concrete acceptance/rejection note before deletion | Task 4 |
| Pre-deletion recovery marker | Task 4 |

## Notes for executor

- Do not rewrite the payload schema unless a parity test proves the current payload cannot represent a required user job.
- Do not keep redirect HTML for retired root dashboard files unless the user explicitly chooses that weaker compatibility tradeoff.
- Do not delete legacy monolith files, tests, or renderer code before Task 4 passes and the user confirms the React app is good enough to replace them.
- Do not edit generated `output/app/assets/*` by hand; rebuild and sync.
- Treat current dirty worktree changes as user work until proven otherwise.
- Prefer one task per commit so regressions can be isolated.
