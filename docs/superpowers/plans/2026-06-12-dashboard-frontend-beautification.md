# Dashboard Frontend Beautification Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the React dashboard app visually coherent, spacious, and readable on desktop and mobile without changing R payload contracts or decision logic.

**Architecture:** Introduce a shared design-token layer in `frontend/src/styles/tokens.css`, port the best legacy comparison-board and probability-track styling from `R/plotting_functions.R` (lines ~3717–3743), then refactor panel components to consume tokens and shared layout primitives. Validate with Playwright screenshot thresholds and existing Vitest/Playwright interaction tests.

**Tech Stack:** React 19, TypeScript, Vite, CSS (no new UI framework), Playwright visual checks, Vitest component tests

---

## File map

| File | Responsibility |
|------|----------------|
| `frontend/src/styles/tokens.css` | Color, spacing, radius, typography CSS variables |
| `frontend/src/styles/layout.css` | Panel shells, section rhythm, grid utilities |
| `frontend/src/styles/components/comparison-board.css` | Ranked/upset/divergence board polish |
| `frontend/src/styles/components/prob-track.css` | Probability track lane styling |
| `frontend/src/styles/components/evidence.css` | Evidence drawer, team cards, advantage chart |
| `frontend/src/styles.css` | Imports only; shrink over time |
| `frontend/src/components/technical/BoardExplainer.tsx` | “What / How / Why / Math” cards above compare boards |
| `frontend/src/components/bracket/PlayInStatusPanel.tsx` | Human-readable play-in table (replaces JSON `<pre>`) |
| `e2e/frontend_visual.spec.cjs` | Screenshot regression for bracket + technical desktop/mobile |
| `docs/dashboard-frontend-architecture.md` | Note beautification complete + visual test commands |

---

### Task 1: Design tokens and typography baseline

**Files:**
- Create: `frontend/src/styles/tokens.css`
- Create: `frontend/src/styles/layout.css`
- Modify: `frontend/src/main.tsx` (or entry that imports `styles.css`)
- Modify: `frontend/index.html`, `frontend/technical.html`
- Test: `e2e/frontend_app.spec.cjs` (existing smoke tests)

- [ ] **Step 1: Add token file**

```css
/* frontend/src/styles/tokens.css */
:root {
  color-scheme: dark;
  --font-sans: Inter, ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
  --font-mono: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;

  --bg: #070a0f;
  --panel: rgba(17, 24, 39, 0.94);
  --panel-2: rgba(15, 23, 42, 0.86);
  --border: rgba(148, 163, 184, 0.28);
  --ink: #f8fafc;
  --muted: #aab6c8;
  --accent: #38bdf8;
  --accent-warm: #fb923c;

  --radius-sm: 10px;
  --radius-md: 14px;
  --radius-lg: 18px;
  --shadow-panel: 0 24px 60px rgba(0, 0, 0, 0.42);

  --space-1: 6px;
  --space-2: 10px;
  --space-3: 14px;
  --space-4: 18px;
  --space-5: 24px;
  --space-6: 32px;

  --text-xs: 0.72rem;
  --text-sm: 0.88rem;
  --text-base: 0.94rem;
  --text-lg: 1.05rem;
  --text-xl: clamp(1.6rem, 3vw, 2.2rem);
}
```

- [ ] **Step 2: Load Inter in HTML shells**

```html
<link rel="preconnect" href="https://fonts.googleapis.com" />
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin />
<link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700&display=swap" rel="stylesheet" />
```

Add to both `frontend/index.html` and `frontend/technical.html` inside `<head>`.

- [ ] **Step 3: Import tokens at top of `frontend/src/styles.css`**

```css
@import "./styles/tokens.css";
@import "./styles/layout.css";
```

- [ ] **Step 4: Replace hard-coded body/hero colors with tokens**

```css
body {
  margin: 0;
  background: var(--bg);
  color: var(--ink);
  font: var(--text-base) / 1.55 var(--font-sans);
}
.hero {
  background: var(--panel);
  border: 1px solid var(--border);
  border-radius: var(--radius-lg);
  box-shadow: var(--shadow-panel);
  padding: var(--space-5);
}
```

- [ ] **Step 5: Run smoke tests**

```sh
cd frontend && npm run build && npm test
cd .. && npm test -- e2e/frontend_app.spec.cjs
```

Expected: all tests PASS.

- [ ] **Step 6: Commit**

```bash
git add frontend/src/styles/tokens.css frontend/src/styles/layout.css frontend/src/styles.css frontend/index.html frontend/technical.html
git commit -m "style(frontend): add design tokens and Inter typography baseline"
```

---

### Task 2: Probability track visual parity

**Files:**
- Create: `frontend/src/styles/components/prob-track.css`
- Modify: `frontend/src/components/bracket/ProbabilityTrack.tsx`
- Test: `frontend/src/components/bracket/ProbabilityTrack.test.tsx` (new)

- [ ] **Step 1: Write failing DOM structure test**

```tsx
// frontend/src/components/bracket/ProbabilityTrack.test.tsx
import { render, screen } from "@testing-library/react";
import { ProbabilityTrack } from "./ProbabilityTrack";

test("renders labeled stats and lane for accessibility", () => {
  render(
    <ProbabilityTrack
      meanProbability={0.72}
      lowerProbability={0.58}
      upperProbability={0.84}
      valueLabel="Posterior mean"
      intervalLabel="Credible interval"
    />,
  );
  expect(screen.getByText("Posterior mean")).toBeInTheDocument();
  expect(screen.getByText("72.0%")).toBeInTheDocument();
  expect(document.querySelector(".prob-track__lane")).toBeTruthy();
});
```

- [ ] **Step 2: Run test to verify it fails or needs lane class**

```sh
cd frontend && npm test -- src/components/bracket/ProbabilityTrack.test.tsx
```

- [ ] **Step 3: Port legacy lane CSS**

```css
/* frontend/src/styles/components/prob-track.css */
.prob-track {
  display: flex;
  flex-direction: column;
  gap: var(--space-2);
  min-width: 0;
}
.prob-track__summary {
  display: grid;
  grid-template-columns: repeat(2, minmax(0, 1fr));
  gap: var(--space-2);
}
.prob-track__stat {
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
  background: var(--panel-2);
  padding: 8px 10px;
}
.prob-track__stat span {
  display: block;
  font-size: var(--text-xs);
  font-weight: 700;
  letter-spacing: 0.07em;
  text-transform: uppercase;
  color: var(--muted);
}
.prob-track__lane {
  position: relative;
  height: 22px;
  border-radius: 999px;
  border: 1px solid var(--border);
  background: linear-gradient(90deg, rgba(15, 23, 42, 0.9), rgba(30, 41, 59, 0.9));
}
.prob-track__range {
  position: absolute;
  top: 50%;
  height: 6px;
  transform: translateY(-50%);
  border-radius: 999px;
  opacity: 0.95;
}
.prob-track__point {
  position: absolute;
  top: 50%;
  width: 14px;
  height: 14px;
  transform: translate(-50%, -50%);
  border-radius: 999px;
  border: 3px solid #0f172a;
  box-shadow: 0 1px 4px rgba(0, 0, 0, 0.35);
}
```

Import in `styles.css`: `@import "./styles/components/prob-track.css";`

Remove duplicate `.prob-track*` rules from `styles.css`.

- [ ] **Step 4: Re-run tests**

```sh
cd frontend && npm test -- src/components/bracket/ProbabilityTrack.test.tsx
```

Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add frontend/src/styles/components/prob-track.css frontend/src/components/bracket/ProbabilityTrack.test.tsx frontend/src/styles.css
git commit -m "style(frontend): polish probability track to match legacy board readability"
```

---

### Task 3: Comparison board spacing and hierarchy

**Files:**
- Create: `frontend/src/styles/components/comparison-board.css`
- Modify: `frontend/src/components/technical/ComparisonBoard.tsx`
- Modify: `frontend/src/components/technical/RankedDecisionBoard.tsx`
- Test: `e2e/frontend_app.spec.cjs` (extend desktop screenshot assertion)

- [ ] **Step 1: Extract board CSS from `styles.css` into `comparison-board.css`**

Key rules to port from legacy (`R/plotting_functions.R` ~3717–3728):
- Header row with uppercase muted labels on `var(--panel-2)` background
- Row padding `18px`, gap `16px`, subtle border between rows (not full card per row on desktop)
- `.board-value` at `15px/700`, `.board-note` at `14px` muted
- On mobile: keep `data-label` pseudo-labels (already present)

- [ ] **Step 2: Hide redundant table fallback on desktop**

```css
@media (min-width: 1241px) {
  .comparison-board__table-fallback {
    display: none !important;
  }
}
```

- [ ] **Step 3: Add Playwright assertion for board minimum height**

```js
// e2e/frontend_app.spec.cjs inside technical page test
await expect(page.getByTestId("ranked-decisions-board")).toBeVisible();
const boardBox = await page.getByTestId("ranked-decisions-board").boundingBox();
expect(boardBox.height).toBeGreaterThan(280);
```

- [ ] **Step 4: Run Playwright**

```sh
cd frontend && npm run build && cd .. && npm test -- e2e/frontend_app.spec.cjs
```

Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add frontend/src/styles/components/comparison-board.css frontend/src/components/technical/*.tsx e2e/frontend_app.spec.cjs frontend/src/styles.css
git commit -m "style(frontend): refine comparison board spacing and desktop hierarchy"
```

---

### Task 4: Board explainer cards (legacy parity)

**Files:**
- Create: `frontend/src/components/technical/BoardExplainer.tsx`
- Modify: `frontend/src/components/technical/CompareWorkspace.tsx`
- Test: `frontend/src/components/technical/BoardExplainer.test.tsx`

- [ ] **Step 1: Write component test**

```tsx
import { render, screen } from "@testing-library/react";
import { BoardExplainer } from "./BoardExplainer";

test("renders four explainer cards", () => {
  render(
    <BoardExplainer
      what="Top decision slots ranked by leverage."
      how="Start with ranked board, then upset pivots."
      why="These slots move bracket identity."
      math="Sorted by decision_score and upset_leverage."
    />,
  );
  expect(screen.getByText(/what this shows/i)).toBeInTheDocument();
  expect(screen.getByText(/underlying math/i)).toBeInTheDocument();
});
```

- [ ] **Step 2: Implement component**

```tsx
interface BoardExplainerProps {
  what: string;
  how: string;
  why: string;
  math: string;
}

export function BoardExplainer({ what, how, why, math }: BoardExplainerProps) {
  const cards = [
    ["What this shows", what],
    ["How to use it", how],
    ["Why it matters", why],
    ["Underlying math", math],
  ] as const;
  return (
    <div className="explain-grid" data-testid="board-explainer">
      {cards.map(([label, body]) => (
        <div key={label} className="explain-card">
          <div className="explain-label">{label}</div>
          <p>{body}</p>
        </div>
      ))}
    </div>
  );
}
```

- [ ] **Step 3: Wire into `CompareWorkspace` above ranked board**

```tsx
<BoardExplainer
  what="Highest-impact review slots with preferred and alternate paths."
  how="Read ranked decisions first, then upset pivots, then divergence forks."
  why="These are the slots most likely to change your bracket outcome."
  math="Ranked by decision_score; upset board sorted by upset_leverage."
/>
```

- [ ] **Step 4: Add `.explain-grid` styles (2x2 desktop, 1-col mobile)**

- [ ] **Step 5: Run tests and commit**

```bash
git add frontend/src/components/technical/BoardExplainer.tsx frontend/src/components/technical/BoardExplainer.test.tsx frontend/src/components/technical/CompareWorkspace.tsx frontend/src/styles.css
git commit -m "feat(frontend): add compare board explainer cards"
```

---

### Task 5: Evidence drawer layout

**Files:**
- Create: `frontend/src/styles/components/evidence.css`
- Modify: `frontend/src/components/bracket/EvidencePanel.tsx`
- Modify: `frontend/src/components/bracket/AdvantageChart.tsx`
- Test: `e2e/frontend_app.spec.cjs` (evidence screenshot after open)

- [ ] **Step 1: Two-column evidence body on wide screens**

```css
@media (min-width: 1100px) {
  .evidence-panel__body {
    display: grid;
    grid-template-columns: minmax(280px, 0.9fr) minmax(360px, 1.1fr);
    gap: var(--space-5);
    align-items: start;
  }
  .evidence-panel__summary-block {
    grid-column: 1 / -1;
  }
  .advantage-chart {
    max-height: 520px;
    overflow: auto;
  }
}
```

Wrap summary/lede/callouts in `evidence-panel__summary-block` in `EvidencePanel.tsx`.

- [ ] **Step 2: Soften team cards**

- Border `var(--border)`, radius `var(--radius-md)`, reduce stat grid density to 3 rows
- Collapse advantage chart rows: alternate row background `#0f172a` / `#111827`

- [ ] **Step 3: Playwright — open evidence and assert advantage chart visible**

```js
await page.locator(".watchlist-card .jump-button[data-open-evidence]").first().click();
await expect(page.getByTestId("advantage-chart")).toBeVisible();
```

- [ ] **Step 4: Commit**

```bash
git commit -m "style(frontend): improve evidence drawer layout and team card density"
```

---

### Task 6: Technical dashboard information architecture

**Files:**
- Modify: `frontend/src/technical/TechnicalApp.tsx`
- Create: `frontend/src/components/technical/CollapsiblePanel.tsx`
- Modify: `frontend/src/styles/layout.css`

- [ ] **Step 1: Collapsible reference sections**

Wrap `ModelOverviewPanel`, decision summary reference, and championship totals in `<CollapsiblePanel defaultOpen={false}>` so Compare + Live + Backtest stay above the fold.

```tsx
export function CollapsiblePanel({
  title,
  children,
  defaultOpen = true,
}: {
  title: string;
  children: React.ReactNode;
  defaultOpen?: boolean;
}) {
  return (
    <details className="collapsible-panel" open={defaultOpen}>
      <summary>{title}</summary>
      <div className="collapsible-panel__body">{children}</div>
    </details>
  );
}
```

- [ ] **Step 2: Sticky compare subnav on desktop**

```css
@media (min-width: 1024px) {
  .compare-workspace__nav {
    position: sticky;
    top: 12px;
    z-index: 2;
  }
}
```

- [ ] **Step 3: Run full test suite**

```sh
Rscript tests/testthat.R
cd frontend && npm run build && npm test && cd ..
npm test
```

- [ ] **Step 4: Commit**

```bash
git commit -m "feat(frontend): collapse technical reference panels and sticky compare nav"
```

---

### Task 7: Play-in appendix and appendix polish

**Files:**
- Create: `frontend/src/components/bracket/PlayInStatusPanel.tsx`
- Modify: `frontend/src/bracket/BracketApp.tsx`
- Test: `frontend/src/components/bracket/PlayInStatusPanel.test.tsx`

- [ ] **Step 1: Test renders resolved/unresolved counts**

```tsx
import { render, screen } from "@testing-library/react";
import { PlayInStatusPanel } from "./PlayInStatusPanel";

test("shows unresolved First Four slots", () => {
  render(
    <PlayInStatusPanel
      rows={[{ expected_slots: 4, resolved_slots: 0, unresolved_slots: 4, has_unresolved_slots: true }]}
    />,
  );
  expect(screen.getByText(/4 First Four slots are still unresolved/i)).toBeInTheDocument();
});
```

- [ ] **Step 2: Implement table UI replacing `<pre>` JSON in `BracketApp.tsx`**

- [ ] **Step 3: Commit**

```bash
git commit -m "feat(frontend): replace play-in JSON dump with readable status panel"
```

---

### Task 8: Visual regression harness

**Files:**
- Create: `e2e/frontend_visual.spec.cjs`
- Modify: `package.json` (optional `test:visual` script)

- [ ] **Step 1: Add screenshot tests at 1320x960 and 390x900**

```js
test("bracket desktop visual baseline", async ({ page }, testInfo) => {
  const stageDir = stageApp();
  await page.setViewportSize({ width: 1320, height: 960 });
  await page.goto(`file://${path.join(stageDir, "index.html")}`);
  await expect(page).toHaveScreenshot("bracket-desktop.png", {
    maxDiffPixelRatio: 0.02,
  });
});
```

- [ ] **Step 2: Document update command in README**

```sh
npm test -- e2e/frontend_visual.spec.cjs --update-snapshots
```

- [ ] **Step 3: Commit**

```bash
git commit -m "test: add Playwright visual regression for dashboard app"
```

---

### Task 9: Sync built assets and docs

**Files:**
- Modify: `output/app/**` (built assets)
- Modify: `docs/dashboard-frontend-architecture.md`
- Modify: `README.md` (visual test command)

- [ ] **Step 1: Rebuild and sync**

```sh
cd frontend && npm run build && cd ..
Rscript -e 'pkgload::load_all(".", quiet=TRUE); sync_frontend_app(getwd(), file.path(getwd(),"output"), file.path(getwd(),"output"))'
```

- [ ] **Step 2: Regenerate fixture shim if needed**

Ensure `output/app/dashboard_payloads.js` exists (from runtime regen or fixture node script used in PR #8).

- [ ] **Step 3: Commit**

```bash
git commit -m "chore: sync beautified dashboard app assets"
```

---

## Self-review

| Spec requirement | Task |
|------------------|------|
| Coherent typography/colors | Task 1 |
| Comparison boards readable | Tasks 2–3 |
| Legacy explainer cards | Task 4 |
| Evidence drawer quality | Task 5 |
| Technical page not a wall of panels | Task 6 |
| Play-in appendix human-readable | Task 7 |
| Prevent visual regressions | Task 8 |
| Publishable output | Task 9 |

**Out of scope (YAGNI for beautification pass):** bracket tree SVG rewrite, round risk heatmap, leverage scatter — track as separate plans if needed.

---

## Verification checklist (final)

```sh
cd frontend && npm run build && npm test && cd ..
Rscript tests/testthat.R
npm test
```

Manual: open `output/app/index.html` and `technical.html`, verify Compare boards, evidence drawer, and collapsed reference sections at 1320px and 390px widths.
