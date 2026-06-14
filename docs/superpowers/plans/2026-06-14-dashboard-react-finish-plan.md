# Dashboard React Finish Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Finish the dashboard migration by making the React app the single maintainable dashboard surface while matching or improving the legacy monolith's first-viewport clarity, bracket-entry workflow, diagnostics workflow, and publication contract.

**Architecture:** R continues to own data loading, modeling, simulation, payload generation, and schema validation. React owns browser rendering from `dashboard_payloads.js`; legacy monolith HTML remains only as a local visual reference until the explicit acceptance gate passes, then root monolith outputs are removed from generated and published surfaces. Visual parity is enforced with captured reference screenshots, Playwright workflow tests, Vitest component tests, and a written acceptance ledger.

**Tech Stack:** R/testthat, React 19, TypeScript, Vite, Vitest, Playwright, static `file://` delivery, JSON Schema v1.1.0, CSS modules by repo convention through `frontend/src/styles/**/*.css`.

---

## Decision

Do not give up on the latest changes wholesale. Keep the React migration work, but stop treating the current React screenshots as accepted. The current state is a useful migration scaffold with payload contracts, component tests, Playwright workflow tests, and captured legacy references. The unfinished part is visual and product acceptance, not the entire migration.

Proceed under these defaults unless the user explicitly says otherwise:

- The finished product is React-only under `output/app/`.
- The old root HTML dashboards are not public deliverables in the final state.
- The legacy screenshots under `docs/dashboard-cutover-reference/` are the visual reference during repair.
- No compatibility fallback renderer is added.
- The React dashboard should preserve the legacy dark sports-analytics cockpit direction, but with cleaner hierarchy, denser first viewports, and more maintainable components.

## What I Need From The User

One decision is enough to proceed:

> Approved visual target: "Use the captured legacy dashboards as the reference for hierarchy and density, keep the React component architecture, and finish a polished dark sports-analytics dashboard rather than redesigning from scratch. The implementation should follow strong web dashboard principles for extensibility, robustness, functionality, user experience, and beautiful design."

Everything else can be handled in-repo. If the user wants a different visual direction, stop after Task 1 and run a design concept pass before implementation.

## Current State To Preserve

The worktree currently contains a large React-only cutover in progress. Before touching code, inspect it and do not revert unrelated user edits:

```sh
git status --short
git diff --stat
git diff -- R/utils.R R/plotting_functions.R R/publishing.R frontend/src e2e tests/testthat README.md docs index.html
```

Expected: output includes React app changes, root dashboard deletions, docs edits, tests, and `docs/dashboard-cutover-reference/`. Treat these as existing work unless a specific step says to replace a hunk.

## File Structure

### Files Created

| File | Responsibility |
|------|----------------|
| `frontend/src/components/DashboardHero.tsx` | Shared compact page header for bracket and technical dashboards. Keeps metadata and nav consistent without forcing every dashboard into a large card. |
| `frontend/src/components/DashboardSection.tsx` | Shared section wrapper with explicit visual roles: `hero`, `status`, `action`, `orientation`, `reference`, `technical`. Reduces one-off card styling. |
| `frontend/src/components/DashboardHero.test.tsx` | Component tests for header metadata, nav rendering, and compact variant behavior. |
| `frontend/src/components/DashboardSection.test.tsx` | Component tests for section role class names and heading rendering. |
| `docs/dashboard-cutover-reference/visual-acceptance-ledger.md` | Manual acceptance record comparing legacy and React screenshots for desktop/mobile bracket and technical views. |

### Files Modified

| File | Responsibility |
|------|----------------|
| `frontend/src/bracket/BracketApp.tsx` | Use shared hero and section shells; keep candidate entry as the first real workflow after status/orientation. |
| `frontend/src/technical/TechnicalApp.tsx` | Move action summary above orientation, use compact header, and make Compare/backtest/ensemble entry points visible sooner. |
| `frontend/src/styles/tokens.css` | Define final design tokens: dark background, panel hierarchy, semantic status colors, tighter radius scale, readable type sizes. |
| `frontend/src/styles/layout.css` | Implement compact page shell, section wrappers, responsive spacing, and first-viewport density. |
| `frontend/src/styles.css` | Keep global imports; remove duplicated hero/section styles once they live in layout/tokens. |
| `frontend/src/components/bracket/EntryWorkspace.tsx` | Improve bracket-entry scannability with denser region headers and consistent review affordances if required by screenshot review. |
| `frontend/src/components/technical/TechnicalActionSummary.tsx` | Ensure the action strip reads as the primary first technical workflow, not a generic card grid. |
| `frontend/src/bracket/BracketApp.test.tsx` | Assert bracket workflow ordering and shared shell rendering. |
| `frontend/src/technical/TechnicalApp.test.tsx` | Assert technical action summary appears before orientation and diagnostics remain available. |
| `e2e/frontend_app.spec.cjs` | Add workflow-order and first-viewport visibility assertions. |
| `e2e/frontend_visual.spec.cjs` | Keep screenshot baselines but refresh only after manual acceptance. |
| `docs/dashboard-cutover-reference/parity-matrix.md` | Replace premature "Accepted" statuses with evidence-backed statuses after review. |
| `docs/dashboard-cutover-reference/README.md` | State that these screenshots are the repair reference until acceptance passes. |
| `docs/dashboard-frontend-architecture.md` | Current-state React-only architecture after acceptance, with no historical or migration framing. |
| `README.md` | Current-state dashboard entrypoint docs only after acceptance. |

## Task 1: Freeze The Cutover Gate

**Files:**
- Modify: `docs/dashboard-cutover-reference/parity-matrix.md`
- Modify: `docs/dashboard-cutover-reference/README.md`
- Create: `docs/dashboard-cutover-reference/visual-acceptance-ledger.md`

- [ ] **Step 1: Mark visual parity as pending**

Edit `docs/dashboard-cutover-reference/parity-matrix.md` so every row whose `Status` is currently `Accepted for pre-cutover review` becomes one of these concrete statuses:

```md
Pending visual repair
```

For rows covering payload load failure, keyboard operation, and real synced payload render, use:

```md
Automated coverage present; pending final visual gate
```

Expected result: no row in `docs/dashboard-cutover-reference/parity-matrix.md` contains `Accepted for pre-cutover review`.

- [ ] **Step 2: Run a text check**

Run:

```sh
rg -n "Accepted for pre-cutover review|Pending visual repair|Automated coverage present" docs/dashboard-cutover-reference/parity-matrix.md
```

Expected: the first phrase has zero matches; the latter two phrases cover all status cells.

- [ ] **Step 3: Update the reference README**

Replace `docs/dashboard-cutover-reference/README.md` with:

```md
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
```

- [ ] **Step 4: Create the acceptance ledger**

Create `docs/dashboard-cutover-reference/visual-acceptance-ledger.md`:

```md
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
```

- [ ] **Step 5: Commit the gate freeze**

Run:

```sh
git add docs/dashboard-cutover-reference/README.md docs/dashboard-cutover-reference/parity-matrix.md docs/dashboard-cutover-reference/visual-acceptance-ledger.md
git commit -m "docs: freeze dashboard visual cutover gate"
```

Expected: commit succeeds. If the user does not want commits yet, skip commit and record that in the implementation report.

## Task 2: Add Shared Dashboard Shell Components

**Files:**
- Create: `frontend/src/components/DashboardHero.tsx`
- Create: `frontend/src/components/DashboardHero.test.tsx`
- Create: `frontend/src/components/DashboardSection.tsx`
- Create: `frontend/src/components/DashboardSection.test.tsx`

- [ ] **Step 1: Create failing tests for `DashboardHero`**

Create `frontend/src/components/DashboardHero.test.tsx`:

```tsx
import { describe, expect, it } from "vitest";
import { render, screen } from "@testing-library/react";
import { DashboardHero } from "./DashboardHero";

describe("DashboardHero", () => {
  it("renders title, eyebrow, lede, metadata, and optional nav", () => {
    render(
      <DashboardHero
        eyebrow="2026 mmBayes bracket dashboard"
        title="Bracket entry workspace"
        lede="Enter Candidate 1, then inspect review picks."
        metadata={{
          render_timestamp: "June 13, 2026 at 01:55 PM EDT",
          git_commit: "f3d8b31",
          repo_snapshot_synced: true,
          repo_snapshot_label: "Synced to tracked repo output in this run",
        }}
        nav={<nav aria-label="Dashboard sections">Jump nav</nav>}
      />,
    );

    expect(screen.getByRole("banner")).toHaveClass("dashboard-hero");
    expect(screen.getByText("2026 mmBayes bracket dashboard")).toBeInTheDocument();
    expect(screen.getByRole("heading", { name: "Bracket entry workspace" })).toBeInTheDocument();
    expect(screen.getByText("Enter Candidate 1, then inspect review picks.")).toBeInTheDocument();
    expect(screen.getByTestId("build-metadata")).toHaveTextContent("f3d8b31");
    expect(screen.getByLabelText("Dashboard sections")).toHaveTextContent("Jump nav");
  });
});
```

- [ ] **Step 2: Run the failing hero test**

Run:

```sh
cd frontend && npm test -- src/components/DashboardHero.test.tsx
```

Expected: FAIL because `DashboardHero.tsx` does not exist.

- [ ] **Step 3: Implement `DashboardHero`**

Create `frontend/src/components/DashboardHero.tsx`:

```tsx
import type { ReactNode } from "react";
import type { BuildMetadata } from "../types/payload";
import { BuildMetadataBanner } from "./BuildMetadataBanner";

interface DashboardHeroProps {
  eyebrow: string;
  title: string;
  lede: string;
  metadata: BuildMetadata;
  nav?: ReactNode;
}

export function DashboardHero({ eyebrow, title, lede, metadata, nav }: DashboardHeroProps) {
  return (
    <header className="dashboard-hero" role="banner">
      <div className="dashboard-hero__copy">
        <div className="eyebrow">{eyebrow}</div>
        <h1>{title}</h1>
        <p className="lede">{lede}</p>
      </div>
      <BuildMetadataBanner metadata={metadata} />
      {nav ? <div className="dashboard-hero__nav">{nav}</div> : null}
    </header>
  );
}
```

- [ ] **Step 4: Create failing tests for `DashboardSection`**

Create `frontend/src/components/DashboardSection.test.tsx`:

```tsx
import { describe, expect, it } from "vitest";
import { render, screen } from "@testing-library/react";
import { DashboardSection } from "./DashboardSection";

describe("DashboardSection", () => {
  it("renders a labelled section with the requested visual role", () => {
    render(
      <DashboardSection
        id="build"
        roleTone="action"
        kicker="Enter bracket"
        title="Candidate Recommendations"
        note="Copy winners by region and round."
      >
        <div>section body</div>
      </DashboardSection>,
    );

    const section = screen.getByRole("region", { name: "Candidate Recommendations" });
    expect(section).toHaveClass("dashboard-section");
    expect(section).toHaveClass("dashboard-section--action");
    expect(screen.getByText("Enter bracket")).toHaveClass("role-kicker");
    expect(screen.getByText("Copy winners by region and round.")).toHaveClass("section-note");
    expect(screen.getByText("section body")).toBeInTheDocument();
  });
});
```

- [ ] **Step 5: Run the failing section test**

Run:

```sh
cd frontend && npm test -- src/components/DashboardSection.test.tsx
```

Expected: FAIL because `DashboardSection.tsx` does not exist.

- [ ] **Step 6: Implement `DashboardSection`**

Create `frontend/src/components/DashboardSection.tsx`:

```tsx
import type { ReactNode } from "react";

type RoleTone = "action" | "orientation" | "evidence" | "reference" | "technical";

interface DashboardSectionProps {
  id?: string;
  roleTone?: RoleTone;
  kicker?: string;
  title: string;
  note?: string;
  children: ReactNode;
}

export function DashboardSection({
  id,
  roleTone = "technical",
  kicker,
  title,
  note,
  children,
}: DashboardSectionProps) {
  return (
    <section
      id={id}
      className={`dashboard-section dashboard-section--${roleTone}`}
      aria-labelledby={id ? `${id}-title` : undefined}
      aria-label={id ? undefined : title}
    >
      {kicker ? <div className={`role-kicker role-kicker--${roleTone}`}>{kicker}</div> : null}
      <h2 id={id ? `${id}-title` : undefined}>{title}</h2>
      {note ? <p className="section-note">{note}</p> : null}
      {children}
    </section>
  );
}
```

- [ ] **Step 7: Verify component tests pass**

Run:

```sh
cd frontend && npm test -- src/components/DashboardHero.test.tsx src/components/DashboardSection.test.tsx
```

Expected: PASS.

- [ ] **Step 8: Commit shared shell components**

Run:

```sh
git add frontend/src/components/DashboardHero.tsx frontend/src/components/DashboardHero.test.tsx frontend/src/components/DashboardSection.tsx frontend/src/components/DashboardSection.test.tsx
git commit -m "feat: add shared dashboard shell components"
```

Expected: commit succeeds unless commits are deferred by user instruction.

## Task 3: Apply The Shared Shell To The Bracket Dashboard

**Files:**
- Modify: `frontend/src/bracket/BracketApp.tsx`
- Modify: `frontend/src/bracket/BracketApp.test.tsx`

- [ ] **Step 1: Add a failing bracket shell test**

In `frontend/src/bracket/BracketApp.test.tsx`, add this test inside `describe("BracketApp", () => { ... })`:

```tsx
it("keeps the bracket dashboard workflow order compact and entry-first", () => {
  render(<BracketApp payload={fixture} />);

  const hero = screen.getByRole("banner");
  const build = screen.getByRole("region", { name: "Candidate Recommendations" });
  const review = screen.getByRole("region", { name: "Review Queue" });

  expect(hero).toHaveClass("dashboard-hero");
  expect(build).toHaveClass("dashboard-section--action");
  expect(review).toHaveClass("dashboard-section--action");
  expect(hero.compareDocumentPosition(build) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();
  expect(build.compareDocumentPosition(review) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();
});
```

- [ ] **Step 2: Run the failing bracket test**

Run:

```sh
cd frontend && npm test -- src/bracket/BracketApp.test.tsx
```

Expected: FAIL because `BracketApp` still uses `header.hero` and `section.section`.

- [ ] **Step 3: Replace the bracket hero and workflow sections**

Modify imports at the top of `frontend/src/bracket/BracketApp.tsx`:

```tsx
import { DashboardHero } from "../components/DashboardHero";
import { DashboardSection } from "../components/DashboardSection";
```

Remove this import:

```tsx
import { BuildMetadataBanner } from "../components/BuildMetadataBanner";
```

Replace the opening hero block with:

```tsx
<DashboardHero
  eyebrow={`${payload.bracket_year} mmBayes bracket dashboard`}
  title="Bracket entry workspace"
  lede={`Enter Candidate 1 like a bracket sheet, pause on review picks, then inspect the alternate route and evidence before you lock in your ${payload.bracket_year} entry.`}
  metadata={payload.build_metadata}
  nav={<JumpNav />}
/>
```

Replace the candidate recommendations section wrapper with:

```tsx
<DashboardSection
  id="build"
  roleTone="action"
  kicker="Enter bracket"
  title="Candidate Recommendations"
  note="Use this section like a bracket sheet: choose Candidate 1 or Candidate 2, copy winners by region and round, then enter the champion and tiebreaker. Review markers show the picks worth pausing on."
>
```

and close it with:

```tsx
</DashboardSection>
```

Replace the review queue section wrapper with:

```tsx
<DashboardSection
  id="review-queue"
  roleTone="action"
  kicker="Decide now"
  title="Review Queue"
  note="Use this after the fill-in view. Read the divergence route map to see where Candidate 2 actually splits, then work the queue for bracket-changing disagreements, upset pivots, and fragile favorites that deserve manual attention."
>
```

and close it with:

```tsx
</DashboardSection>
```

Replace the bracket tree section wrapper with:

```tsx
<DashboardSection
  roleTone="evidence"
  kicker="Understand why"
  title="Bracket Tree"
  note="Each node is a game in the selected candidate path. Color shows confidence tier. Click or hover for context, then open the evidence drawer for the full matchup summary."
>
```

and close it with:

```tsx
</DashboardSection>
```

Replace the technical appendix section wrapper with:

```tsx
<DashboardSection
  id="technical-appendix"
  roleTone="reference"
  kicker="Reference"
  title="Technical appendix"
  note="Decision sheet, play-in resolution, and candidate summaries for offline reference."
>
```

and close it with:

```tsx
</DashboardSection>
```

- [ ] **Step 4: Verify bracket component tests pass**

Run:

```sh
cd frontend && npm test -- src/bracket/BracketApp.test.tsx
```

Expected: PASS.

- [ ] **Step 5: Commit bracket shell application**

Run:

```sh
git add frontend/src/bracket/BracketApp.tsx frontend/src/bracket/BracketApp.test.tsx
git commit -m "refactor: apply shared shell to bracket dashboard"
```

Expected: commit succeeds unless commits are deferred.

## Task 4: Apply The Shared Shell And Better Workflow Order To The Technical Dashboard

**Files:**
- Modify: `frontend/src/technical/TechnicalApp.tsx`
- Modify: `frontend/src/technical/TechnicalApp.test.tsx`

- [ ] **Step 1: Add a failing technical workflow-order test**

In `frontend/src/technical/TechnicalApp.test.tsx`, add this test inside `describe("TechnicalApp", () => { ... })`:

```tsx
it("puts the technical action summary before generic orientation content", () => {
  render(<TechnicalApp payload={fullFixture} />);

  const hero = screen.getByRole("banner");
  const actionSummary = screen.getByTestId("technical-action-summary");
  const orientation = screen.getByRole("region", { name: "How to use this dashboard" });

  expect(hero).toHaveClass("dashboard-hero");
  expect(actionSummary.compareDocumentPosition(orientation) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();
});
```

If `TechnicalActionSummary` does not currently expose `data-testid="technical-action-summary"`, this test should fail until Step 3.

- [ ] **Step 2: Run the failing technical test**

Run:

```sh
cd frontend && npm test -- src/technical/TechnicalApp.test.tsx
```

Expected: FAIL because `TechnicalApp` still renders orientation before action summary and may not expose the test id.

- [ ] **Step 3: Add a test id to `TechnicalActionSummary` if missing**

Open `frontend/src/components/technical/TechnicalActionSummary.tsx`. Ensure its outermost rendered `<section>` has this attribute:

```tsx
data-testid="technical-action-summary"
```

The opening section should be equivalent to:

```tsx
<section className="technical-panel action-summary-panel" data-testid="technical-action-summary">
```

Use the existing class names if they differ; add only the test id if the classes already exist.

- [ ] **Step 4: Replace the technical hero and reorder the action summary**

Modify imports at the top of `frontend/src/technical/TechnicalApp.tsx`:

```tsx
import { DashboardHero } from "../components/DashboardHero";
import { DashboardSection } from "../components/DashboardSection";
```

Remove this import:

```tsx
import { BuildMetadataBanner } from "../components/BuildMetadataBanner";
```

Replace the opening hero block with:

```tsx
<DashboardHero
  eyebrow={`${payload.bracket_year} mmBayes technical dashboard`}
  title="Technical Bracket Dashboard"
  lede="Start in Compare to decide which games deserve attention, then open backtest and ensemble diagnostics when you want the evidence behind the recommendation."
  metadata={payload.build_metadata}
/>
```

Move this component so it appears immediately after `StatusPanel` and before the orientation section:

```tsx
<TechnicalActionSummary summary={payload.action_summary} />
```

Replace the orientation section wrapper with:

```tsx
<DashboardSection
  roleTone="orientation"
  kicker="Orientation"
  title="How to use this dashboard"
>
```

and close it with:

```tsx
</DashboardSection>
```

Do not duplicate `TechnicalActionSummary`; after this edit there should be exactly one `<TechnicalActionSummary summary={payload.action_summary} />` in the file.

- [ ] **Step 5: Verify technical component tests pass**

Run:

```sh
cd frontend && npm test -- src/technical/TechnicalApp.test.tsx
```

Expected: PASS.

- [ ] **Step 6: Commit technical workflow ordering**

Run:

```sh
git add frontend/src/technical/TechnicalApp.tsx frontend/src/technical/TechnicalApp.test.tsx frontend/src/components/technical/TechnicalActionSummary.tsx
git commit -m "refactor: prioritize technical dashboard action summary"
```

Expected: commit succeeds unless commits are deferred.

## Task 5: Repair The Visual System

**Files:**
- Modify: `frontend/src/styles/tokens.css`
- Modify: `frontend/src/styles/layout.css`
- Modify: `frontend/src/styles.css`

- [ ] **Step 1: Add a first-viewport density assertion before CSS changes**

In `e2e/frontend_app.spec.cjs`, add this helper near the existing helper functions:

```js
async function expectInFirstViewport(locator, maxBottom = 960) {
  await expect(locator).toBeVisible();
  const box = await locator.boundingBox();
  expect(box).toBeTruthy();
  expect(box.y + box.height).toBeLessThanOrEqual(maxBottom);
}
```

Then add this test inside `test.describe("static frontend dashboards", () => { ... })`:

```js
test("first desktop viewport keeps primary workflow visible", async ({ page }) => {
  const stageDir = stageApp();
  await page.setViewportSize({ width: 1320, height: 960 });

  await page.goto(`file://${path.join(stageDir, "index.html")}`, { waitUntil: "domcontentloaded" });
  await expectInFirstViewport(page.locator("#build"));
  await expectInFirstViewport(page.locator(".next-action-panel"));

  await page.goto(`file://${path.join(stageDir, "technical.html")}`, { waitUntil: "domcontentloaded" });
  await expectInFirstViewport(page.getByTestId("technical-action-summary"));
  await expectInFirstViewport(page.getByTestId("compare-workspace"), 1200);
});
```

- [ ] **Step 2: Run the first-viewport test and record current failure**

Run:

```sh
npm test -- e2e/frontend_app.spec.cjs -g "first desktop viewport keeps primary workflow visible"
```

Expected before CSS/workflow repair: FAIL or near-fail if the technical action summary or compare workspace sits too low. This failure is the regression target.

- [ ] **Step 3: Replace design tokens**

Replace `frontend/src/styles/tokens.css` with:

```css
/* frontend/src/styles/tokens.css */
:root {
  color-scheme: dark;
  --font-sans: Inter, ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
  --font-mono: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;

  --bg: #071018;
  --bg-2: #0a1420;
  --panel: rgba(15, 23, 42, 0.94);
  --panel-strong: rgba(17, 24, 39, 0.98);
  --panel-soft: rgba(21, 32, 50, 0.72);
  --border: rgba(125, 151, 183, 0.34);
  --border-strong: rgba(148, 183, 221, 0.52);
  --ink: #f8fafc;
  --ink-strong: #ffffff;
  --muted: #b7c4d7;
  --muted-2: #8ea0b8;
  --accent: #38bdf8;
  --accent-strong: #0ea5e9;
  --accent-warm: #f59e0b;
  --success: #22c55e;
  --warning: #f97316;
  --danger: #ef4444;

  --radius-sm: 6px;
  --radius-md: 10px;
  --radius-lg: 14px;
  --shadow-panel: 0 18px 42px rgba(0, 0, 0, 0.32);

  --space-1: 4px;
  --space-2: 8px;
  --space-3: 12px;
  --space-4: 16px;
  --space-5: 22px;
  --space-6: 30px;

  --text-xs: 0.72rem;
  --text-sm: 0.84rem;
  --text-base: 0.94rem;
  --text-lg: 1.06rem;
  --text-xl: 1.42rem;
  --text-hero: clamp(1.75rem, 2.5vw, 2.35rem);
}
```

- [ ] **Step 4: Replace layout shell CSS**

Replace the top shell section of `frontend/src/styles/layout.css` through the `.collapsible-panel__body > .technical-panel` rule with:

```css
.dashboard,
.page {
  max-width: 1320px;
  margin: 0 auto;
  padding: 18px 18px 34px;
}

.dashboard-hero,
.hero {
  background: var(--panel);
  border: 1px solid var(--border);
  border-radius: var(--radius-lg);
  box-shadow: var(--shadow-panel);
  padding: 18px 20px;
  margin-bottom: 14px;
}

.dashboard-hero h1,
.hero h1 {
  margin: 8px 0 10px;
  font-size: var(--text-hero);
  line-height: 1.08;
  letter-spacing: 0;
}

.dashboard-hero__copy {
  max-width: 980px;
}

.dashboard-hero__nav {
  margin-top: 12px;
}

.dashboard-section,
.section,
.technical-panel {
  background: var(--panel);
  border: 1px solid var(--border);
  border-radius: var(--radius-lg);
  padding: 18px 20px;
  margin: 14px 0;
}

.dashboard-section--action {
  border-color: rgba(56, 189, 248, 0.44);
}

.dashboard-section--orientation {
  border-color: rgba(148, 163, 184, 0.34);
}

.dashboard-section--evidence {
  border-color: rgba(96, 165, 250, 0.4);
}

.dashboard-section--reference {
  border-color: rgba(148, 163, 184, 0.3);
}

.dashboard-section h2,
.section h2,
.technical-panel h2 {
  margin: 8px 0 8px;
  font-size: var(--text-xl);
  line-height: 1.18;
  letter-spacing: 0;
}

.section-note,
.lede {
  color: var(--muted);
  max-width: 1040px;
}

.lede {
  margin: 0 0 14px;
}

.collapsible-panel {
  background: var(--panel);
  border: 1px solid var(--border);
  border-radius: var(--radius-lg);
  margin: 14px 0;
  box-shadow: var(--shadow-panel);
}

.collapsible-panel > summary {
  cursor: pointer;
  font-weight: 700;
  font-size: var(--text-lg);
  padding: 14px 18px;
  list-style: none;
  color: var(--ink);
}

.collapsible-panel > summary::-webkit-details-marker {
  display: none;
}

.collapsible-panel > summary::after {
  content: "▸";
  float: right;
  color: var(--muted);
  transition: transform 0.15s ease;
}

.collapsible-panel[open] > summary::after {
  transform: rotate(90deg);
}

.collapsible-panel__body {
  padding: 0 18px 18px;
}

.collapsible-panel__body > .technical-panel {
  margin-top: 0;
}
```

Leave the rest of `layout.css` intact unless selectors duplicate the replaced rules.

- [ ] **Step 5: Tighten global chips and nav**

In `frontend/src/styles.css`, replace the `.hero-meta`, `.hero-meta__chip`, `.jump-nav`, `.jump-nav a`, `.filter-chip`, `.show-more-button`, `.jump-button`, `.entry-toggle`, `.entry-review-button`, and `.btree-toggle` rules with:

```css
.hero-meta {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(210px, 1fr));
  gap: 10px;
  margin: 0;
}

.hero-meta__chip {
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
  padding: 9px 11px;
  background: rgba(10, 20, 32, 0.72);
  min-width: 0;
}

.hero-meta__chip span {
  display: block;
  font-size: 10px;
  text-transform: uppercase;
  letter-spacing: 0.08em;
  color: var(--muted);
  margin-bottom: 3px;
}

.hero-meta__chip strong {
  display: block;
  font-size: 13px;
  overflow-wrap: anywhere;
}

.hero-meta__chip code {
  font-size: 12px;
  font-family: var(--font-mono);
}

.jump-nav {
  display: flex;
  flex-wrap: wrap;
  gap: 8px;
  margin-top: 0;
  position: sticky;
  top: 0;
  z-index: 20;
  padding: 8px 0;
  background: linear-gradient(180deg, rgba(7, 16, 24, 0.96), rgba(7, 16, 24, 0.76));
  backdrop-filter: blur(10px);
  overflow-x: auto;
}

.jump-nav a,
.filter-chip,
.show-more-button,
.jump-button,
.entry-toggle,
.entry-review-button,
.btree-toggle {
  border: 1px solid var(--border);
  background: rgba(30, 41, 59, 0.92);
  color: var(--ink);
  border-radius: 999px;
  padding: 7px 12px;
  font-weight: 700;
  font-size: 12px;
  cursor: pointer;
  text-decoration: none;
  display: inline-flex;
  align-items: center;
  line-height: 1.2;
}

.jump-nav a {
  background: var(--accent);
  border-color: var(--accent);
  color: #03131f;
}
```

- [ ] **Step 6: Run the first-viewport test again**

Run:

```sh
cd frontend && npm run build
cd ..
npm test -- e2e/frontend_app.spec.cjs -g "first desktop viewport keeps primary workflow visible"
```

Expected: PASS. If it fails because `#build` or `technical-action-summary` is still below the threshold, reduce vertical padding in `dashboard-hero`, `.dashboard-section`, and `.hero-meta__chip` by 2px increments. Do not hide content to make the test pass.

- [ ] **Step 7: Commit visual shell repair**

Run:

```sh
git add frontend/src/styles/tokens.css frontend/src/styles/layout.css frontend/src/styles.css e2e/frontend_app.spec.cjs
git commit -m "style: repair dashboard shell density and hierarchy"
```

Expected: commit succeeds unless commits are deferred.

## Task 6: Verify And Tune Bracket Entry Readability

**Files:**
- Modify: `frontend/src/components/bracket/EntryWorkspace.tsx`
- Modify: `frontend/src/bracket/BracketApp.test.tsx`
- Modify: `frontend/src/styles.css`

- [ ] **Step 1: Add a component assertion for review affordances**

In `frontend/src/bracket/BracketApp.test.tsx`, add:

```tsx
it("renders bracket entry picks with visible review and ready states", () => {
  render(<BracketApp payload={fixture} />);

  expect(document.querySelectorAll(".entry-pick").length).toBeGreaterThan(20);
  expect(document.querySelectorAll(".entry-pick--review").length).toBeGreaterThan(0);
  expect(screen.getAllByText("Ready to enter").length).toBeGreaterThan(0);
  expect(screen.getAllByRole("button", { name: "Review" }).length).toBeGreaterThan(0);
});
```

- [ ] **Step 2: Run the bracket test**

Run:

```sh
cd frontend && npm test -- src/bracket/BracketApp.test.tsx
```

Expected: PASS if review and ready states are present. If it fails, inspect `EntryWorkspace.tsx` before editing styles.

- [ ] **Step 3: Tighten entry card CSS**

Find the existing `.entry-pick`, `.entry-pick__round`, `.entry-pick__main`, `.entry-pick__meta`, `.entry-region`, and `.entry-region__head` rules in `frontend/src/styles.css`. Replace them with:

```css
.entry-region {
  border: 1px solid var(--border);
  border-radius: var(--radius-md);
  background: rgba(10, 20, 32, 0.54);
  overflow: hidden;
}

.entry-region__head {
  display: flex;
  justify-content: space-between;
  gap: 12px;
  align-items: baseline;
  padding: 10px 12px;
  border-bottom: 1px solid var(--border);
  background: rgba(15, 23, 42, 0.8);
}

.entry-region__head h4 {
  margin: 0;
  font-size: 15px;
  letter-spacing: 0;
}

.entry-region__head span {
  color: var(--muted);
  font-size: 12px;
  font-weight: 700;
}

.entry-pick-list {
  display: grid;
  gap: 0;
}

.entry-pick {
  display: grid;
  grid-template-columns: 88px minmax(0, 1fr) auto;
  gap: 10px;
  align-items: center;
  padding: 10px 12px;
  border-bottom: 1px solid rgba(125, 151, 183, 0.22);
}

.entry-pick:last-child {
  border-bottom: 0;
}

.entry-pick--review {
  background: rgba(245, 158, 11, 0.08);
}

.entry-pick__round {
  color: var(--muted);
  font-size: 12px;
  font-weight: 800;
}

.entry-pick__main {
  min-width: 0;
}

.entry-pick__main strong,
.entry-pick__main span {
  display: block;
}

.entry-pick__main strong {
  color: var(--ink-strong);
  font-size: 14px;
  line-height: 1.25;
}

.entry-pick__main span {
  color: var(--muted);
  font-size: 12px;
  overflow-wrap: anywhere;
}

.entry-pick__meta {
  display: flex;
  flex-wrap: wrap;
  gap: 6px;
  justify-content: flex-end;
  color: var(--muted);
  font-size: 11px;
}

.entry-check {
  color: var(--success);
  font-size: 12px;
  font-weight: 800;
  white-space: nowrap;
}

@media (max-width: 720px) {
  .entry-pick {
    grid-template-columns: 1fr;
    align-items: start;
  }

  .entry-pick__meta {
    justify-content: flex-start;
  }
}
```

- [ ] **Step 4: Build and run bracket visual smoke**

Run:

```sh
cd frontend && npm run build
cd ..
npm test -- e2e/frontend_app.spec.cjs -g "bracket React app covers the legacy entry"
```

Expected: PASS.

- [ ] **Step 5: Commit bracket readability**

Run:

```sh
git add frontend/src/components/bracket/EntryWorkspace.tsx frontend/src/bracket/BracketApp.test.tsx frontend/src/styles.css frontend/dist output/app
git commit -m "style: improve bracket entry readability"
```

Expected: commit succeeds unless generated `frontend/dist` is intentionally untracked. If `frontend/dist` is not tracked, omit it from `git add`.

## Task 7: Capture New React Screenshots And Complete Manual Ledger

**Files:**
- Modify: `docs/dashboard-cutover-reference/react-*.png`
- Modify: `docs/dashboard-cutover-reference/visual-acceptance-ledger.md`
- Modify: `docs/dashboard-cutover-reference/parity-matrix.md`

- [ ] **Step 1: Build the frontend**

Run:

```sh
cd frontend && npm run build
```

Expected: PASS and `frontend/dist/index.html`, `frontend/dist/technical.html`, and assets are updated.

- [ ] **Step 2: Sync or stage the app if needed**

If the repo output app is synced by R helpers in this worktree, run:

```sh
Rscript scripts/regenerate_and_sync_dashboards.R
```

Expected: `output/app/index.html`, `output/app/technical.html`, `output/app/assets/*`, and `output/app/dashboard_payloads.js` are present.

If this script requires runtime artifacts that are unavailable, use the Playwright staging flow from `e2e/frontend_visual.spec.cjs`; record the blocker in the ledger and continue with staged screenshots for visual review.

- [ ] **Step 3: Recapture React comparison screenshots**

Run:

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

Expected: eight React PNG files are refreshed under `docs/dashboard-cutover-reference/`.

- [ ] **Step 4: Inspect the screenshot pairs manually**

Open these pairs with `view_image` or local preview:

```text
docs/dashboard-cutover-reference/legacy-bracket-desktop-viewport.png
docs/dashboard-cutover-reference/react-bracket-desktop-viewport.png
docs/dashboard-cutover-reference/legacy-bracket-mobile-viewport.png
docs/dashboard-cutover-reference/react-bracket-mobile-viewport.png
docs/dashboard-cutover-reference/legacy-technical-desktop-viewport.png
docs/dashboard-cutover-reference/react-technical-desktop-viewport.png
docs/dashboard-cutover-reference/legacy-technical-mobile-viewport.png
docs/dashboard-cutover-reference/react-technical-mobile-viewport.png
```

Expected: React has equal or better hierarchy, no clipped primary text, no accidental wrapping, and the primary action workflow is visible without excessive scrolling.

- [ ] **Step 5: Update the ledger**

For every row in `docs/dashboard-cutover-reference/visual-acceptance-ledger.md`, change `Pending` to either:

```md
Pass
```

or:

```md
Fail
```

For each `Pass`, add one concrete note, for example:

```md
Pass | Candidate entry, status, metadata, and next action all fit in the first viewport; review affordances remain visible.
```

For each `Fail`, add one concrete note naming the fix, for example:

```md
Fail | Technical action summary still starts below the first viewport on desktop; reduce hero height or move orientation lower.
```

- [ ] **Step 6: Update the parity matrix only for passed rows**

In `docs/dashboard-cutover-reference/parity-matrix.md`, change a row to:

```md
Accepted after visual repair
```

only when its workflow has automated evidence and a `Pass` row in the ledger. Leave all other rows as:

```md
Pending visual repair
```

- [ ] **Step 7: Commit refreshed visual evidence**

Run:

```sh
git add docs/dashboard-cutover-reference
git commit -m "docs: refresh dashboard visual acceptance evidence"
```

Expected: commit succeeds unless commits are deferred.

## Task 8: Run The Full Verification Gate

**Files:**
- No code changes expected unless tests expose real defects.

- [ ] **Step 1: Run frontend unit tests**

Run:

```sh
cd frontend && npm test
```

Expected: PASS.

- [ ] **Step 2: Run frontend typecheck and build**

Run:

```sh
cd frontend && npm run typecheck && npm run build
```

Expected: PASS.

- [ ] **Step 3: Run Playwright workflow tests**

Run from repo root:

```sh
npm test
```

Expected: PASS.

- [ ] **Step 4: Refresh visual snapshots only if manual ledger passed**

Run from repo root:

```sh
npm test -- e2e/frontend_visual.spec.cjs --update-snapshots
```

Expected: PASS and snapshots update only after manual screenshot review is `Pass`.

- [ ] **Step 5: Run visual regression tests against refreshed snapshots**

Run:

```sh
npm run test:visual
```

Expected: PASS.

- [ ] **Step 6: Run R package tests**

Run:

```sh
Rscript tests/testthat.R
```

Expected: PASS. If this fails because local R packages are missing, record the exact missing package or error in the implementation report.

- [ ] **Step 7: Commit verification snapshot updates**

Run:

```sh
git add e2e/frontend_visual.spec.cjs-snapshots frontend/dist output/app
git commit -m "test: update accepted dashboard visual baselines"
```

Expected: commit succeeds if snapshots or built app files changed. If no files changed, do not create an empty commit.

## Task 9: Finish The React-Only Cutover

**Files:**
- Modify: `R/utils.R`
- Modify: `R/publishing.R`
- Modify: `scripts/publish_github_pages.R`
- Modify: `scripts/regenerate_and_sync_dashboards.R`
- Modify: `scripts/run_simulation.R`
- Modify: `tests/testthat/test_dashboard_payload_schema.R`
- Modify: `tests/testthat/test_pipeline_smoke.R`
- Modify: `tests/testthat/test_project_layout.R`
- Modify: `tests/testthat/test_documentation_links.R`
- Modify: `README.md`
- Modify: `docs/dashboard-frontend-architecture.md`
- Modify: `docs/methods-and-interpretation.md`
- Modify: `index.html`
- Delete: `output/bracket_dashboard.html`
- Delete: `output/technical_dashboard.html`

- [ ] **Step 1: Confirm visual gate is fully passed**

Run:

```sh
rg -n "\| Pending \||Fail|Pending visual repair" docs/dashboard-cutover-reference/visual-acceptance-ledger.md docs/dashboard-cutover-reference/parity-matrix.md
```

Expected: no ledger rows still contain `| Pending |`, no `Fail` rows are present, and no `Pending visual repair` rows remain in the parity matrix. If this command finds any match, stop and return to Task 5, Task 6, or Task 7.

- [ ] **Step 2: Confirm root monolith outputs are not required by tests**

Run:

```sh
rg -n "bracket_dashboard\\.html|technical_dashboard\\.html|model_comparison_dashboard\\.html" R scripts tests README.md docs index.html
```

Expected: references only mention removal, absence checks, or archived visual reference docs. Runtime scripts, publication manifests, and current-state docs must point to `output/app/index.html` and `output/app/technical.html`.

- [ ] **Step 3: Keep or complete existing cutover code**

Inspect existing edits in:

```sh
git diff -- R/utils.R R/publishing.R scripts/publish_github_pages.R scripts/regenerate_and_sync_dashboards.R scripts/run_simulation.R tests/testthat/test_dashboard_payload_schema.R tests/testthat/test_pipeline_smoke.R tests/testthat/test_project_layout.R tests/testthat/test_documentation_links.R
```

Expected: these files already mostly implement React-only app sync and root dashboard removal. Do not rewrite them unless tests fail.

- [ ] **Step 4: Remove tracked root monolith snapshots after gate**

Run:

```sh
git rm -f output/bracket_dashboard.html output/technical_dashboard.html
```

Expected: files are staged for deletion. Do this only after Step 1 passes.

- [ ] **Step 5: Update README current-state dashboard wording**

Ensure `README.md` says the dashboard entry points are:

```md
- `output/app/index.html` - bracket-entry workspace
- `output/app/technical.html` - technical diagnostics workspace
```

Do not use historical phrases such as `now supported`, `replaces`, `migrated`, or `legacy fallback`.

- [ ] **Step 6: Run cutover tests**

Run:

```sh
Rscript tests/testthat.R
npm test
npm run test:visual
```

Expected: all PASS.

- [ ] **Step 7: Commit final cutover**

Run:

```sh
git add R/utils.R R/publishing.R scripts/publish_github_pages.R scripts/regenerate_and_sync_dashboards.R scripts/run_simulation.R tests/testthat README.md docs index.html output/app
git add -u output/bracket_dashboard.html output/technical_dashboard.html e2e/bracket_tree.spec.cjs e2e/generate_fixture_dashboard.R
git commit -m "feat: finish react dashboard cutover"
```

Expected: commit succeeds.

## Task 10: Final Product Acceptance Report

**Files:**
- Modify: `docs/dashboard-cutover-reference/visual-acceptance-ledger.md`

- [ ] **Step 1: Fill final sign-off fields**

In `docs/dashboard-cutover-reference/visual-acceptance-ledger.md`, fill:

```md
## Final Sign-Off

- Functional tests passed: `npm test`
- Visual tests passed: `npm run test:visual`
- R package tests passed: `Rscript tests/testthat.R`
- Agent screenshot review passed: bracket desktop, bracket mobile, technical desktop, technical mobile
- User screenshot review passed: bracket desktop, bracket mobile, technical desktop, technical mobile
- Intentional deviations: none
```

If any command could not run, replace that line with the exact command and failure reason.

- [ ] **Step 2: Commit acceptance report**

Run:

```sh
git add docs/dashboard-cutover-reference/visual-acceptance-ledger.md
git commit -m "docs: record dashboard acceptance signoff"
```

Expected: commit succeeds unless no ledger fields changed.

- [ ] **Step 3: Final status check**

Run:

```sh
git status --short
```

Expected: clean worktree, or only intentionally uncommitted files called out in the final report.

## Self-Review

Spec coverage:

- Finished React-only product: Tasks 8 and 9.
- Do not give up wholesale: Decision section and Task 1 preserve useful current work.
- Visual parity: Tasks 1, 5, 6, and 7.
- Functional parity: Tasks 3, 4, 8, and 9.
- Maintainability: Tasks 2, 3, 4, and 5 introduce shared shells instead of more one-off monolith structure.
- No fallback/patchwork: Decision section and Task 9 keep legacy as reference only.
- User decision captured: visual target approved, with explicit emphasis on extensibility, robustness, functionality, user experience, and polished dashboard design.

Placeholder scan:

- No placeholder markers or vague deferred-work instructions remain.
- Every code-editing step includes exact code or exact selectors to change.
- Every verification step has commands and expected outcomes.

Type consistency:

- `DashboardHero` props match the component test.
- `DashboardSection` props match the component test and app usage.
- `technical-action-summary` test id is introduced before tests depend on it passing.
