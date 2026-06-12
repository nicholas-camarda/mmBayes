# Dashboard Frontend Architecture

Design document for the mmBayes dashboard frontend refactor. This document records the baseline audit, framework evaluation, target architecture, and migration/testing/compatibility/rollback plans. Legacy R-rendered dashboards remain the production surface until feature parity is validated.

---

## 1. Clean-start confirmation

| Item | Value |
|------|-------|
| Feature branch | `refactor/mmbayes-dashboard-frontend` |
| Baseline commit | `4ca78e0c4a628d3c6cc87e302f07d6479236b2d8` |
| Working tree | Clean (no uncommitted changes at branch creation) |

Task 1 created the feature branch from `master` at the baseline commit above. All subsequent work in this refactor proceeds from that isolated starting point.

---

## 2. Current-state audit

### 2.1 Dashboard generation call graph

**Full pipeline path** (authoritative run):

```
run_tournament_simulation()
  └─ save_results()
       └─ save_decision_outputs()
            └─ write_dashboard_outputs()
                 ├─ create_bracket_dashboard_html()   → bracket_dashboard.html
                 └─ create_technical_dashboard_html() → technical_dashboard.html
```

- `run_tournament_simulation()` (`R/main.R`) orchestrates fit → backtest → simulate → export and calls `save_results()` with the result bundle.
- `save_results()` (`R/utils.R:3108`) writes the `.rds` bundle, summaries, and delegates decision artifacts to `save_decision_outputs()`.
- `save_decision_outputs()` (`R/utils.R:2781`) builds the decision sheet, CSV/RDS artifacts, and calls `write_dashboard_outputs()`.
- `write_dashboard_outputs()` (`R/utils.R:2918–3001`) resolves model-quality context, then writes both HTML files via the two `create_*_dashboard_html()` renderers.

**Regenerate path** (re-render from cached bundle without re-fitting):

```
regenerate_dashboards_from_saved_results()
  └─ regenerate_dashboard_outputs_from_results()
       └─ write_dashboard_outputs()
            ├─ create_bracket_dashboard_html()
            └─ create_technical_dashboard_html()
       └─ sync_dashboard_html_files()   (when repo_output_dir is set)
```

- `regenerate_dashboards_from_saved_results()` (`R/utils.R:3076`) loads `tournament_sim.rds` from the runtime output directory.
- `regenerate_dashboard_outputs_from_results()` (`R/utils.R:3011`) reconstructs model-overview and play-in context from the saved bundle, re-renders dashboards, and optionally syncs HTML into the tracked repo `output/` directory via `sync_dashboard_html_files()`.

**Sync and manifest helpers:**

| Function | Location | Role |
|----------|----------|------|
| `dashboard_html_manifest()` | `R/utils.R:21` | Returns exactly `bracket_dashboard.html` and `technical_dashboard.html` |
| `sync_dashboard_html_files()` | `R/utils.R:36` | Copies manifest HTML files from runtime output to a destination (typically repo `output/`) |
| `write_dashboard_outputs()` | `R/utils.R:2918–3001` | Renders and writes both legacy HTML dashboards |
| `save_decision_outputs()` | `R/utils.R:2781` | Wraps CSV/RDS writes and calls `write_dashboard_outputs()` |
| `regenerate_dashboard_outputs_from_results()` | `R/utils.R:3011` | Re-renders from an in-memory results bundle |

### 2.2 HTML renderers

Dashboards are built by **pure string-paste R** — no htmltools, no embedded JSON. Data is baked into markup, inline styles, and `data-*` attributes.

| Renderer | Location | Approx. size |
|----------|----------|--------------|
| `create_technical_dashboard_html()` | `R/plotting_functions.R:3387–3881` | ~495 lines |
| `create_bracket_dashboard_html()` | `R/plotting_functions.R:3906–5671` | ~1,765 lines |

The retired `model_comparison_dashboard.html` is explicitly unlinked when present (`write_dashboard_outputs()` deletes it if found). `model_comparison_dashboard` is no longer part of the release or sync manifest.

### 2.3 Pipeline scripts

| Script | Role |
|--------|------|
| `scripts/run_simulation.R` | Full pipeline via `run_tournament_simulation()`, then `regenerate_dashboard_outputs_from_results()` with repo sync to `output/` |
| `scripts/regenerate_and_sync_dashboards.R` | Re-render from `tournament_sim.rds` via `regenerate_dashboards_from_saved_results()` + repo sync |
| `scripts/run_bracket_candidates.R` | Fast path: calls `save_decision_outputs()` directly (runtime-only, no full backtest) |
| `scripts/publish_github_pages.R` | Copy-only publish of tracked artifacts to GitHub Pages |
| `scripts/publish_release.R` | Copies `release_deliverable_manifest()` deliverables (`R/publishing.R:11–41`) to cloud release folders |

All five scripts are invoked as `Rscript scripts/<name>.R` and must remain behavior-compatible throughout this refactor.

### 2.4 Three directory roots

mmBayes uses three filesystem roots with distinct responsibilities:

| Root | Default path | Purpose |
|------|--------------|---------|
| **Code checkout** | `~/Projects/mmBayes` | Source code, tests, tracked `output/` HTML snapshots, GitHub Pages hub (`index.html`) |
| **Runtime** | `~/ProjectsRuntime/mmBayes` | Authoritative live run location: generated outputs, logs, model cache, runtime dashboards |
| **Cloud (OneDrive)** | `~/Library/CloudStorage/OneDrive-Personal/SideProjects/mmBayes` | Canonical input spreadsheets (`pre_tournament_team_features.xlsx`, `tournament_game_results.xlsx`) and published releases |

Path resolution is centralized in `R/configuration.R` (`default_runtime_root()`, `default_cloud_root()`, `default_runtime_output_root()`, etc.). Runtime `output/` is authoritative for live runs; tracked repo `output/` holds publishable HTML snapshots synced from runtime.

### 2.5 GitHub Pages surface

GitHub Pages serves the repository root:

- **Hub:** `index.html` — primary CTA links to `output/bracket_dashboard.html`
- **Bracket dashboard:** `output/bracket_dashboard.html` — bracket-entry decision workspace
- **Technical dashboard:** `output/technical_dashboard.html` — linked from README and the bracket dashboard appendix (not from the hub primary CTA)

`.gitignore` ignores `output/*` except `!output/*.html` (and `!output/.gitkeep`), so only HTML snapshots are tracked in the repo output directory.

### 2.6 Node tooling today

Root `package.json` contains only `@playwright/test`. `npm test` runs `e2e/bracket_tree.spec.cjs` against fixture HTML generated by `e2e/generate_fixture_dashboard.R`. No frontend build toolchain exists yet.

### 2.7 Payload and schema status

- **No dashboard payload, schema, or versioning exists anywhere** in the current codebase.
- `jsonlite` is already listed in `DESCRIPTION` Imports and is the natural choice for JSON emission.
- All dashboard state is embedded in HTML strings; there is no machine-readable contract between the R pipeline and any client renderer.

### 2.8 Existing test coverage

| Test file | Dashboard-related assertions |
|-----------|------------------------------|
| `test_pipeline_smoke.R` | Both HTML files written by full pipeline; regeneration path re-renders without re-fitting; repo sync copies exactly the two manifest files |
| `test_model_and_simulation.R` | Hundreds of HTML substring assertions against `create_bracket_dashboard_html()` and `create_technical_dashboard_html()` output (review queue, divergence map, confidence tiers, technical panels, etc.) |
| `test_project_layout.R` | `release_deliverable_manifest()` contents; `regenerate_dashboards_from_saved_results()` error when bundle missing |
| `test_documentation_links.R` | `index.html` links to `output/bracket_dashboard.html`; README documents bracket dashboard as entry point |

Additional Playwright coverage exists in `e2e/bracket_tree.spec.cjs` (bracket tree interactions against fixture HTML). None of the existing suites forbid adding `frontend/`, new output files, or new R modules — they constrain legacy HTML behavior and sync semantics.

---

## 3. Framework comparison

Evaluation criteria reflect mmBayes deployment constraints: static GitHub Pages hosting, offline `file://` bracket review during tournament week, reproducible pipeline output, and long-term maintainability as dashboard complexity grows.

| Criterion | R string-paste HTML (current) | R Shiny | Python Dash / Streamlit | Plain JavaScript | TypeScript + React + Vite |
|-----------|------------------------------|---------|-------------------------|------------------|---------------------------|
| GitHub Pages / static deployment | **Yes** — single HTML files | **No** — requires R server | **No** — requires Python server | **Yes** — static build | **Yes** — static build |
| Local bracket-review use | **Yes** — open HTML in browser | Partial — needs `shiny::runApp()` | Partial — needs server process | **Yes** | **Yes** |
| Offline usability (`file://`) | **Yes** | **No** — server dependency | **No** — server dependency | **Yes** (with data inlined or shim) | **Yes** (with `dashboard_payloads.js` shim) |
| Reproducibility (R pipeline owns data) | **Yes** — but data locked in HTML | **Yes** | Requires Python sidecar | Requires separate data pipeline | **Yes** — R emits versioned JSON payloads |
| Typed payload contracts | **No** — implicit in HTML | Possible but coupled to Shiny | Possible | **No** — no compile-time types | **Yes** — JSON Schema + TypeScript types |
| UI extensibility | **Poor** — 1,765-line string paste | Moderate | Moderate | Moderate | **Strong** — component model |
| Component testing | **Poor** — substring assertions only | Limited | Limited | Manual DOM tests | **Yes** — Vitest + Testing Library |
| End-to-end testing | **Yes** — Playwright on fixture HTML | Harder (server lifecycle) | Harder (server lifecycle) | **Yes** | **Yes** — Playwright on static build |
| Long-term maintainability | **Poor** — primary pain point | Moderate (R-only team) | Split stack | Moderate | **Strong** — typed, componentized, testable |

### Conclusions

1. **R Shiny and Python Dash/Streamlit are disqualified.** Each requires a live server process, which breaks GitHub Pages static hosting and offline `file://` bracket review — both non-negotiable for tournament-week usage.
2. **Plain JavaScript loses typed contracts and structured component testing.** Without TypeScript and a component framework, the same maintainability problems reappear at scale.
3. **Current R string-paste HTML is the problem being solved.** It works for deployment and offline use but embeds ~2,260 lines of intertwined markup, CSS, and data in `plotting_functions.R`, tested only via brittle substring assertions.
4. **Recommendation: TypeScript + React + Vite static build.** This is the only evaluated option that satisfies all nine criteria: static deployable output, offline `file://` support (via a window-global payload shim), R-owned reproducible payloads, typed contracts, component and e2e testing, and long-term UI extensibility.

---

## 4. Architecture outline

### 4.1 Division of responsibility

| Layer | Owner | Responsibility |
|-------|-------|----------------|
| Data & validation | **R** | Build dashboard payloads from existing pipeline structures; validate against JSON Schema; write JSON + JS shim alongside legacy HTML |
| Presentation | **TypeScript/React** | Consume validated payloads; render bracket and technical dashboards as static SPA pages |
| Legacy fallback | **R (unchanged)** | Continue generating `bracket_dashboard.html` and `technical_dashboard.html` until parity cutover |

R remains the **source of truth**. The frontend never recomputes model outputs — it renders what the pipeline emits.

### 4.2 Payload contract v1.0.0

- Each payload carries a required `dashboard_schema_version` field (const `"1.0.0"`).
- Two schemas: `bracket_dashboard_payload` and `technical_dashboard_payload`.
- R validation is **fail-closed**: missing required fields or version mismatches abort emission.
- TypeScript types in `frontend/src/types/payload.ts` mirror the schemas and guard on `dashboard_schema_version` at load time.

### 4.3 Schema location: `inst/schemas/`

JSON Schema files live at:

- `inst/schemas/bracket_dashboard_payload.schema.json`
- `inst/schemas/technical_dashboard_payload.schema.json`

**Deviation from planning spec:** An early draft suggested a top-level `schemas/` directory. We use `inst/schemas/` instead because it follows R package convention — schemas are resolvable via `system.file()` in both `pkgload::load_all()` development and installed-package contexts. No path hacks or working-directory assumptions are needed.

### 4.4 New R modules (planned)

| Module | Responsibility |
|--------|----------------|
| `R/dashboard_schema.R` | Schema path resolution (`dashboard_schema_path()`), fail-closed validation (`validate_dashboard_payload()`) |
| `R/dashboard_payloads.R` | Payload builders, JSON writers, `dashboard_payloads.js` shim generation |

`write_dashboard_outputs()` will be extended to emit validated payloads alongside legacy HTML (additive change only).

### 4.5 Payload artifacts

Written into the runtime output directory on each dashboard render:

| File | Scope | Purpose |
|------|-------|---------|
| `bracket_dashboard_payload.json` | Runtime only | Canonical machine-readable bracket payload |
| `technical_dashboard_payload.json` | Runtime only | Canonical machine-readable technical payload |
| `dashboard_payloads.js` | Synced to `app/` | Window-global shim for `file://` loading |

The JS shim format:

```javascript
window.__MMBAYES_PAYLOADS__ = { bracket: {…}, technical: {…} };
```

### 4.6 `file://` constraint and the window-global shim

Browsers block `fetch()` of local JSON files under the `file://` protocol (CORS / mixed-content restrictions). Because offline bracket review — opening dashboards directly from the filesystem without a web server — is a core use case, the frontend cannot rely on fetching sibling `.json` files.

**Solution:** R writes `dashboard_payloads.js` containing inlined payloads as a window-global assignment. The frontend loader (`loadPayloads.ts`) reads `window.__MMBAYES_PAYLOADS__` first, then falls back to `fetch()` when served over HTTP (GitHub Pages or local dev server).

### 4.7 Frontend layout and deployment paths

```
frontend/                          # Vite + React + TypeScript source
├── index.html                     # Bracket dashboard entry
├── technical.html                 # Technical dashboard entry
├── src/
│   ├── bracket/BracketApp.tsx
│   ├── technical/TechnicalApp.tsx
│   ├── components/*.tsx
│   ├── lib/loadPayloads.ts
│   └── types/payload.ts
└── dist/                          # Build output (gitignored)

{runtime}/output/app/              # Synced static frontend (authoritative live)
output/app/                        # Tracked repo snapshot (publishable)
```

- `sync_frontend_app()` (planned in `R/utils.R`) copies `frontend/dist/` into runtime `output/app/` and tracked `output/app/` when a build exists.
- Legacy dashboards remain at their existing URLs: `output/bracket_dashboard.html`, `output/technical_dashboard.html`.

### 4.8 What stays untouched in this phase

- `create_bracket_dashboard_html()` and `create_technical_dashboard_html()` — no removal or modification.
- `dashboard_html_manifest()` — still exactly two HTML filenames.
- `release_deliverable_manifest()` — unchanged in this phase.
- GitHub Pages hub — still points at legacy HTML until cutover.

---

## 5. Migration plan

Migration proceeds in three phases. Full feature parity (bracket tree SVG, divergence map, evidence drawers, watchlist, calibration charts, mobile nav — ~1,765 lines of bracket renderer logic) is **explicitly out of scope** for the foundation plan and is a separate follow-up effort.

### Phase 1 — Foundation (this plan)

1. JSON Schema files in `inst/schemas/`
2. R payload builders and fail-closed validation
3. Payload emission alongside legacy HTML in `write_dashboard_outputs()`
4. `frontend/` scaffold with foundational panels and graceful degradation for missing optional sections
5. `sync_frontend_app()` integration in `run_simulation.R` and `regenerate_and_sync_dashboards.R`
6. R schema tests, Vitest component tests, Playwright e2e over `file://` with fixture payloads

### Phase 2 — Per-panel parity ports

Port legacy dashboard panels one at a time into React components, validated against:

- Existing Playwright interaction tests (bracket tree, review queue, etc.)
- OpenSpec UX requirements in `openspec/specs/bracket-dashboard-decision-ux/spec.md`

Priority panels for bracket dashboard:

- Bracket tree SVG with candidate switching
- Divergence map
- Evidence drawers and review queue
- Watchlist
- Confidence tiers and mobile navigation

Priority panels for technical dashboard:

- Calibration charts
- Backtest summaries
- Ensemble/component diagnostics
- Live tournament performance

### Phase 3 — Cutover

Cutover happens **only after** parity validation:

1. Hub `index.html` primary CTA points at `output/app/` (or `output/app/index.html`)
2. Legacy renderers retired from the pipeline (separate change, not automatic)
3. `dashboard_html_manifest()` and release manifest updated to reflect new primary surface

Until Phase 3 completes, legacy dashboards are the production surface and the frontend is an opt-in preview.

---

## 6. Testing plan

### 6.1 R — payload schema and emission

New file: `tests/testthat/test_dashboard_payload_schema.R`

| Test | Assertion |
|------|-----------|
| Schema files exist | Both schemas declare `dashboard_schema_version` const `"1.0.0"` |
| Minimal valid payload | `validate_dashboard_payload()` accepts required fields only |
| Fail-closed on missing fields | Error names the missing required field |
| Version mismatch | Rejects `dashboard_schema_version` other than `"1.0.0"` |
| Optional sections | Absent optional fields do not fail validation |
| Emission integration | `write_dashboard_outputs()` writes JSON + JS shim alongside HTML |

Fixture generation script: `scripts/generate_dashboard_payload_fixtures.R` produces checked-in `tests/fixtures/dashboard_payload_*.json` for deterministic frontend and e2e tests.

### 6.2 Frontend — Vitest component tests

- Located in `frontend/src/**/*.test.tsx`
- Test foundational panels with fixture payloads
- Verify graceful degradation when optional payload sections are absent
- Schema-version guard rejects mismatched payloads

### 6.3 End-to-end — Playwright

- New: `e2e/frontend_app.spec.cjs` — loads built `frontend/dist/` over `file://` with fixture `dashboard_payloads.js`
- Existing: `e2e/bracket_tree.spec.cjs` — continues against legacy fixture HTML (unchanged)

### 6.4 Regression — existing suites stay green

No modifications to existing test assertions in:

- `test_pipeline_smoke.R`
- `test_model_and_simulation.R`
- `test_project_layout.R`
- `test_documentation_links.R`

Legacy HTML behavior, sync semantics, and release manifest contents must pass unchanged throughout the foundation phase.

---

## 7. Compatibility plan

### 7.1 Rscript commands unchanged

All five pipeline scripts retain their current CLI interface and primary behavior:

```bash
Rscript scripts/run_simulation.R
Rscript scripts/regenerate_and_sync_dashboards.R
Rscript scripts/run_bracket_candidates.R
Rscript scripts/publish_github_pages.R
Rscript scripts/publish_release.R
```

Payload emission and frontend sync are **additive** steps — they do not replace or reorder existing operations.

### 7.2 Legacy URLs preserved

| URL | Status |
|-----|--------|
| `output/bracket_dashboard.html` | Continues to be generated and synced |
| `output/technical_dashboard.html` | Continues to be generated and synced |
| `index.html` → `output/bracket_dashboard.html` | Unchanged until Phase 3 cutover |

### 7.3 Manifests unchanged in foundation phase

- `dashboard_html_manifest()` — still `bracket_dashboard.html`, `technical_dashboard.html`
- `release_deliverable_manifest()` — same deliverable list; frontend `app/` is not yet a release artifact

### 7.4 Frontend sync soft-skip

`sync_frontend_app()` checks for `frontend/dist/`. When absent (Node not installed or frontend not built):

- Logs an actionable message: build with `cd frontend && npm install && npm run build`
- Does **not** fail the R pipeline
- Node is never required for any R workflow

---

## 8. Rollback and fallback plan

The frontend refactor is **additive by design**. Legacy dashboards never stop being generated until explicit cutover.

### Immediate rollback (foundation phase)

To restore the pre-refactor state:

1. Delete `output/app/` and `frontend/`
2. Remove `R/dashboard_payloads.R` and `R/dashboard_schema.R`
3. Revert the `write_dashboard_outputs()` hunk that emits payloads
4. Revert `sync_frontend_app()` calls in pipeline scripts

Legacy HTML generation, sync, GitHub Pages links, and release publishing continue to work throughout — the new artifacts are optional additions.

### Runtime fallback during migration

| Scenario | Behavior |
|----------|----------|
| Frontend not built | Legacy HTML dashboards work as today |
| Payload validation fails | Pipeline aborts (fail-closed) — legacy HTML is not written with invalid data |
| Frontend built but incomplete | Legacy HTML remains the recommended entry point; `output/app/` is preview-only |
| Parity not yet validated | Hub, README, and release manifest point to legacy HTML |

### Cutover guardrails

Legacy renderers are retired only when:

1. OpenSpec UX spec (`openspec/specs/bracket-dashboard-decision-ux/spec.md`) requirements are met by the React frontend
2. Playwright e2e coverage passes for all ported panels
3. Explicit decision to update `index.html`, manifests, and documentation

Until then, deleting the frontend directory alone fully restores prior behavior with zero impact on production dashboards.

---

## References

- Implementation plan: `docs/superpowers/plans/2026-06-12-dashboard-frontend-refactor.md`
- UX spec: `openspec/specs/bracket-dashboard-decision-ux/spec.md`
- Project roots and commands: `AGENTS.md`, `CLAUDE.md`
