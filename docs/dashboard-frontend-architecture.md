# Dashboard Frontend Architecture

The mmBayes dashboard surface is the static React app under `output/app/`.
R owns model fitting, simulation, decision artifacts, payload construction, and
schema validation. React owns browser rendering for both dashboard entry points:

- `output/app/index.html` - bracket-entry workspace
- `output/app/technical.html` - technical diagnostics workspace

## Data Contract

The R pipeline writes three dashboard payload artifacts into the configured
runtime output directory:

- `bracket_dashboard_payload.json`
- `technical_dashboard_payload.json`
- `dashboard_payloads.js`

The JSON payloads validate against the schemas in `inst/schemas/`. The
`dashboard_payloads.js` shim exposes the same payloads as a window global so the
static app works over `file://` and GitHub Pages without a server.

## Runtime And Publication Flow

`write_dashboard_outputs()` writes validated payload artifacts only. It removes
retired root dashboard HTML files from the runtime output directory when they
exist.

`sync_frontend_app()` copies `frontend/dist/` plus the runtime
`dashboard_payloads.js` shim into:

- `{runtime output}/app/`
- `output/app/` in the repository when a repo output directory is supplied

Dashboard publication requires a built frontend. If `frontend/dist/` is missing,
publication fails with an actionable error.

## Release Contract

`release_deliverable_manifest()` publishes `app/` as the only dashboard
deliverable. The app directory must contain:

- `index.html`
- `technical.html`
- `dashboard_payloads.js`

CSV, TXT, cache, log, and RDS artifacts remain governed by the runtime and
release artifact boundaries documented in the README and `docs/runtime-roots.md`.
