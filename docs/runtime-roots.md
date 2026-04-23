# Runtime Roots

`mmBayes` uses a local source checkout, a configured runtime root, and a configured synced project home so the canonical data and published release bundles do not overlap with the code tree.

## Roots

| Role | Path |
|------|------|
| Code | local source checkout |
| Runtime root | configured runtime root |
| Runtime output | configured runtime output directory (`output/` under the runtime root) |
| Synced project home | configured synced project home |
| Synced data | `data/` under the synced project home |
| Synced outputs | `output/` under the synced project home |
| Published releases | `releases/<YYYY-MM-DD>/` under the synced project home |

## Runtime Policy

- Active runs use the configured runtime root for scratch artifacts.
- The repository checkout is a local non-synced working tree.
- The configured synced project home holds the canonical input data and published release bundles.
- By default, canonical input workbooks live at `data/pre_tournament_team_features.xlsx` and `data/tournament_game_results.xlsx` under the synced project home.
- Live generated outputs belong in the configured runtime output directory during active runs.
- Release drops must be append-only and contain only approved human-facing deliverables plus a manifest.

## Publish Layout

Each dated release folder should contain:

- `deliverables/` for human-facing outputs such as dashboards, CSV summaries, and text summaries.
- a plain-text release manifest with the release date, source runtime output directory, and copied deliverable list.

The publish command is `Rscript scripts/publish_release.R`. It copies approved deliverables from the configured runtime output directory into `releases/<YYYY-MM-DD>/deliverables` under the configured synced project home, writes a plain-text release manifest, and fails closed if a deliverable is missing or if the dated publish folder already exists. Runtime artifacts outside the deliverable manifest are outside the release contract.
