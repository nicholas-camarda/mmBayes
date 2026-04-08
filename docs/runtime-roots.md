# Runtime Roots

`mmBayes` uses a local checkout, a local scratch runtime, and a cloud project root so the canonical data and published release bundles do not overlap with the code tree.

## Roots

| Role | Path |
|------|------|
| Code | the local source checkout |
| Runtime | the configured runtime output directory |
| Synced project root | the configured synced project home |
| Canonical data | `data/` |
| Published outputs | `output/` |
| Published releases | `releases/<YYYY-MM-DD>/` |

## Runtime Policy

- Active runs may still use the configured runtime root for scratch artifacts.
- The repository checkout is a local non-synced working tree.
- Cloud storage holds the canonical input data and published release bundles.
- Live generated outputs belong in `output/` under the runtime root during active runs.
- Release drops must be append-only and split into `deliverables/` and `data_snapshot/`.

## Publish Layout

Each dated release folder should contain:

- `deliverables/` for human-facing outputs such as dashboards, CSV summaries, and text summaries.
- `data_snapshot/` for the odds-history and other scraped inputs used to produce the release.

The publish command is `Rscript scripts/publish_release.R`. It fails closed if a deliverable is missing or if the dated publish folder already exists.
