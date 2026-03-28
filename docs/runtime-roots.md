# Runtime Roots

`mmBayes` uses a local checkout, a local scratch runtime, and a cloud project root so the canonical data and shared outputs do not overlap with the code tree.

## Roots

| Role | Path |
|------|------|
| Code | `~/Projects/mmBayes` |
| Runtime | `~/ProjectsRuntime/mmBayes` |
| Cloud project root | `~/Library/CloudStorage/OneDrive-Personal/SideProjects/mmBayes/` |
| Cloud data | `~/Library/CloudStorage/OneDrive-Personal/SideProjects/mmBayes/data/` |
| Cloud outputs | `~/Library/CloudStorage/OneDrive-Personal/SideProjects/mmBayes/output/` |
| Published releases | `~/Library/CloudStorage/OneDrive-Personal/SideProjects/mmBayes/releases/<YYYY-MM-DD>/` |

## Runtime Policy

- Active runs may still use `~/ProjectsRuntime/mmBayes` for scratch artifacts.
- The repository checkout is a local non-synced working tree.
- Cloud storage holds the canonical input data, shared outputs, and published release bundles.
- Release drops must be append-only and split into `deliverables/` and `data_snapshot/`.

## Publish Layout

Each dated release folder should contain:

- `deliverables/` for human-facing outputs such as dashboards, CSV summaries, and text summaries.
- `data_snapshot/` for the odds-history and other scraped inputs used to produce the release.

The publish command is `Rscript scripts/publish_release.R`. It fails closed if a deliverable is missing or if the dated publish folder already exists.
