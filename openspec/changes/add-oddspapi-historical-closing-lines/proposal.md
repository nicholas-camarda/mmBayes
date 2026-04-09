## Why

mmBayes currently has no trustworthy multi-year historical betting-line archive for model evaluation, blending studies, or betting-feature ablation. We need a clean, reproducible way to build real historical `closing_lines.csv` files from an external provider without depending on the live runtime collector path or contaminating runtime or cloud canonical roots with fixture data.

OddsPapi appears to offer fixture-scoped historical bookmaker timelines that are sufficient to derive per-game closing consensus lines, and that is a materially better fit for the repo's current historical feature contract than continuing to rely on ad hoc live snapshots. This change is needed now because the repo's existing historical odds artifacts were shown to be synthetic test data rather than real archive data, and because the model-improvement question is specifically about pre-existing closing lines for completed tournament games, not live market polling.

## What Changes

- Add a historical-only OddsPapi ingestion workflow that derives tournament-game `closing_lines.csv` files for selected completed seasons.
- Add a provider-specific path that discovers NCAA tournament fixtures, fetches fixture-level historical bookmaker odds, and computes a closing consensus that matches the repo's historical betting-feature contract.
- Keep this workflow separate from the existing live Odds API collector and current-season `latest_lines_matchups.csv` path.
- Support a reduced bookmaker set of up to three books when required by the provider's historical endpoint limits.
- Read the OddsPapi credential from the repo-root `.env` file via a dedicated environment variable so the new historical path does not overload the existing Odds API key name.
- Define explicit name-matching, fixture-to-results reconciliation, and failure reporting rules so unmatched games and partial bookmaker coverage are visible rather than silently coerced into usable-looking data.
- Preserve the current optional-betting behavior: the main simulation pipeline must still run when historical betting data are absent or incomplete.
- Document that this change targets historical model-evaluation inputs, not live bracket-submission betting overlays.

## Capabilities

### New Capabilities
- `historical-betting-closing-lines`: Build real historical tournament `closing_lines.csv` artifacts from provider fixture histories and local completed-game records for use in backtests and betting-feature evaluation.

### Modified Capabilities

## Impact

- Affected R modules: [R/betting_lines.R](/Users/ncamarda/Projects/mmBayes/R/betting_lines.R), [R/data_loading.R](/Users/ncamarda/Projects/mmBayes/R/data_loading.R), [R/configuration.R](/Users/ncamarda/Projects/mmBayes/R/configuration.R)
- Affected scripts: [scripts/build_closing_lines.R](/Users/ncamarda/Projects/mmBayes/scripts/build_closing_lines.R) and likely a new historical import/backfill script
- Affected tests: betting evaluation, loading, and fixture-safety coverage under [tests/testthat](/Users/ncamarda/Projects/mmBayes/tests/testthat)
- Affected config: new provider-specific keys may be needed under `betting:` in [config.yml](/Users/ncamarda/Projects/mmBayes/config.yml), and the historical importer should default to reading `ODDSPAPI_API_KEY` from the repo-root `.env`, but no change should be required for the default winner/total-points production path
- Data and artifact boundaries: runtime and cloud roots must remain protected; historical imports should write only explicit odds-history artifacts and must not alter saved-results bundle assumptions, tracked repo snapshots, or release-manifest membership unless a future change explicitly expands publish scope
- Compatibility and rollback: this should be additive and reversible by disabling or ignoring the new historical provider path, without changing existing runtime output names or release contracts
