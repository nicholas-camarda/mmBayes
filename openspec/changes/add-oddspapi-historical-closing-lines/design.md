## Context

mmBayes currently has two betting-line paths:

- a live/current snapshot path that fetches provider odds, normalizes them into `lines_long.csv` and `lines_matchups.csv`, and optionally overlays current matchup probabilities
- a historical path that consumes `closing_lines.csv` to augment historical matchup rows for backtests and betting-feature ablation

The critical repo fact is that historical model evaluation does not require the full live snapshot archive. Historical betting features are loaded from `closing_lines.csv` and normalized by `build_historical_betting_feature_table()` in [R/betting_lines.R](/Users/ncamarda/Projects/mmBayes/R/betting_lines.R#L877). That means the shortest useful design is a historical importer that writes trustworthy `closing_lines.csv` files directly, rather than first recreating the entire live collector pipeline.

OddsPapi's public documentation suggests a feasible historical flow:

- discover tournaments and fixtures
- fetch fixture-scoped historical bookmaker odds
- derive final pre-game moneyline/spread consensus
- reconcile those fixtures to local NCAA tournament results

The downloaded AI bundle and OpenAPI spec are v5-first, while the simpler public docs still expose v4 endpoints. Public v4 docs do not demonstrate retirement, but they do carry warning banners and evolving response-format notes. This change therefore needs a provider boundary that lets the initial implementation target the simpler public historical surface without hard-coding the repo forever to a brittle version assumption.

## Goals / Non-Goals

**Goals:**

- Generate real historical `closing_lines.csv` files for completed NCAA tournament seasons using OddsPapi fixture histories.
- Reuse the repo's existing historical betting-feature loading and evaluation path without requiring a live runtime collector rewrite.
- Support a reduced set of up to three bookmakers when provider historical endpoint limits require it.
- Make unmatched fixtures, missing books, and missing line types observable in logs and output summaries instead of silently producing zero-like features.
- Keep runtime, cloud canonical, and release boundaries explicit so historical imports only write intended odds-history artifacts.
- Choose an initial provider integration path that works with the current public docs, while minimizing lock-in if OddsPapi moves users toward v5.

**Non-Goals:**

- Replacing the current live Odds API collector or adding live OddsPapi polling.
- Using closing lines for current-season bracket submission decisions; this change is for historical model evaluation and betting-feature studies.
- Building a general multi-provider abstraction across every betting workflow in one step.
- Publishing historical odds artifacts as new release deliverables by default.
- Depending on WebSocket, B2B streaming, or other live OddsPapi capabilities.

## Decisions

### 1. Historical integration will target `closing_lines.csv` directly

Decision:
- The first implementation writes historical `closing_lines.csv` directly instead of reproducing `lines_long.csv`, `lines_matchups.csv`, and the live snapshot collector for past seasons.

Rationale:
- The repo's historical path only consumes `closing_lines.csv` through [R/data_loading.R](/Users/ncamarda/Projects/mmBayes/R/data_loading.R#L509) and [R/betting_lines.R](/Users/ncamarda/Projects/mmBayes/R/betting_lines.R#L877).
- Replaying the full live snapshot archive adds complexity without improving historical backtests if the final closing table is already available.
- This keeps the scope aligned with the user's real goal: evaluate whether historical betting lines improve predictive performance.

Alternatives considered:
- Reconstruct full historical `lines_long.csv` and `lines_matchups.csv` archives.
  Rejected for the first change because it is materially more work and not required by the historical modeling contract.
- Modify the current live collector to operate in a historical replay mode.
  Rejected because the collector is built around current-season timing, quotas, and live schedule windows.

### 2. Historical OddsPapi credentials will be loaded from the repo-root `.env` via a dedicated env var

Decision:
- The historical importer will authenticate using a dedicated environment variable, defaulting to `ODDSPAPI_API_KEY`, and will rely on the repo's existing `.env` loading pattern rather than introducing a separate secrets file or cloud-stored credential path.

Rationale:
- The repo already loads environment variables from a repo-root `.env` in script entrypoints.
- Keeping the OddsPapi key in the code checkout avoids contaminating runtime and cloud roots with secrets.
- A dedicated variable name keeps the new historical path separate from the existing `ODDS_API_KEY` used by the live Odds API collector.

Alternatives considered:
- Reuse `ODDS_API_KEY` for both providers.
  Rejected because it creates ambiguous operator behavior and complicates side-by-side provider support.
- Store the key under runtime or cloud project roots.
  Rejected because those roots are for artifacts and shared data, not secrets.

### 3. The initial provider target is OddsPapi v4-style historical endpoints, with version-isolation at the adapter boundary

Decision:
- The initial design assumes a historical importer built around the simpler public fixture and historical-odds endpoints, but isolates provider parsing so response evolution does not leak into the rest of the repo.

Rationale:
- The public v4 docs expose the needed concepts directly: tournaments, fixtures, odds, historical odds, and bookmaker catalogs.
- The downloaded AI docs and OpenAPI are v5-first, which is useful for schema awareness but not enough on their own to prove public self-serve coverage for this exact use case.
- A version-isolated adapter lets us ship against the most practical documented surface now while retaining a migration path if v4 drifts or v5 becomes the only viable surface.

Alternatives considered:
- Build directly against the v5 B2B/AI schema only.
  Rejected for the first change because the user wants to avoid unnecessary B2B/live complexity and the repo only needs historical closing data.
- Wait for explicit provider retirement guidance before designing anything.
  Rejected because the repo can still prepare a clean adapter contract and gated spike workflow now.

### 4. Historical import will be fixture-centric, then reconciled against local NCAA results to assign round metadata

Decision:
- Provider fixture discovery determines what bookmaker history exists, but local `tournament_game_results.xlsx` remains the source of truth for `Year`, `round`, `region`, `game_index`, and canonical team pairing labels.

Rationale:
- The repo's historical joins key on `Year + round + matchup_key` in [R/betting_lines.R](/Users/ncamarda/Projects/mmBayes/R/betting_lines.R#L889) and [R/betting_lines.R](/Users/ncamarda/Projects/mmBayes/R/betting_lines.R#L985).
- Provider fixtures do not naturally encode repo-native round labels like `Round of 64`, `Sweet 16`, or `Elite 8`.
- Using local results as the round source avoids provider-specific tournament-stage assumptions and keeps the artifact aligned with the rest of the pipeline.

Alternatives considered:
- Infer rounds from provider tournament metadata or timestamps alone.
  Rejected because it is harder to verify and less aligned with the repo's existing historical join contract.

### 5. Closing consensus will be computed from the last pre-game odds entry per bookmaker, with support for up to three books

Decision:
- For each matched fixture, the importer will choose the most recent pre-game entry for each configured bookmaker, then compute consensus probability, spread, bookmaker count, and dispersion from the available books.

Rationale:
- This most closely matches the repo's current notion of closing lines in [scripts/build_closing_lines.R](/Users/ncamarda/Projects/mmBayes/scripts/build_closing_lines.R#L131).
- The user explicitly accepted a three-book limit if required by the provider.
- Dispersion features remain meaningful only when per-book values are preserved before consensus.

Alternatives considered:
- Use a provider CLV/opening-vs-closing summary endpoint if available.
  Rejected for the first pass because the repo needs actual closing probabilities and spreads in its native contract, not just line-movement metadata.
- Use a single sharp bookmaker only.
  Rejected as the default because it discards the repo's bookmaker-count and dispersion features, though it remains a fallback option if coverage is sparse.

### 6. Odds-format handling must normalize from provider-native prices rather than assuming legacy `oddsFormat=american`

Decision:
- The importer will accept provider-native responses that include decimal and, when available, American-formatted prices, and normalize them into the repo's closing-line schema without requiring the old `oddsFormat` request contract.

Rationale:
- The current repo's live normalizer assumes American odds in [R/betting_lines.R](/Users/ncamarda/Projects/mmBayes/R/betting_lines.R#L324).
- The v4 warning banner shown by the user indicates `oddsFormat` can no longer be relied on as a stable response contract.
- Historical ingestion should depend on normalized output fields, not on a request parameter that may drift.

Alternatives considered:
- Continue requiring American-only responses end to end.
  Rejected because it creates unnecessary coupling to a provider format flag already called out as unstable.

### 7. Historical import remains an explicit sidecar workflow with hard output boundaries

Decision:
- Historical import will run as an explicit backfill/build step under the odds-history root and will not write model bundles, runtime dashboards, or release deliverables automatically.

Rationale:
- The repo already treats betting as optional and sidecar-only.
- This reduces the blast radius of provider issues and keeps rollback simple: remove or ignore imported `closing_lines.csv` artifacts.
- It aligns with the recent need to keep test, runtime, and cloud odds-history boundaries safe and explicit.

Alternatives considered:
- Automatically integrate historical import into `run_simulation.R` or `update_data.R`.
  Rejected because provider dependence and long-running backfill logic should remain opt-in until proven stable.

## Risks / Trade-offs

- [OddsPapi NCAA coverage is weaker than expected] -> Mitigation: treat authenticated coverage discovery as a first-class spike gate; fail with explicit year/fixture coverage reporting rather than partial silent success.
- [v4 public docs diverge from actual responses] -> Mitigation: isolate provider parsing behind a dedicated adapter and normalize into repo-native closing-line fields before touching downstream code.
- [Historical endpoint request fan-out is too high for free-tier limits] -> Mitigation: start with one season and up to three books, cache fixture-level raw responses, and support resumable backfill by year.
- [Provider prices use a different market/outcome structure than the current Odds API assumptions] -> Mitigation: historical importer writes `closing_lines.csv` directly instead of forcing everything through the existing `h2h`/`spreads` raw snapshot summarizer.
- [Name matching between provider fixtures and local results is imperfect] -> Mitigation: use canonical team-name helpers and require explicit unmatched-game reporting with manual review hooks.
- [Partial coverage produces misleadingly neutral model features] -> Mitigation: mark line availability explicitly and make bookmaker count, dispersion, and unmatched-rate summaries visible in the import report.
- [Users misinterpret this as support for live betting overlays] -> Mitigation: document historical-only scope in config, scripts, and proposal/spec language; keep live collector untouched in this change.

## Migration Plan

1. Add a historical-only provider configuration surface for OddsPapi credentials, target bookmakers, target seasons, and import paths, defaulting the credential lookup to `ODDSPAPI_API_KEY` loaded from the repo-root `.env`.
2. Implement fixture discovery and raw historical odds retrieval for one authenticated season as a spike path.
3. Build reconciliation logic from provider fixtures to local NCAA result rows and generate one-year `closing_lines.csv`.
4. Validate schema compatibility by loading the generated file through the existing historical betting-feature path and running the ablation workflow.
5. Expand to additional seasons only after coverage, rate-limit cost, and matchup reconciliation are acceptable.
6. Keep rollback simple: disable the historical provider path and remove imported `closing_lines.csv` files if the backfill proves unreliable.

Rollback:
- No runtime or publish contract changes are required for rollback.
- Existing simulation behavior remains unchanged when historical betting files are absent.

## Open Questions

- Which exact OddsPapi `sportId` and `tournamentId` values represent the men's NCAA tournament seasons we care about?
- Does self-serve access actually include the historical bookmaker coverage needed for the target seasons, or only recent seasons?
- Which three bookmakers give the best combination of NCAA coverage and modeling value: broad U.S. books, sharp books, or a hybrid set?
- Are main-line spread and moneyline histories available consistently enough to derive both implied probability and spread features for most games?
- Should the importer write only `closing_lines.csv`, or also persist raw fixture-level response caches under the odds-history year directory for reproducibility?
