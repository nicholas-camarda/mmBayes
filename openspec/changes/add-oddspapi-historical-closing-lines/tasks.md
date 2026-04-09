## 1. Provider Discovery And Configuration

- [ ] 1.1 Add historical OddsPapi configuration keys for API access, target seasons, target bookmakers, and historical import paths, defaulting credential lookup to `ODDSPAPI_API_KEY` from the repo-root `.env`, without changing the existing live odds collector defaults
- [ ] 1.2 Implement a small provider discovery layer that resolves the target basketball sport, tournament, and season identifiers needed for NCAA historical imports
- [ ] 1.3 Add an authenticated preflight that reports accessible bookmakers, seasons, and endpoint limits before any backfill runs

## 2. Historical Import Pipeline

- [ ] 2.1 Implement fixture retrieval for completed tournament seasons using the provider's historical fixture workflow
- [ ] 2.2 Implement fixture-level historical odds retrieval with resumable raw caching and support for up to three configured bookmakers
- [ ] 2.3 Implement price normalization that can derive repo-native moneyline probabilities and spreads from provider-native historical responses without relying on legacy `oddsFormat` requests

## 3. Closing Line Derivation And Reconciliation

- [ ] 3.1 Implement selection of the last pre-game moneyline and spread entry for each configured bookmaker on each matched fixture
- [ ] 3.2 Implement fixture-to-results reconciliation that maps provider fixtures onto local tournament results and assigns `Year`, `round`, `region`, `game_index`, and canonical team labels
- [ ] 3.3 Implement direct writing of repo-compatible `closing_lines.csv` artifacts plus an import summary that exposes matched games, unmatched games, missing markets, and bookmaker coverage

## 4. Script Integration And Safety Boundaries

- [ ] 4.1 Add an explicit historical import/backfill entrypoint script that writes only under the configured odds-history root and never touches runtime dashboards, saved results bundles, or release publishing paths automatically
- [ ] 4.2 Ensure the historical path remains separate from the live Odds API collector and does not alter current-season `latest_lines_matchups.csv` behavior
- [ ] 4.3 Document operator-visible failure modes and guardrails for partial season coverage, missing `ODDSPAPI_API_KEY`, and unsafe output-path assumptions

## 5. Validation And Regression Coverage

- [ ] 5.1 Add unit tests for provider parsing, odds normalization, last-pre-game selection, and fixture-to-results reconciliation
- [ ] 5.2 Add integration coverage that loads imported `closing_lines.csv` through the existing historical betting-feature pipeline and verifies graceful behavior under partial bookmaker coverage
- [ ] 5.3 Run one authenticated one-season spike, record coverage and unresolved gaps, and update the change artifacts if NCAA historical access or field mappings differ from the current design assumptions
