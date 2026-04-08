## ADDED Requirements

### Requirement: Historical provider import SHALL generate repo-compatible closing line artifacts
The system SHALL support an explicit historical betting-line import workflow that generates `closing_lines.csv` files compatible with the existing historical betting-feature loader for completed NCAA tournament seasons.

#### Scenario: Imported closing lines load through the existing historical feature path
- **WHEN** a historical OddsPapi import completes for one or more target seasons
- **THEN** the workflow SHALL write `closing_lines.csv` files under the configured odds-history root using the column contract required by the historical betting-feature loader
- **THEN** `load_historical_closing_lines()` and `build_historical_betting_feature_table()` SHALL be able to consume the resulting files without requiring live snapshot archives

### Requirement: Historical provider credentials SHALL load from the repo-root environment file
The historical OddsPapi workflow SHALL use the repo's existing `.env` loading convention and SHALL default to reading a dedicated `ODDSPAPI_API_KEY` environment variable rather than reusing the live Odds API key name.

#### Scenario: Historical import authenticates without introducing a new secret-storage path
- **WHEN** a user configures historical OddsPapi access for the repository
- **THEN** the importer SHALL resolve its default credential from `ODDSPAPI_API_KEY`
- **THEN** the expected storage location for that secret SHALL be the repo-root `.env` file already used by project scripts
- **THEN** the workflow SHALL not require storing provider secrets under runtime or cloud artifact roots

### Requirement: Historical fixture histories SHALL be reconciled against local tournament results
The import workflow SHALL treat the local tournament results workbook as the source of truth for season, round, and canonical matchup identity when mapping provider fixtures into historical closing-line rows.

#### Scenario: Provider fixture data is matched to local round metadata
- **WHEN** provider historical odds are fetched for a completed tournament fixture
- **THEN** the importer SHALL reconcile that fixture to the corresponding local completed-game row using canonical team identity
- **THEN** the written closing-line row SHALL carry the local `Year`, `round`, and team pairing needed for repo-native historical joins

### Requirement: Closing consensus SHALL be derived from the last pre-game bookmaker entries
The import workflow SHALL derive closing lines from the most recent pre-game odds entry available for each configured bookmaker and SHALL compute a consensus from the configured bookmaker set.

#### Scenario: A game has multiple bookmaker histories before tip-off
- **WHEN** the importer processes historical odds for a matched fixture with multiple timestamped bookmaker entries
- **THEN** it SHALL select the last pre-game entry for each configured bookmaker
- **THEN** it SHALL compute the output closing probability and spread fields from those final pre-game bookmaker values

### Requirement: Historical import SHALL support reduced bookmaker coverage without silent success
The import workflow SHALL support a configured bookmaker set of up to three books and SHALL surface missing bookmaker or market coverage explicitly when full coverage is unavailable.

#### Scenario: Only a subset of configured books or line types is available
- **WHEN** a matched historical fixture is missing one or more configured bookmakers or lacks either moneyline or spread history
- **THEN** the importer SHALL still write the game row when the minimum required closing-line fields can be derived
- **THEN** the importer SHALL record the actual bookmaker count and missing-coverage status in the generated artifact summary or logs

### Requirement: Historical import SHALL remain separate from live betting workflows
The system SHALL keep provider-based historical closing-line ingestion separate from live current-season odds collection and SHALL not require live betting data for bracket-submission workflows.

#### Scenario: Historical backfill runs without changing live collector behavior
- **WHEN** a user runs the historical OddsPapi import workflow
- **THEN** the existing live collector and `latest_lines_matchups.csv` workflow SHALL remain unchanged
- **THEN** the main simulation pipeline SHALL continue to run when live betting is disabled or absent

### Requirement: Historical import SHALL expose reconciliation and coverage failures
The import workflow SHALL make it possible to identify which seasons, fixtures, or matchups were missing, unmatched, or only partially covered rather than silently degrading those rows into neutral-looking historical features.

#### Scenario: A historical season imports with incomplete fixture coverage
- **WHEN** one or more completed tournament games cannot be matched to provider fixtures or cannot produce valid closing-line fields
- **THEN** the workflow SHALL report the affected games and the reason they failed
- **THEN** the import result SHALL make the season-level matched-game count and unmatched-game count visible to the operator
