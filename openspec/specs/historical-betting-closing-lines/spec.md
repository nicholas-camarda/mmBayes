## ADDED Requirements

### Requirement: Historical provider import SHALL generate repo-compatible closing line artifacts
The system SHALL support an explicit historical betting-line import workflow that generates `closing_lines.csv` files compatible with the existing historical betting-feature loader for NCAA tournament seasons that are still eligible under the public OddsPapi v4 retention window.

#### Scenario: Imported closing lines load through the existing historical feature path
- **WHEN** a historical OddsPapi import completes for one or more target seasons
- **THEN** the workflow SHALL write `closing_lines.csv` files under the configured odds-history root using the column contract required by the historical betting-feature loader
- **THEN** `load_historical_closing_lines()` and `build_historical_betting_feature_table()` SHALL be able to consume the resulting files without requiring live snapshot archives

### Requirement: Historical import SHALL default to the forward-only v4 archive window
The system SHALL treat public OddsPapi v4 historical odds as a short-retention source and SHALL default the supported archive workflow to `2026+` seasons that remain inside the configured retention window.

#### Scenario: A user runs the importer without manually forcing years
- **WHEN** the historical importer is run with default configuration
- **THEN** it SHALL resolve eligible import seasons from the configured archive start year and retention window rather than attempting unsupported long-range backfills
- **THEN** it SHALL document and skip stale pre-archive seasons or future seasons explicitly

### Requirement: Historical provider credentials SHALL load from the repo-root environment file
The historical OddsPapi workflow SHALL use the repo's existing `.env` loading convention and SHALL default to reading a dedicated `ODDS_PAPI_API_KEY` environment variable rather than reusing the live Odds API key name.

#### Scenario: Historical import authenticates without introducing a new secret-storage path
- **WHEN** a user configures historical OddsPapi access for the repository
- **THEN** the importer SHALL resolve its default credential from `ODDS_PAPI_API_KEY`
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

### Requirement: Missing historical betting seasons SHALL degrade gracefully in modeling
The system SHALL continue to fit and evaluate models when some historical seasons have no archived closing lines, and SHALL mark those seasons as unavailable rather than treating them as weak real lines.

#### Scenario: Training seasons precede the supported archive window
- **WHEN** historical matchup rows are built for seasons without imported closing lines
- **THEN** the betting feature columns SHALL fall back to neutral numeric defaults only after availability is marked absent
- **THEN** `betting_line_available` SHALL be `0`
- **THEN** those seasons SHALL remain usable in the broader modeling pipeline

### Requirement: Historical import SHALL replace the launchd live collector as the supported betting workflow
The system SHALL treat explicit historical closing-line import as the supported betting-line workflow for this repository and SHALL retire the launchd-based live collector path from supported usage.

#### Scenario: A user follows the supported betting workflow
- **WHEN** a user runs the historical OddsPapi import workflow
- **THEN** the repository SHALL direct the user toward historical import rather than launchctl/live polling
- **THEN** the main simulation pipeline SHALL continue to run when live betting is disabled or absent
- **THEN** deprecated live-collector entrypoints and guidance SHALL be removed or clearly marked unsupported

### Requirement: Repository documentation SHALL reflect the historical-only betting contract
The system SHALL update repository documentation and operator guidance so they reflect historical OddsPapi import, dedicated repo-root `.env` credentials, and the retirement of the launchd collector workflow.

#### Scenario: A contributor reads betting-related docs after the change
- **WHEN** a contributor consults README or script-level guidance for betting-line support
- **THEN** the docs SHALL describe historical closing-line import as the primary supported path
- **THEN** the docs SHALL not present `install_odds_collector_launchd.R`, `run_odds_collector.R`, or `uninstall_odds_collector_launchd.R` as current recommended setup
- **THEN** the docs SHALL describe how missing historical betting data degrades gracefully in the model pipeline

### Requirement: Historical import SHALL expose reconciliation and coverage failures
The import workflow SHALL make it possible to identify which seasons, fixtures, or matchups were missing, unmatched, or only partially covered rather than silently degrading those rows into neutral-looking historical features.

#### Scenario: A historical season imports with incomplete fixture coverage
- **WHEN** one or more completed tournament games cannot be matched to provider fixtures or cannot produce valid closing-line fields
- **THEN** the workflow SHALL report the affected games and the reason they failed
- **THEN** the import result SHALL make the season-level matched-game count and unmatched-game count visible to the operator

### Requirement: Historical import SHALL attempt multiple recovery paths before classifying games as missing
The import workflow SHALL make a good-faith attempt to recover the full tournament set before concluding that missing games reflect provider limitations.

#### Scenario: Tournament coverage is incomplete on the first pass
- **WHEN** an initial import pass fails to recover all expected games for a target season
- **THEN** the workflow SHALL try additional recovery strategies such as broader fixture discovery windows, alternative verified bookmaker sets, and repeated fixture-to-results reconciliation
- **THEN** the workflow SHALL distinguish between games recovered by those additional passes and games that remain unavailable after recovery attempts

### Requirement: External provider coverage gaps SHALL be documented without stopping usable imports
The import workflow SHALL continue to write usable historical artifacts when some games remain unavailable after the planned recovery attempts, and SHALL classify those residual gaps as external provider limitations in operator-visible reporting.

#### Scenario: Some games remain unavailable after recovery attempts
- **WHEN** one or more target games still cannot be recovered after the configured recovery strategies have been exhausted
- **THEN** the workflow SHALL continue with the recoverable `closing_lines.csv` rows for that season
- **THEN** the generated summary or logs SHALL identify the residual missing games as external coverage limitations rather than silent omissions
- **THEN** the workflow SHALL not represent those provider-side misses as a successful full-coverage import

### Requirement: Historical betting impact SHALL be evaluated at the bracket-selection level for the 2026 tournament
The system SHALL include an explicit evaluation that compares baseline bracket selection against historical-betting-feature bracket selection for bracket year 2026 and SHALL report whether historical betting features improved, did not improve, or worsened bracket outcomes.

#### Scenario: A completed 2026 tournament is used to evaluate bracket-selection impact
- **WHEN** imported historical closing lines are available for the target backfill window and 2026 actual tournament results are present in the local results workbook
- **THEN** the workflow SHALL fit one baseline model using only the core matchup predictors and one enhanced model using the core predictors plus the betting matchup feature columns
- **THEN** both models SHALL use the same engine, seed, draw budget, and candidate-generation settings
- **THEN** both models SHALL run without live 2026 betting lines so the comparison isolates the effect of historical betting features in training
- **THEN** the workflow SHALL generate and score the deterministic reference bracket and the standard candidate bracket set for both models against the actual 2026 tournament results
- **THEN** the evaluation output SHALL report bracket score, correct picks, champion correctness, and the changed decisions between the baseline and enhanced candidates
- **THEN** the evaluation SHALL explicitly state whether the enhanced model improved, matched, or underperformed the baseline on the 2026 bracket example

#### Scenario: Archived closing lines exist only for the current bracket year
- **WHEN** the repository has current-year closing lines but no archived closing lines for the model's historical training seasons
- **THEN** the evaluation SHALL not present that comparison as a real estimate of historical betting-signal value
- **THEN** it SHALL write an operator-facing note explaining that betting data were available only outside the training seasons
