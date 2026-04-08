## ADDED Requirements

### Requirement: Current-season archive completeness SHALL define operational success
The system SHALL define operational success for the supported betting-line workflow as a complete current-season `closing_lines.csv` archive for the target tournament year, where completeness is measured against the set of completed games present in the local tournament results workbook for that same year.

#### Scenario: A current-season archive is complete
- **WHEN** the importer finishes for a supported target season in the forward archive window
- **THEN** the workflow SHALL compare the written `closing_lines.csv` rows against the completed-game rows in the local results workbook for that season
- **THEN** the season SHALL only be labeled successful when every completed local game has a corresponding closing-line row with usable betting values

#### Scenario: A current-season archive is only partial
- **WHEN** the importer writes a usable but incomplete `closing_lines.csv` for the current supported season
- **THEN** the workflow SHALL preserve that artifact
- **THEN** it SHALL classify the season as partial or provider-limited rather than successful
- **THEN** the missing games and missing-value reasons SHALL remain visible to the operator

### Requirement: Complete current-year closing lines SHALL support a retrospective bracket-value study
The system SHALL define a separate retrospective evaluation that asks whether a complete set of current-year closing lines would have improved bracket prediction for that same tournament year, and SHALL label that study as non-deployable current-year hindsight rather than pre-tournament prediction.

#### Scenario: The repository has complete current-year closing lines
- **WHEN** the supported current tournament year has complete closing-line coverage for its completed games
- **THEN** the workflow SHALL run an explicit bracket-impact analysis using that complete current-year closing-line set
- **THEN** the resulting report SHALL state whether those complete current-year lines improved, matched, or worsened bracket outcomes relative to the baseline comparison being tested
- **THEN** the report SHALL explicitly identify the analysis as a retrospective upper-bound or hindsight study rather than a deployable bracket-submission workflow

## MODIFIED Requirements

### Requirement: Historical provider import SHALL generate repo-compatible closing line artifacts
The system SHALL support an explicit historical betting-line import workflow that generates clean, non-synthetic `closing_lines.csv` files compatible with the existing historical betting-feature loader for NCAA tournament seasons that are still eligible under the public OddsPapi v4 retention window.

#### Scenario: Imported closing lines load through the existing historical feature path
- **WHEN** a historical OddsPapi import completes for one or more target seasons
- **THEN** the workflow SHALL write `closing_lines.csv` files under the configured odds-history root using the column contract required by the historical betting-feature loader
- **THEN** `load_historical_closing_lines()` and `build_historical_betting_feature_table()` SHALL be able to consume the resulting files without requiring live snapshot archives
- **THEN** the importer SHALL not treat a stale, synthetic, or all-missing artifact as a valid completed season archive

### Requirement: Historical import SHALL default to the forward-only v4 archive window
The system SHALL treat public OddsPapi v4 historical odds as a short-retention source and SHALL default the supported archive workflow to `2026+` seasons that remain inside the configured retention window.

#### Scenario: A user runs the importer without manually forcing years
- **WHEN** the historical importer is run with default configuration
- **THEN** it SHALL resolve eligible import seasons from the configured archive start year and retention window rather than attempting unsupported long-range backfills
- **THEN** it SHALL document and skip stale pre-archive seasons or future seasons explicitly
- **THEN** successful operation of the supported workflow SHALL not depend on recovering `2025` or earlier seasons

### Requirement: Historical import SHALL support reduced bookmaker coverage without silent success
The import workflow SHALL support a configured bookmaker set of up to three books and SHALL surface missing bookmaker or market coverage explicitly when full coverage is unavailable.

#### Scenario: Only a subset of configured books or line types is available
- **WHEN** a matched historical fixture is missing one or more configured bookmakers or lacks either moneyline or spread history
- **THEN** the importer SHALL still write the game row when the minimum required closing-line fields can be derived
- **THEN** the importer SHALL record the actual bookmaker count and missing-coverage status in the generated artifact summary or logs
- **THEN** reduced bookmaker or market coverage SHALL not be misrepresented as complete season success

### Requirement: External provider coverage gaps SHALL be documented without stopping usable imports
The import workflow SHALL continue to write usable historical artifacts when some games remain unavailable after the planned recovery attempts, and SHALL classify those residual gaps as external provider limitations in operator-visible reporting.

#### Scenario: Some games remain unavailable after recovery attempts
- **WHEN** one or more target games still cannot be recovered after the configured recovery strategies have been exhausted
- **THEN** the workflow SHALL continue with the recoverable `closing_lines.csv` rows for that season
- **THEN** the generated summary or logs SHALL identify the residual missing games as external coverage limitations rather than silent omissions
- **THEN** the workflow SHALL not represent those provider-side misses as a successful full-coverage import

### Requirement: Historical betting impact SHALL be evaluated at the bracket-selection level for the 2026 tournament
The system SHALL include an explicit evaluation that compares baseline bracket selection against betting-informed bracket selection for bracket year `2026`, and SHALL report whether betting information improved, did not improve, or worsened bracket outcomes under the specific evaluation design used.

#### Scenario: Archived prior-season closing lines are available for a deployable-style historical-training test
- **WHEN** imported closing lines are available for one or more historical training seasons and `2026` actual tournament results are present in the local results workbook
- **THEN** the workflow SHALL fit one baseline model using only the core matchup predictors and one enhanced model using the core predictors plus the betting matchup feature columns
- **THEN** both models SHALL use the same engine, seed, draw budget, and candidate-generation settings
- **THEN** the workflow SHALL report bracket score, correct picks, champion correctness, and changed decisions between the baseline and enhanced candidates
- **THEN** the evaluation SHALL explicitly state whether the enhanced model improved, matched, or underperformed the baseline under that historical-training design

#### Scenario: Complete 2026 closing lines are used for a retrospective current-year test
- **WHEN** the repository has complete closing-line coverage for the completed `2026` tournament games
- **THEN** the workflow SHALL run a distinct retrospective evaluation that uses those complete `2026` closing lines to assess whether current-year closing-line information would have improved bracket prediction for the completed `2026` tournament
- **THEN** the evaluation output SHALL report the scoring comparison and changed bracket decisions under that same-year complete-lines design
- **THEN** the workflow SHALL identify that result as retrospective and non-deployable for upfront bracket submission

#### Scenario: Only partial or current-year-only betting coverage exists
- **WHEN** the repository has incomplete `2026` closing-line coverage or current-year betting lines without the required comparison context
- **THEN** the evaluation SHALL not present that comparison as a real estimate of deployable betting-signal value
- **THEN** it SHALL write an operator-facing note explaining whether the limitation is incomplete current-year coverage, absence of prior archived training seasons, or both
