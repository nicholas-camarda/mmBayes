## ADDED Requirements

### Requirement: Historical collection SHALL align with the analysis window
The refresh pipeline SHALL derive the oldest historical year to collect from the model's actual analysis window, and SHALL not scrape extra legacy years unless an explicit buffer policy is configured or documented.

#### Scenario: The refresh path does not collect unnecessary legacy years
- **WHEN** `update_tournament_data()` prepares the historical refresh range for a bracket year
- **THEN** the oldest collected completed year SHALL match the oldest year needed by the configured analysis window, unless an explicit extra buffer policy applies
- **THEN** any additional buffered years SHALL be visible in the runtime summary or configuration contract

### Requirement: Historical years SHALL be reusable when canonical coverage is complete
The refresh pipeline SHALL reuse a historical year from the canonical cloud files when that year is already present and passes completeness validation, instead of re-scraping the same year on every run.

#### Scenario: A complete historical year is reused
- **WHEN** `update_tournament_data()` runs and a historical year is already present with complete team-feature and game-result coverage
- **THEN** the refresh pipeline SHALL reuse that year instead of re-scraping it from upstream sources

### Requirement: Incomplete historical years SHALL be refreshed from source
The refresh pipeline SHALL treat any missing or incomplete historical year as a cache miss and SHALL refresh that year from upstream sources before writing the canonical outputs.

#### Scenario: A year with missing coverage is not silently reused
- **WHEN** a historical year is absent or fails completeness validation
- **THEN** the refresh pipeline SHALL refresh that year from source rather than omitting it from the regenerated canonical files

### Requirement: Current-year refresh SHALL remain live
The refresh pipeline SHALL continue to refresh the current bracket year on every run, even when historical years are reused from cache.

#### Scenario: Historical cache hits do not suppress current-year updates
- **WHEN** the current bracket year is refreshed alongside cached historical years
- **THEN** the pipeline SHALL still poll current-year scoreboard data and merge the current-year results path

### Requirement: Refresh reporting SHALL expose reuse decisions
The refresh pipeline SHALL report which historical years were reused, refreshed, or rejected as incomplete so operators can verify refresh coverage from the runtime summary and log.

#### Scenario: The operator can see cache behavior
- **WHEN** a refresh completes
- **THEN** the runtime summary SHALL identify the reused years and refreshed years
- **THEN** the summary SHALL make missing or rejected historical coverage visible

### Requirement: Canonical output contracts SHALL remain stable
The refresh pipeline SHALL continue writing the canonical team-feature and tournament-result files to the configured cloud root using the existing workbook names unless a future change explicitly revises that contract.

#### Scenario: Incremental refresh preserves the publishable files
- **WHEN** the refresh completes successfully
- **THEN** the pipeline SHALL still write `pre_tournament_team_features.xlsx` and `tournament_game_results.xlsx` to the configured canonical data root
