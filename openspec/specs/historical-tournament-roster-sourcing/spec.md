## Purpose

Define the historical tournament roster source contract for completed seasons and its separation from current-year monitoring.

## Requirements

### Requirement: Historical tournament roster SHALL come from Sports-Reference
The refresh workflow SHALL source completed historical tournament field membership, seeds, and regions from the Sports-Reference postseason page for each historical year instead of Bart `tourneytime.php`.

#### Scenario: A completed historical year is refreshed
- **WHEN** `update_tournament_data()` refreshes a historical year before the active bracket year
- **THEN** the roster ingestion step SHALL build tournament field rows from the Sports-Reference postseason page for that year
- **THEN** the roster rows SHALL include team identity, seed, and region values for bracket validation and feature joining

### Requirement: Historical roster rows SHALL remain compatible with Bart metrics
The system SHALL join the Sports-Reference historical roster to Bart ratings through the canonical team identity for the same year and SHALL reject years whose tournament teams are not fully covered by Bart metrics.

#### Scenario: A tournament team is missing from Bart metrics
- **WHEN** a Sports-Reference historical roster contains a team that does not resolve to a Bart ratings row after canonicalization
- **THEN** the refresh workflow SHALL treat that year as unavailable for canonical output
- **THEN** the refresh evidence SHALL identify the affected year and the failed stage

### Requirement: Historical roster validation SHALL remain strict
The source change SHALL not weaken the structural rules for a valid tournament field.

#### Scenario: Parsed historical roster violates bracket structure
- **WHEN** a Sports-Reference historical roster produces an invalid field layout for a year
- **THEN** the pipeline SHALL reject that year rather than silently writing malformed roster rows
- **THEN** the validation failure SHALL identify the violated structural rule

### Requirement: Current-year monitoring SHALL remain separate from historical roster sourcing
The historical roster source change SHALL not alter the current-year completed-results flow.

#### Scenario: The active bracket year refreshes after the source split
- **WHEN** `update_tournament_data()` refreshes the active bracket year
- **THEN** Bart metrics SHALL remain the source of pre-tournament team features
- **THEN** ESPN current-year completed-game ingestion SHALL remain responsible for live monitoring rows
- **THEN** the historical Sports-Reference roster source SHALL apply only to completed historical years
