## Purpose

Define the forensic evidence needed to explain historical refresh omissions and distinguish source, join, and overwrite failure points.

## Requirements

### Requirement: Historical refresh runs SHALL emit year-scoped evidence
The data refresh workflow SHALL produce year-scoped evidence that identifies which historical years were intended, which years refreshed successfully, which years were omitted, and the first stage where omission occurred.

#### Scenario: A historical roster-source failure is traceable by year
- **WHEN** `update_data` refreshes a historical season and historical roster extraction fails for one or more years
- **THEN** the refresh evidence SHALL identify the affected year numbers
- **THEN** the refresh evidence SHALL identify the stage where each affected year stopped progressing
- **THEN** later years in the requested range SHALL still be accounted for in the same evidence trail

### Requirement: Historical-year loss SHALL be distinguishable from a successful overwrite
The refresh workflow SHALL make it possible to determine whether a missing historical year was absent before the run or was lost during the refresh pipeline.

#### Scenario: Pre-refresh and post-refresh canonical counts are comparable
- **WHEN** a refresh run starts with existing canonical cloud spreadsheets
- **THEN** the investigation evidence SHALL record the pre-refresh year coverage for the canonical team features and tournament results
- **THEN** the investigation evidence SHALL record the post-refresh year coverage written by the run
- **THEN** the evidence SHALL allow a reader to determine whether any years were overwritten or dropped

### Requirement: Roster-join omission SHALL be observable
The refresh pipeline SHALL surface whether historical years disappear during roster ingestion or during `build_team_feature_dataset()` rather than silently reducing the canonical dataset.

#### Scenario: Missing historical roster data affects feature construction
- **WHEN** a historical year has Bart ratings but no usable historical roster data from the configured roster source
- **THEN** the refresh evidence SHALL show whether that year reached feature construction
- **THEN** the evidence SHALL show whether the year contributed rows to the canonical team-feature dataset

### Requirement: Investigation artifacts SHALL support root-cause analysis before fallback design
The investigation workflow SHALL preserve enough information to establish the root cause of historical-year loss before any graceful-degradation or fallback behavior is designed.

#### Scenario: The follow-up design starts from confirmed behavior
- **WHEN** the investigation completes
- **THEN** the resulting artifacts SHALL identify the confirmed failure point or points
- **THEN** the artifacts SHALL distinguish source failure, join failure, and overwrite failure as separate possibilities
- **THEN** the artifacts SHALL be sufficient to justify the next change without guessing
