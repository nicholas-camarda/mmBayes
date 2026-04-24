## Purpose

Define the supported `master` branch contract for tests, canonical validation, bracket scoring, and release publishing.

## Requirements

### Requirement: The supported master branch SHALL have a passing top-level test surface
The `master` branch SHALL keep `Rscript tests/testthat.R` aligned with the supported betting-free workflow, and SHALL not retain tests that require removed betting-only helpers or retired odds paths.

#### Scenario: Top-level test entrypoint matches the branch contract
- **WHEN** the full `tests/testthat.R` suite runs on `master`
- **THEN** it SHALL not fail because of references to removed betting helpers, retired odds collectors, or other unsupported branch-only behavior

### Requirement: Canonical game-result validation SHALL be row-correct
The canonical game-result validator SHALL verify each row’s `winner` against that row’s `teamA` and `teamB`, and SHALL reject scored rows whose winner does not match the higher score or whose scores imply a tied game under the current contract.

#### Scenario: Winner must belong to the same row
- **WHEN** a game-results row names a `winner` that is not equal to that row’s `teamA` or `teamB`
- **THEN** validation SHALL fail even if that team name appears elsewhere in the table

#### Scenario: Scored rows must agree with the recorded winner
- **WHEN** a game-results row includes both team scores
- **THEN** validation SHALL require `total_points` to equal the row’s two scores
- **THEN** validation SHALL require `winner` to match the team with the higher score

#### Scenario: Tied scored rows are rejected
- **WHEN** a game-results row includes equal `teamA_score` and `teamB_score`
- **THEN** validation SHALL fail because the current canonical tournament-results contract does not permit ties

### Requirement: Bracket scoring SHALL require an unambiguous game identity
Bracket scoring SHALL not silently score predicted games against ambiguous actual-result rows. The scorer SHALL require `Year` as part of the game identity and SHALL fail clearly when callers provide actual results that omit `Year` or otherwise leave the join ambiguous.

#### Scenario: Multi-year actual results are scored safely
- **WHEN** predicted matchups and actual results both include `Year`
- **THEN** bracket scoring SHALL include `Year` in the game identity used to compare winners and assign round scores

#### Scenario: Missing year fails fast
- **WHEN** callers provide actual results without a `Year` column
- **THEN** bracket scoring SHALL fail with a message indicating that `Year` is required for scoring

#### Scenario: Ambiguous actual results fail fast
- **WHEN** actual results contain multiple rows that match the same predicted game under the required join keys
- **THEN** bracket scoring SHALL fail with a message indicating that the actual-result input is ambiguous

### Requirement: Release publishing SHALL follow the runtime-output contract
Release publishing SHALL source deliverables from the runtime output directory and SHALL keep candidate deliverable inclusion consistent with the candidate artifacts that the supported simulation workflow actually writes.

#### Scenario: Runtime output remains the authoritative publish source
- **WHEN** `publish_release_bundle()` is called without an explicit overridden output path
- **THEN** it SHALL use the configured runtime output directory as the source of publishable artifacts
- **THEN** it SHALL not silently fall back to a cloud output directory as though it were the active execution target

#### Scenario: Candidate deliverables cannot drift from generation behavior
- **WHEN** the release bundle is assembled
- **THEN** candidate CSV inclusion SHALL be derived from the generated `bracket_candidate_*.csv` files for the current run
- **THEN** publishing SHALL fail clearly rather than silently omitting expected candidate artifacts when the manifest and generated files diverge
