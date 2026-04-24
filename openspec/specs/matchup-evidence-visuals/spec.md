## Purpose

Define the matchup evidence visual contracts used by the bracket dashboard to explain favorite, underdog, and metric-direction evidence.
## Requirements
### Requirement: Evidence summaries SHALL name the favorite and underdog teams
The bracket dashboard SHALL render matchup evidence summaries with team-specific favorite and underdog labels so a reader can identify the model's current stance without consulting candidate usage or raw tables.

#### Scenario: Favorite and underdog labels are explicit
- **WHEN** a matchup evidence panel is rendered
- **THEN** the evidence summary SHALL show the favorite team's name with the favorite win probability
- **THEN** the evidence summary SHALL show the underdog team's name with the underdog win probability

### Requirement: Team cards SHALL expose matchup role identity
The bracket dashboard SHALL visually label the two matchup team cards with their role in the current model view and SHALL present the posterior favorite first.

#### Scenario: Favorite card appears first
- **WHEN** a matchup evidence panel renders the team comparison area
- **THEN** the first team card SHALL correspond to the posterior favorite
- **THEN** that card SHALL include a visible `Favorite` role label
- **THEN** the second team card SHALL include a visible `Underdog` role label

### Requirement: Matchup comparison SHALL use a directional advantage chart
The bracket dashboard SHALL present the primary model-facing matchup comparison as a horizontal advantage chart rather than an always-visible raw diff table.

#### Scenario: Directional metrics indicate the favored side
- **WHEN** a matchup evidence panel renders model-facing metrics with an advantageous direction
- **THEN** each metric row SHALL show both teams' raw values
- **THEN** the row SHALL include a visual and textual indication of which team the metric favors

### Requirement: Neutral metrics SHALL remain contextual
The bracket dashboard SHALL preserve neutral or context-only metrics without implying that one team has an advantage when the model does not treat that metric directionally.

#### Scenario: Context-only metrics do not imply an edge
- **WHEN** a matchup evidence panel renders a context-only metric such as `same_conf` or another neutral row
- **THEN** the metric SHALL be displayed as contextual information
- **THEN** the chart SHALL not assign that row to either team as an advantage

### Requirement: Exact model-input detail SHALL remain accessible
The bracket dashboard SHALL keep the exact numeric metric comparison available behind a collapsed disclosure so readers can inspect the raw values used by the model without keeping the full diff table visible by default.

#### Scenario: Raw detail can be expanded on demand
- **WHEN** a user expands the raw-details disclosure in a matchup evidence panel
- **THEN** the panel SHALL reveal the detailed metric comparison table with exact values and favor labels

### Requirement: Evidence visuals SHALL remain legible in the dark dashboard palette
Matchup evidence visuals SHALL render with sufficient contrast and semantic consistency when embedded in the redesigned dark bracket dashboard.

#### Scenario: Evidence panels render in dark context
- **WHEN** a matchup evidence panel appears in the redesigned bracket dashboard
- **THEN** favorite and underdog labels, probability tracks, directional advantage charts, raw-details disclosures, and jump controls SHALL remain readable against the dark dashboard surfaces
- **THEN** confidence-tier and team-role colors SHALL remain consistent with the dashboard legend

#### Scenario: Raw detail disclosure remains accessible
- **WHEN** a user expands exact model-input detail inside a dark evidence panel
- **THEN** the detailed metric comparison table SHALL remain readable
- **THEN** the table SHALL preserve exact values and favor labels from the existing evidence visualization behavior

