## ADDED Requirements

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
