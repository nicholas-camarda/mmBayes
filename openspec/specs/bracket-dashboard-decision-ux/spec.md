# bracket-dashboard-decision-ux Specification

## Purpose
TBD - created by archiving change redesign-bracket-dashboard-ux. Update Purpose after archive.
## Requirements
### Requirement: Dashboard SHALL expose a bracket-entry-first workflow
The bracket dashboard SHALL organize the main generated page around filling out a March Madness bracket, with complete candidate entry guidance before optional evidence exploration.

#### Scenario: Bracket-entry zones are visible
- **WHEN** `bracket_dashboard.html` is generated
- **THEN** the page SHALL contain visible sections for entering bracket picks, deciding uncertain calls, inspecting evidence, and opening reference material
- **THEN** the bracket-entry section SHALL appear before detailed evidence exploration
- **THEN** reference-only material SHALL appear after the primary decision and inspection sections

#### Scenario: Existing anchors remain valid
- **WHEN** the redesigned dashboard is generated
- **THEN** existing section links for the review queue, candidate entries, bracket tree, evidence, and reference material SHALL continue to resolve within the page

### Requirement: Dashboard SHALL provide a complete candidate fill-in surface
The bracket dashboard SHALL provide a candidate-specific fill-in surface that can be used to transcribe a complete bracket into a common March Madness bracket-entry form without reconstructing picks from evidence drawers.

#### Scenario: Candidate fill-in surface lists winners by bracket structure
- **WHEN** candidate path data are available
- **THEN** the dashboard SHALL show each selected candidate's winners organized by region and round
- **THEN** each row or cell SHALL include the matchup, seed context, selected winner, and confidence tier
- **THEN** the surface SHALL identify the champion, Final Four, title-game matchup, and championship tiebreaker when available

#### Scenario: User can switch candidate entry views
- **WHEN** at least two candidate brackets are available
- **THEN** the fill-in surface SHALL allow the user to view Candidate 1 and Candidate 2 entry paths without losing the bracket-entry context
- **THEN** Candidate 1 SHALL remain identifiable as the baseline or safe entry
- **THEN** Candidate 2 SHALL remain identifiable as the alternate entry

#### Scenario: Fill-in surface distinguishes review-needed picks
- **WHEN** a pick appears in the fill-in surface and is also in the review queue
- **THEN** the fill-in surface SHALL visibly mark that pick as requiring review
- **THEN** the mark SHALL link or jump to the relevant evidence or review card

### Requirement: Dashboard SHALL use a dark semantic visual system
The bracket dashboard SHALL use a dark system palette with semantic color roles for page surfaces, elevated cards, action states, candidate identity, confidence tiers, status states, and reference-only material.

#### Scenario: Dark theme renders primary surfaces
- **WHEN** the dashboard is opened in a browser
- **THEN** the page background, section surfaces, cards, text, muted text, borders, and controls SHALL render using dark-theme tokens
- **THEN** normal text and compact labels SHALL remain readable against their surfaces

#### Scenario: Semantic colors do not conflict
- **WHEN** candidate identity, confidence tiers, action controls, and reference-only material appear in the same viewport
- **THEN** each semantic role SHALL be visually distinguishable without relying only on surrounding prose

### Requirement: Dashboard SHALL show the next bracket-entry action before detailed review
The bracket dashboard SHALL summarize the current bracket-entry workflow before detailed review material so the user can see what to enter and where to pause.

#### Scenario: Next-action summary appears
- **WHEN** watchlist rows, candidate summaries, and tiebreaker values are available
- **THEN** the dashboard SHALL show a compact next-action summary before detailed evidence or reference material
- **THEN** the summary SHALL identify the candidate entry to use, the main review burden, and available entry values

#### Scenario: Missing optional values do not create false instructions
- **WHEN** championship tiebreaker values are unavailable
- **THEN** the next-action summary SHALL omit tiebreaker entry instructions or clearly mark them unavailable

### Requirement: Dashboard SHALL present candidate entry tradeoffs directly
The bracket dashboard SHALL show a direct Candidate 1 versus Candidate 2 comparison that helps the user choose which bracket to enter.

#### Scenario: Candidate comparison summarizes entry tradeoffs
- **WHEN** at least two candidate summaries are available
- **THEN** the dashboard SHALL render a comparison surface with champion, Final Four, bracket log probability, mean picked-game probability, changed slots, late-round changes, and tiebreaker values when available
- **THEN** Candidate 1 SHALL be labeled as the baseline or safe entry
- **THEN** Candidate 2 SHALL be labeled as the alternate entry

#### Scenario: Candidate detail remains available
- **WHEN** the candidate comparison is rendered
- **THEN** detailed candidate cards SHALL remain available below or adjacent to the comparison
- **THEN** links to full paths, evidence, and the review queue SHALL remain available

### Requirement: Divergence summary SHALL be graphical and actionable
The bracket dashboard SHALL render Candidate 2 divergence as a compact graphical route map or heatmap organized by bracket round and region.

#### Scenario: Divergence map shows where the alternate path changes
- **WHEN** candidate divergence rows are available
- **THEN** the divergence surface SHALL show changed buckets by round and region
- **THEN** late-round changes SHALL be visually emphasized relative to early-round changes
- **THEN** winner changes and path-only changes SHALL be distinguishable

#### Scenario: Divergence cells preserve click-through behavior
- **WHEN** a user activates a divergence cell tied to surfaced evidence
- **THEN** the dashboard SHALL open the relevant surfaced evidence or filtered review row

#### Scenario: Reference-only divergence remains accessible
- **WHEN** a divergence cell cannot map directly to surfaced evidence
- **THEN** the dashboard SHALL open or navigate to the exact diff reference material for that bucket

### Requirement: Review cards SHALL include compact probability graphics
The bracket dashboard SHALL include compact probability and interval graphics in review cards when posterior probability summaries are available.

#### Scenario: Review card shows uncertainty at a glance
- **WHEN** a review card has posterior mean and interval values for the recommended side
- **THEN** the card SHALL show a compact probability track with the mean and interval
- **THEN** the track SHALL be readable in the dark palette

#### Scenario: Review card remains useful without probability graphics
- **WHEN** posterior probability summary values are unavailable for a review card
- **THEN** the card SHALL still show the matchup, review reason, candidate usage, and evidence jump action

### Requirement: Bracket tree SHALL expose route differences without hover
The bracket tree SHALL show route-difference cues directly in the visible SVG or surrounding legend so the user can see how Candidate 2 departs from Candidate 1 without relying on hover text.

#### Scenario: Candidate tree indicates path differences
- **WHEN** the bracket tree is rendered for Candidate 2
- **THEN** games or route segments that differ from Candidate 1 SHALL be visually distinguishable from shared path games
- **THEN** the tree legend SHALL explain the route-difference cue

#### Scenario: Tree interactions remain intact
- **WHEN** a user switches between candidate trees or clicks a game node
- **THEN** the candidate toggle and evidence-opening behavior SHALL continue to work

### Requirement: Candidate cards SHALL expose tiebreaker distribution summaries
The bracket dashboard SHALL show compact championship tiebreaker distribution summaries in candidate recommendations when total-points prediction data are available.

#### Scenario: Candidate card includes tiebreaker graphic
- **WHEN** candidate-level championship total distributions are available
- **THEN** each candidate recommendation SHALL include a compact graphic or sparkline summarizing the distribution, recommended value, and interval

#### Scenario: Candidate card handles missing tiebreaker data
- **WHEN** total-points predictions are unavailable
- **THEN** candidate cards SHALL continue to render bracket-path context without an empty or misleading tiebreaker graphic

### Requirement: Mobile layout SHALL prioritize bracket entry
The bracket dashboard SHALL provide a mobile layout that minimizes navigation height and prioritizes bracket fill-in, review-needed picks, and surfaced evidence over wide tree inspection.

#### Scenario: Mobile navigation is compact
- **WHEN** the dashboard is viewed at a mobile viewport width
- **THEN** primary navigation SHALL avoid stacking five large full-width buttons before the main work
- **THEN** the first viewport SHALL still communicate the dashboard title, status, selected candidate entry, and next useful action

#### Scenario: Mobile entry flow is usable
- **WHEN** a mobile user opens the dashboard
- **THEN** the candidate fill-in surface, review queue, and surfaced evidence SHALL be reachable without requiring horizontal bracket-tree inspection
- **THEN** wide bracket-tree content SHALL remain horizontally scrollable when opened

### Requirement: Dashboard verification SHALL include desktop and mobile interaction checks
The redesigned dashboard SHALL be verified with automated checks that exercise generated HTML behavior and graphical rendering.

#### Scenario: Desktop dashboard smoke test passes
- **WHEN** the fixture dashboard is generated and opened in a desktop browser viewport
- **THEN** Playwright or equivalent browser coverage SHALL verify visible workflow zones, active controls, evidence jumps, candidate toggles, and nonblank graphical elements

#### Scenario: Mobile dashboard smoke test passes
- **WHEN** the fixture dashboard is opened in a mobile browser viewport
- **THEN** browser coverage SHALL verify compact navigation, readable candidate fill-in guidance, accessible review cards, and usable evidence jumps

