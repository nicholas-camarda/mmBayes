import type { MatchupContextRow } from "../../types/payload";

interface EvidencePanelProps {
  rows: MatchupContextRow[];
  openEvidenceId: string | null;
  onClose: () => void;
}

function findEvidenceRow(
  rows: MatchupContextRow[],
  evidenceId: string | null,
): MatchupContextRow | undefined {
  if (!evidenceId) return undefined;
  return rows.find(
    (row) =>
      row.evidence_id === evidenceId ||
      `evidence-${row.slot_key}` === evidenceId ||
      row.slot_key === evidenceId.replace(/^evidence-/, ""),
  );
}

export function EvidencePanel({ rows, openEvidenceId, onClose }: EvidencePanelProps) {
  const row = findEvidenceRow(rows, openEvidenceId);
  const matchupLabel = row
    ? String(row.matchup_label ?? `${row.teamA} vs ${row.teamB}`)
    : "Select a matchup";

  return (
    <section id="evidence" aria-label="Matchup evidence" className="evidence-section">
      <div className="role-kicker role-kicker--evidence">Understand why</div>
      <h2>Matchup Evidence</h2>
      <p className="section-note">
        Targeted matchup drawers surfaced by the review queue and bracket tree. Use jump buttons to
        open a specific drawer.
      </p>
      <div className="evidence-shell">
        {!openEvidenceId ? (
          <p className="missing-section">
            Select a bracket node, divergence cell, or review card to open evidence.
          </p>
        ) : !row ? (
          <p className="missing-section">Evidence details are not available for {openEvidenceId}.</p>
        ) : (
          <details
            className="evidence-panel"
            id={openEvidenceId}
            open
            data-testid="evidence-card"
          >
            <summary>
              <div className="evidence-panel__summary">
                <div>
                  <div className="surface-pill">Matchup evidence</div>
                  <h3>{matchupLabel}</h3>
                </div>
                <button
                  type="button"
                  className="evidence-close"
                  onClick={(event) => {
                    event.preventDefault();
                    onClose();
                  }}
                >
                  Close
                </button>
              </div>
            </summary>
            <div className="evidence-panel__body">
              <p className="evidence-panel__lede">
                <strong>{String(row.posterior_favorite ?? "n/a")}</strong> is the posterior favorite
                at{" "}
                {typeof row.win_prob_favorite === "number"
                  ? `${(100 * row.win_prob_favorite).toFixed(1)}%`
                  : "n/a"}
                .
              </p>
              <dl className="evidence-summary">
                <dt>Round</dt>
                <dd>{String(row.round ?? "n/a")}</dd>
                <dt>Region</dt>
                <dd>{String(row.region ?? "n/a")}</dd>
                <dt>Confidence</dt>
                <dd>{String(row.confidence_tier ?? "n/a")}</dd>
                <dt>Candidate usage</dt>
                <dd>{String(row.candidate_usage ?? "n/a")}</dd>
              </dl>
              {row.why_this_matters ? (
                <p className="evidence-callout">{String(row.why_this_matters)}</p>
              ) : null}
              {row.rationale_short ? <p>{String(row.rationale_short)}</p> : null}
            </div>
          </details>
        )}
      </div>
    </section>
  );
}
