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

  return (
    <section id="evidence" aria-label="Matchup evidence" className="evidence-panel">
      <div className="evidence-panel__head">
        <h2>Matchup evidence</h2>
        {openEvidenceId ? (
          <button type="button" className="evidence-close" onClick={onClose}>
            Close
          </button>
        ) : null}
      </div>
      {!openEvidenceId ? (
        <p className="missing-section">
          Select a bracket node or divergence cell to open evidence.
        </p>
      ) : !row ? (
        <p className="missing-section">Evidence details are not available for {openEvidenceId}.</p>
      ) : (
        <article className="evidence-card" data-testid="evidence-card" id={openEvidenceId}>
          <h3>{String(row.matchup_label ?? `${row.teamA} vs ${row.teamB}`)}</h3>
          <dl className="evidence-summary">
            <dt>Round</dt>
            <dd>{String(row.round ?? "n/a")}</dd>
            <dt>Region</dt>
            <dd>{String(row.region ?? "n/a")}</dd>
            <dt>Favorite</dt>
            <dd>{String(row.posterior_favorite ?? "n/a")}</dd>
            <dt>Win probability</dt>
            <dd>
              {typeof row.win_prob_favorite === "number" ? row.win_prob_favorite.toFixed(3) : "n/a"}
            </dd>
            <dt>Confidence</dt>
            <dd>{String(row.confidence_tier ?? "n/a")}</dd>
            <dt>Candidate usage</dt>
            <dd>{String(row.candidate_usage ?? "n/a")}</dd>
          </dl>
          {row.why_this_matters ? (
            <p className="evidence-callout">{String(row.why_this_matters)}</p>
          ) : null}
          {row.rationale_short ? <p>{String(row.rationale_short)}</p> : null}
        </article>
      )}
    </section>
  );
}
