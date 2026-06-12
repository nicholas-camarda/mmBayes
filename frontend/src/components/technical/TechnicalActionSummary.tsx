import type { TechnicalActionSummary } from "../../types/payload";

export function TechnicalActionSummary({ summary }: { summary?: TechnicalActionSummary | null }) {
  if (!summary) return null;

  return (
    <section className="technical-panel action-callout" aria-label="Action summary">
      <div className="role-kicker role-kicker--act">Act now</div>
      <h2>Action summary</h2>
      <div className="summary-strip">
        {summary.candidates?.map((candidate) => (
          <div key={candidate.candidate_id} className="summary-chip">
            <span>Candidate {candidate.candidate_id}</span>
            <strong>{candidate.champion}</strong>
            <small>
              {candidate.type} | Final Four: {candidate.final_four}
            </small>
          </div>
        ))}
        <div className="summary-chip">
          <span>Toss-up games</span>
          <strong>{summary.tier_counts?.["Toss-up"] ?? 0}</strong>
        </div>
        <div className="summary-chip">
          <span>Volatile games</span>
          <strong>{summary.tier_counts?.Volatile ?? 0}</strong>
        </div>
        <div className="summary-chip">
          <span>Candidate differences</span>
          <strong>{summary.candidate_diff_count ?? 0}</strong>
        </div>
        <div className="summary-chip summary-chip--wide">
          <span>Top leverage upset</span>
          <strong>{summary.top_leverage_label ?? "n/a"}</strong>
        </div>
      </div>
    </section>
  );
}
