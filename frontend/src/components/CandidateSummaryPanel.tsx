import type { Candidate } from "../types/payload";

function formatProb(value: number | null | undefined): string {
  return value == null ? "n/a" : value.toFixed(3);
}

export function CandidateSummaryPanel({ candidates }: { candidates: Candidate[] }) {
  return (
    <section aria-label="Candidate brackets" className="candidate-grid">
      {candidates.map((candidate) => (
        <article key={candidate.candidate_id} data-testid="candidate-card" className="candidate-card">
          <h3>
            Candidate {candidate.candidate_id} ({candidate.type})
          </h3>
          <p>
            Champion: <strong>{candidate.champion}</strong>
          </p>
          {candidate.final_four && candidate.final_four.length > 0 && (
            <p>Final Four: {candidate.final_four.join(", ")}</p>
          )}
          <dl>
            <dt>Bracket log probability</dt>
            <dd>{formatProb(candidate.bracket_log_prob)}</dd>
            <dt>Mean picked-game probability</dt>
            <dd>{formatProb(candidate.mean_game_prob)}</dd>
            <dt>Title-path mean probability</dt>
            <dd>{formatProb(candidate.title_path_mean_prob)}</dd>
          </dl>
          {candidate.path_support_label && <p className="support-note">{candidate.path_support_label}</p>}
        </article>
      ))}
    </section>
  );
}
