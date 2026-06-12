import type { Candidate } from "../../types/payload";

interface CandidatePathsPanelProps {
  candidates: Candidate[];
}

export function CandidatePathsPanel({ candidates }: CandidatePathsPanelProps) {
  return (
    <details className="dashboard-disclosure dashboard-disclosure--reference" id="paths">
      <summary>
        <div>
          <div className="role-kicker role-kicker--reference">Reference</div>
          <div className="dashboard-disclosure__title">Full candidate paths</div>
          <div className="dashboard-disclosure__note">
            Reference material only. Open this when you want the full bracket-ordered sequence after
            you have decided what to enter.
          </div>
        </div>
        <span className="dashboard-disclosure__chevron" aria-hidden />
      </summary>
      <div className="dashboard-disclosure__body">
        {candidates.map((candidate) => (
          <details key={candidate.candidate_id} className="path-panel" id={`candidate-path-${candidate.candidate_id}`}>
            <summary>
              <strong>Candidate {candidate.candidate_id} full path</strong>
            </summary>
            <p className="path-panel__lede">
              Bracket-ordered winners for Candidate {candidate.candidate_id} ({candidate.type}).
            </p>
            <ol className="path-list">
              {(candidate.matchups ?? []).map((matchup) => (
                <li key={String(matchup.slot_key)}>
                  <span>{String(matchup.round ?? "n/a")}</span>
                  <span>{String(matchup.region ?? "n/a")}</span>
                  <strong>{String(matchup.winner ?? "n/a")}</strong>
                  <span>
                    {String(matchup.teamA ?? "n/a")} vs {String(matchup.teamB ?? "n/a")}
                  </span>
                </li>
              ))}
            </ol>
          </details>
        ))}
      </div>
    </details>
  );
}
