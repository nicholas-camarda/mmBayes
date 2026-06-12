import type { Candidate, DecisionSheetRow } from "../../types/payload";

interface CandidateComparisonPanelProps {
  candidates: Candidate[];
  decisionSheet: DecisionSheetRow[];
}

export function CandidateComparisonPanel({
  candidates,
  decisionSheet,
}: CandidateComparisonPanelProps) {
  const divergentCount = decisionSheet.filter((row) => row.candidate_diff_flag).length;
  const lateDiffCount = decisionSheet.filter(
    (row) =>
      row.candidate_diff_flag &&
      ["Sweet 16", "Elite 8", "Final Four", "Championship"].includes(String(row.round ?? "")),
  ).length;

  return (
    <div className="candidate-comparison-panel" aria-label="Candidate comparison">
      <div className="candidate-comparison-panel__head">
        <div>
          <div className="section-label">Compare candidates</div>
          <h3>Baseline vs alternate bracket</h3>
          <p className="section-note">
            Candidate 1 is the reference path. Candidate 2 keeps the same model but accepts the
            alternate route where the baseline and alternate disagree.
          </p>
        </div>
      </div>
      <table className="dashboard-table candidate-comparison-table">
        <thead>
          <tr>
            <th>Candidate</th>
            <th>Champion</th>
            <th>Final Four</th>
            <th>Bracket log prob</th>
            <th>Mean game prob</th>
            <th>Changes</th>
          </tr>
        </thead>
        <tbody>
          {candidates.map((candidate) => (
            <tr key={candidate.candidate_id}>
              <td>
                <strong>Candidate {candidate.candidate_id}</strong>
                <span className="comparison-role">
                  {candidate.candidate_id === 1 ? "Baseline entry" : "Alternate entry"}
                </span>
              </td>
              <td>{candidate.champion}</td>
              <td>{candidate.final_four?.join(", ") ?? "n/a"}</td>
              <td>{candidate.bracket_log_prob?.toFixed(3) ?? "n/a"}</td>
              <td>{candidate.mean_game_prob?.toFixed(3) ?? "n/a"}</td>
              <td>
                {candidate.candidate_id === 2
                  ? `${divergentCount} total / ${lateDiffCount} late`
                  : "Reference"}
              </td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}
