import { useState } from "react";
import type { TechnicalCandidateDifferenceRow, TechnicalDecisionRow } from "../../types/payload";
import { ProbabilityTrack } from "../bracket/ProbabilityTrack";

type ViewId = "compare" | "candidate-1" | "candidate-2";

interface CompareWorkspaceProps {
  rankedDecisions?: TechnicalDecisionRow[] | null;
  candidateDifferences?: TechnicalCandidateDifferenceRow[] | null;
}

function inspectionClass(level?: string): string {
  if (level === "primary") return "inspection-primary";
  if (level === "secondary") return "inspection-secondary";
  return "";
}

export function CompareWorkspace({
  rankedDecisions,
  candidateDifferences,
}: CompareWorkspaceProps) {
  const [view, setView] = useState<ViewId>("compare");

  return (
    <section className="technical-panel" aria-label="Compare workspace" data-testid="compare-workspace">
      <div className="role-kicker role-kicker--act">Act now</div>
      <h2>Compare workspace</h2>
      <div className="toggle-bar" role="group" aria-label="Candidate dashboard view">
        {(
          [
            ["compare", "Compare"],
            ["candidate-1", "Candidate 1"],
            ["candidate-2", "Candidate 2"],
          ] as const
        ).map(([id, label]) => (
          <button
            key={id}
            type="button"
            className={`toggle-button${view === id ? " is-active" : ""}`}
            aria-pressed={view === id}
            onClick={() => setView(id)}
          >
            {label}
          </button>
        ))}
      </div>
      {view === "compare" ? (
        <>
          <div className="technical-subpanel">
            <h3>Ranked decision board</h3>
            {(rankedDecisions ?? []).length > 0 ? (
              <table className="dashboard-table" data-testid="ranked-decisions-table">
                <thead>
                  <tr>
                    <th>Rank</th>
                    <th>Round</th>
                    <th>Matchup</th>
                    <th>Favorite</th>
                    <th>Probability</th>
                    <th>Tier</th>
                    <th>Recommended</th>
                  </tr>
                </thead>
                <tbody>
                  {rankedDecisions!.map((row) => (
                    <tr
                      key={`${row.rank}-${row.matchup}`}
                      className={inspectionClass(row.inspection_level)}
                    >
                      <td>{row.rank}</td>
                      <td>{row.round}</td>
                      <td>{row.matchup}</td>
                      <td>{row.favorite}</td>
                      <td>
                        <ProbabilityTrack
                          meanProbability={row.favorite_prob}
                          lowerProbability={row.ci_lower}
                          upperProbability={row.ci_upper}
                          axisMin={0.5}
                          axisMax={1}
                          confidenceTier={row.tier}
                        />
                      </td>
                      <td>{row.tier}</td>
                      <td>{row.recommended_pick}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            ) : (
              <p className="missing-section">Ranked decisions are not available.</p>
            )}
          </div>
          <div className="technical-subpanel">
            <h3>Candidate divergence</h3>
            {(candidateDifferences ?? []).length > 0 ? (
              <table className="dashboard-table" data-testid="candidate-differences-table">
                <thead>
                  <tr>
                    <th>Round</th>
                    <th>Region</th>
                    <th>Matchup</th>
                    <th>Candidate 1</th>
                    <th>Candidate 2</th>
                    <th>Tier</th>
                    <th>Leverage</th>
                  </tr>
                </thead>
                <tbody>
                  {candidateDifferences!.map((row) => (
                    <tr key={`${row.round}-${row.slot}-${row.matchup}`}>
                      <td>{row.round}</td>
                      <td>{row.region}</td>
                      <td>{row.matchup}</td>
                      <td>{row.candidate_1}</td>
                      <td>{row.candidate_2}</td>
                      <td>{row.tier}</td>
                      <td>{row.leverage?.toFixed(2) ?? "n/a"}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            ) : (
              <p className="missing-section">No candidate differences were recorded.</p>
            )}
          </div>
        </>
      ) : (
        <div className="technical-subpanel">
          <h3>{view === "candidate-1" ? "Candidate 1" : "Candidate 2"} focus</h3>
          <p className="section-note">
            Use the Compare view for the full ranked board and divergence table. Candidate tabs
            reserve space for path-specific fragility charts in a later parity pass.
          </p>
          <dl className="evidence-summary">
            <dt>Decision count</dt>
            <dd>{rankedDecisions?.length ?? 0}</dd>
            <dt>Divergent slots</dt>
            <dd>{candidateDifferences?.length ?? 0}</dd>
          </dl>
        </div>
      )}
    </section>
  );
}
