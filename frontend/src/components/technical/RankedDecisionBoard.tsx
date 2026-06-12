import type { TechnicalDecisionRow } from "../../types/payload";
import { ProbabilityTrack } from "../bracket/ProbabilityTrack";
import { BoardRow, ComparisonBoard } from "./ComparisonBoard";

function inspectionClass(level?: string): string {
  if (level === "primary") return "inspection-primary";
  if (level === "secondary") return "inspection-secondary";
  return "";
}

interface RankedDecisionBoardProps {
  rows: TechnicalDecisionRow[];
}

export function RankedDecisionBoard({ rows }: RankedDecisionBoardProps) {
  if (rows.length === 0) {
    return <p className="missing-section">Ranked decisions are not available.</p>;
  }

  return (
    <ComparisonBoard
      variant="ranked"
      testId="ranked-decisions-board"
      columns={[
        "Game to revisit",
        "Preferred pick",
        "Alternate path",
        "Posterior uncertainty",
        "How to use this",
      ]}
    >
      {rows.map((row) => (
        <BoardRow
          key={`${row.rank}-${row.matchup}`}
          variant="ranked"
          cells={[
            {
              label: "Game to revisit",
              value: row.matchup,
              note: `${row.round ?? "n/a"} | ${row.tier ?? "n/a"}`,
            },
            {
              label: "Preferred pick",
              value: row.recommended_pick,
              note: `Posterior favorite: ${row.favorite ?? "n/a"}`,
            },
            {
              label: "Alternate path",
              value:
                row.candidate_diff_flag && row.alternate_pick
                  ? row.alternate_pick
                  : "Same pick in both candidates",
              note: row.alternate_note ?? "No alternate path divergence",
            },
            {
              label: "Posterior uncertainty",
              value: (
                <ProbabilityTrack
                  meanProbability={row.favorite_prob}
                  lowerProbability={row.ci_lower}
                  upperProbability={row.ci_upper}
                  axisMin={0.5}
                  axisMax={1}
                  confidenceTier={row.tier}
                  valueLabel="Posterior mean"
                  intervalLabel="Posterior credible interval"
                />
              ),
              plot: true,
            },
            {
              label: "How to use this",
              value: "How to use this",
              note: row.usage_note ?? row.rationale ?? "Review this slot before locking the bracket.",
            },
          ]}
        />
      ))}
      <div className="comparison-board__table-fallback table-shell">
        <table className="dashboard-table" data-testid="ranked-decisions-table">
          <thead>
            <tr>
              <th>Rank</th>
              <th>Round</th>
              <th>Matchup</th>
              <th>Tier</th>
            </tr>
          </thead>
          <tbody>
            {rows.map((row) => (
              <tr key={`table-${row.rank}`} className={inspectionClass(row.inspection_level)}>
                <td>{row.rank}</td>
                <td>{row.round}</td>
                <td>{row.matchup}</td>
                <td>{row.tier}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </ComparisonBoard>
  );
}
