import type { UpsetOpportunityRow } from "../../types/payload";
import { ProbabilityTrack } from "../bracket/ProbabilityTrack";
import { BoardRow, ComparisonBoard } from "./ComparisonBoard";

interface UpsetOpportunityBoardProps {
  rows: UpsetOpportunityRow[];
}

function candidateUsageNote(row: UpsetOpportunityRow): string {
  if (row.candidate_diff_flag) {
    return `Candidate 1 takes ${row.candidate_1_pick ?? "n/a"}; Candidate 2 takes ${row.candidate_2_pick ?? "n/a"}.`;
  }
  return `Both candidates currently take ${row.candidate_1_pick ?? "n/a"}.`;
}

export function UpsetOpportunityBoard({ rows }: UpsetOpportunityBoardProps) {
  if (rows.length === 0) {
    return <p className="missing-section">No upset opportunities were recorded.</p>;
  }

  return (
    <ComparisonBoard
      variant="upset"
      testId="upset-opportunities-board"
      columns={[
        "Underdog pivot",
        "Underdog posterior uncertainty",
        "Candidate usage",
        "Why to consider it",
      ]}
    >
      {rows.map((row) => (
        <BoardRow
          key={`${row.round}-${row.matchup}`}
          variant="upset"
          cells={[
            {
              label: "Underdog pivot",
              value: `${row.underdog ?? "n/a"} over ${row.favorite ?? "n/a"}`,
              note: `Round: ${row.round ?? "n/a"} | Tier: ${row.tier ?? "n/a"}`,
            },
            {
              label: "Underdog posterior uncertainty",
              value: (
                <ProbabilityTrack
                  meanProbability={row.underdog_prob}
                  lowerProbability={row.underdog_ci_lower}
                  upperProbability={row.underdog_ci_upper}
                  axisMin={0}
                  axisMax={0.5}
                  confidenceTier={row.tier}
                  valueLabel="Underdog posterior mean"
                  intervalLabel="Derived posterior credible interval"
                />
              ),
              plot: true,
            },
            {
              label: "Candidate usage",
              value: "Candidate usage",
              note: candidateUsageNote(row),
            },
            {
              label: "Why to consider it",
              value: "Why to consider it",
              note:
                row.pivot_note ??
                (row.leverage != null
                  ? `Leverage score ${row.leverage.toFixed(2)}.`
                  : "High-payoff pivot worth manual review."),
            },
          ]}
        />
      ))}
    </ComparisonBoard>
  );
}
