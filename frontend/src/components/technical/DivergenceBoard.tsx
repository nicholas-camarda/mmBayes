import type { TechnicalCandidateDifferenceRow } from "../../types/payload";
import { BoardRow, ComparisonBoard } from "./ComparisonBoard";

interface DivergenceBoardProps {
  rows: TechnicalCandidateDifferenceRow[];
}

export function DivergenceBoard({ rows }: DivergenceBoardProps) {
  if (rows.length === 0) {
    return <p className="missing-section">No candidate differences were recorded.</p>;
  }

  return (
    <ComparisonBoard
      variant="divergence"
      testId="candidate-differences-board"
      columns={["Divergent slot", "Candidate 1", "Candidate 2", "Why it matters"]}
    >
      {rows.map((row) => (
        <BoardRow
          key={`${row.round}-${row.slot}-${row.matchup}`}
          variant="divergence"
          cells={[
            {
              label: "Divergent slot",
              value: row.matchup,
              note: `${row.round ?? "n/a"} | ${row.region ?? "n/a"} | ${row.tier ?? "n/a"}`,
            },
            {
              label: "Candidate 1",
              value: row.candidate_1,
            },
            {
              label: "Candidate 2",
              value: row.candidate_2,
            },
            {
              label: "Why it matters",
              value: "Why it matters",
              note:
                row.why ??
                (row.leverage != null
                  ? `Leverage score ${row.leverage.toFixed(2)}.`
                  : "This slot changes the downstream bracket path."),
            },
          ]}
        />
      ))}
      <div className="comparison-board__table-fallback table-shell">
        <table className="dashboard-table" data-testid="candidate-differences-table">
          <thead>
            <tr>
              <th>Round</th>
              <th>Matchup</th>
              <th>Candidate 1</th>
              <th>Candidate 2</th>
            </tr>
          </thead>
          <tbody>
            {rows.map((row) => (
              <tr key={`table-${row.round}-${row.slot}`}>
                <td>{row.round}</td>
                <td>{row.matchup}</td>
                <td>{row.candidate_1}</td>
                <td>{row.candidate_2}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </ComparisonBoard>
  );
}
