import type { DecisionSheetRow } from "../types/payload";

export function DecisionSheetTable({ rows }: { rows: DecisionSheetRow[] }) {
  const sorted = [...rows].sort(
    (a, b) =>
      (a.decision_rank ?? Number.MAX_SAFE_INTEGER) - (b.decision_rank ?? Number.MAX_SAFE_INTEGER),
  );
  return (
    <section aria-label="Decision sheet" className="table-shell">
      <table data-testid="decision-sheet" className="decision-sheet dashboard-table">
        <thead>
          <tr>
            <th>Rank</th>
            <th>Matchup</th>
            <th>Round</th>
            <th>Favorite</th>
            <th>Win prob</th>
            <th>Tier</th>
            <th>Pick (C1)</th>
            <th>Pick (C2)</th>
            <th>Diverges</th>
          </tr>
        </thead>
        <tbody>
          {sorted.map((row) => (
            <tr key={row.slot_key} className={row.candidate_diff_flag ? "diverges" : undefined}>
              <td>{row.decision_rank ?? ""}</td>
              <td>{row.matchup_label ?? row.slot_key}</td>
              <td>{row.round ?? ""}</td>
              <td>{row.posterior_favorite ?? ""}</td>
              <td>{row.win_prob_favorite != null ? row.win_prob_favorite.toFixed(3) : ""}</td>
              <td>{row.confidence_tier ?? ""}</td>
              <td>{row.candidate_1_pick ?? ""}</td>
              <td>{row.candidate_2_pick ?? ""}</td>
              <td>{row.candidate_diff_flag ? "yes" : ""}</td>
            </tr>
          ))}
        </tbody>
      </table>
    </section>
  );
}
