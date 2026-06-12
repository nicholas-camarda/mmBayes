import { tierColor } from "../../lib/tierColors";

const LEGEND_TIERS = ["Lock", "Lean", "Toss-up", "Volatile"] as const;

export function BracketTreeLegend() {
  return (
    <div className="btree-legend" aria-label="Bracket tree legend">
      {LEGEND_TIERS.map((tier) => (
        <span key={tier} className="btree-legend-item">
          <span className="btree-legend-swatch" style={{ background: tierColor(tier) }} aria-hidden />
          {tier}
        </span>
      ))}
      <span className="btree-legend-item">
        <span style={{ fontWeight: 800, color: "#e76f51", fontSize: 11 }}>U</span>
        Upset pick
      </span>
      <span className="btree-legend-item">
        <span className="btree-legend-line" aria-hidden />
        Candidate 2 route change
      </span>
    </div>
  );
}
