import type { CandidateFragilePickRow } from "../../types/payload";
import { formatProbability, formatProbabilityInterval } from "../../lib/format";
import { tierColor } from "../../lib/tierColors";

interface CandidateFragilityChartProps {
  rows: CandidateFragilePickRow[];
}

function clampPosition(value: number, min: number, max: number): number {
  if (max <= min) return 50;
  return Math.max(0, Math.min(100, ((value - min) / (max - min)) * 100));
}

export function CandidateFragilityChart({ rows }: CandidateFragilityChartProps) {
  if (rows.length === 0) {
    return <p className="missing-section">No fragile picks were recorded for this candidate.</p>;
  }

  return (
    <div className="fragility-chart" data-testid="candidate-fragility-chart" role="img" aria-label="Candidate fragility board">
      <div className="fragility-chart__header">
        <span>Most fragile selected matchups</span>
        <div className="fragility-chart__axis" aria-hidden="true">
          {[0, 0.2, 0.4, 0.6, 0.8, 1].map((tick) => (
            <span key={tick}>{formatProbability(tick)}</span>
          ))}
        </div>
      </div>
      {rows.map((row) => {
        const mean = row.chosen_prob ?? 0.5;
        const lower = row.chosen_ci_lower ?? mean;
        const upper = row.chosen_ci_upper ?? mean;
        const color = tierColor(row.confidence_tier);
        const lowerPos = clampPosition(lower, 0, 1);
        const upperPos = clampPosition(upper, 0, 1);
        const meanPos = clampPosition(mean, 0, 1);

        return (
          <div key={`${row.round}-${row.matchup_label}`} className="fragility-row">
            <div className="fragility-row__meta">
              <strong>{row.matchup_label}</strong>
              <span>
                {row.winner} | {formatProbability(mean)} | {row.round}
              </span>
            </div>
            <div className="fragility-row__lane">
              <div
                className="fragility-row__range"
                style={{
                  left: `${lowerPos}%`,
                  width: `${Math.max(upperPos - lowerPos, 1)}%`,
                  background: color,
                }}
              />
              <div
                className="fragility-row__point"
                style={{ left: `${meanPos}%`, background: color }}
              />
            </div>
            <div className="fragility-row__interval">
              {formatProbabilityInterval(lower, upper)}
            </div>
          </div>
        );
      })}
    </div>
  );
}
