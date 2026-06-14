import type { CSSProperties } from "react";
import { formatProbability, formatProbabilityInterval, tierClassName } from "../../lib/format";
import { tierColor } from "../../lib/tierColors";

interface ProbabilityTrackProps {
  meanProbability?: number;
  lowerProbability?: number;
  upperProbability?: number;
  axisMin?: number;
  axisMax?: number;
  color?: string;
  confidenceTier?: string;
  valueLabel?: string;
  intervalLabel?: string;
}

function clampPosition(value: number, min: number, max: number): number {
  if (max <= min) return 50;
  return Math.max(0, Math.min(100, ((value - min) / (max - min)) * 100));
}

function niceRange(lo: number, hi: number): [number, number] {
  if (hi <= lo) return [0, 1];
  const pad = (hi - lo) * 0.15;
  const rawMin = lo - pad;
  const rawMax = hi + pad;
  // Round to nice 5% boundaries
  const step = 0.05;
  const niceMin = Math.max(0, Math.floor(rawMin / step) * step);
  const niceMax = Math.min(1, Math.ceil(rawMax / step) * step);
  return [niceMin, niceMax === niceMin ? niceMin + step : niceMax];
}

function axisTicks(lo: number, hi: number): number[] {
  if (hi <= lo) return [lo, lo + 0.25, lo + 0.5];
  const mid = (lo + hi) / 2;
  return [lo, mid, hi];
}

export function ProbabilityTrack({
  meanProbability,
  lowerProbability,
  upperProbability,
  axisMin,
  axisMax,
  color,
  confidenceTier,
  valueLabel = "Favorite probability",
  intervalLabel = "Credible interval",
}: ProbabilityTrackProps) {
  const trackColor = color ?? tierColor(confidenceTier);
  const mean = meanProbability ?? 0.5;
  const lower = lowerProbability ?? mean;
  const upper = upperProbability ?? mean;

  // Auto-scale unless explicit bounds provided
  const [visMin, visMax] =
    axisMin != null && axisMax != null
      ? [axisMin, axisMax]
      : niceRange(Math.min(lower, mean), Math.max(upper, mean));

  const meanPos = clampPosition(mean, visMin, visMax);
  const lowerPos = clampPosition(lower, visMin, visMax);
  const upperPos = clampPosition(upper, visMin, visMax);
  const ticks = axisTicks(visMin, visMax);

  const laneLabel = `${valueLabel} ${formatProbability(meanProbability)} with ${intervalLabel.toLowerCase()} ${formatProbabilityInterval(lowerProbability, upperProbability)}`;

  return (
    <div
      className="prob-track"
      data-tier={tierClassName(confidenceTier)}
      style={{ "--track-color": trackColor } as CSSProperties}
    >
      <div className="prob-track__summary">
        <div className="prob-track__stat prob-track__stat-hero">
          <span>{valueLabel}</span>
          <strong>{formatProbability(meanProbability)}</strong>
        </div>
        <div className="prob-track__stat">
          <span>{intervalLabel}</span>
          <strong>
            {formatProbability(lowerProbability)} – {formatProbability(upperProbability)}
          </strong>
        </div>
      </div>

      <div className="prob-track__chart">
        <div className="prob-track__lane" role="img" aria-label={laneLabel}>
          <div
            className="prob-track__range"
            style={{
              left: `${lowerPos}%`,
              width: `${Math.max(upperPos - lowerPos, 1)}%`,
            }}
          />
          <span className="prob-track__point" style={{ left: `${meanPos}%` }} aria-hidden="true" />
        </div>
        <div className="prob-track__scale" aria-hidden="true">
          {ticks.map((tick, index) => (
            <span
              key={`t-${tick}`}
              className={`prob-track__scale-label${
                index === 0 ? " prob-track__scale-label--start" : ""
              }${index === ticks.length - 1 ? " prob-track__scale-label--end" : ""}`}
            >
              {formatProbability(tick)}
            </span>
          ))}
        </div>
      </div>
    </div>
  );
}
