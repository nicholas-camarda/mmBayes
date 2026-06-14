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

function axisTickValues(axisMin: number, axisMax: number, tickCount = 3): number[] {
  if (tickCount < 2 || axisMax <= axisMin) {
    return [axisMin, axisMax];
  }
  const step = (axisMax - axisMin) / (tickCount - 1);
  return Array.from({ length: tickCount }, (_, index) => axisMin + step * index);
}

export function ProbabilityTrack({
  meanProbability,
  lowerProbability,
  upperProbability,
  axisMin = 0.5,
  axisMax = 1,
  color,
  confidenceTier,
  valueLabel = "Favorite probability",
  intervalLabel = "Credible interval",
}: ProbabilityTrackProps) {
  const trackColor = color ?? tierColor(confidenceTier);
  const mean = meanProbability ?? 0.5;
  const lower = lowerProbability ?? mean;
  const upper = upperProbability ?? mean;
  const meanPosition = clampPosition(mean, axisMin, axisMax);
  const lowerPosition = clampPosition(lower, axisMin, axisMax);
  const upperPosition = clampPosition(upper, axisMin, axisMax);
  const ticks = axisTickValues(axisMin, axisMax, 3);
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
          <strong>{formatProbabilityInterval(lowerProbability, upperProbability)}</strong>
        </div>
      </div>

      <div className="prob-track__chart">
        <div className="prob-track__lane" role="img" aria-label={laneLabel}>
          <div className="prob-track__grid" aria-hidden="true">
            {ticks.map((tick) => (
              <span
                key={tick}
                className="prob-track__grid-line"
                style={{ left: `${clampPosition(tick, axisMin, axisMax)}%` }}
              />
            ))}
          </div>
          <div
            className="prob-track__range"
            style={{
              left: `${lowerPosition}%`,
              width: `${Math.max(upperPosition - lowerPosition, 2)}%`,
            }}
          />
          <span
            className="prob-track__bound prob-track__bound--lower"
            style={{ left: `${lowerPosition}%` }}
            aria-hidden="true"
          >
            <em>{formatProbability(lower)}</em>
          </span>
          <span
            className="prob-track__bound prob-track__bound--upper"
            style={{ left: `${upperPosition}%` }}
            aria-hidden="true"
          >
            <em>{formatProbability(upper)}</em>
          </span>
          <span className="prob-track__point" style={{ left: `${meanPosition}%` }} aria-hidden="true" />
        </div>
        <div className="prob-track__scale" aria-hidden="true">
          {ticks.map((tick, index) => (
            <span
              key={`label-${tick}`}
              className={[
                "prob-track__scale-label",
                index === 0 ? "prob-track__scale-label--start" : "",
                index === ticks.length - 1 ? "prob-track__scale-label--end" : "",
              ]
                .filter(Boolean)
                .join(" ")}
            >
              {formatProbability(tick)}
            </span>
          ))}
        </div>
      </div>
    </div>
  );
}
