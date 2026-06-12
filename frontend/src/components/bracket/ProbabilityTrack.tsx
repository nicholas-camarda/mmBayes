import { tierColor } from "../../lib/tierColors";
import { formatProbability, formatProbabilityInterval } from "../../lib/format";

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

  return (
    <div className="prob-track">
      <div className="prob-track__summary">
        <div className="prob-track__stat">
          <span>{valueLabel}</span>
          <strong>{formatProbability(meanProbability)}</strong>
        </div>
        <div className="prob-track__stat">
          <span>{intervalLabel}</span>
          <strong>{formatProbabilityInterval(lowerProbability, upperProbability)}</strong>
        </div>
      </div>
      <div className="prob-track__lane">
        <div
          className="prob-track__range"
          style={{
            left: `${lowerPosition}%`,
            width: `${Math.max(upperPosition - lowerPosition, 1)}%`,
            background: trackColor,
          }}
        />
        <div
          className="prob-track__point"
          style={{
            left: `${meanPosition}%`,
            background: trackColor,
          }}
        />
      </div>
    </div>
  );
}
