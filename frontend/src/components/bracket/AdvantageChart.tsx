import {
  MATCHUP_FEATURE_SPECS,
  advantageDirection,
  advantageTrackWidth,
  favorLabel,
  formatFeatureValue,
} from "../../lib/matchupFeatures";

interface AdvantageChartProps {
  row: Record<string, unknown>;
  teamAName: string;
  teamBName: string;
}

export function AdvantageChart({ row, teamAName, teamBName }: AdvantageChartProps) {
  const rows = MATCHUP_FEATURE_SPECS.map((spec) => {
    const teamAValue = row[spec.teamAColumn];
    const teamBValue = row[spec.teamBColumn];
    const diffValue = row[spec.diffColumn];
    const favored = favorLabel(
      typeof diffValue === "number" ? diffValue : Number(diffValue),
      spec.preferredDirection,
      teamAName,
      teamBName,
    );
    const direction = advantageDirection(favored, teamAName, teamBName);
    const trackWidth = advantageTrackWidth(
      typeof diffValue === "number" ? diffValue : Number(diffValue),
      typeof teamAValue === "number" ? teamAValue : Number(teamAValue),
      typeof teamBValue === "number" ? teamBValue : Number(teamBValue),
      direction,
    );

    return {
      feature: spec.label,
      teamA: formatFeatureValue(teamAValue, spec.digits),
      teamB: formatFeatureValue(teamBValue, spec.digits),
      diff: formatFeatureValue(diffValue, spec.digits),
      favored,
      direction,
      trackWidth,
    };
  });

  return (
    <div className="advantage-chart" data-testid="advantage-chart">
      <div className="advantage-chart__header">
        <span>{teamAName}</span>
        <span>Advantage</span>
        <span>{teamBName}</span>
      </div>
      {rows.map((metric) => (
        <div key={metric.feature} className="advantage-row">
          <div className="advantage-row__values">
            <span>{metric.teamA}</span>
            <span className="advantage-row__feature">{metric.feature}</span>
            <span>{metric.teamB}</span>
          </div>
          <div className="advantage-row__lane" aria-hidden="true">
            <div className="advantage-row__center" />
            {metric.direction === "left" ? (
              <div
                className="advantage-row__bar advantage-row__bar--left"
                style={{ width: `${metric.trackWidth}%` }}
              />
            ) : null}
            {metric.direction === "right" ? (
              <div
                className="advantage-row__bar advantage-row__bar--right"
                style={{ width: `${metric.trackWidth}%` }}
              />
            ) : null}
          </div>
          <div className="advantage-row__favor">
            {metric.favored === "Context only" ? "Context only" : `Favors ${metric.favored}`}
          </div>
        </div>
      ))}
    </div>
  );
}
