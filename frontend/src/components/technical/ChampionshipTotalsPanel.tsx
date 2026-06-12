import type { ChampionshipTotalsPayload } from "../../types/payload";
import { MissingSection } from "../MissingSection";

interface CandidateSummary {
  candidate_id?: number;
  championship_matchup?: string;
  recommended_tiebreaker_points?: number;
  predicted_total_median?: number;
  predicted_total_80_lower?: number;
  predicted_total_80_upper?: number;
}

function formatInterval(lower?: number, upper?: number): string {
  if (lower == null || upper == null || !Number.isFinite(lower) || !Number.isFinite(upper)) {
    return "n/a";
  }
  return `${Math.round(lower)}-${Math.round(upper)}`;
}

export function ChampionshipTotalsPanel({
  totals,
}: {
  totals?: ChampionshipTotalsPayload | null;
}) {
  if (!totals?.candidate_summaries?.length && !totals?.championship_distribution?.length) {
    return <MissingSection label="Championship tiebreaker comparison" />;
  }

  const scale = totals.scale ?? { min_total: 100, max_total: 200, max_probability: 0.05 };
  const minTotal = scale.min_total ?? 100;
  const maxTotal = scale.max_total ?? 200;
  const maxProbability = scale.max_probability ?? 0.05;
  const span = Math.max(maxTotal - minTotal, 1);

  const byCandidate = new Map<number, NonNullable<ChampionshipTotalsPayload["championship_distribution"]>>();
  for (const row of totals.championship_distribution ?? []) {
    const candidateId = Number(row.candidate_id ?? 0);
    const bucket = byCandidate.get(candidateId) ?? [];
    bucket.push(row);
    byCandidate.set(candidateId, bucket);
  }

  return (
    <section
      className="technical-panel"
      aria-label="Championship tiebreaker comparison"
      data-testid="championship-totals"
    >
      <h2>Championship tiebreaker comparison</h2>
      <p className="section-note">
        Recommended tiebreaker values are the rounded median championship total for each candidate
        path. Bars show the probability mass over exact totals on a shared axis.
      </p>
      <div className="distribution-grid">
        {(totals.candidate_summaries ?? []).map((rawSummary) => {
          const summary = rawSummary as CandidateSummary;
          const candidateId = Number(summary.candidate_id ?? 0);
          const distribution = [...(byCandidate.get(candidateId) ?? [])].sort(
            (a, b) => Number(a.total_points ?? 0) - Number(b.total_points ?? 0),
          );
          const width = 460;
          const height = 220;
          const marginLeft = 42;
          const marginRight = 24;
          const marginTop = 18;
          const marginBottom = 42;
          const plotWidth = width - marginLeft - marginRight;
          const plotHeight = height - marginTop - marginBottom;
          const toX = (total: number) =>
            marginLeft + ((total - minTotal) / span) * plotWidth;
          const toY = (probability: number) =>
            marginTop + plotHeight - (probability / maxProbability) * plotHeight;
          const barWidth = Math.max(3, plotWidth / Math.max(distribution.length, 40));
          const recommended = summary.recommended_tiebreaker_points;
          const median = summary.predicted_total_median;
          const intervalLow = summary.predicted_total_80_lower;
          const intervalHigh = summary.predicted_total_80_upper;
          const peak = distribution.reduce(
            (best, row) =>
              Number(row.probability ?? 0) > Number(best.probability ?? 0) ? row : best,
            distribution[0] ?? {},
          );

          return (
            <div key={candidateId} className="technical-subpanel">
              <h3>Candidate {candidateId} championship distribution</h3>
              <div className="distribution-meta">
                <p>
                  <strong>Projected title game:</strong>{" "}
                  {String(summary.championship_matchup ?? "n/a")}
                </p>
                <p>
                  <strong>Recommended tiebreaker:</strong>{" "}
                  {String(summary.recommended_tiebreaker_points ?? "n/a")}
                </p>
                <p>
                  <strong>Median total:</strong>{" "}
                  {median != null ? Number(median).toFixed(1) : "n/a"}
                  <span className="muted">
                    {" "}
                    | 80% interval {formatInterval(intervalLow, intervalHigh)}
                  </span>
                </p>
                {peak?.total_points != null ? (
                  <p>
                    <strong>Most likely total:</strong> {Math.round(Number(peak.total_points))}
                    <span className="muted">
                      {" "}
                      | peak probability{" "}
                      {typeof peak.probability === "number"
                        ? `${(100 * peak.probability).toFixed(1)}%`
                        : "n/a"}
                    </span>
                  </p>
                ) : null}
                <p className="distribution-note">
                  Use the recommended tiebreaker as the default entry. It is the model&apos;s rounded
                  median championship total, so it minimizes miss distance more often than chasing an
                  extreme outcome.
                </p>
              </div>
              {distribution.length > 0 ? (
                <svg
                  viewBox={`0 0 ${width} ${height}`}
                  className="tech-svg tiebreaker-svg"
                  role="img"
                  aria-label={`Candidate ${candidateId} championship tiebreaker distribution`}
                >
                  <line
                    x1={marginLeft}
                    y1={marginTop + plotHeight}
                    x2={marginLeft + plotWidth}
                    y2={marginTop + plotHeight}
                    stroke="#94a3b8"
                    strokeWidth={1}
                  />
                  <line
                    x1={marginLeft}
                    y1={marginTop}
                    x2={marginLeft}
                    y2={marginTop + plotHeight}
                    stroke="#94a3b8"
                    strokeWidth={1}
                  />
                  {intervalLow != null && intervalHigh != null ? (
                    <rect
                      x={toX(intervalLow)}
                      y={marginTop + 6}
                      width={Math.max(6, toX(intervalHigh) - toX(intervalLow))}
                      height={plotHeight - 12}
                      fill="#fde68a"
                      fillOpacity={0.35}
                      rx={5}
                    />
                  ) : null}
                  {distribution.map((row, index) => {
                    const total = Number(row.total_points ?? minTotal);
                    const probability = Number(row.probability ?? 0);
                    const x = toX(total) - barWidth / 2;
                    const y = toY(probability);
                    const barHeight = marginTop + plotHeight - y;
                    return (
                      <rect
                        key={`${candidateId}-${index}`}
                        x={x}
                        y={y}
                        width={barWidth}
                        height={barHeight}
                        rx={1.5}
                        fill="#7c9bff"
                        fillOpacity={0.75}
                      />
                    );
                  })}
                  {recommended != null ? (
                    <line
                      x1={toX(recommended)}
                      y1={marginTop}
                      x2={toX(recommended)}
                      y2={marginTop + plotHeight}
                      stroke="#dc2626"
                      strokeWidth={2}
                      strokeDasharray="6 4"
                    />
                  ) : null}
                  {median != null ? (
                    <line
                      x1={toX(median)}
                      y1={marginTop}
                      x2={toX(median)}
                      y2={marginTop + plotHeight}
                      stroke="#1d4ed8"
                      strokeWidth={2}
                    />
                  ) : null}
                  {peak?.total_points != null && peak.probability != null ? (
                    <circle
                      cx={toX(Number(peak.total_points))}
                      cy={toY(Number(peak.probability))}
                      r={4.5}
                      fill="#1d4ed8"
                      stroke="#ffffff"
                      strokeWidth={1.5}
                    />
                  ) : null}
                  <text x={marginLeft} y={height - 10} fontSize={11} fill="#6b7280">
                    {Math.round(minTotal)}
                  </text>
                  <text
                    x={marginLeft + plotWidth}
                    y={height - 10}
                    textAnchor="end"
                    fontSize={11}
                    fill="#6b7280"
                  >
                    {Math.round(maxTotal)}
                  </text>
                  <text
                    x={marginLeft + plotWidth / 2}
                    y={height - 10}
                    textAnchor="middle"
                    fontSize={11}
                    fill="#6b7280"
                  >
                    Championship total points
                  </text>
                  <text
                    x={12}
                    y={marginTop + plotHeight / 2}
                    fontSize={11}
                    fill="#6b7280"
                    transform={`rotate(-90 12 ${marginTop + plotHeight / 2})`}
                  >
                    Probability
                  </text>
                  <rect
                    x={width - 168}
                    y={14}
                    width={10}
                    height={10}
                    fill="#fde68a"
                    fillOpacity={0.55}
                    rx={2}
                  />
                  <text x={width - 152} y={23} fontSize={10} fill="#6b7280">
                    80% interval
                  </text>
                  <line
                    x1={width - 168}
                    y1={38}
                    x2={width - 156}
                    y2={38}
                    stroke="#1d4ed8"
                    strokeWidth={2}
                  />
                  <text x={width - 152} y={41} fontSize={10} fill="#6b7280">
                    Median
                  </text>
                  <line
                    x1={width - 96}
                    y1={38}
                    x2={width - 84}
                    y2={38}
                    stroke="#dc2626"
                    strokeWidth={2}
                    strokeDasharray="6 4"
                  />
                  <text x={width - 80} y={41} fontSize={10} fill="#6b7280">
                    Recommended
                  </text>
                </svg>
              ) : (
                <p className="missing-section">Distribution not available for this candidate.</p>
              )}
            </div>
          );
        })}
      </div>
    </section>
  );
}
