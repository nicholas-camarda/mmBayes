import type { ChampionshipTotalsPayload } from "../../types/payload";
import { MissingSection } from "../MissingSection";

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
        {(totals.candidate_summaries ?? []).map((summary) => {
          const candidateId = Number(summary.candidate_id ?? 0);
          const distribution = byCandidate.get(candidateId) ?? [];
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

          return (
            <div key={candidateId} className="technical-subpanel">
              <h3>Candidate {candidateId} championship distribution</h3>
              <p>
                <strong>Recommended tiebreaker:</strong>{" "}
                {String(summary.recommended_tiebreaker_points ?? "n/a")}
              </p>
              <p>
                <strong>Median total:</strong>{" "}
                {summary.predicted_total_median != null
                  ? Number(summary.predicted_total_median).toFixed(1)
                  : "n/a"}
              </p>
              {distribution.length > 0 ? (
                <svg viewBox={`0 0 ${width} ${height}`} className="tech-svg" role="img">
                  <rect
                    x={marginLeft}
                    y={marginTop}
                    width={plotWidth}
                    height={plotHeight}
                    fill="#0b1220"
                    stroke="#334155"
                  />
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
