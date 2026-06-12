import type { DivergenceMapRow } from "../../types/payload";

const ROUND_ORDER = [
  "Round of 64",
  "Round of 32",
  "Sweet 16",
  "Elite 8",
  "Final Four",
  "Championship",
];

interface DivergenceMapProps {
  rows: DivergenceMapRow[];
  onOpenEvidence: (evidenceId: string) => void;
}

export function DivergenceMap({ rows, onOpenEvidence }: DivergenceMapProps) {
  if (rows.length === 0) {
    return <p className="missing-section">Divergence map is not available in this payload.</p>;
  }

  const rounds = ROUND_ORDER.filter((round) => rows.some((row) => row.round === round));

  return (
    <section className="divergence-map-panel" aria-label="Divergence map">
      <div className="divergence-map-panel__head">
        <h3>Divergence Map</h3>
        <p className="section-note">
          Active cells show where Candidate 2 departs from the baseline by round and region.
        </p>
      </div>
      <div className="divergence-map divergence-map--route">
        {rounds.map((round) => {
          const roundRows = rows.filter((row) => row.round === round);
          const activeCount = roundRows.filter((row) => (row.total_count ?? 0) > 0).length;
          return (
            <section
              key={round}
              className={`divergence-round${roundRows.some((row) => row.late_round_only) ? " divergence-round--late" : ""}`}
            >
              <div className="divergence-round__head">
                <div className="divergence-round__title">{round}</div>
                <div className="divergence-round__note">
                  {activeCount === 0
                    ? "No candidate split buckets."
                    : `${activeCount} split bucket${activeCount === 1 ? "" : "s"} in this round.`}
                </div>
              </div>
              <div className="divergence-round__grid">
                {roundRows.map((row) => {
                  const hasDivergence = (row.total_count ?? 0) > 0;
                  const targetId = row.target_evidence_id ?? "";
                  const className = [
                    "divergence-cell",
                    hasDivergence ? "is-active" : "divergence-cell--empty",
                    row.late_round_only ? "divergence-cell--late" : "",
                  ]
                    .filter(Boolean)
                    .join(" ");

                  if (!hasDivergence || !targetId) {
                    return (
                      <div key={`${row.round}-${row.region}`} className={className}>
                        <div className="divergence-cell__region">{row.region}</div>
                        <div className="divergence-cell__count">0</div>
                      </div>
                    );
                  }

                  return (
                    <button
                      key={`${row.round}-${row.region}`}
                      type="button"
                      className={className}
                      data-divergence-round={row.round}
                      data-divergence-region={row.region}
                      data-open-evidence={targetId}
                      onClick={() => onOpenEvidence(targetId)}
                    >
                      <div className="divergence-cell__region">{row.region}</div>
                      <div className="divergence-cell__count">{row.total_count}</div>
                      <div className="divergence-cell__meta">
                        {(row.winner_change_count ?? 0) > 0 ? (
                          <span className="divergence-pill divergence-pill--winner">
                            {row.winner_change_count} winner
                          </span>
                        ) : null}
                        {(row.path_only_count ?? 0) > 0 ? (
                          <span className="divergence-pill divergence-pill--path">
                            {row.path_only_count} path only
                          </span>
                        ) : null}
                      </div>
                    </button>
                  );
                })}
              </div>
            </section>
          );
        })}
      </div>
    </section>
  );
}
