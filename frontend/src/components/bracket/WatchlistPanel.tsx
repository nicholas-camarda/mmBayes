import { useMemo, useState } from "react";
import type { DecisionSheetRow, WatchlistRow } from "../../types/payload";
import { ProbabilityTrack } from "./ProbabilityTrack";

interface WatchlistPanelProps {
  watchlist: WatchlistRow[];
  decisionSheet: DecisionSheetRow[];
  activeEvidenceId?: string | null;
  divergenceFilter?: { round: string; region: string } | null;
  onOpenEvidence: (evidenceId: string) => void;
}

const SURFACE_FILTERS = [
  { id: "all", label: "All surfaces" },
  { id: "Bracket-changing toss-ups", label: "Bracket-changing" },
  { id: "Upset pivots", label: "Upset pivots" },
  { id: "Fragile favorites", label: "Fragile favorites" },
] as const;

function surfaceCount(rows: WatchlistRow[], surface: string): number {
  return rows.filter((row) => row.reason_surface === surface).length;
}

export function WatchlistPanel({
  watchlist,
  decisionSheet,
  activeEvidenceId,
  divergenceFilter,
  onOpenEvidence,
}: WatchlistPanelProps) {
  const [surfaceFilter, setSurfaceFilter] = useState<string>("all");
  const [expanded, setExpanded] = useState(false);

  const filteredRows = useMemo(() => {
    return watchlist.filter((row) => {
      if (surfaceFilter !== "all" && row.reason_surface !== surfaceFilter) {
        return false;
      }
      if (divergenceFilter) {
        return (
          row.candidate_diff_flag === true &&
          row.round === divergenceFilter.round &&
          row.region === divergenceFilter.region
        );
      }
      return true;
    });
  }, [watchlist, surfaceFilter, divergenceFilter]);

  if (watchlist.length === 0) {
    return <p className="missing-section">No rows met the current watchlist rules.</p>;
  }

  const divergentCount = decisionSheet.filter((row) => row.candidate_diff_flag).length;

  return (
    <div className="review-board" aria-label="Review queue board">
      <div className="review-board__head">
        <div>
          <div className="role-kicker role-kicker--act">Work the queue</div>
          <h3>Review picks that still need attention</h3>
          <p>
            The divergence map above explains where the alternate path splits. These counts,
            filters, and cards are the action board for deciding which picks to inspect.
          </p>
        </div>
      </div>
      <div className="summary-strip">
        <div className="summary-chip">
          <span>Candidate 2 changes</span>
          <strong>{divergentCount}</strong>
        </div>
        <div className="summary-chip">
          <span>Bracket-changing rows</span>
          <strong>{surfaceCount(watchlist, "Bracket-changing toss-ups")}</strong>
        </div>
        <div className="summary-chip">
          <span>Upset pivots</span>
          <strong>{surfaceCount(watchlist, "Upset pivots")}</strong>
        </div>
        <div className="summary-chip">
          <span>Fragile favorites</span>
          <strong>{surfaceCount(watchlist, "Fragile favorites")}</strong>
        </div>
      </div>
      <div className="watchlist-toolbar filter-toolbar" role="toolbar" aria-label="Watchlist filters">
        {SURFACE_FILTERS.map((filter) => (
          <button
            key={filter.id}
            type="button"
            className={`filter-chip${surfaceFilter === filter.id ? " is-active" : ""}`}
            onClick={() => setSurfaceFilter(filter.id)}
          >
            {filter.label}
          </button>
        ))}
      </div>
      <div className={`watchlist-shell${expanded ? " is-expanded" : ""}`} data-shell="watchlist">
        {filteredRows.map((row, index) => {
          const evidenceId = String(row.evidence_id ?? `evidence-${row.slot_key}`);
          const hiddenByBucket =
            divergenceFilter != null &&
            !(
              row.candidate_diff_flag === true &&
              row.round === divergenceFilter.round &&
              row.region === divergenceFilter.region
            );
          const collapsed = !expanded && index >= 5;
          const matchupLabel = String(
            row.matchup_label ?? `${row.teamA ?? "n/a"} vs ${row.teamB ?? "n/a"}`,
          );

          return (
            <article
              key={evidenceId}
              className={[
                "watchlist-card",
                collapsed ? "collapsed-row" : "",
                hiddenByBucket ? "is-hidden-by-bucket" : "",
              ]
                .filter(Boolean)
                .join(" ")}
              data-surface={String(row.reason_surface ?? "")}
              data-evidence-id={evidenceId}
              data-round={String(row.round ?? "")}
              data-region={String(row.region ?? "")}
              data-candidate-diff={row.candidate_diff_flag ? "true" : "false"}
            >
              <div className="watchlist-card__head">
                <div>
                  <div className="surface-pill">{String(row.reason_surface ?? "Watchlist")}</div>
                  <h3>{matchupLabel}</h3>
                  <div className="watchlist-card__meta-row">
                    <span>{String(row.round ?? "n/a")}</span>
                    <span>{String(row.region ?? "n/a")}</span>
                    <span>
                      Seeds {String(row.teamA_seed ?? "n/a")} vs {String(row.teamB_seed ?? "n/a")}
                    </span>
                  </div>
                </div>
                <button
                  type="button"
                  className="jump-button"
                  data-open-evidence={evidenceId}
                  onClick={() => onOpenEvidence(evidenceId)}
                >
                  Open evidence
                </button>
              </div>
              <div className="watchlist-card__body">
                <div className="watchlist-card__callout">
                  <span>Why this matters</span>
                  <strong>{String(row.why_this_matters ?? "No summary available.")}</strong>
                </div>
                <div className="watchlist-card__callout">
                  <span>Candidate usage</span>
                  <strong>{String(row.candidate_usage ?? "n/a")}</strong>
                </div>
              </div>
              {typeof row.win_prob_favorite === "number" ? (
                <div className="watchlist-card__probability">
                  <ProbabilityTrack
                    meanProbability={row.win_prob_favorite as number}
                    lowerProbability={row.ci_lower as number | undefined}
                    upperProbability={row.ci_upper as number | undefined}
                    confidenceTier={
                      row.confidence_tier != null ? String(row.confidence_tier) : undefined
                    }
                    axisMin={0.5}
                    axisMax={1}
                  />
                </div>
              ) : null}
              {activeEvidenceId === evidenceId ? (
                <p className="watchlist-card__note">Evidence drawer open below.</p>
              ) : null}
            </article>
          );
        })}
      </div>
      {filteredRows.length > 5 ? (
        <div className="show-more-row">
          <button
            type="button"
            className="show-more-button"
            data-shell-toggle="watchlist"
            onClick={() => setExpanded((value) => !value)}
          >
            {expanded ? "Show fewer" : "Show more"}
          </button>
        </div>
      ) : null}
    </div>
  );
}
