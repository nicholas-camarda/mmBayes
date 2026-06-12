import type { MatchupContextRow, WatchlistRow } from "../../types/payload";
import { MATCHUP_FEATURE_SPECS, formatFeatureValue, sameConferenceLabel } from "../../lib/matchupFeatures";
import { AdvantageChart } from "./AdvantageChart";
import { TeamCard } from "./TeamCard";

interface EvidencePanelProps {
  rows: MatchupContextRow[];
  watchlist?: WatchlistRow[];
  openEvidenceId: string | null;
  onClose: () => void;
}

function findEvidenceRow(
  rows: MatchupContextRow[],
  watchlist: WatchlistRow[] | undefined,
  evidenceId: string | null,
): Record<string, unknown> | undefined {
  if (!evidenceId) return undefined;

  const slotKey = evidenceId.replace(/^evidence-/, "");
  const watchlistRow = watchlist?.find(
    (row) =>
      row.evidence_id === evidenceId ||
      row.slot_key === slotKey ||
      `evidence-${row.slot_key}` === evidenceId,
  );
  const contextRow = rows.find(
    (row) =>
      row.evidence_id === evidenceId ||
      `evidence-${row.slot_key}` === evidenceId ||
      row.slot_key === slotKey,
  );

  if (watchlistRow && contextRow) {
    return { ...contextRow, ...watchlistRow };
  }
  return (watchlistRow ?? contextRow) as Record<string, unknown> | undefined;
}

function teamName(row: Record<string, unknown>, side: "A" | "B"): string {
  const direct = row[side === "A" ? "teamA" : "teamB"];
  if (typeof direct === "string" && direct) return direct;
  const featureName = row[side === "A" ? "teamA_Team" : "teamB_Team"];
  return typeof featureName === "string" && featureName ? featureName : `Team ${side}`;
}

function teamStats(row: Record<string, unknown>, side: "A" | "B") {
  return MATCHUP_FEATURE_SPECS.slice(0, 6).map((spec) => ({
    label: spec.label,
    value: formatFeatureValue(
      row[side === "A" ? spec.teamAColumn : spec.teamBColumn],
      spec.digits,
    ),
  }));
}

export function EvidencePanel({
  rows,
  watchlist,
  openEvidenceId,
  onClose,
}: EvidencePanelProps) {
  const row = findEvidenceRow(rows, watchlist, openEvidenceId);
  const teamAName = row ? teamName(row, "A") : "Team A";
  const teamBName = row ? teamName(row, "B") : "Team B";
  const matchupLabel = row
    ? String(row.matchup_label ?? `${teamAName} vs ${teamBName}`)
    : "Select a matchup";

  return (
    <section id="evidence" aria-label="Matchup evidence" className="evidence-section">
      <div className="role-kicker role-kicker--evidence">Understand why</div>
      <h2>Matchup Evidence</h2>
      <p className="section-note">
        Targeted matchup drawers surfaced by the review queue and bracket tree. Use jump buttons to
        open a specific drawer.
      </p>
      <div className="evidence-shell">
        {!openEvidenceId ? (
          <p className="missing-section">
            Select a bracket node, divergence cell, or review card to open evidence.
          </p>
        ) : !row ? (
          <p className="missing-section">Evidence details are not available for {openEvidenceId}.</p>
        ) : (
          <details
            className="evidence-panel"
            id={openEvidenceId}
            open
            data-testid="evidence-card"
          >
            <summary>
              <div className="evidence-panel__summary">
                <div>
                  <div className="surface-pill">Matchup evidence</div>
                  <h3>{matchupLabel}</h3>
                </div>
                <button
                  type="button"
                  className="evidence-close"
                  onClick={(event) => {
                    event.preventDefault();
                    onClose();
                  }}
                >
                  Close
                </button>
              </div>
            </summary>
            <div className="evidence-panel__body">
              <p className="evidence-panel__lede">
                <strong>{String(row.posterior_favorite ?? "n/a")}</strong> is the posterior favorite
                at{" "}
                {typeof row.win_prob_favorite === "number"
                  ? `${(100 * row.win_prob_favorite).toFixed(1)}%`
                  : "n/a"}
                .
              </p>
              <dl className="evidence-summary">
                <dt>Round</dt>
                <dd>{String(row.round ?? "n/a")}</dd>
                <dt>Region</dt>
                <dd>{String(row.region ?? "n/a")}</dd>
                <dt>Confidence</dt>
                <dd>{String(row.confidence_tier ?? "n/a")}</dd>
                <dt>Candidate usage</dt>
                <dd>{String(row.candidate_usage ?? "n/a")}</dd>
              </dl>
              {row.why_this_matters ? (
                <p className="evidence-callout">{String(row.why_this_matters)}</p>
              ) : null}
              {row.downstream_implication_text ? (
                <p className="evidence-callout evidence-callout--implication">
                  {String(row.downstream_implication_text)}
                </p>
              ) : null}
              {row.rationale_short ? <p>{String(row.rationale_short)}</p> : null}

              <div className="matchup-context-flags" aria-label="Model context flags">
                <span className="context-flag context-flag--conference">
                  <span>Conference</span>
                  <strong>{sameConferenceLabel(row.same_conf)}</strong>
                </span>
              </div>

              <div className="evidence-team-grid">
                <TeamCard
                  teamName={teamAName}
                  seed={row.teamA_Seed as number | string | undefined}
                  stats={teamStats(row, "A")}
                />
                <TeamCard
                  teamName={teamBName}
                  seed={row.teamB_Seed as number | string | undefined}
                  stats={teamStats(row, "B")}
                />
              </div>

              <AdvantageChart row={row} teamAName={teamAName} teamBName={teamBName} />
            </div>
          </details>
        )}
      </div>
    </section>
  );
}
