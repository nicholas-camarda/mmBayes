import { useMemo, useState } from "react";
import type { Candidate, CandidateSummaryRow, DecisionSheetRow } from "../../types/payload";
import {
  ENTRY_REGION_ORDER,
  ENTRY_ROUND_ORDER,
  entryFillStep,
  entryRegionDisplay,
} from "../../lib/entryRegions";
import { tierClassName } from "../../lib/format";

interface EntryWorkspaceProps {
  candidates: Candidate[];
  decisionSheet: DecisionSheetRow[];
  candidateSummaries?: CandidateSummaryRow[] | null;
  onOpenEvidence: (evidenceId: string) => void;
}

interface EntryRow extends DecisionSheetRow {
  regionDisplay: string;
  entryMatchup: string;
  entryWinner: string;
  evidenceId: string;
  reviewNeeded: boolean;
  reviewLabel: string;
  seedLabel: string;
  confidenceTierClass: string;
}

function needsReview(row: DecisionSheetRow): boolean {
  const level = String(row.inspection_level ?? "");
  return level === "primary" || level === "secondary" || Boolean(row.candidate_diff_flag);
}

function buildEntryRows(decisionSheet: DecisionSheetRow[], candidateId: number): EntryRow[] {
  const matchupColumn = candidateId === 1 ? "matchup_label" : "candidate_2_matchup";
  const winnerColumn = candidateId === 1 ? "candidate_1_pick" : "candidate_2_pick";

  return decisionSheet
    .map((row) => {
      const regionDisplay = entryRegionDisplay(
        row.region != null ? String(row.region) : undefined,
        row.round != null ? String(row.round) : undefined,
      );
      const entryMatchup = String(row[matchupColumn] ?? row.matchup_label ?? "n/a");
      const entryWinner = String(row[winnerColumn] ?? "n/a");
      const reviewNeeded = needsReview(row);
      const reviewLabel = row.candidate_diff_flag
        ? "Review"
        : String(row.inspection_level) === "primary" || String(row.inspection_level) === "secondary"
          ? "Review"
          : "Ready";
      const teamASeed = row.teamA_seed != null ? String(row.teamA_seed) : "n/a";
      const teamBSeed = row.teamB_seed != null ? String(row.teamB_seed) : "n/a";

      return {
        ...row,
        regionDisplay,
        entryMatchup,
        entryWinner,
        evidenceId: `evidence-${String(row.slot_key)}`,
        reviewNeeded,
        reviewLabel,
        seedLabel: `${teamASeed} vs ${teamBSeed}`,
        confidenceTierClass: tierClassName(
          row.confidence_tier != null ? String(row.confidence_tier) : undefined,
        ),
      };
    })
    .sort((a, b) => {
      const regionA = ENTRY_REGION_ORDER.indexOf(
        a.regionDisplay as (typeof ENTRY_REGION_ORDER)[number],
      );
      const regionB = ENTRY_REGION_ORDER.indexOf(
        b.regionDisplay as (typeof ENTRY_REGION_ORDER)[number],
      );
      const roundA = ENTRY_ROUND_ORDER.indexOf(a.round as (typeof ENTRY_ROUND_ORDER)[number]);
      const roundB = ENTRY_ROUND_ORDER.indexOf(b.round as (typeof ENTRY_ROUND_ORDER)[number]);
      const regionOrderA = regionA === -1 ? ENTRY_REGION_ORDER.length : regionA;
      const regionOrderB = regionB === -1 ? ENTRY_REGION_ORDER.length : regionB;
      const roundOrderA = roundA === -1 ? ENTRY_ROUND_ORDER.length : roundA;
      const roundOrderB = roundB === -1 ? ENTRY_ROUND_ORDER.length : roundB;
      if (regionOrderA !== regionOrderB) return regionOrderA - regionOrderB;
      if (roundOrderA !== roundOrderB) return roundOrderA - roundOrderB;
      return Number(a.matchup_number ?? 0) - Number(b.matchup_number ?? 0);
    });
}

function EntryPanel({
  candidateId,
  candidate,
  summary,
  rows,
  active,
  onOpenEvidence,
}: {
  candidateId: number;
  candidate?: Candidate;
  summary?: CandidateSummaryRow;
  rows: EntryRow[];
  active: boolean;
  onOpenEvidence: (evidenceId: string) => void;
}) {
  const grouped = useMemo(() => {
    const groups = new Map<string, EntryRow[]>();
    for (const row of rows) {
      const bucket = groups.get(row.regionDisplay) ?? [];
      bucket.push(row);
      groups.set(row.regionDisplay, bucket);
    }
    return ENTRY_REGION_ORDER.filter((region) => groups.has(region)).map((region) => ({
      region,
      rows: groups.get(region) ?? [],
    }));
  }, [rows]);

  return (
    <div
      className={`entry-panel${active ? " is-active" : ""}`}
      data-entry-panel={`candidate-${candidateId}`}
      hidden={!active}
    >
      <div className="entry-summary-panel" aria-label="Selected candidate entry summary">
        <div className="entry-summary-panel__head">
          <span>Entry summary</span>
          <strong>Candidate {candidateId}</strong>
        </div>
        <div className="entry-summary-strip">
          <div className="entry-summary-item">
            <span>Champion</span>
            <strong>{summary?.champion ?? candidate?.champion ?? "n/a"}</strong>
          </div>
          <div className="entry-summary-item entry-summary-item--wide">
            <span>Final Four</span>
            <strong>
              {summary?.final_four ??
                (candidate?.final_four ? candidate.final_four.join(", ") : "n/a")}
            </strong>
          </div>
          <div className="entry-summary-item">
            <span>Title game</span>
            <strong>{summary?.championship_matchup ?? "n/a"}</strong>
          </div>
          <div className="entry-summary-item entry-summary-item--tiebreaker">
            <span>Tiebreaker</span>
            <strong>
              {summary?.recommended_tiebreaker_points != null
                ? String(summary.recommended_tiebreaker_points)
                : "n/a"}
            </strong>
          </div>
        </div>
      </div>
      <div className="entry-grid">
        {grouped.map(({ region, rows: regionRows }) => (
          <section
            key={region}
            className={`entry-region entry-region--${region.toLowerCase().replace(/[^a-z0-9]+/g, "-")}`}
          >
            <div className="entry-region__head">
              <h4>{region}</h4>
              <span>{entryFillStep(region)}</span>
            </div>
            <div className="entry-pick-list">
              {regionRows.map((row) => (
                <article
                  key={String(row.slot_key)}
                  className={[
                    "entry-pick",
                    `entry-pick--tier-${row.confidenceTierClass}`,
                    row.reviewNeeded ? "entry-pick--review" : "",
                    row.candidate_diff_flag ? "entry-pick--diff" : "",
                  ]
                    .filter(Boolean)
                    .join(" ")}
                >
                  <div className="entry-pick__round">{String(row.round ?? "n/a")}</div>
                  <div className="entry-pick__main">
                    <strong>{row.entryWinner}</strong>
                    <span>{row.entryMatchup}</span>
                  </div>
                  <div className="entry-pick__meta">
                    <span>Seeds {row.seedLabel}</span>
                    <span
                      className={`entry-tier-badge entry-tier-badge--${row.confidenceTierClass}`}
                    >
                      Confidence: {String(row.confidence_tier ?? "n/a")}
                    </span>
                    {row.candidate_diff_flag ? (
                      <span className="entry-split-badge">C1/C2 split</span>
                    ) : null}
                  </div>
                  {row.reviewNeeded ? (
                    <button
                      type="button"
                      className="entry-review-button"
                      data-open-evidence={row.evidenceId}
                      onClick={() => onOpenEvidence(row.evidenceId)}
                    >
                      {row.reviewLabel}
                    </button>
                  ) : (
                    <span className="entry-check">Ready to enter</span>
                  )}
                </article>
              ))}
            </div>
          </section>
        ))}
      </div>
    </div>
  );
}

export function EntryWorkspace({
  candidates,
  decisionSheet,
  candidateSummaries,
  onOpenEvidence,
}: EntryWorkspaceProps) {
  const [activeCandidateId, setActiveCandidateId] = useState(
    candidates[0]?.candidate_id ?? 1,
  );

  if (decisionSheet.length === 0) {
    return <p className="missing-section">Candidate entry rows are not available for this run.</p>;
  }

  return (
    <div className="entry-workspace">
      <div className="entry-controls" role="group" aria-label="Candidate entry selector">
        {candidates.map((candidate) => (
          <button
            key={candidate.candidate_id}
            type="button"
            className={`entry-toggle${candidate.candidate_id === activeCandidateId ? " is-active" : ""}`}
            data-entry-target={`candidate-${candidate.candidate_id}`}
            onClick={() => setActiveCandidateId(candidate.candidate_id)}
          >
            Candidate {candidate.candidate_id}
          </button>
        ))}
      </div>
      {candidates.map((candidate) => (
        <EntryPanel
          key={candidate.candidate_id}
          candidateId={candidate.candidate_id}
          candidate={candidate}
          summary={candidateSummaries?.find((row) => row.candidate_id === candidate.candidate_id)}
          rows={buildEntryRows(decisionSheet, candidate.candidate_id)}
          active={candidate.candidate_id === activeCandidateId}
          onOpenEvidence={onOpenEvidence}
        />
      ))}
    </div>
  );
}
