import type { Candidate, CandidateSummaryRow, WatchlistRow } from "../../types/payload";

interface NextActionPanelProps {
  candidates: Candidate[];
  candidateSummaries?: CandidateSummaryRow[] | null;
  watchlist?: WatchlistRow[] | null;
}

function summaryFor(
  summaries: CandidateSummaryRow[] | null | undefined,
  candidateId: number,
): CandidateSummaryRow | undefined {
  return summaries?.find((row) => row.candidate_id === candidateId);
}

export function NextActionPanel({
  candidates,
  candidateSummaries,
  watchlist,
}: NextActionPanelProps) {
  const primary = candidates.find((c) => c.candidate_id === 1) ?? candidates[0];
  const alternate = candidates.find((c) => c.candidate_id === 2) ?? candidates[1];
  const primarySummary = primary ? summaryFor(candidateSummaries, primary.candidate_id) : undefined;
  const reviewCount = watchlist?.length ?? 0;
  const bracketChangeCount =
    watchlist?.filter((row) => row.reason_surface === "Bracket-changing toss-ups").length ?? 0;
  const tiebreaker =
    primarySummary?.recommended_tiebreaker_points != null
      ? String(primarySummary.recommended_tiebreaker_points)
      : "n/a";

  return (
    <section className="next-action-panel" aria-label="Next bracket entry action">
      <div>
        <div className="section-label">Next action</div>
        <h3>Enter Candidate 1, then pause at the marked review picks.</h3>
        <p>
          Candidate 1 is the baseline bracket. Candidate 2 is the alternate if you want the changed
          route shown below.
        </p>
      </div>
      <div className="next-action-grid">
        <div className="next-action-stat">
          <span>Baseline champion</span>
          <strong>{primary?.champion ?? "n/a"}</strong>
        </div>
        <div className="next-action-stat">
          <span>Alternate champion</span>
          <strong>{alternate?.champion ?? "n/a"}</strong>
        </div>
        <div className="next-action-stat next-action-stat--review">
          <span>Review picks</span>
          <strong>{reviewCount}</strong>
          <small>{bracketChangeCount} bracket-changing</small>
        </div>
        <div className="next-action-stat">
          <span>Baseline tiebreaker</span>
          <strong>{tiebreaker}</strong>
        </div>
      </div>
    </section>
  );
}
