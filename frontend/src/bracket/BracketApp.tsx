import { useState } from "react";
import type { BracketPayload, DivergenceMapRow } from "../types/payload";
import { CandidateSummaryPanel } from "../components/CandidateSummaryPanel";
import { DashboardHero } from "../components/DashboardHero";
import { DashboardSection } from "../components/DashboardSection";
import { DecisionSheetTable } from "../components/DecisionSheetTable";
import { MissingSection } from "../components/MissingSection";
import { BracketTree } from "../components/bracket/BracketTree";
import { DivergenceMap } from "../components/bracket/DivergenceMap";
import { EvidencePanel } from "../components/bracket/EvidencePanel";
import { JumpNav } from "../components/bracket/JumpNav";
import { StatusPanel } from "../components/bracket/StatusPanel";
import { NextActionPanel } from "../components/bracket/NextActionPanel";
import { CandidateComparisonPanel } from "../components/bracket/CandidateComparisonPanel";
import { EntryWorkspace } from "../components/bracket/EntryWorkspace";
import { WatchlistPanel } from "../components/bracket/WatchlistPanel";
import { CandidatePathsPanel } from "../components/bracket/CandidatePathsPanel";
import { PlayInStatusPanel } from "../components/bracket/PlayInStatusPanel";
import { ReadingGuidePanel } from "../components/bracket/ReadingGuidePanel";
import { ConfidenceLegend } from "../components/bracket/ConfidenceLegend";

export function BracketApp({ payload }: { payload: BracketPayload }) {
  const [openEvidenceId, setOpenEvidenceId] = useState<string | null>(null);
  const [divergenceFilter, setDivergenceFilter] = useState<{
    round: string;
    region: string;
  } | null>(null);

  const openEvidence = (evidenceId: string) => {
    setOpenEvidenceId(evidenceId);
    const target = document.getElementById("evidence");
    target?.scrollIntoView?.({ behavior: "smooth", block: "start" });
  };

  const openEvidenceFromDivergence = (evidenceId: string, row: DivergenceMapRow) => {
    if (row.all_in_watchlist) {
      setDivergenceFilter({ round: String(row.round), region: String(row.region) });
    } else {
      setDivergenceFilter(null);
    }
    openEvidence(evidenceId);
  };

  return (
    <main className="dashboard page">
      <DashboardHero
        eyebrow={`${payload.bracket_year} mmBayes bracket dashboard`}
        title="Bracket entry workspace"
        lede={`Enter Candidate 1 like a bracket sheet, pause on review picks, then inspect the alternate route and evidence before you lock in your ${payload.bracket_year} entry.`}
        metadata={payload.build_metadata}
        nav={<JumpNav />}
      />

      <StatusPanel playInResolution={payload.play_in_resolution} />

      <ReadingGuidePanel />

      <DashboardSection
        id="build"
        roleTone="action"
        kicker="Enter bracket"
        title="Candidate Recommendations"
        note="Use this section like a bracket sheet: choose Candidate 1 or Candidate 2, copy winners by region and round, then enter the champion and tiebreaker. Review markers show the picks worth pausing on."
      >
        <NextActionPanel
          candidates={payload.candidates}
          candidateSummaries={payload.candidate_summaries}
          watchlist={payload.watchlist}
        />
        <CandidateComparisonPanel
          candidates={payload.candidates}
          decisionSheet={payload.decision_sheet}
        />
        <EntryWorkspace
          candidates={payload.candidates}
          decisionSheet={payload.decision_sheet}
          candidateSummaries={payload.candidate_summaries}
          onOpenEvidence={openEvidence}
        />
        <div className="candidate-grid candidate-grid--detail">
          <CandidateSummaryPanel candidates={payload.candidates} />
        </div>
      </DashboardSection>

      <DashboardSection
        id="review-queue"
        roleTone="action"
        kicker="Decide now"
        title="Review Queue"
        note="Use this after the fill-in view. Read the divergence route map to see where Candidate 2 actually splits, then work the queue for bracket-changing disagreements, upset pivots, and fragile favorites that deserve manual attention."
      >
        <ConfidenceLegend />
        {payload.divergence_map && payload.divergence_map.length > 0 ? (
          <DivergenceMap
            rows={payload.divergence_map}
            activeEvidenceId={openEvidenceId}
            onOpenEvidence={openEvidenceFromDivergence}
          />
        ) : (
          <MissingSection label="Divergence map" />
        )}
        {payload.watchlist && payload.watchlist.length > 0 ? (
          <WatchlistPanel
            watchlist={payload.watchlist}
            decisionSheet={payload.decision_sheet}
            activeEvidenceId={openEvidenceId}
            divergenceFilter={divergenceFilter}
            onOpenEvidence={openEvidence}
          />
        ) : (
          <MissingSection label="Review queue" />
        )}
      </DashboardSection>

      {payload.bracket_tree?.trees?.length ? (
        <DashboardSection
          roleTone="evidence"
          kicker="Understand why"
          title="Bracket Tree"
          note="Each node is a game in the selected candidate path. Color shows confidence tier. Click or hover for context, then open the evidence drawer for the full matchup summary."
        >
          <BracketTree trees={payload.bracket_tree.trees} onOpenEvidence={openEvidence} />
        </DashboardSection>
      ) : (
        <MissingSection label="Bracket tree" />
      )}

      <EvidencePanel
        rows={payload.matchup_context ?? []}
        watchlist={payload.watchlist}
        openEvidenceId={openEvidenceId}
        onClose={() => {
          setOpenEvidenceId(null);
          setDivergenceFilter(null);
        }}
      />

      <CandidatePathsPanel candidates={payload.candidates} />

      <DashboardSection
        id="technical-appendix"
        roleTone="reference"
        kicker="Reference"
        title="Technical appendix"
        note="Decision sheet, play-in resolution, and candidate summaries for offline reference."
      >
        <div className="diagnostic-callout">
          <strong>Need more diagnostics?</strong>
          <p>
            The technical dashboard keeps ensemble validation, calibration, backtest, and component
            diagnostics separate from the bracket workflow.
          </p>
          <p>
            <a href="technical.html">Open technical dashboard</a>
          </p>
        </div>
        <h3>Key decisions</h3>
        <DecisionSheetTable rows={payload.decision_sheet} />
        <h3>Play-in status</h3>
        {payload.play_in_resolution && payload.play_in_resolution.length > 0 ? (
          <PlayInStatusPanel rows={payload.play_in_resolution} />
        ) : (
          <MissingSection label="Play-in resolution" />
        )}
      </DashboardSection>
    </main>
  );
}
