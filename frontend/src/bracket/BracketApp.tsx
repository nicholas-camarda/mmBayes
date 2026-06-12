import { useState } from "react";
import type { BracketPayload, DivergenceMapRow } from "../types/payload";
import { BuildMetadataBanner } from "../components/BuildMetadataBanner";
import { CandidateSummaryPanel } from "../components/CandidateSummaryPanel";
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
      <header className="hero">
        <div className="eyebrow">{payload.bracket_year} mmBayes bracket dashboard</div>
        <h1>Bracket entry workspace</h1>
        <p className="lede">
          Enter Candidate 1 like a bracket sheet, pause on review picks, then inspect the alternate
          route and evidence before you lock in your {payload.bracket_year} entry.
        </p>
        <BuildMetadataBanner metadata={payload.build_metadata} />
        <JumpNav />
      </header>

      <StatusPanel playInResolution={payload.play_in_resolution} />

      <section className="section section--entry" id="build">
        <div className="role-kicker role-kicker--act">Enter bracket</div>
        <h2>Candidate Recommendations</h2>
        <p className="section-note">
          Use this section like a bracket sheet: choose Candidate 1 or Candidate 2, copy winners by
          region and round, then enter the champion and tiebreaker. Review markers show the picks
          worth pausing on.
        </p>
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
      </section>

      <section className="section" id="review-queue">
        <div className="role-kicker role-kicker--act">Decide now</div>
        <h2>Review Queue</h2>
        <p className="section-note">
          Use this after the fill-in view. Read the divergence route map to see where Candidate 2
          actually splits, then work the queue for bracket-changing disagreements, upset pivots, and
          fragile favorites that deserve manual attention.
        </p>
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
      </section>

      {payload.bracket_tree?.trees?.length ? (
        <section className="section">
          <div className="role-kicker role-kicker--evidence">Understand why</div>
          <h2>Bracket Tree</h2>
          <p className="section-note">
            Each node is a game in the selected candidate path. Color shows confidence tier. Click or
            hover for context, then open the evidence drawer for the full matchup summary.
          </p>
          <BracketTree trees={payload.bracket_tree.trees} onOpenEvidence={openEvidence} />
        </section>
      ) : (
        <MissingSection label="Bracket tree" />
      )}

      <EvidencePanel
        rows={payload.matchup_context ?? []}
        openEvidenceId={openEvidenceId}
        onClose={() => {
          setOpenEvidenceId(null);
          setDivergenceFilter(null);
        }}
      />

      <CandidatePathsPanel candidates={payload.candidates} />

      <section className="section" id="technical-appendix">
        <div className="role-kicker role-kicker--reference">Reference</div>
        <h2>Technical appendix</h2>
        <p className="section-note">
          Decision sheet, play-in resolution, and candidate summaries for offline reference.
        </p>
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
          <pre data-testid="play-in-panel">
            {JSON.stringify(payload.play_in_resolution[0], null, 2)}
          </pre>
        ) : (
          <MissingSection label="Play-in resolution" />
        )}
      </section>
    </main>
  );
}
