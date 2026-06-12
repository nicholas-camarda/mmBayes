import { useState } from "react";
import type { BracketPayload } from "../types/payload";
import { BuildMetadataBanner } from "../components/BuildMetadataBanner";
import { CandidateSummaryPanel } from "../components/CandidateSummaryPanel";
import { DecisionSheetTable } from "../components/DecisionSheetTable";
import { MissingSection } from "../components/MissingSection";
import { BracketTree } from "../components/bracket/BracketTree";
import { DivergenceMap } from "../components/bracket/DivergenceMap";
import { EvidencePanel } from "../components/bracket/EvidencePanel";

export function BracketApp({ payload }: { payload: BracketPayload }) {
  const [openEvidenceId, setOpenEvidenceId] = useState<string | null>(null);

  const openEvidence = (evidenceId: string) => {
    setOpenEvidenceId(evidenceId);
    const target = document.getElementById("evidence");
    target?.scrollIntoView({ behavior: "smooth", block: "start" });
  };

  return (
    <main className="dashboard">
      <header>
        <h1>{payload.bracket_year} Bracket Dashboard</h1>
        <BuildMetadataBanner metadata={payload.build_metadata} />
      </header>

      <section id="build">
        <h2>Candidate brackets</h2>
        <CandidateSummaryPanel candidates={payload.candidates} />
      </section>

      {payload.divergence_map && payload.divergence_map.length > 0 ? (
        <DivergenceMap rows={payload.divergence_map} onOpenEvidence={openEvidence} />
      ) : (
        <MissingSection label="Divergence map" />
      )}

      {payload.bracket_tree?.trees?.length ? (
        <BracketTree trees={payload.bracket_tree.trees} onOpenEvidence={openEvidence} />
      ) : (
        <MissingSection label="Bracket tree" />
      )}

      <EvidencePanel
        rows={payload.matchup_context ?? []}
        openEvidenceId={openEvidenceId}
        onClose={() => setOpenEvidenceId(null)}
      />

      <h2>Key decisions</h2>
      <DecisionSheetTable rows={payload.decision_sheet} />

      <h2>Play-in status</h2>
      {payload.play_in_resolution && payload.play_in_resolution.length > 0 ? (
        <pre data-testid="play-in-panel">
          {JSON.stringify(payload.play_in_resolution[0], null, 2)}
        </pre>
      ) : (
        <MissingSection label="Play-in resolution" />
      )}
    </main>
  );
}
