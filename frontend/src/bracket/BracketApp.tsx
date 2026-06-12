import type { BracketPayload } from "../types/payload";
import { BuildMetadataBanner } from "../components/BuildMetadataBanner";
import { CandidateSummaryPanel } from "../components/CandidateSummaryPanel";
import { DecisionSheetTable } from "../components/DecisionSheetTable";
import { MissingSection } from "../components/MissingSection";

export function BracketApp({ payload }: { payload: BracketPayload }) {
  return (
    <main className="dashboard">
      <header>
        <h1>{payload.bracket_year} Bracket Dashboard</h1>
        <BuildMetadataBanner metadata={payload.build_metadata} />
      </header>
      <h2>Candidate brackets</h2>
      <CandidateSummaryPanel candidates={payload.candidates} />
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
      <h2>Matchup context</h2>
      {payload.matchup_context && payload.matchup_context.length > 0 ? (
        <p data-testid="matchup-context-count">
          {payload.matchup_context.length} matchup context rows available.
        </p>
      ) : (
        <MissingSection label="Matchup context" />
      )}
    </main>
  );
}
