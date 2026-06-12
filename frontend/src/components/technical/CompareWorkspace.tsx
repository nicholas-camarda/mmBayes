import { useState } from "react";
import type {
  CandidateProfile,
  TechnicalCandidateDifferenceRow,
  TechnicalDecisionRow,
  UpsetOpportunityRow,
} from "../../types/payload";
import { BoardExplainer } from "./BoardExplainer";
import { CandidateProfilePanel } from "./CandidateProfilePanel";
import { DivergenceBoard } from "./DivergenceBoard";
import { RankedDecisionBoard } from "./RankedDecisionBoard";
import { UpsetOpportunityBoard } from "./UpsetOpportunityBoard";

type ViewId = "compare" | "candidate-1" | "candidate-2";

interface CompareWorkspaceProps {
  rankedDecisions?: TechnicalDecisionRow[] | null;
  candidateDifferences?: TechnicalCandidateDifferenceRow[] | null;
  upsetOpportunities?: UpsetOpportunityRow[] | null;
  candidateProfiles?: CandidateProfile[] | null;
}

export function CompareWorkspace({
  rankedDecisions,
  candidateDifferences,
  upsetOpportunities,
  candidateProfiles,
}: CompareWorkspaceProps) {
  const [view, setView] = useState<ViewId>("compare");
  const profileFor = (candidateId: number) =>
    candidateProfiles?.find((profile) => profile.candidate_id === candidateId) ?? null;

  return (
    <section className="technical-panel" aria-label="Compare workspace" data-testid="compare-workspace">
      <div className="role-kicker role-kicker--act">Act now</div>
      <h2>Compare workspace</h2>
      <div
        className="toggle-bar compare-workspace__nav"
        role="group"
        aria-label="Candidate dashboard view"
      >
        {(
          [
            ["compare", "Compare"],
            ["candidate-1", "Candidate 1"],
            ["candidate-2", "Candidate 2"],
          ] as const
        ).map(([id, label]) => (
          <button
            key={id}
            type="button"
            className={`toggle-button${view === id ? " is-active" : ""}`}
            aria-pressed={view === id}
            onClick={() => setView(id)}
          >
            {label}
          </button>
        ))}
      </div>
      {view === "compare" ? (
        <>
          <div className="technical-subpanel">
            <h3>Ranked decision board</h3>
            <p className="section-note">
              Highest-impact review slots with preferred picks, alternate paths, and posterior
              uncertainty in a responsive layout that uses space better than the legacy SVG boards.
            </p>
            <BoardExplainer
              what="Highest-impact review slots with preferred and alternate paths."
              how="Read ranked decisions first, then upset pivots, then divergence forks."
              why="These are the slots most likely to change your bracket outcome."
              math="Ranked by decision_score; upset board sorted by upset_leverage."
            />
            <RankedDecisionBoard rows={rankedDecisions ?? []} />
          </div>
          <div className="technical-subpanel">
            <h3>Upset opportunity board</h3>
            <p className="section-note">
              Underdog pivots sorted by leverage so the highest-payoff swings appear first.
            </p>
            <UpsetOpportunityBoard rows={upsetOpportunities ?? []} />
          </div>
          <div className="technical-subpanel">
            <h3>Candidate divergence</h3>
            <p className="section-note">
              Only the slots that change between Candidate 1 and Candidate 2, focused on where the
              paths actually fork.
            </p>
            <DivergenceBoard rows={candidateDifferences ?? []} />
          </div>
        </>
      ) : (
        <div className="technical-subpanel">
          <h3>{view === "candidate-1" ? "Candidate 1" : "Candidate 2"} focus</h3>
          <p className="section-note">
            Path-specific overview and the most fragile selected matchups on this candidate bracket.
          </p>
          <CandidateProfilePanel
            profile={profileFor(view === "candidate-1" ? 1 : 2)}
          />
        </div>
      )}
    </section>
  );
}
