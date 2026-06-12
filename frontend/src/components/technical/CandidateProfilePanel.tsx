import type { CandidateProfile } from "../../types/payload";
import { CandidateFragilityChart } from "./CandidateFragilityChart";

interface CandidateProfilePanelProps {
  profile?: CandidateProfile | null;
}

export function CandidateProfilePanel({ profile }: CandidateProfilePanelProps) {
  if (!profile) {
    return <p className="missing-section">Candidate profile is not available.</p>;
  }

  return (
    <div className="candidate-profile-panel" data-testid="candidate-profile-panel">
      <div className="candidate-profile-panel__summary">
        <div className="guide-card">
          <div className="guide-label">Champion</div>
          <p>{profile.champion ?? "n/a"}</p>
        </div>
        <div className="guide-card">
          <div className="guide-label">Final Four</div>
          <p>{profile.final_four ?? "n/a"}</p>
        </div>
        <div className="guide-card">
          <div className="guide-label">Bracket profile</div>
          <p>
            {profile.type ?? "n/a"}
            {profile.mean_game_prob != null
              ? ` | mean game prob ${(100 * profile.mean_game_prob).toFixed(1)}%`
              : ""}
          </p>
        </div>
      </div>
      {profile.diff_summary ? (
        <p className="section-note candidate-profile-panel__diff">{profile.diff_summary}</p>
      ) : null}
      <h4>Fragile picks on this path</h4>
      <p className="section-note">
        These are the selected matchups with the widest uncertainty or highest decision score on
        this candidate path.
      </p>
      <CandidateFragilityChart rows={profile.fragile_picks ?? []} />
    </div>
  );
}
