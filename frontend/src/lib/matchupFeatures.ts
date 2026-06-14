export type PreferredDirection = "higher" | "lower" | "neutral";

export interface MatchupFeatureSpec {
  /** Human-readable label shown in the comparison chart. */
  label: string;
  /** Torvik / model column name, shown in hover help when different from label. */
  code: string;
  /** Plain-language definition for hover tooltips. */
  definition: string;
  teamAColumn: string;
  teamBColumn: string;
  diffColumn: string;
  digits: number;
  preferredDirection: PreferredDirection;
}

export const MATCHUP_FEATURE_SPECS: MatchupFeatureSpec[] = [
  {
    label: "Seed",
    code: "Seed",
    definition: "Tournament seed. Lower numbers are stronger on paper.",
    teamAColumn: "teamA_Seed",
    teamBColumn: "teamB_Seed",
    diffColumn: "seed_diff",
    digits: 0,
    preferredDirection: "lower",
  },
  {
    label: "Overall strength",
    code: "Barthag logit",
    definition: "Logit-transformed Barthag rating summarizing overall team strength.",
    teamAColumn: "teamA_barthag_logit",
    teamBColumn: "teamB_barthag_logit",
    diffColumn: "barthag_logit_diff",
    digits: 3,
    preferredDirection: "higher",
  },
  {
    label: "Adj. offense",
    code: "AdjOE",
    definition: "Adjusted offensive efficiency (points scored per 100 possessions).",
    teamAColumn: "teamA_AdjOE",
    teamBColumn: "teamB_AdjOE",
    diffColumn: "AdjOE_diff",
    digits: 1,
    preferredDirection: "higher",
  },
  {
    label: "Adj. defense",
    code: "AdjDE",
    definition: "Adjusted defensive efficiency allowed. Lower is better.",
    teamAColumn: "teamA_AdjDE",
    teamBColumn: "teamB_AdjDE",
    diffColumn: "AdjDE_diff",
    digits: 1,
    preferredDirection: "lower",
  },
  {
    label: "Wins above bubble",
    code: "WAB",
    definition: "Torvik wins above bubble — resume quality vs the tournament field.",
    teamAColumn: "teamA_WAB",
    teamBColumn: "teamB_WAB",
    diffColumn: "WAB_diff",
    digits: 1,
    preferredDirection: "higher",
  },
  {
    label: "Turnover rate",
    code: "TOR",
    definition: "Team turnover rate. Lower is better on offense.",
    teamAColumn: "teamA_TOR",
    teamBColumn: "teamB_TOR",
    diffColumn: "TOR_diff",
    digits: 3,
    preferredDirection: "lower",
  },
  {
    label: "Forced turnovers",
    code: "TORD",
    definition: "Opponent turnover rate forced by this defense.",
    teamAColumn: "teamA_TORD",
    teamBColumn: "teamB_TORD",
    diffColumn: "TORD_diff",
    digits: 3,
    preferredDirection: "higher",
  },
  {
    label: "Off. rebounding",
    code: "ORB",
    definition: "Offensive rebounding rate.",
    teamAColumn: "teamA_ORB",
    teamBColumn: "teamB_ORB",
    diffColumn: "ORB_diff",
    digits: 3,
    preferredDirection: "higher",
  },
  {
    label: "Def. rebounding",
    code: "DRB",
    definition: "Defensive rebounding rate.",
    teamAColumn: "teamA_DRB",
    teamBColumn: "teamB_DRB",
    diffColumn: "DRB_diff",
    digits: 3,
    preferredDirection: "higher",
  },
  {
    label: "3P shooting",
    code: "3P%",
    definition: "Three-point make rate.",
    teamAColumn: "teamA_3P%",
    teamBColumn: "teamB_3P%",
    diffColumn: "3P%_diff",
    digits: 3,
    preferredDirection: "higher",
  },
  {
    label: "3P defense",
    code: "3P%D",
    definition: "Opponent three-point percentage allowed. Lower is better.",
    teamAColumn: "teamA_3P%D",
    teamBColumn: "teamB_3P%D",
    diffColumn: "3P%D_diff",
    digits: 3,
    preferredDirection: "lower",
  },
  {
    label: "Adj. tempo",
    code: "Adj T.",
    definition: "Adjusted pace (possessions per 40 minutes).",
    teamAColumn: "teamA_Adj T.",
    teamBColumn: "teamB_Adj T.",
    diffColumn: "Adj T._diff",
    digits: 1,
    preferredDirection: "neutral",
  },
];

export function featureTooltip(spec: Pick<MatchupFeatureSpec, "label" | "code" | "definition">): string {
  if (spec.code === spec.label) {
    return spec.definition;
  }
  return `${spec.definition} Torvik column: ${spec.code}.`;
}

function toNumber(value: unknown): number | undefined {
  if (typeof value === "number" && Number.isFinite(value)) return value;
  if (typeof value === "string" && value.trim() !== "") {
    const parsed = Number(value);
    if (Number.isFinite(parsed)) return parsed;
  }
  return undefined;
}

export function formatFeatureValue(value: unknown, digits: number): string {
  const numeric = toNumber(value);
  if (numeric == null) return "n/a";
  return numeric.toFixed(digits);
}

export function favorLabel(
  diffValue: number | undefined,
  preferredDirection: PreferredDirection,
  teamAName: string,
  teamBName: string,
): string {
  if (diffValue == null || !Number.isFinite(diffValue)) return "n/a";
  if (preferredDirection === "neutral") return "Context only";
  if (Math.abs(diffValue) < 1e-9) return "Even";
  if (preferredDirection === "higher") return diffValue > 0 ? teamAName : teamBName;
  return diffValue < 0 ? teamAName : teamBName;
}

export function advantageDirection(
  favoredTeam: string,
  teamAName: string,
  teamBName: string,
): "left" | "right" | "even" | "neutral" {
  if (favoredTeam === teamAName) return "left";
  if (favoredTeam === teamBName) return "right";
  if (favoredTeam === "Even") return "even";
  return "neutral";
}

export function advantageTrackWidth(
  diffValue: number | undefined,
  teamAValue: number | undefined,
  teamBValue: number | undefined,
  direction: "left" | "right" | "even" | "neutral",
): number {
  if (direction !== "left" && direction !== "right") return 0;
  if (diffValue == null || !Number.isFinite(diffValue)) return 0;
  const denominator = Math.max(
    Math.abs(teamAValue ?? 0),
    Math.abs(teamBValue ?? 0),
    Math.abs(diffValue),
    1,
  );
  return Math.min(100, Math.max(8, (Math.abs(diffValue) / denominator) * 100));
}

export function sameConferenceLabel(value: unknown): string {
  const numeric = toNumber(value);
  if (numeric == null) return "Conference relationship unavailable";
  return numeric >= 1 ? "Same conference" : "Different conferences";
}
