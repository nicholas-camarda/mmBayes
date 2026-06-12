export const SUPPORTED_SCHEMA_VERSION = "1.0.0";

export interface BuildMetadata {
  git_commit?: string;
  render_timestamp?: string;
  repo_snapshot_synced?: boolean;
  [key: string]: unknown;
}

export interface MatchupRow {
  slot_key: string;
  region: string;
  round: string;
  matchup_number: number;
  teamA: string;
  teamB: string;
  teamA_seed: number;
  teamB_seed: number;
  winner: string;
  win_prob_A: number;
  [key: string]: unknown;
}

export interface DecisionSheetRow {
  slot_key: string;
  region?: string;
  round?: string;
  matchup_label?: string;
  posterior_favorite?: string;
  win_prob_favorite?: number;
  confidence_tier?: string;
  decision_rank?: number;
  upset_leverage?: number;
  candidate_1_pick?: string;
  candidate_2_pick?: string;
  candidate_diff_flag?: boolean;
  rationale_short?: string;
  [key: string]: unknown;
}

export interface Candidate {
  candidate_id: number;
  type: string;
  champion: string;
  final_four?: string[];
  bracket_log_prob?: number | null;
  mean_game_prob?: number | null;
  title_path_mean_prob?: number | null;
  path_support_label?: string | null;
  matchups?: MatchupRow[];
}

interface PayloadBase {
  dashboard_schema_version: string;
  bracket_year: number;
  generated_at: string;
  build_metadata: BuildMetadata;
}

export interface BracketPayload extends PayloadBase {
  dashboard: "bracket";
  candidates: Candidate[];
  decision_sheet: DecisionSheetRow[];
  matchup_context?: Record<string, unknown>[];
  candidate_summaries?: Record<string, unknown>[];
  play_in_resolution?: Record<string, unknown>[];
}

export interface TechnicalPayload extends PayloadBase {
  dashboard: "technical";
  model_quality?: { source_label?: string; used_cached_quality?: boolean };
  decision_summary?: {
    n_decisions?: number;
    n_divergent?: number;
    confidence_tiers?: Record<string, number>;
  };
  candidate_count?: number;
}

export interface DashboardPayloads {
  bracket: BracketPayload;
  technical: TechnicalPayload;
}

export function assertSchemaCompatible(payload: PayloadBase): void {
  const major = (v: string) => v.split(".")[0];
  if (major(payload.dashboard_schema_version) !== major(SUPPORTED_SCHEMA_VERSION)) {
    throw new Error(
      `Unsupported dashboard_schema_version ${payload.dashboard_schema_version}; ` +
        `this frontend supports ${SUPPORTED_SCHEMA_VERSION}`
    );
  }
}
