export const SUPPORTED_SCHEMA_VERSION = "1.1.0";

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
  matchup_number?: number;
  teamA_seed?: number;
  teamB_seed?: number;
  inspection_level?: string;
  matchup_label?: string;
  candidate_2_matchup?: string;
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

export interface MatchupContextRow extends Record<string, unknown> {
  slot_key?: string;
  evidence_id?: string;
  matchup_label?: string;
  teamA?: string;
  teamB?: string;
  posterior_favorite?: string;
  win_prob_favorite?: number;
  confidence_tier?: string;
  rationale_short?: string;
  candidate_usage?: string;
  why_this_matters?: string;
}

export interface BracketTreeNode {
  slot_key: string;
  teamA: string;
  teamB: string;
  teamA_seed?: number;
  teamB_seed?: number;
  round: string;
  region: string;
  matchup_number?: number;
  node_x: number;
  node_y: number;
  confidence_tier?: string;
  winner?: string;
  candidate_pick?: string;
  posterior_favorite?: string;
  win_prob_favorite?: number;
  upset?: boolean;
  route_diff?: boolean;
  evidence_id?: string;
  rationale_short?: string;
}

export interface BracketTreeEdge {
  from_slot: string;
  to_slot: string;
  x1: number;
  y1: number;
  x2: number;
  y2: number;
  route_diff?: boolean;
}

export interface BracketTreeEntry {
  candidate_id: number;
  candidate_label: string;
  nodes: BracketTreeNode[];
  edges: BracketTreeEdge[];
}

export interface BracketTreePayload {
  trees: BracketTreeEntry[];
}

export interface DivergenceMapRow {
  round: string;
  region: string;
  total_count: number;
  winner_change_count?: number;
  path_only_count?: number;
  unsurfaced_count?: number;
  has_divergence?: boolean;
  all_in_watchlist?: boolean;
  late_round_only?: boolean;
  target_evidence_id?: string;
}

export interface WatchlistRow extends Record<string, unknown> {
  slot_key?: string;
  evidence_id?: string;
  reason_surface?: string;
  matchup_label?: string;
  round?: string;
  region?: string;
  teamA?: string;
  teamB?: string;
  teamA_seed?: number;
  teamB_seed?: number;
  candidate_diff_flag?: boolean;
  why_this_matters?: string;
  candidate_usage?: string;
  win_prob_favorite?: number;
  ci_lower?: number;
  ci_upper?: number;
  confidence_tier?: string;
  late_round_only?: boolean;
}

export interface CandidateSummaryRow {
  candidate_id: number;
  champion?: string;
  final_four?: string;
  championship_matchup?: string;
  recommended_tiebreaker_points?: number;
  predicted_total_median?: number;
  predicted_total_80_lower?: number;
  predicted_total_80_upper?: number;
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
  matchup_context?: MatchupContextRow[];
  candidate_summaries?: CandidateSummaryRow[];
  play_in_resolution?: Record<string, unknown>[];
  bracket_tree?: BracketTreePayload;
  divergence_map?: DivergenceMapRow[];
  watchlist?: WatchlistRow[];
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
        `this frontend supports ${SUPPORTED_SCHEMA_VERSION}`,
    );
  }
}
