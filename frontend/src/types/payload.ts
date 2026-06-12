export const SUPPORTED_SCHEMA_VERSION = "1.1.0";

export interface BuildMetadata {
  git_commit?: string;
  render_timestamp?: string;
  rendered_at?: string;
  rendered_at_label?: string;
  commit_short?: string;
  commit_full?: string;
  repo_snapshot_synced?: boolean;
  repo_snapshot_label?: string;
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

export interface TechnicalDecisionRow {
  rank?: number;
  round?: string;
  region?: string;
  matchup?: string;
  favorite?: string;
  favorite_prob?: number;
  ci_lower?: number;
  ci_upper?: number;
  tier?: string;
  recommended_pick?: string;
  alternate_pick?: string;
  candidate_diff_flag?: boolean;
  alternate_note?: string;
  usage_note?: string;
  inspection_level?: string;
  rationale?: string;
}

export interface UpsetOpportunityRow {
  matchup?: string;
  round?: string;
  region?: string;
  tier?: string;
  underdog?: string;
  favorite?: string;
  underdog_prob?: number;
  underdog_ci_lower?: number;
  underdog_ci_upper?: number;
  leverage?: number;
  candidate_1_pick?: string;
  candidate_2_pick?: string;
  candidate_diff_flag?: boolean;
  pivot_note?: string;
}

export interface CandidateFragilePickRow {
  round?: string;
  matchup_label?: string;
  winner?: string;
  chosen_prob?: number;
  chosen_ci_lower?: number;
  chosen_ci_upper?: number;
  confidence_tier?: string;
  decision_score?: number;
}

export interface CandidateProfile {
  candidate_id: number;
  type?: string;
  champion?: string;
  final_four?: string;
  bracket_log_prob?: number | null;
  mean_game_prob?: number | null;
  diff_summary?: string;
  fragile_picks?: CandidateFragilePickRow[];
}

export interface LivePerformancePayload {
  model_label?: string;
  status?: string;
  monitoring_note?: string;
  interpretive_status?: string;
  summary?: {
    games_played?: number;
    log_loss?: number;
    brier?: number;
    accuracy?: number;
  };
  main_bracket_summary?: {
    games_played?: number;
    log_loss?: number;
    brier?: number;
    accuracy?: number;
  };
  round_summary?: Array<{
    round?: string;
    games?: number;
    accuracy?: number;
    mean_predicted_prob?: number;
  }>;
  recent_games?: Array<{
    round?: string;
    teamA?: string;
    teamB?: string;
    actual_winner?: string;
    model_pick?: string;
    model_pick_note?: string;
    predicted_prob?: number;
  }>;
  recent_games_title?: string;
  recent_games_note?: string;
  games_played?: number;
  main_bracket_games_played?: number;
}

export interface TechnicalCandidateDifferenceRow {
  round?: string;
  region?: string;
  slot?: number;
  matchup?: string;
  candidate_1?: string;
  candidate_2?: string;
  tier?: string;
  leverage?: number;
  why?: string;
}

export interface BacktestSummaryRow {
  mean_log_loss?: number;
  mean_brier?: number;
  mean_accuracy?: number;
  mean_bracket_score?: number;
  mean_correct_picks?: number;
}

export interface CalibrationRow {
  bin?: string;
  mean_predicted?: number;
  empirical_rate?: number;
  n_games?: number;
}

export interface RoundPerformanceRow {
  round?: string;
  games?: number;
  accuracy?: number;
  log_loss?: number;
  brier?: number;
  empirical_rate?: number;
}

export interface BacktestPayload {
  source_label?: string;
  summary?: BacktestSummaryRow | BacktestSummaryRow[];
  calibration?: CalibrationRow[];
  round_performance?: RoundPerformanceRow[];
  strengths?: string[];
  weaknesses?: string[];
  backtest_years?: string;
}

export interface EnsembleDiagnosticsPayload {
  weight_stan_glm?: number;
  weight_bart?: number;
  intercept?: number;
  gate_passed?: boolean;
  validation_summary?: Record<string, unknown>[];
  gate_conditions?: Record<string, unknown>[];
}

export interface ModelOverviewPayload {
  engine?: string;
  engine_label?: string;
  predictor_count?: number;
  predictor_columns?: string[];
  prior_type?: string;
  interaction_terms?: string[];
  configured_history_window?: number;
  effective_historical_years?: number;
}

export interface ChampionshipTotalsPayload {
  candidate_summaries?: Record<string, unknown>[];
  championship_distribution?: Array<{
    candidate_id?: number;
    total_points?: number;
    probability?: number;
  }>;
  scale?: {
    min_total?: number;
    max_total?: number;
    max_probability?: number;
  };
}

export interface TechnicalActionSummary {
  candidates?: Array<{
    candidate_id?: number;
    type?: string;
    champion?: string;
    final_four?: string;
  }>;
  tier_counts?: Record<string, number>;
  candidate_diff_count?: number;
  top_leverage_label?: string;
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
  action_summary?: TechnicalActionSummary;
  key_warnings?: string[];
  ranked_decisions?: TechnicalDecisionRow[];
  candidate_differences?: TechnicalCandidateDifferenceRow[];
  upset_opportunities?: UpsetOpportunityRow[];
  candidate_profiles?: CandidateProfile[];
  live_performance?: LivePerformancePayload;
  backtest?: BacktestPayload;
  model_overview?: ModelOverviewPayload;
  ensemble_diagnostics?: EnsembleDiagnosticsPayload;
  championship_totals?: ChampionshipTotalsPayload;
  play_in_resolution?: Record<string, unknown>[];
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
