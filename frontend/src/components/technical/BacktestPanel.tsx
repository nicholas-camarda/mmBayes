import type { BacktestPayload, BacktestSummaryRow } from "../../types/payload";
import { formatProbability } from "../../lib/format";
import { normalizePayloadRow } from "../../lib/normalizePayloadRow";
import { CalibrationChart } from "./CalibrationChart";
import { CalibrationHelp } from "./CalibrationHelp";
import { MissingSection } from "../MissingSection";

function metricNote(metric: string): string {
  switch (metric) {
    case "mean_log_loss":
      return "Lower means the forecast probabilities are more honest.";
    case "mean_brier":
      return "Lower means the probability estimates are tighter overall.";
    case "mean_accuracy":
      return "Share of held-out games where the winner was picked correctly.";
    case "mean_bracket_score":
      return "Average pool-style points under the bracket scoring rules.";
    case "mean_correct_picks":
      return "Average number of correct winner picks across holdout seasons.";
    default:
      return "Historical backtest metric.";
  }
}

const METRIC_LABELS: Record<string, string> = {
  mean_log_loss: "Log loss",
  mean_brier: "Brier score",
  mean_accuracy: "Accuracy",
  mean_bracket_score: "Bracket score",
  mean_correct_picks: "Correct picks",
};

function formatMetricValue(key: string, value: unknown): string {
  if (typeof value !== "number" || !Number.isFinite(value)) return "n/a";
  if (key === "mean_accuracy") return formatProbability(value);
  if (key === "mean_bracket_score" || key === "mean_correct_picks") {
    return value.toFixed(1);
  }
  return value.toFixed(3);
}

export function BacktestPanel({ backtest }: { backtest?: BacktestPayload | null }) {
  const summary = normalizePayloadRow<BacktestSummaryRow>(
    backtest?.summary as BacktestSummaryRow | BacktestSummaryRow[] | null | undefined,
  );
  if (!summary) {
    return <MissingSection label="Backtest diagnostics" />;
  }

  const metrics = Object.entries(summary).filter(([key]) => key in METRIC_LABELS);
  const roundPerformance = backtest?.round_performance ?? [];

  return (
    <section className="technical-panel" aria-label="Backtest diagnostics" data-testid="backtest-panel">
      <div className="role-kicker role-kicker--evidence">Historical baseline</div>
      <h2>Backtest and calibration</h2>
      {backtest?.source_label ? (
        <p className="section-note">
          Source: <strong>{backtest.source_label}</strong>
          {backtest.backtest_years ? ` | Holdout years: ${backtest.backtest_years}` : ""}
        </p>
      ) : null}
      <div className="diagnostics-overview-grid">
        <div className="quality-card">
          <h3>Backtest summary</h3>
          <div className="summary-strip">
            {metrics.map(([key, value]) => (
              <div key={key} className="summary-chip">
                <span>{METRIC_LABELS[key]}</span>
                <strong>{formatMetricValue(key, value)}</strong>
                <small>{metricNote(key)}</small>
              </div>
            ))}
          </div>
        </div>
        <div className="quality-card">
          <h3>What this means</h3>
          <div className="diagnostic-callout">
            <strong>Doing well</strong>
            <ul>
              {(backtest?.strengths ?? []).length > 0 ? (
                backtest!.strengths!.map((item) => <li key={item}>{item}</li>)
              ) : (
                <li>No strengths identified.</li>
              )}
            </ul>
          </div>
          <div className="diagnostic-callout">
            <strong>Needs attention</strong>
            <ul>
              {(backtest?.weaknesses ?? []).length > 0 ? (
                backtest!.weaknesses!.map((item) => <li key={item}>{item}</li>)
              ) : (
                <li>No weaknesses identified.</li>
              )}
            </ul>
          </div>
        </div>
      </div>
      <div className="diagnostics-detail-grid">
        <div className="quality-card quality-card--full">
          <h3>Backtest calibration curve</h3>
          <p className="section-note">
            Each point groups held-out games by the model&apos;s predicted win probability. Points
            on the dashed diagonal mean predicted and observed rates matched; points below it mean
            the model was too optimistic, and points above it mean the model was too pessimistic.
          </p>
          <div className="calibration-layout">
            <div className="calibration-layout__plot">
              <CalibrationChart rows={backtest?.calibration ?? []} />
            </div>
            <div className="calibration-layout__meta">
              <CalibrationHelp />
            </div>
          </div>
        </div>
        <div className="quality-card quality-card--full">
          <h3>Backtest by round</h3>
          <p className="section-note">
            Empirical rate is the share of held-out games in that round where the team the model
            favored actually won.
          </p>
          <div className="table-shell">
            <table className="dashboard-table">
              <thead>
                <tr>
                  <th>Round</th>
                  <th>Games</th>
                  <th>Accuracy</th>
                  <th>Log loss</th>
                  <th>Brier</th>
                  <th>Empirical rate</th>
                </tr>
              </thead>
              <tbody>
                {roundPerformance.length > 0 ? (
                  roundPerformance.map((row) => (
                    <tr key={String(row.round)}>
                      <td>{row.round}</td>
                      <td>{row.games}</td>
                      <td>{formatProbability(row.accuracy)}</td>
                      <td>{row.log_loss?.toFixed(3) ?? "n/a"}</td>
                      <td>{row.brier?.toFixed(3) ?? "n/a"}</td>
                      <td>{formatProbability(row.empirical_rate)}</td>
                    </tr>
                  ))
                ) : (
                  <tr>
                    <td colSpan={6} className="missing-section">
                      Round-level diagnostics were not available for this snapshot.
                    </td>
                  </tr>
                )}
              </tbody>
            </table>
          </div>
        </div>
      </div>
    </section>
  );
}
