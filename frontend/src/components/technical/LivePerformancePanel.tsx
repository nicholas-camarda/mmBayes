import type { LivePerformancePayload } from "../../types/payload";
import { MissingSection } from "../MissingSection";
import { formatProbability } from "../../lib/format";

interface LivePerformancePanelProps {
  livePerformance?: LivePerformancePayload | null;
}

function metric(value: unknown, digits = 3): string {
  if (typeof value !== "number" || !Number.isFinite(value)) return "n/a";
  if (value <= 1 && value >= 0) return formatProbability(value);
  return value.toFixed(digits);
}

export function LivePerformancePanel({ livePerformance }: LivePerformancePanelProps) {
  if (!livePerformance) {
    return (
      <section
        className="technical-panel"
        aria-label="Live tournament performance"
        data-testid="live-performance-panel"
      >
        <MissingSection label="Live tournament performance" />
      </section>
    );
  }

  const summary = livePerformance.summary;
  const mainSummary = livePerformance.main_bracket_summary;
  const rounds = livePerformance.round_summary ?? [];
  const games = livePerformance.recent_games ?? [];

  return (
    <section className="technical-panel" aria-label="Live tournament performance" data-testid="live-performance-panel">
      <div className="role-kicker role-kicker--reference">Monitoring</div>
      <h2>Live tournament performance</h2>
      {livePerformance.model_label ? (
        <p className="section-note">Model: {livePerformance.model_label}</p>
      ) : null}
      <p className="section-note">{livePerformance.status}</p>
      {livePerformance.monitoring_note ? (
        <p className="diagnostic-callout">{livePerformance.monitoring_note}</p>
      ) : null}
      {livePerformance.interpretive_status ? (
        <p>
          <strong>Interpretive status:</strong> {livePerformance.interpretive_status}
        </p>
      ) : null}

      <div className="live-performance__summary guide-grid">
        <div className="guide-card">
          <div className="guide-label">All completed games</div>
          <p>
            {summary?.games_played ?? livePerformance.games_played ?? 0} games | accuracy{" "}
            {metric(summary?.accuracy)} | Brier {metric(summary?.brier)}
          </p>
        </div>
        <div className="guide-card">
          <div className="guide-label">Main bracket only</div>
          <p>
            {mainSummary?.games_played ?? livePerformance.main_bracket_games_played ?? 0} games |
            accuracy {metric(mainSummary?.accuracy)} | Brier {metric(mainSummary?.brier)}
          </p>
        </div>
      </div>

      {rounds.length > 0 ? (
        <div className="technical-subpanel">
          <h3>Live by round</h3>
          <div className="live-round-chart" data-testid="live-round-chart">
            {rounds.map((roundRow) => {
              const accuracy = roundRow.accuracy ?? 0;
              const meanPredicted = roundRow.mean_predicted_prob ?? 0;
              return (
                <div key={String(roundRow.round)} className="live-round-row">
                  <div className="live-round-row__label">{roundRow.round}</div>
                  <div className="live-round-row__bars">
                    <div className="live-round-bar live-round-bar--accuracy">
                      <span>Accuracy</span>
                      <div className="live-round-bar__track">
                        <div
                          className="live-round-bar__fill"
                          style={{ width: `${Math.max(accuracy * 100, 2)}%` }}
                        />
                      </div>
                      <strong>{metric(accuracy)}</strong>
                    </div>
                    <div className="live-round-bar live-round-bar--predicted">
                      <span>Mean predicted</span>
                      <div className="live-round-bar__track">
                        <div
                          className="live-round-bar__fill"
                          style={{ width: `${Math.max(meanPredicted * 100, 2)}%` }}
                        />
                      </div>
                      <strong>{metric(meanPredicted)}</strong>
                    </div>
                  </div>
                  <div className="live-round-row__meta">{roundRow.games ?? 0} games</div>
                </div>
              );
            })}
          </div>
        </div>
      ) : null}

      {games.length > 0 ? (
        <div className="technical-subpanel">
          <h3>{livePerformance.recent_games_title ?? "Recent games"}</h3>
          {livePerformance.recent_games_note ? (
            <p className="section-note">{livePerformance.recent_games_note}</p>
          ) : null}
          <div className="table-shell">
            <table className="dashboard-table" data-testid="live-recent-games-table">
              <thead>
                <tr>
                  <th>Round</th>
                  <th>Matchup</th>
                  <th>Result</th>
                  <th>Model pick</th>
                  <th>Predicted</th>
                </tr>
              </thead>
              <tbody>
                {games.map((game, index) => (
                  <tr key={`${game.round}-${game.teamA}-${game.teamB}-${index}`}>
                    <td>{game.round}</td>
                    <td>
                      {game.teamA} vs {game.teamB}
                    </td>
                    <td>{game.actual_winner ?? game.model_pick_note}</td>
                    <td>{game.model_pick}</td>
                    <td>{metric(game.predicted_prob)}</td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        </div>
      ) : null}
    </section>
  );
}
