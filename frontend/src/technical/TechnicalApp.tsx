import type { TechnicalPayload } from "../types/payload";
import { DashboardHero } from "../components/DashboardHero";
import { DashboardSection } from "../components/DashboardSection";
import { MissingSection } from "../components/MissingSection";
import { StatusPanel } from "../components/bracket/StatusPanel";
import { TechnicalActionSummary } from "../components/technical/TechnicalActionSummary";
import { CollapsiblePanel } from "../components/technical/CollapsiblePanel";
import { CompareWorkspace } from "../components/technical/CompareWorkspace";
import { BacktestPanel } from "../components/technical/BacktestPanel";
import { EnsembleDiagnosticsPanel } from "../components/technical/EnsembleDiagnosticsPanel";
import { ModelOverviewPanel } from "../components/technical/ModelOverviewPanel";
import { ChampionshipTotalsPanel } from "../components/technical/ChampionshipTotalsPanel";
import { LivePerformancePanel } from "../components/technical/LivePerformancePanel";

export function TechnicalApp({ payload }: { payload: TechnicalPayload }) {
  return (
    <main className="dashboard page technical-dashboard">
      <DashboardHero
        eyebrow={`${payload.bracket_year} mmBayes technical dashboard`}
        title="Technical Bracket Dashboard"
        lede="Start in Compare to decide which games deserve attention, then open backtest and ensemble diagnostics when you want the evidence behind the recommendation."
        metadata={payload.build_metadata}
      />

      <StatusPanel playInResolution={payload.play_in_resolution} />

      <TechnicalActionSummary summary={payload.action_summary} />

      <DashboardSection
        roleTone="orientation"
        kicker="Orientation"
        title="How to use this dashboard"
      >
        <div className="guide-grid">
          <div className="guide-card">
            <div className="guide-label">Workflow</div>
            <p>
              Use Compare first for the review queue and candidate divergence table, then open
              backtest evidence for historical calibration.
            </p>
          </div>
          <div className="guide-card">
            <div className="guide-label">Confidence tiers</div>
            <p>
              Locks are stable favorites. Leans still favor one side. Toss-ups are true decision
              points. Volatile games have wider uncertainty.
            </p>
          </div>
        </div>
      </DashboardSection>

      {payload.key_warnings && payload.key_warnings.length > 0 ? (
        <section className="technical-panel warning-callout" aria-label="Key warnings">
          <div className="role-kicker role-kicker--act">Act now</div>
          <h2>Key warnings</h2>
          <ul className="diagnostic-bullets" data-testid="key-warnings">
            {payload.key_warnings.map((warning) => (
              <li key={warning}>{warning}</li>
            ))}
          </ul>
        </section>
      ) : null}

      <CompareWorkspace
        rankedDecisions={payload.ranked_decisions}
        candidateDifferences={payload.candidate_differences}
        upsetOpportunities={payload.upset_opportunities}
        candidateProfiles={payload.candidate_profiles}
      />

      <LivePerformancePanel livePerformance={payload.live_performance} />

      <CollapsiblePanel title="Championship tiebreaker comparison" defaultOpen={false}>
        <ChampionshipTotalsPanel totals={payload.championship_totals} />
      </CollapsiblePanel>

      <BacktestPanel backtest={payload.backtest} />

      <EnsembleDiagnosticsPanel diagnostics={payload.ensemble_diagnostics} />

      <CollapsiblePanel title="Model overview" defaultOpen={false}>
        <ModelOverviewPanel overview={payload.model_overview} />
      </CollapsiblePanel>

      <CollapsiblePanel title="Decision summary" defaultOpen={false}>
        <section className="technical-panel" aria-label="Decision summary reference">
          <h2>Decision summary</h2>
          {payload.decision_summary ? (
            <dl data-testid="decision-summary">
              <dt>Decisions</dt>
              <dd>{payload.decision_summary.n_decisions ?? "n/a"}</dd>
              <dt>Candidate divergences</dt>
              <dd>{payload.decision_summary.n_divergent ?? "n/a"}</dd>
              <dt>Confidence tiers</dt>
              <dd>
                {Object.entries(payload.decision_summary.confidence_tiers ?? {})
                  .map(([tier, count]) => `${tier}: ${count}`)
                  .join(", ") || "n/a"}
              </dd>
            </dl>
          ) : (
            <MissingSection label="Decision summary" />
          )}
          {payload.model_quality ? (
            <p data-testid="model-quality" className="section-note">
              Model quality source: {payload.model_quality.source_label ?? "unknown"}
              {payload.model_quality.used_cached_quality ? " (cached validation snapshot)" : ""}
            </p>
          ) : (
            <MissingSection label="Model quality" />
          )}
          {payload.candidate_count != null ? (
            <p>{payload.candidate_count} candidate brackets generated.</p>
          ) : null}
        </section>
      </CollapsiblePanel>
    </main>
  );
}
