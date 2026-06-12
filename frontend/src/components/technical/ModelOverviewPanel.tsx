import type { ModelOverviewPayload } from "../../types/payload";
import { MissingSection } from "../MissingSection";

export function ModelOverviewPanel({ overview }: { overview?: ModelOverviewPayload | null }) {
  if (!overview?.engine) {
    return <MissingSection label="Model overview" />;
  }

  return (
    <section className="technical-panel" aria-label="Model overview" data-testid="model-overview">
      <div className="role-kicker role-kicker--reference">Reference</div>
      <h2>Model overview</h2>
      <dl className="evidence-summary">
        <dt>Engine</dt>
        <dd>{overview.engine_label ?? overview.engine}</dd>
        <dt>Predictors</dt>
        <dd>{overview.predictor_count ?? overview.predictor_columns?.length ?? "n/a"}</dd>
        <dt>Prior</dt>
        <dd>{overview.prior_type ?? "n/a"}</dd>
        <dt>History window</dt>
        <dd>
          {overview.configured_history_window != null
            ? String(overview.configured_history_window)
            : "n/a"}
          {overview.effective_historical_years != null
            ? ` (${overview.effective_historical_years} effective seasons)`
            : ""}
        </dd>
      </dl>
      {overview.predictor_columns && overview.predictor_columns.length > 0 ? (
        <p className="section-note">Predictors: {overview.predictor_columns.join(", ")}</p>
      ) : null}
      {overview.interaction_terms && overview.interaction_terms.length > 0 ? (
        <p className="section-note">
          Interaction terms: {overview.interaction_terms.join(", ")}
        </p>
      ) : null}
    </section>
  );
}
