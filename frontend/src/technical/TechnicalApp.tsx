import type { TechnicalPayload } from "../types/payload";
import { BuildMetadataBanner } from "../components/BuildMetadataBanner";
import { MissingSection } from "../components/MissingSection";

export function TechnicalApp({ payload }: { payload: TechnicalPayload }) {
  return (
    <main className="dashboard">
      <header>
        <h1>{payload.bracket_year} Technical Dashboard</h1>
        <BuildMetadataBanner metadata={payload.build_metadata} />
      </header>
      <h2>Model quality</h2>
      {payload.model_quality ? (
        <p data-testid="model-quality">
          Source: {payload.model_quality.source_label ?? "unknown"}
          {payload.model_quality.used_cached_quality ? " (cached validation snapshot)" : ""}
        </p>
      ) : (
        <MissingSection label="Model quality" />
      )}
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
      <h2>Candidates</h2>
      {payload.candidate_count != null ? (
        <p>{payload.candidate_count} candidate brackets generated.</p>
      ) : (
        <MissingSection label="Candidate count" />
      )}
    </main>
  );
}
