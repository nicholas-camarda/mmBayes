import type { EnsembleDiagnosticsPayload } from "../../types/payload";
import { MissingSection } from "../MissingSection";

export function EnsembleDiagnosticsPanel({
  diagnostics,
}: {
  diagnostics?: EnsembleDiagnosticsPayload | null;
}) {
  if (!diagnostics) {
    return <MissingSection label="Ensemble diagnostics" />;
  }

  const learned = diagnostics.validation_summary?.find(
    (row) => String(row.model ?? "") === "learned_ensemble",
  );

  return (
    <section
      className="technical-panel"
      aria-label="Ensemble diagnostics"
      data-testid="ensemble-diagnostics"
    >
      <div className="role-kicker role-kicker--act">Primary model</div>
      <h2>Ensemble diagnostics</h2>
      <p className="section-note">
        The bracket picker combines Stan GLM and BART probabilities on the logit scale.
      </p>
      <div className="summary-strip">
        <div className="summary-chip">
          <span>Stan GLM weight</span>
          <strong>{diagnostics.weight_stan_glm?.toFixed(3) ?? "n/a"}</strong>
        </div>
        <div className="summary-chip">
          <span>BART weight</span>
          <strong>{diagnostics.weight_bart?.toFixed(3) ?? "n/a"}</strong>
        </div>
        <div className="summary-chip">
          <span>Intercept</span>
          <strong>{diagnostics.intercept?.toFixed(3) ?? "n/a"}</strong>
        </div>
        <div className="summary-chip">
          <span>Proof gate</span>
          <strong>{diagnostics.gate_passed ? "Pass" : "Not recorded"}</strong>
        </div>
      </div>
      {learned ? (
        <p className="section-note">
          Learned ensemble validation: bracket score{" "}
          {String(learned.mean_bracket_score ?? "n/a")}, correct picks{" "}
          {String(learned.mean_correct_picks ?? "n/a")}, log loss{" "}
          {String(learned.mean_log_loss ?? "n/a")}, Brier {String(learned.mean_brier ?? "n/a")}.
        </p>
      ) : null}
      {(diagnostics.gate_conditions ?? []).length > 0 ? (
        <table className="dashboard-table">
          <thead>
            <tr>
              <th>Condition</th>
              <th>Status</th>
              <th>Observed</th>
              <th>Threshold</th>
            </tr>
          </thead>
          <tbody>
            {diagnostics.gate_conditions!.map((row, index) => (
              <tr key={`${String(row.condition)}-${index}`}>
                <td>{String(row.condition ?? "n/a")}</td>
                <td>{row.passed ? "Pass" : "Fail"}</td>
                <td>{Number(row.observed).toFixed(3)}</td>
                <td>{Number(row.threshold).toFixed(3)}</td>
              </tr>
            ))}
          </tbody>
        </table>
      ) : (
        <p className="missing-section">No gate conditions recorded.</p>
      )}
    </section>
  );
}
