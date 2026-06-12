import { describe, expect, it } from "vitest";
import { render, screen } from "@testing-library/react";
import { TechnicalApp } from "./TechnicalApp";
import technicalFixture from "../../../tests/fixtures/dashboard_payload_technical.json";
import type { TechnicalPayload } from "../types/payload";

const fixture = technicalFixture as unknown as TechnicalPayload;

describe("TechnicalApp", () => {
  it("renders compare workspace, backtest, and decision summary from a full payload", () => {
    render(<TechnicalApp payload={fixture} />);
    expect(screen.getByRole("heading", { name: /technical bracket dashboard/i })).toBeInTheDocument();
    expect(screen.getByTestId("build-metadata")).toBeInTheDocument();
    expect(screen.getByTestId("decision-summary")).toBeInTheDocument();
    expect(screen.getByTestId("compare-workspace")).toBeInTheDocument();
    expect(screen.getByTestId("backtest-panel")).toBeInTheDocument();
    expect(screen.getByTestId("calibration-chart")).toBeInTheDocument();
    expect(screen.getByTestId("ranked-decisions-table")).toBeInTheDocument();
  });

  it("degrades gracefully when optional sections are absent", () => {
    const minimal: TechnicalPayload = {
      ...fixture,
      model_quality: undefined,
      decision_summary: undefined,
      action_summary: undefined,
      key_warnings: undefined,
      ranked_decisions: undefined,
      candidate_differences: undefined,
      backtest: undefined,
      model_overview: undefined,
      ensemble_diagnostics: undefined,
      championship_totals: undefined,
    };
    render(<TechnicalApp payload={minimal} />);
    expect(screen.getAllByTestId("missing-section").length).toBeGreaterThanOrEqual(3);
  });
});
