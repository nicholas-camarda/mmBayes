import { describe, expect, it } from "vitest";
import { render, screen } from "@testing-library/react";
import { TechnicalApp } from "./TechnicalApp";
import technicalFixture from "../../../tests/fixtures/dashboard_payload_technical.json";
import type { TechnicalPayload } from "../types/payload";

const fixture = technicalFixture as unknown as TechnicalPayload;
const fullFixture: TechnicalPayload = {
  ...fixture,
  ensemble_diagnostics: {
    weight_stan_glm: 0.62,
    weight_bart: 0.38,
    intercept: -0.04,
    gate_passed: true,
    validation_summary: [
      {
        metric: "Bracket score",
        ensemble: 85.4,
        stan_glm: 82.1,
        bart: 83.2,
      },
    ],
    gate_conditions: [
      {
        condition: "Calibration guardrail",
        passed: true,
      },
    ],
  },
  championship_totals: {
    candidate_summaries: [
      {
        candidate_id: 1,
        championship_matchup: "West_01_2025 vs Midwest_01_2025",
        recommended_tiebreaker_points: 143,
        predicted_total_median: 142.5,
      },
    ],
    championship_distribution: [
      { candidate_id: 1, total_points: 140, probability: 0.24 },
      { candidate_id: 1, total_points: 145, probability: 0.31 },
    ],
    scale: {
      min_total: 130,
      max_total: 155,
      max_probability: 0.31,
    },
  },
};

describe("TechnicalApp", () => {
  it("renders compare workspace, backtest, and decision summary from a full payload", () => {
    render(<TechnicalApp payload={fullFixture} />);
    expect(screen.getByRole("heading", { name: /technical bracket dashboard/i })).toBeInTheDocument();
    expect(screen.getByTestId("build-metadata")).toBeInTheDocument();
    expect(screen.getByTestId("decision-summary")).toBeInTheDocument();
    expect(screen.getByTestId("compare-workspace")).toBeInTheDocument();
    expect(screen.getByTestId("backtest-panel")).toBeInTheDocument();
    expect(screen.getByTestId("calibration-chart")).toBeInTheDocument();
    expect(screen.getByTestId("ranked-decisions-table")).toBeInTheDocument();
    expect(screen.getByTestId("ensemble-diagnostics")).toBeInTheDocument();
    expect(screen.getByTestId("championship-totals")).toBeInTheDocument();
    expect(screen.getByRole("heading", { name: /Ensemble diagnostics/i })).toBeInTheDocument();
  });

  it("puts the technical action summary before generic orientation content", () => {
    render(<TechnicalApp payload={fullFixture} />);

    const hero = screen
      .getByRole("heading", { name: /technical bracket dashboard/i })
      .closest("header");
    const actionSummary = screen.getByTestId("technical-action-summary");
    const orientation = screen.getByRole("region", { name: "How to use this dashboard" });

    expect(hero).toHaveClass("dashboard-hero");
    expect(
      actionSummary.compareDocumentPosition(orientation) & Node.DOCUMENT_POSITION_FOLLOWING,
    ).toBeTruthy();
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
