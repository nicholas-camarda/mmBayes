import { describe, expect, it } from "vitest";
import { render, screen } from "@testing-library/react";
import { TechnicalApp } from "./TechnicalApp";
import technicalFixture from "../../../tests/fixtures/dashboard_payload_technical.json";
import type { TechnicalPayload } from "../types/payload";

const fixture = technicalFixture as unknown as TechnicalPayload;

describe("TechnicalApp", () => {
  it("renders provenance and decision summary from a full payload", () => {
    render(<TechnicalApp payload={fixture} />);
    expect(screen.getByRole("heading", { name: /technical dashboard/i })).toBeInTheDocument();
    expect(screen.getByTestId("build-metadata")).toBeInTheDocument();
    expect(screen.getByTestId("decision-summary")).toBeInTheDocument();
  });

  it("degrades gracefully when optional sections are absent", () => {
    const minimal: TechnicalPayload = {
      ...fixture,
      model_quality: undefined,
      decision_summary: undefined,
    };
    render(<TechnicalApp payload={minimal} />);
    expect(screen.getAllByTestId("missing-section").length).toBeGreaterThanOrEqual(2);
  });
});
