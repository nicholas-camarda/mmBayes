import { describe, expect, it } from "vitest";
import { render, screen } from "@testing-library/react";
import { BracketApp } from "./BracketApp";
import bracketFixture from "../../../tests/fixtures/dashboard_payload_bracket.json";
import type { BracketPayload } from "../types/payload";

const fixture = bracketFixture as unknown as BracketPayload;

describe("BracketApp", () => {
  it("renders candidate summaries and the decision sheet from a full payload", () => {
    render(<BracketApp payload={fixture} />);
    expect(screen.getByRole("heading", { name: /bracket dashboard/i })).toBeInTheDocument();
    expect(screen.getAllByTestId("candidate-card").length).toBe(fixture.candidates.length);
    expect(screen.getByTestId("decision-sheet")).toBeInTheDocument();
    expect(screen.getAllByTestId("candidate-card")[0]).toHaveTextContent(
      new RegExp(fixture.candidates[0].champion),
    );
  });

  it("degrades gracefully when optional sections are absent", () => {
    const minimal: BracketPayload = {
      ...fixture,
      matchup_context: undefined,
      candidate_summaries: undefined,
      play_in_resolution: undefined,
      bracket_tree: undefined,
      divergence_map: undefined,
      watchlist: undefined,
    };
    render(<BracketApp payload={minimal} />);
    expect(screen.getByTestId("decision-sheet")).toBeInTheDocument();
    expect(screen.queryByTestId("play-in-panel")).not.toBeInTheDocument();
    expect(screen.getAllByTestId("missing-section").length).toBeGreaterThan(0);
  });

  it("renders bracket tree and divergence map when payload includes them", () => {
    if (!fixture.bracket_tree?.trees?.length || !fixture.divergence_map?.length) {
      return;
    }
    render(<BracketApp payload={fixture} />);
    expect(screen.getByLabelText("Bracket tree")).toBeInTheDocument();
    expect(screen.getByLabelText("Divergence map")).toBeInTheDocument();
  });

  it("shows build metadata when present", () => {
    render(<BracketApp payload={fixture} />);
    expect(screen.getByTestId("build-metadata")).toHaveTextContent(/fixture/);
  });
});
