import { describe, expect, it } from "vitest";
import { render, screen, fireEvent } from "@testing-library/react";
import { BracketApp } from "./BracketApp";
import bracketFixture from "../../../tests/fixtures/dashboard_payload_bracket.json";
import type { BracketPayload } from "../types/payload";

const fixture = bracketFixture as unknown as BracketPayload;

describe("BracketApp", () => {
  it("keeps the bracket dashboard workflow order compact and entry-first", () => {
    render(<BracketApp payload={fixture} />);

    const hero = screen
      .getByRole("heading", { name: /bracket entry workspace/i })
      .closest("header");
    const build = screen.getByRole("region", { name: "Candidate Recommendations" });
    const review = screen.getByRole("region", { name: "Review Queue" });

    expect(hero).toHaveClass("hero");
    expect(hero).toHaveClass("dashboard-hero");
    expect(build).toHaveClass("dashboard-section--action");
    expect(review).toHaveClass("dashboard-section--action");
    expect(hero!.compareDocumentPosition(build) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();
    expect(build.compareDocumentPosition(review) & Node.DOCUMENT_POSITION_FOLLOWING).toBeTruthy();
  });

  it("renders bracket entry picks with visible review and ready states", () => {
    render(<BracketApp payload={fixture} />);

    expect(document.querySelectorAll(".entry-pick").length).toBeGreaterThan(20);
    expect(document.querySelectorAll(".entry-pick--review").length).toBeGreaterThan(0);
    expect(screen.getAllByText("Ready to enter").length).toBeGreaterThan(0);
    expect(screen.getAllByRole("button", { name: "Review" }).length).toBeGreaterThan(0);
  });

  it("renders entry workspace, review queue, and decision sheet from a full payload", () => {
    render(<BracketApp payload={fixture} />);
    expect(screen.getByRole("heading", { name: /bracket entry workspace/i })).toBeInTheDocument();
    expect(screen.getByText(/Candidate Recommendations/i)).toBeInTheDocument();
    expect(screen.getByRole("heading", { name: /review queue/i })).toBeInTheDocument();
    expect(screen.getByText(/Matchup Evidence/i)).toBeInTheDocument();
    expect(screen.getByText(/Full candidate paths/i)).toBeInTheDocument();
    expect(screen.getByTestId("decision-sheet")).toBeInTheDocument();
    expect(screen.getByLabelText("Candidate entry selector")).toBeInTheDocument();
    expect(screen.getByLabelText("Review queue board")).toBeInTheDocument();
    expect(screen.getByLabelText("Dashboard sections")).toBeInTheDocument();
  });

  it("toggles candidate entry panels", () => {
    render(<BracketApp payload={fixture} />);
    const entryControls = screen.getByLabelText("Candidate entry selector");
    expect(entryControls.querySelector(".entry-toggle.is-active")).toHaveAttribute(
      "data-entry-target",
      "candidate-1",
    );
    fireEvent.click(entryControls.querySelector(".entry-toggle[data-entry-target='candidate-2']")!);
    expect(entryControls.querySelector(".entry-toggle.is-active")).toHaveAttribute(
      "data-entry-target",
      "candidate-2",
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

  it("opens evidence from a watchlist card", () => {
    if (!fixture.watchlist?.length || !fixture.matchup_context?.length) {
      return;
    }
    render(<BracketApp payload={fixture} />);
    const jumpButton = screen.getAllByRole("button", { name: "Open evidence" })[0];
    fireEvent.click(jumpButton);
    expect(screen.getByTestId("evidence-card")).toBeInTheDocument();
  });
});
