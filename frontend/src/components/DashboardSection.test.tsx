import { describe, expect, it } from "vitest";
import { render, screen } from "@testing-library/react";
import { DashboardSection } from "./DashboardSection";

describe("DashboardSection", () => {
  it("renders a labelled section with the requested visual role", () => {
    render(
      <DashboardSection
        id="build"
        roleTone="action"
        kicker="Enter bracket"
        title="Candidate Recommendations"
        note="Copy winners by region and round."
      >
        <div>section body</div>
      </DashboardSection>,
    );

    const section = screen.getByRole("region", { name: "Candidate Recommendations" });
    expect(section).toHaveClass("section");
    expect(section).toHaveClass("dashboard-section");
    expect(section).toHaveClass("dashboard-section--action");
    expect(screen.getByText("Enter bracket")).toHaveClass("role-kicker");
    expect(screen.getByText("Enter bracket")).toHaveClass("role-kicker--act");
    expect(screen.getByText("Copy winners by region and round.")).toHaveClass("section-note");
    expect(screen.getByText("section body")).toBeInTheDocument();
  });
});
