import { describe, expect, it } from "vitest";
import { render, screen } from "@testing-library/react";
import { DashboardHero } from "./DashboardHero";

describe("DashboardHero", () => {
  it("renders title, eyebrow, lede, metadata, and optional nav", () => {
    render(
      <DashboardHero
        eyebrow="2026 mmBayes bracket dashboard"
        title="Bracket entry workspace"
        lede="Enter Candidate 1, then inspect review picks."
        metadata={{
          render_timestamp: "June 13, 2026 at 01:55 PM EDT",
          git_commit: "f3d8b31",
          repo_snapshot_synced: true,
          repo_snapshot_label: "Synced to tracked repo output in this run",
        }}
        nav={<nav aria-label="Dashboard sections">Jump nav</nav>}
      />,
    );

    const hero = screen.getByRole("heading", { name: "Bracket entry workspace" }).closest("header");
    expect(hero).toHaveClass("hero");
    expect(hero).toHaveClass("dashboard-hero");
    expect(screen.getByText("2026 mmBayes bracket dashboard")).toBeInTheDocument();
    expect(screen.getByRole("heading", { name: "Bracket entry workspace" })).toBeInTheDocument();
    expect(screen.getByText("Enter Candidate 1, then inspect review picks.")).toBeInTheDocument();
    expect(screen.getByTestId("build-metadata")).toHaveTextContent("f3d8b31");
    expect(screen.getByLabelText("Dashboard sections")).toHaveTextContent("Jump nav");
  });
});
