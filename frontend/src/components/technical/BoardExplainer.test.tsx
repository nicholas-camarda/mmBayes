import { test, expect } from "vitest";
import { render, screen } from "@testing-library/react";
import { BoardExplainer } from "./BoardExplainer";

test("renders four explainer cards", () => {
  render(
    <BoardExplainer
      what="Top decision slots ranked by leverage."
      how="Start with ranked board, then upset pivots."
      why="These slots move bracket identity."
      math="Sorted by decision_score and upset_leverage."
    />,
  );
  expect(screen.getByText(/what this shows/i)).toBeInTheDocument();
  expect(screen.getByText(/underlying math/i)).toBeInTheDocument();
});
