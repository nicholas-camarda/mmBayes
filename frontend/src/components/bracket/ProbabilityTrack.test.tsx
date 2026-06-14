// frontend/src/components/bracket/ProbabilityTrack.test.tsx
import { test, expect } from "vitest";
import { render, screen } from "@testing-library/react";
import { ProbabilityTrack } from "./ProbabilityTrack";

test("renders labeled stats and lane for accessibility", () => {
  render(
    <ProbabilityTrack
      meanProbability={0.72}
      lowerProbability={0.58}
      upperProbability={0.84}
      valueLabel="Posterior mean"
      intervalLabel="Credible interval"
    />,
  );
  expect(screen.getByText("Posterior mean")).toBeInTheDocument();
  expect(screen.getByText("72.0%")).toBeInTheDocument();
  expect(document.querySelector(".prob-track__lane")).toBeTruthy();
  expect(document.querySelector(".prob-track__scale")).toBeTruthy();
  expect(document.querySelector(".prob-track__range")).toBeTruthy();
  expect(screen.getByRole("img", { name: /Posterior mean 72.0%/ })).toBeInTheDocument();
});
