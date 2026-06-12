import { test, expect } from "vitest";
import { render, screen } from "@testing-library/react";
import { PlayInStatusPanel } from "./PlayInStatusPanel";

test("shows unresolved First Four slots", () => {
  render(
    <PlayInStatusPanel
      rows={[
        {
          expected_slots: 4,
          resolved_slots: 0,
          unresolved_slots: 4,
          has_unresolved_slots: true,
        },
      ]}
    />,
  );
  expect(screen.getByText(/4 First Four slots are still unresolved/i)).toBeInTheDocument();
});

test("shows resolved status when all slots are final", () => {
  render(
    <PlayInStatusPanel
      rows={[
        {
          expected_slots: 4,
          resolved_slots: 4,
          unresolved_slots: 0,
          has_unresolved_slots: false,
        },
      ]}
    />,
  );
  expect(screen.getByText(/All 4 First Four slots are resolved/i)).toBeInTheDocument();
});

test("renders slot counts in table", () => {
  render(
    <PlayInStatusPanel
      rows={[
        {
          expected_slots: 4,
          resolved_slots: 2,
          unresolved_slots: 2,
          has_unresolved_slots: true,
        },
      ]}
    />,
  );
  expect(screen.getAllByRole("cell").map((cell) => cell.textContent)).toEqual([
    "4",
    "2",
    "2",
  ]);
});
