interface StatusPanelProps {
  playInResolution?: Record<string, unknown>[] | null;
}

export function StatusPanel({ playInResolution }: StatusPanelProps) {
  const row = playInResolution?.[0];
  if (!row) {
    return (
      <div className="status-panel status-unknown">
        <strong>Status not supplied.</strong>
        No play-in summary was supplied, so the review panel cannot tell whether these slots
        are simulated or final.
      </div>
    );
  }

  const status = String(row.status ?? row.resolution_status ?? "simulated").toLowerCase();
  if (status.includes("final")) {
    return (
      <div className="status-panel status-final">
        <strong>Status: Final result.</strong>
        First Four slots are resolved, so the displayed bracket path reflects finalized play-in
        outcomes.
      </div>
    );
  }

  return (
    <div className="status-panel status-simulated">
      <strong>Status: Simulated bracket path.</strong>
      Play-in slots may still change before the tournament locks.
    </div>
  );
}
