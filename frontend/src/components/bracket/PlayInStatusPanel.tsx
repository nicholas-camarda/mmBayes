export interface PlayInResolutionRow {
  expected_slots?: number;
  resolved_slots?: number;
  unresolved_slots?: number;
  has_unresolved_slots?: boolean;
}

interface PlayInStatusPanelProps {
  rows: PlayInResolutionRow[];
}

export function PlayInStatusPanel({ rows }: PlayInStatusPanelProps) {
  const row = rows[0];
  if (!row) {
    return null;
  }

  const expected = Number(row.expected_slots ?? 0);
  const resolved = Number(row.resolved_slots ?? 0);
  const unresolved = Number(
    row.unresolved_slots ?? Math.max(expected - resolved, 0),
  );
  const hasUnresolved = Boolean(row.has_unresolved_slots ?? unresolved > 0);

  return (
    <div className="play-in-status" data-testid="play-in-panel">
      {hasUnresolved ? (
        <p className="play-in-status__warning" role="status">
          {unresolved} First Four slots are still unresolved ({resolved} of {expected} resolved).
          Bracket paths assume simulated winners until play-in games finish.
        </p>
      ) : (
        <p className="play-in-status__ok" role="status">
          All {expected} First Four slots are resolved.
        </p>
      )}
      <table className="play-in-status__table">
        <thead>
          <tr>
            <th scope="col">Expected slots</th>
            <th scope="col">Resolved</th>
            <th scope="col">Unresolved</th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td>{expected}</td>
            <td>{resolved}</td>
            <td>{unresolved}</td>
          </tr>
        </tbody>
      </table>
    </div>
  );
}
