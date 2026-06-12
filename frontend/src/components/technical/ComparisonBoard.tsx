import type { ReactNode } from "react";

interface ComparisonBoardProps {
  variant: "ranked" | "upset" | "divergence";
  columns: string[];
  children: ReactNode;
  testId?: string;
}

export function ComparisonBoard({ variant, columns, children, testId }: ComparisonBoardProps) {
  return (
    <div className={`comparison-board comparison-board--${variant}`} data-testid={testId}>
      <div
        className={`comparison-board__header${
          columns.length === 4 ? " comparison-board__header--four" : ""
        }`}
      >
        {columns.map((column) => (
          <div key={column}>{column}</div>
        ))}
      </div>
      {children}
    </div>
  );
}

interface BoardRowProps {
  variant: "ranked" | "upset" | "divergence";
  cells: Array<{ label: string; value: ReactNode; note?: ReactNode; plot?: boolean }>;
}

export function BoardRow({ variant, cells }: BoardRowProps) {
  return (
    <div className={`board-row board-row--${variant}`}>
      {cells.map((cell) => (
        <div
          key={cell.label}
          className={`board-cell${cell.plot ? " board-cell--plot" : ""}${
            cell.label.toLowerCase().includes("game") || cell.label.toLowerCase().includes("underdog")
              ? " board-cell--game"
              : ""
          }`}
          data-label={cell.label}
        >
          <div className="board-value">{cell.value}</div>
          {cell.note ? <div className="board-note">{cell.note}</div> : null}
        </div>
      ))}
    </div>
  );
}
