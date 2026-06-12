interface BoardExplainerProps {
  what: string;
  how: string;
  why: string;
  math: string;
}

export function BoardExplainer({ what, how, why, math }: BoardExplainerProps) {
  const cards = [
    ["What this shows", what],
    ["How to use it", how],
    ["Why it matters", why],
    ["Underlying math", math],
  ] as const;
  return (
    <div className="explain-grid" data-testid="board-explainer">
      {cards.map(([label, body]) => (
        <div key={label} className="explain-card">
          <div className="explain-label">{label}</div>
          <p>{body}</p>
        </div>
      ))}
    </div>
  );
}
