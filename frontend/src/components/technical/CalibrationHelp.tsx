const HELP_ITEMS = [
  {
    label: "Point",
    text: "Held-out games grouped by predicted win-probability range; for example, among games where the model gave one side about a 35% chance to win.",
  },
  {
    label: "Dashed line",
    text: "Perfect long-run calibration: predicted rate equals observed rate, so the curve shows long-run frequency matching.",
  },
  {
    label: "Below line",
    text: "The model was too optimistic in that probability range.",
  },
  {
    label: "Above line",
    text: "The model was too pessimistic in that probability range.",
  },
] as const;

export function CalibrationHelp() {
  return (
    <div className="calibration-help-grid" aria-label="How to read the calibration chart">
      {HELP_ITEMS.map((item) => (
        <div key={item.label} className="explain-card">
          <div className="explain-label">{item.label}</div>
          <p>{item.text}</p>
        </div>
      ))}
    </div>
  );
}
