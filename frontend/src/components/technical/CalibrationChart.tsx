import type { CalibrationRow } from "../../types/payload";
import { formatProbability } from "../../lib/format";

const WIDTH = 760;
const HEIGHT = 420;
const MARGIN_LEFT = 88;
const MARGIN_RIGHT = 34;
const MARGIN_TOP = 50;
const MARGIN_BOTTOM = 64;

function toX(probability: number, plotWidth: number): number {
  return MARGIN_LEFT + probability * plotWidth;
}

function toY(probability: number, plotHeight: number): number {
  return MARGIN_TOP + (1 - probability) * plotHeight;
}

export function CalibrationChart({ rows }: { rows: CalibrationRow[] }) {
  if (rows.length === 0) {
    return <p className="missing-section">Backtest calibration was not available for this run.</p>;
  }

  const plotWidth = WIDTH - MARGIN_LEFT - MARGIN_RIGHT;
  const plotHeight = HEIGHT - MARGIN_BOTTOM - MARGIN_TOP;
  const sorted = [...rows].sort(
    (a, b) => (a.mean_predicted ?? 0) - (b.mean_predicted ?? 0),
  );
  const tickValues = [0, 0.25, 0.5, 0.75, 1];
  const polyline = sorted
    .map((row) => {
      const x = toX(row.mean_predicted ?? 0, plotWidth);
      const y = toY(row.empirical_rate ?? 0, plotHeight);
      return `${x},${y}`;
    })
    .join(" ");

  return (
    <svg
      viewBox={`0 0 ${WIDTH} ${HEIGHT}`}
      className="tech-svg tech-svg--calibration"
      role="img"
      aria-label="Backtest calibration chart"
      data-testid="calibration-chart"
    >
      <rect
        x={MARGIN_LEFT}
        y={MARGIN_TOP}
        width={plotWidth}
        height={plotHeight}
        rx={12}
        fill="#0b1220"
        stroke="#334155"
        strokeWidth={1}
      />
      {tickValues.map((tick) => {
        const x = toX(tick, plotWidth);
        const y = toY(tick, plotHeight);
        return (
          <g key={tick}>
            <line
              x1={x}
              y1={MARGIN_TOP}
              x2={x}
              y2={MARGIN_TOP + plotHeight}
              stroke="#223047"
              strokeWidth={1}
            />
            <line
              x1={MARGIN_LEFT}
              y1={y}
              x2={MARGIN_LEFT + plotWidth}
              y2={y}
              stroke="#223047"
              strokeWidth={1}
            />
            <text
              x={x}
              y={HEIGHT - 22}
              textAnchor="middle"
              fontSize={13}
              fontWeight={700}
              fill="#a9b6ca"
            >
              {formatProbability(tick)}
            </text>
            <text
              x={MARGIN_LEFT - 14}
              y={y + 5}
              textAnchor="end"
              fontSize={13}
              fontWeight={700}
              fill="#a9b6ca"
            >
              {formatProbability(tick)}
            </text>
          </g>
        );
      })}
      <line
        x1={MARGIN_LEFT}
        y1={MARGIN_TOP + plotHeight}
        x2={MARGIN_LEFT + plotWidth}
        y2={MARGIN_TOP}
        stroke="#e5edf7"
        strokeDasharray="6 6"
        strokeWidth={2}
        opacity={0.85}
      />
      <line
        x1={MARGIN_LEFT}
        y1={MARGIN_TOP}
        x2={MARGIN_LEFT}
        y2={MARGIN_TOP + plotHeight}
        stroke="#94a3b8"
        strokeWidth={2}
      />
      <line
        x1={MARGIN_LEFT}
        y1={MARGIN_TOP + plotHeight}
        x2={MARGIN_LEFT + plotWidth}
        y2={MARGIN_TOP + plotHeight}
        stroke="#94a3b8"
        strokeWidth={2}
      />
      <polyline
        points={polyline}
        fill="none"
        stroke="#38bdf8"
        strokeWidth={4}
        strokeLinecap="round"
        strokeLinejoin="round"
      />
      {sorted.map((row, index) => {
        const x = toX(row.mean_predicted ?? 0, plotWidth);
        const y = toY(row.empirical_rate ?? 0, plotHeight);
        const title = `${row.bin ?? "bucket"} | predicted ${formatProbability(row.mean_predicted)} | observed ${formatProbability(row.empirical_rate)} | ${row.n_games ?? 0} games`;
        return (
          <circle
            key={`${row.bin ?? index}`}
            cx={x}
            cy={y}
            r={7}
            fill="#38bdf8"
            stroke="#0f172a"
            strokeWidth={2}
          >
            <title>{title}</title>
          </circle>
        );
      })}
      <text
        x={MARGIN_LEFT + plotWidth / 2}
        y={HEIGHT - 2}
        textAnchor="middle"
        fontSize={14}
        fontWeight={700}
        fill="#cbd5e1"
      >
        Average predicted win probability
      </text>
      <text
        x={24}
        y={MARGIN_TOP + plotHeight / 2}
        textAnchor="middle"
        fontSize={14}
        fontWeight={700}
        fill="#cbd5e1"
        transform={`rotate(-90 24 ${MARGIN_TOP + plotHeight / 2})`}
      >
        Observed win rate
      </text>
      <text x={MARGIN_LEFT} y={26} fontSize={13} fontWeight={700} fill="#38bdf8">
        Observed bucket rate
      </text>
      <line
        x1={MARGIN_LEFT + 186}
        y1={21}
        x2={MARGIN_LEFT + 226}
        y2={21}
        stroke="#e5edf7"
        strokeDasharray="6 6"
        strokeWidth={2}
      />
      <text x={MARGIN_LEFT + 236} y={26} fontSize={13} fontWeight={700} fill="#cbd5e1">
        Perfect calibration
      </text>
    </svg>
  );
}
