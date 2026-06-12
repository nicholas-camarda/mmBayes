import { useMemo, useState, type FocusEvent, type MouseEvent } from "react";
import type { BracketTreeEntry, BracketTreeNode } from "../../types/payload";
import { tierColor, tierTextColor, truncateLabel } from "../../lib/tierColors";
import { BracketTreeLegend } from "./BracketTreeLegend";

const SVG_WIDTH = 1400;
const SVG_HEIGHT = 1070;
const NODE_W = 120;
const NODE_H = 28;

interface BracketTreeProps {
  trees: BracketTreeEntry[];
  onOpenEvidence: (evidenceId: string) => void;
}

function seedLabel(seed: number | undefined, team: string): string {
  const prefix = seed != null ? `(${seed}) ` : "";
  return `${prefix}${truncateLabel(team)}`;
}

function tooltipLines(node: BracketTreeNode, candidateLabel: string): string[] {
  const matchup = `${node.teamA} vs ${node.teamB}`;
  const pick = node.candidate_pick ?? node.winner ?? "n/a";
  const favorite = node.posterior_favorite ?? "n/a";
  const prob =
    typeof node.win_prob_favorite === "number"
      ? `${(100 * node.win_prob_favorite).toFixed(0)}%`
      : "n/a";
  return [
    matchup,
    `${candidateLabel}: ${pick}`,
    `Favorite: ${favorite} (${prob})`,
    node.confidence_tier ? `Tier: ${node.confidence_tier}` : "",
    node.rationale_short ? String(node.rationale_short) : "",
  ].filter(Boolean);
}

function TreeNode({
  node,
  candidateLabel,
  onOpenEvidence,
  onHover,
  onLeave,
}: {
  node: BracketTreeNode;
  candidateLabel: string;
  onOpenEvidence: (evidenceId: string) => void;
  onHover: (
    node: BracketTreeNode,
    candidateLabel: string,
    event: MouseEvent<SVGGElement> | FocusEvent<SVGGElement>,
  ) => void;
  onLeave: () => void;
}) {
  const x = Math.round(node.node_x);
  const y = Math.round(node.node_y);
  if (!Number.isFinite(x) || !Number.isFinite(y)) return null;

  const tier = node.confidence_tier;
  const fill = tierColor(tier);
  const textColor = tierTextColor(tier);
  const routeDiff = Boolean(node.route_diff);
  const rx = x - NODE_W / 2;
  const ry = y - NODE_H / 2;
  const evidenceId = node.evidence_id ?? `evidence-${node.slot_key}`;
  const tipMatchup = `${node.teamA} vs ${node.teamB}`;

  return (
    <g
      className={`btree-node${routeDiff ? " btree-node--route-diff" : ""}`}
      data-slot={node.slot_key}
      data-open-evidence={evidenceId}
      data-tip-matchup={tipMatchup}
      data-tip-candidate={candidateLabel}
      onClick={() => onOpenEvidence(evidenceId)}
      onMouseEnter={(event) => onHover(node, candidateLabel, event)}
      onMouseLeave={onLeave}
      onFocus={(event) => onHover(node, candidateLabel, event)}
      onBlur={onLeave}
      onKeyDown={(event) => {
        if (event.key === "Enter" || event.key === " ") {
          event.preventDefault();
          onOpenEvidence(evidenceId);
        }
      }}
      role="button"
      tabIndex={0}
      aria-label={tipMatchup}
    >
      <rect
        x={rx}
        y={ry}
        width={NODE_W}
        height={NODE_H}
        rx={4}
        fill={fill}
        stroke={routeDiff ? "#f59e0b" : "rgba(255,255,255,0.18)"}
        strokeWidth={routeDiff ? 3 : 1}
      />
      <text x={x} y={y - 7} textAnchor="middle" fontSize={9} fill={textColor}>
        {seedLabel(node.teamA_seed, node.teamA)}
      </text>
      <text x={x} y={y + 7} textAnchor="middle" fontSize={9} fill={textColor}>
        {seedLabel(node.teamB_seed, node.teamB)}
      </text>
      {node.upset ? (
        <text x={rx + NODE_W - 10} y={ry + 10} fontSize={8} fontWeight={800} fill="#e76f51">
          U
        </text>
      ) : null}
    </g>
  );
}

export function BracketTree({ trees, onOpenEvidence }: BracketTreeProps) {
  const [activeCandidateId, setActiveCandidateId] = useState(trees[0]?.candidate_id ?? 1);
  const [tooltip, setTooltip] = useState<{ lines: string[]; x: number; y: number } | null>(null);

  const activeTree = useMemo(
    () => trees.find((tree) => tree.candidate_id === activeCandidateId) ?? trees[0],
    [trees, activeCandidateId],
  );

  if (!activeTree || activeTree.nodes.length === 0) {
    return <p className="missing-section">Bracket tree is not available in this payload.</p>;
  }

  return (
    <section id="bracket-tree" aria-label="Bracket tree" className="bracket-tree-panel">
      <BracketTreeLegend />
      <div className="bracket-tree-controls">
        {trees.map((tree) => (
          <button
            key={tree.candidate_id}
            type="button"
            className={`btree-toggle${tree.candidate_id === activeCandidateId ? " is-active" : ""}`}
            data-btree-target={`candidate-${tree.candidate_id}`}
            onClick={() => setActiveCandidateId(tree.candidate_id)}
          >
            {tree.candidate_label}
          </button>
        ))}
      </div>
      {trees.map((tree) => {
        const isActive = tree.candidate_id === activeCandidateId;
        return (
          <div
            key={tree.candidate_id}
            className={`btree-panel${isActive ? " is-active" : ""}`}
            data-btree-panel={`candidate-${tree.candidate_id}`}
            hidden={!isActive}
          >
            <div className="bracket-tree-container">
              <svg
                id={`btree-svg-${tree.candidate_id}`}
                viewBox={`0 0 ${SVG_WIDTH} ${SVG_HEIGHT}`}
                className="btree-svg"
              >
                {tree.edges.map((edge) => {
                  const sx = edge.x1 + NODE_W / 2;
                  const tx = edge.x2 - NODE_W / 2;
                  const mid = (sx + tx) / 2;
                  const routeDiff = Boolean(edge.route_diff);
                  return (
                    <path
                      key={`${edge.from_slot}-${edge.to_slot}`}
                      d={`M${sx},${edge.y1} L${mid},${edge.y1} L${mid},${edge.y2} L${tx},${edge.y2}`}
                      fill="none"
                      stroke={routeDiff ? "#f59e0b" : "#64748b"}
                      strokeWidth={routeDiff ? 2.6 : 1.4}
                      className={routeDiff ? "btree-edge btree-edge--route-diff" : "btree-edge"}
                    />
                  );
                })}
                {tree.nodes.map((node) => (
                  <TreeNode
                    key={node.slot_key}
                    node={node}
                    candidateLabel={tree.candidate_label}
                    onOpenEvidence={onOpenEvidence}
                    onHover={(hoveredNode, candidateLabel, event) => {
                      if (!("clientX" in event)) return;
                      setTooltip({
                        lines: tooltipLines(hoveredNode, candidateLabel),
                        x: event.clientX + 12,
                        y: event.clientY + 12,
                      });
                    }}
                    onLeave={() => setTooltip(null)}
                  />
                ))}
              </svg>
            </div>
          </div>
        );
      })}
      <div
        id="btree-tooltip"
        className={tooltip ? "is-visible" : ""}
        style={
          tooltip
            ? {
                left: tooltip.x,
                top: tooltip.y,
              }
            : undefined
        }
      >
        {tooltip?.lines.map((line) => <div key={line}>{line}</div>)}
      </div>
    </section>
  );
}
