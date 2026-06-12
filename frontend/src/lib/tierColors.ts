const TIER_PALETTE: Record<string, string> = {
  Lock: "#2a9d8f",
  Lean: "#457b9d",
  "Toss-up": "#e9c46a",
  Volatile: "#e76f51",
};

export function tierColor(tier: string | undefined): string {
  if (!tier) return "#6b7280";
  return TIER_PALETTE[tier] ?? "#6b7280";
}

export function tierTextColor(tier: string | undefined): string {
  return tier === "Toss-up" || tier === "Volatile" ? "#0f172a" : "#ffffff";
}

export function truncateLabel(value: string, width = 9): string {
  if (value.length <= width) return value;
  return `${value.slice(0, width - 1)}…`;
}
