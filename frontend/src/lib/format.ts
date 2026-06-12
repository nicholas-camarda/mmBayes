export function formatProbability(value: number | null | undefined): string {
  if (value == null || Number.isNaN(value)) return "n/a";
  return `${(100 * value).toFixed(1)}%`;
}

export function formatProbabilityInterval(
  lower: number | null | undefined,
  upper: number | null | undefined,
): string {
  if (lower == null || upper == null) return "n/a";
  return `${formatProbability(lower)} – ${formatProbability(upper)}`;
}

export function tierClassName(tier: string | undefined): string {
  return (tier ?? "unknown").toLowerCase().replace(/[^a-z0-9]+/g, "-");
}
