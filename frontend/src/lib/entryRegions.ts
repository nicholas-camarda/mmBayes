export const ENTRY_ROUND_ORDER = [
  "First Four",
  "Round of 64",
  "Round of 32",
  "Sweet 16",
  "Elite 8",
  "Final Four",
  "Championship",
] as const;

export const ENTRY_REGION_ORDER = [
  "East",
  "West",
  "South",
  "Midwest",
  "National",
  "Unassigned",
] as const;

export function entryRegionDisplay(region: string | undefined, round: string | undefined): string {
  const regionText = region?.trim();
  if (regionText && regionText !== "NA" && regionText !== "n/a" && regionText !== "N/A") {
    return regionText;
  }
  if (round === "Final Four" || round === "Championship") {
    return "National";
  }
  return "Unassigned";
}

export function entryFillStep(regionName: string): string {
  switch (regionName) {
    case "East":
      return "Fill first";
    case "West":
      return "Fill second";
    case "South":
      return "Fill third";
    case "Midwest":
      return "Fill fourth";
    case "National":
      return "Finals";
    default:
      return "Review";
  }
}
