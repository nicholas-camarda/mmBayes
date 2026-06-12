import type { ReactNode } from "react";
import type { BuildMetadata } from "../types/payload";

export function BuildMetadataBanner({ metadata }: { metadata: BuildMetadata }) {
  const chips: { label: string; value: ReactNode }[] = [];

  if (metadata.render_timestamp) {
    chips.push({ label: "Updated", value: metadata.render_timestamp });
  }
  if (metadata.git_commit) {
    chips.push({
      label: "Built from commit",
      value: <code>{metadata.git_commit}</code>,
    });
  }
  if (metadata.repo_snapshot_synced) {
    chips.push({ label: "Repo snapshot", value: "Synced with cloud inputs" });
  }

  if (chips.length === 0) return null;

  return (
    <div data-testid="build-metadata" className="hero-meta">
      {chips.map((chip) => (
        <div key={chip.label} className="hero-meta__chip">
          <span>{chip.label}</span>
          <strong>{chip.value}</strong>
        </div>
      ))}
    </div>
  );
}
