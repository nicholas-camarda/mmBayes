import type { ReactNode } from "react";
import type { BuildMetadata } from "../types/payload";

export function BuildMetadataBanner({ metadata }: { metadata: BuildMetadata }) {
  const chips: { label: string; value: ReactNode }[] = [];

  const updatedAt =
    metadata.render_timestamp ??
    metadata.rendered_at_label ??
    metadata.rendered_at;
  if (updatedAt) {
    chips.push({ label: "Updated", value: String(updatedAt) });
  }

  const commit =
    metadata.git_commit ?? metadata.commit_short ?? metadata.commit_full;
  if (commit) {
    chips.push({
      label: "Built from commit",
      value: <code>{String(commit)}</code>,
    });
  }

  if (metadata.repo_snapshot_synced) {
    chips.push({
      label: "Repo snapshot",
      value: String(metadata.repo_snapshot_label ?? "Synced with cloud inputs"),
    });
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
