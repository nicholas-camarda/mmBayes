import type { BuildMetadata } from "../types/payload";

export function BuildMetadataBanner({ metadata }: { metadata: BuildMetadata }) {
  const parts = [
    metadata.git_commit ? `commit ${metadata.git_commit}` : null,
    metadata.render_timestamp ? `rendered ${metadata.render_timestamp}` : null,
    metadata.repo_snapshot_synced ? "repo snapshot synced" : null,
  ].filter(Boolean);
  if (parts.length === 0) return null;
  return (
    <p data-testid="build-metadata" className="build-metadata">
      {parts.join(" | ")}
    </p>
  );
}
