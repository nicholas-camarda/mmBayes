export function MissingSection({ label }: { label: string }) {
  return (
    <p data-testid="missing-section" className="missing-section">
      {label} is not available in this payload.
    </p>
  );
}
