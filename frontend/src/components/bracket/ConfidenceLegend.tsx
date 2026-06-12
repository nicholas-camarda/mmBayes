const TIERS = [
  { name: "Lock", color: "#2a9d8f", description: "Strong favorite; route rarely flips across posterior draws." },
  { name: "Lean", color: "#457b9d", description: "Clear lean, but not a lock if you want a contrarian path." },
  { name: "Toss-up", color: "#e9c46a", description: "Near coin flip; manual review is usually worthwhile." },
  { name: "Volatile", color: "#e76f51", description: "Wide uncertainty or leverage; inspect before you commit." },
] as const;

export function ConfidenceLegend({ title = "Confidence tiers" }: { title?: string }) {
  return (
    <section className="term-legend" aria-label={title}>
      <h3 className="term-legend__title">{title}</h3>
      <ul className="term-legend__list">
        {TIERS.map((tier) => (
          <li key={tier.name}>
            <span className="term-legend__swatch" style={{ background: tier.color }} aria-hidden />
            <strong>{tier.name}</strong>
            <span>{tier.description}</span>
          </li>
        ))}
      </ul>
    </section>
  );
}
