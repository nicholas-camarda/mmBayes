const NAV_ITEMS = [
  { href: "#build", label: "Enter bracket" },
  { href: "#review-queue", label: "Review picks" },
  { href: "#bracket-tree", label: "Tree" },
  { href: "#evidence", label: "Evidence" },
  { href: "#technical-appendix", label: "Reference" },
] as const;

export function JumpNav() {
  return (
    <nav className="jump-nav" aria-label="Dashboard sections">
      {NAV_ITEMS.map((item) => (
        <a key={item.href} href={item.href}>
          {item.label}
        </a>
      ))}
    </nav>
  );
}
