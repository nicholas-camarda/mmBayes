import type { ReactNode } from "react";
import type { BuildMetadata } from "../types/payload";
import { BuildMetadataBanner } from "./BuildMetadataBanner";

interface DashboardHeroProps {
  eyebrow: string;
  title: string;
  lede: string;
  metadata: BuildMetadata;
  nav?: ReactNode;
}

export function DashboardHero({ eyebrow, title, lede, metadata, nav }: DashboardHeroProps) {
  return (
    <header className="dashboard-hero" role="banner">
      <div className="dashboard-hero__copy">
        <div className="eyebrow">{eyebrow}</div>
        <h1>{title}</h1>
        <p className="lede">{lede}</p>
      </div>
      <BuildMetadataBanner metadata={metadata} />
      {nav ? <div className="dashboard-hero__nav">{nav}</div> : null}
    </header>
  );
}
