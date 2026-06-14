import type { ReactNode } from "react";

type RoleTone = "action" | "orientation" | "evidence" | "reference" | "technical";

interface DashboardSectionProps {
  id?: string;
  roleTone?: RoleTone;
  kicker?: string;
  title: string;
  note?: string;
  children: ReactNode;
}

function roleKickerClass(roleTone: RoleTone): string {
  return roleTone === "action" ? "act" : roleTone;
}

export function DashboardSection({
  id,
  roleTone = "technical",
  kicker,
  title,
  note,
  children,
}: DashboardSectionProps) {
  return (
    <section
      id={id}
      className={`section dashboard-section dashboard-section--${roleTone}`}
      aria-labelledby={id ? `${id}-title` : undefined}
      aria-label={id ? undefined : title}
    >
      {kicker ? (
        <div className={`role-kicker role-kicker--${roleKickerClass(roleTone)}`}>{kicker}</div>
      ) : null}
      <h2 id={id ? `${id}-title` : undefined}>{title}</h2>
      {note ? <p className="section-note">{note}</p> : null}
      {children}
    </section>
  );
}
