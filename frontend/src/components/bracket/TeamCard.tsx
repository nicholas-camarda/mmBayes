interface TeamCardProps {
  teamName: string;
  seed?: number | string;
  conferenceLabel?: string;
  stats: Array<{ label: string; value: string }>;
}

export function TeamCard({ teamName, seed, conferenceLabel, stats }: TeamCardProps) {
  return (
    <article className="team-card" aria-label={`${teamName} profile`}>
      <header className="team-card__header">
        <div>
          <div className="team-card__eyebrow">Team profile</div>
          <h4>{teamName}</h4>
        </div>
        {seed != null ? <span className="team-card__seed">#{seed}</span> : null}
      </header>
      {conferenceLabel ? (
        <p className="team-card__conference">{conferenceLabel}</p>
      ) : null}
      <dl className="team-card__stats">
        {stats.map((stat) => (
          <div key={stat.label} className="team-card__stat">
            <dt>{stat.label}</dt>
            <dd>{stat.value}</dd>
          </div>
        ))}
      </dl>
    </article>
  );
}
