import { ConfidenceLegend } from "./ConfidenceLegend";

const ROW_GUIDE = [
  { label: "Dot", text: "Posterior mean probability for the recommended side." },
  { label: "Bar", text: "Posterior interval around that mean, showing how much uncertainty still remains." },
  { label: "Color", text: "Confidence tier from Lock through Volatile so you can scan stability quickly." },
] as const;

const BOARD_GUIDE = [
  { label: "Ranked board", text: "Sorted by decision score so the most consequential calls rise first." },
  { label: "Upset board", text: "Sorted by leverage so the highest-payoff underdog pivots are obvious." },
  {
    label: "Divergence map",
    text: "Shows where Candidate 2 splits from Candidate 1, with late-round route changes carrying more visual weight.",
  },
] as const;

export function ReadingGuidePanel() {
  return (
    <details className="dashboard-disclosure dashboard-disclosure--orientation" id="how-to-read">
      <summary>
        <div>
          <div className="role-kicker role-kicker--orientation">Orientation</div>
          <div className="dashboard-disclosure__title">How to read this workspace</div>
          <div className="dashboard-disclosure__note">
            Open this for the confidence legend, glossary, and row-reading guide.
          </div>
        </div>
        <span className="dashboard-disclosure__chevron" aria-hidden />
      </summary>
      <div className="dashboard-disclosure__body">
        <section className="reading-guide" aria-label="How to read this workspace">
          <div className="reading-guide__hero">
            <div className="reading-guide__eyebrow">Quick orientation</div>
            <h3 className="reading-guide__title">
              Read the signal first, then open evidence only when the call still matters.
            </h3>
            <p className="reading-guide__lede">
              The review queue tells you what deserves attention. The confidence styling tells you how
              stable the current recommendation is. The evidence drawers are for the handful of games
              where you still want the deeper matchup detail.
            </p>
          </div>
          <div className="reading-guide__section">
            <div className="guide-label">How to read a row</div>
            <div className="reading-guide__cards">
              {ROW_GUIDE.map((item) => (
                <div key={item.label} className="reading-guide__card">
                  <strong>{item.label}</strong>
                  <span>{item.text}</span>
                </div>
              ))}
            </div>
          </div>
          <div className="reading-guide__section">
            <div className="guide-label">Why the boards are ordered this way</div>
            <div className="reading-guide__cards">
              {BOARD_GUIDE.map((item) => (
                <div key={item.label} className="reading-guide__card">
                  <strong>{item.label}</strong>
                  <span>{item.text}</span>
                </div>
              ))}
            </div>
          </div>
          <div className="reading-guide__section reading-guide__section--legend">
            <ConfidenceLegend />
          </div>
        </section>
      </div>
    </details>
  );
}
