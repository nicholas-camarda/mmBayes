import { StrictMode } from "react";
import { createRoot } from "react-dom/client";
import { loadTechnicalPayload } from "../lib/loadPayloads";
import { TechnicalApp } from "./TechnicalApp";
import "../styles.css";

const root = createRoot(document.getElementById("root")!);
loadTechnicalPayload()
  .then((payload) => root.render(<StrictMode><TechnicalApp payload={payload} /></StrictMode>))
  .catch((error: Error) => {
    root.render(
      <main className="dashboard">
        <h1>Technical Dashboard</h1>
        <p role="alert">Failed to load dashboard payload: {error.message}</p>
      </main>
    );
  });
