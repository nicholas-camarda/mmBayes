import { StrictMode } from "react";
import { createRoot } from "react-dom/client";
import { loadBracketPayload } from "../lib/loadPayloads";
import { BracketApp } from "./BracketApp";
import "../styles.css";

const root = createRoot(document.getElementById("root")!);
loadBracketPayload()
  .then((payload) => root.render(<StrictMode><BracketApp payload={payload} /></StrictMode>))
  .catch((error: Error) => {
    root.render(
      <main className="dashboard">
        <h1>Bracket Dashboard</h1>
        <p role="alert">Failed to load dashboard payload: {error.message}</p>
      </main>
    );
  });
