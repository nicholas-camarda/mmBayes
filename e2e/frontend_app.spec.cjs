const { test, expect } = require("@playwright/test");
const fs = require("node:fs");
const os = require("node:os");
const path = require("node:path");

const repoRoot = path.resolve(__dirname, "..");
const distDir = path.join(repoRoot, "frontend", "dist");

function stageApp() {
  const stageDir = fs.mkdtempSync(path.join(os.tmpdir(), "mmbayes-frontend-e2e-"));
  fs.cpSync(distDir, stageDir, { recursive: true });
  const bracket = fs.readFileSync(
    path.join(repoRoot, "tests", "fixtures", "dashboard_payload_bracket.json"),
    "utf8"
  );
  const technical = fs.readFileSync(
    path.join(repoRoot, "tests", "fixtures", "dashboard_payload_technical.json"),
    "utf8"
  );
  fs.writeFileSync(
    path.join(stageDir, "dashboard_payloads.js"),
    `window.__MMBAYES_PAYLOADS__ = {"bracket": ${bracket}, "technical": ${technical}};`
  );
  return stageDir;
}

test.describe("static frontend dashboards", () => {
  test.skip(!fs.existsSync(distDir), "frontend/dist missing; run `npm run build` in frontend/ first");

  test("bracket page renders fixture payload offline over file://", async ({ page }) => {
    const stageDir = stageApp();
    const errors = [];
    page.on("console", (msg) => {
      if (msg.type() === "error") errors.push(msg.text());
    });
    await page.goto(`file://${path.join(stageDir, "index.html")}`);
    await expect(page.getByRole("heading", { name: /bracket dashboard/i })).toBeVisible();
    await expect(page.getByTestId("candidate-card").first()).toBeVisible();
    await expect(page.getByTestId("decision-sheet")).toBeVisible();
    expect(errors).toEqual([]);
  });

  test("technical page renders fixture payload offline over file://", async ({ page }) => {
    const stageDir = stageApp();
    await page.goto(`file://${path.join(stageDir, "technical.html")}`);
    await expect(page.getByRole("heading", { name: /technical dashboard/i })).toBeVisible();
    await expect(page.getByTestId("decision-summary")).toBeVisible();
  });
});
