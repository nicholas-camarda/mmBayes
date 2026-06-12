const { test, expect } = require("@playwright/test");
const fs = require("node:fs");
const os = require("node:os");
const path = require("node:path");

const repoRoot = path.resolve(__dirname, "..");
const distDir = path.join(repoRoot, "frontend", "dist");

function stageApp() {
  const stageDir = fs.mkdtempSync(path.join(os.tmpdir(), "mmbayes-frontend-visual-"));
  fs.cpSync(distDir, stageDir, { recursive: true });
  const bracket = fs.readFileSync(
    path.join(repoRoot, "tests", "fixtures", "dashboard_payload_bracket.json"),
    "utf8",
  );
  const technical = fs.readFileSync(
    path.join(repoRoot, "tests", "fixtures", "dashboard_payload_technical.json"),
    "utf8",
  );
  fs.writeFileSync(
    path.join(stageDir, "dashboard_payloads.js"),
    `window.__MMBAYES_PAYLOADS__ = {"bracket": ${bracket}, "technical": ${technical}};`,
  );
  return stageDir;
}

async function waitForFonts(page) {
  await page.evaluate(() => document.fonts.ready);
}

test.describe("frontend visual regression", () => {
  test.skip(!fs.existsSync(distDir), "frontend/dist missing; run `npm run build` in frontend/ first");

  test("bracket desktop visual baseline", async ({ page }) => {
    const stageDir = stageApp();
    await page.setViewportSize({ width: 1320, height: 960 });
    await page.goto(`file://${path.join(stageDir, "index.html")}`, { waitUntil: "domcontentloaded" });
    await expect(page.getByRole("heading", { name: /bracket entry workspace/i })).toBeVisible();
    await waitForFonts(page);
    await expect(page).toHaveScreenshot("bracket-desktop.png", {
      maxDiffPixelRatio: 0.02,
    });
  });

  test("bracket mobile visual baseline", async ({ page }) => {
    const stageDir = stageApp();
    await page.setViewportSize({ width: 390, height: 900 });
    await page.goto(`file://${path.join(stageDir, "index.html")}`, { waitUntil: "domcontentloaded" });
    await expect(page.getByRole("heading", { name: /bracket entry workspace/i })).toBeVisible();
    await waitForFonts(page);
    await expect(page).toHaveScreenshot("bracket-mobile.png", {
      maxDiffPixelRatio: 0.02,
    });
  });

  test("technical desktop visual baseline", async ({ page }) => {
    const stageDir = stageApp();
    await page.setViewportSize({ width: 1320, height: 960 });
    await page.goto(`file://${path.join(stageDir, "technical.html")}`, { waitUntil: "domcontentloaded" });
    await expect(page.getByRole("heading", { name: /technical bracket dashboard/i })).toBeVisible();
    await waitForFonts(page);
    await expect(page).toHaveScreenshot("technical-desktop.png", {
      maxDiffPixelRatio: 0.02,
    });
  });
});
