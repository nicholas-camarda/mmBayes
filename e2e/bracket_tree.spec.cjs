const { test, expect } = require("@playwright/test");
const { execFileSync } = require("node:child_process");
const path = require("node:path");
const os = require("node:os");
const fs = require("node:fs");
const { pathToFileURL } = require("node:url");

function resolveRscript() {
  const candidates = [
    process.env.RSCRIPT_PATH,
    "/usr/local/bin/Rscript",
    "/opt/homebrew/bin/Rscript",
    "/Library/Frameworks/R.framework/Resources/bin/Rscript",
  ].filter(Boolean);

  const existing = candidates.find((candidate) => fs.existsSync(candidate));
  if (existing) return existing;

  throw new Error("Unable to locate Rscript. Set RSCRIPT_PATH for the Playwright run.");
}

test("bracket tree hover/click/toggles work without console errors", async ({ page }) => {
  const errors = [];
  page.on("pageerror", (err) => errors.push(String(err)));
  page.on("console", (msg) => {
    if (msg.type() === "error") errors.push(msg.text());
  });

  const tmpDir = fs.mkdtempSync(path.join(os.tmpdir(), "mmBayes-playwright-"));
  const repoRoot = path.resolve(__dirname, "..");
  const generator = path.join(__dirname, "generate_fixture_dashboard.R");
  const htmlPath = execFileSync(resolveRscript(), [generator, tmpDir], {
    cwd: repoRoot,
    encoding: "utf8",
  }).trim();

  await page.goto(pathToFileURL(htmlPath).href, { waitUntil: "domcontentloaded" });

  await expect(page.locator("#btree-svg")).toBeVisible();
  await expect(page.locator("g.btree-node")).toHaveCount(63);

  const matchupNode = page.locator('g.btree-node[data-tip-matchup*="St. John"]').first();

  await expect(matchupNode).toBeVisible();
  await matchupNode.hover();
  await expect(page.locator("#btree-tooltip")).toHaveClass(/is-visible/);
  await expect(page.locator("#btree-tooltip")).toContainText("St. John's");
  await expect(page.locator("#btree-tooltip")).toContainText("Texas A&M");

  const evidenceId = await matchupNode.getAttribute("data-open-evidence");
  expect(evidenceId).toBeTruthy();

  await matchupNode.click();
  await page.waitForFunction((id) => {
    const el = document.getElementById(id);
    return !!el && el.open === true;
  }, evidenceId);

  await page.locator(".btree-toggle[data-btree-view='c1']").click();
  await expect(page.locator("#btree-svg")).toHaveClass(/btree-view-c1/);

  await page.locator(".btree-toggle[data-btree-view='c2']").click();
  await expect(page.locator("#btree-svg")).toHaveClass(/btree-view-c2/);

  expect(errors).toEqual([]);
});
