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

  await expect(page.locator("#btree-svg-1")).toBeVisible();
  await expect(page.locator("svg.btree-svg")).toHaveCount(2);
  await expect(page.locator(".btree-toggle", { hasText: "Both candidates" })).toHaveCount(0);

  const specialCharNode = page
    .locator("[data-btree-panel='candidate-1'] g.btree-node[data-tip-matchup*=\"St. John\"]")
    .first();

  await expect(specialCharNode).toBeVisible();
  await specialCharNode.hover();
  await expect(page.locator("#btree-tooltip")).toHaveClass(/is-visible/);
  await expect(page.locator("#btree-tooltip")).toContainText("St. John's");
  await expect(page.locator("#btree-tooltip")).toContainText("Texas A&M");

  const evidenceId = await specialCharNode.getAttribute("data-open-evidence");
  expect(evidenceId).toBeTruthy();

  await specialCharNode.click();
  await page.waitForFunction((id) => {
    const el = document.getElementById(id);
    return !!el && el.open === true;
  }, evidenceId);

  const candidateOneEastNode = page.locator(
    "[data-btree-panel='candidate-1'] g.btree-node[data-slot='East|Round of 32|3']",
  );
  await candidateOneEastNode.hover();
  await expect(page.locator("#btree-tooltip")).toContainText("Louisville vs Michigan State");
  await expect(page.locator("#btree-tooltip")).toContainText("Candidate 1: Louisville");

  await page.locator(".btree-toggle[data-btree-target='candidate-2']").click();
  await expect(page.locator("[data-btree-panel='candidate-1']")).toBeHidden();
  await expect(page.locator("[data-btree-panel='candidate-2']")).toBeVisible();

  const candidateTwoEastNode = page.locator(
    "[data-btree-panel='candidate-2'] g.btree-node[data-slot='East|Round of 32|3']",
  );
  await candidateTwoEastNode.hover();
  await expect(page.locator("#btree-tooltip")).toContainText("South Florida vs Michigan State");
  await expect(page.locator("#btree-tooltip")).toContainText("Candidate 2: Michigan State");
  await expect(page.locator("#btree-tooltip")).not.toContainText("Candidate 1: Louisville");

  expect(errors).toEqual([]);
});
