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

function generateDashboardHtml() {
  const tmpDir = fs.mkdtempSync(path.join(os.tmpdir(), "mmBayes-playwright-"));
  const repoRoot = path.resolve(__dirname, "..");
  const generator = path.join(__dirname, "generate_fixture_dashboard.R");

  const output = execFileSync(resolveRscript(), [generator, tmpDir], {
    cwd: repoRoot,
    encoding: "utf8",
  });

  const generatedPath = output
    .trim()
    .split(/\r?\n/)
    .map((line) => line.trim())
    .reverse()
    .find((line) => line.endsWith(".html") && fs.existsSync(line));

  if (!generatedPath) {
    throw new Error(`Fixture generator did not report a generated HTML path.\n${output}`);
  }

  return generatedPath;
}

async function expectElementBox(locator, minimumWidth = 20, minimumHeight = 20) {
  await expect(locator).toBeVisible();
  const box = await locator.boundingBox();
  expect(box).toBeTruthy();
  expect(box.width).toBeGreaterThan(minimumWidth);
  expect(box.height).toBeGreaterThan(minimumHeight);
}

async function expectSvgHasVisibleContent(locator) {
  await expectElementBox(locator, 200, 120);
  const stats = await locator.evaluate((svg) => ({
    textCount: svg.querySelectorAll("text").length,
    shapeCount: svg.querySelectorAll("rect,path,line,circle,polyline,polygon").length,
  }));
  expect(stats.textCount).toBeGreaterThan(5);
  expect(stats.shapeCount).toBeGreaterThan(10);
}

function expectUsefulScreenshot(buffer) {
  expect(buffer.length).toBeGreaterThan(15000);
  expect(buffer.subarray(1, 4).toString("ascii")).toBe("PNG");
}

function collectBrowserErrors(page) {
  const errors = [];
  page.on("pageerror", (err) => errors.push(String(err)));
  page.on("console", (msg) => {
    if (msg.type() === "error") errors.push(msg.text());
  });
  return errors;
}

test("bracket tree hover/click/toggles work without console errors", async ({ page }) => {
  const errors = collectBrowserErrors(page);

  const htmlPath = generateDashboardHtml();
  await page.goto(pathToFileURL(htmlPath).href, { waitUntil: "domcontentloaded" });

  await expect(page.locator(".entry-workspace")).toBeVisible();
  await expect(page.locator("[data-entry-panel='candidate-1']")).toBeVisible();
  await expect(page.locator("[data-entry-panel='candidate-2']")).toBeHidden();
  await expect(page.locator(".entry-pick--review").first()).toBeVisible();
  await page.locator(".entry-toggle[data-entry-target='candidate-2']").click();
  await expect(page.locator("[data-entry-panel='candidate-1']")).toBeHidden();
  await expect(page.locator("[data-entry-panel='candidate-2']")).toBeVisible();
  await page.locator(".entry-toggle[data-entry-target='candidate-1']").click();
  await expect(page.locator("[data-entry-panel='candidate-1']")).toBeVisible();

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
  await expect(page.locator("[data-btree-panel='candidate-2'] .btree-node--route-diff").first()).toBeVisible();
  await expect(page.locator("[data-btree-panel='candidate-2'] .btree-edge--route-diff").first()).toBeVisible();

  const candidateTwoEastNode = page.locator(
    "[data-btree-panel='candidate-2'] g.btree-node[data-slot='East|Round of 32|3']",
  );
  await candidateTwoEastNode.hover();
  await expect(page.locator("#btree-tooltip")).toContainText("South Florida vs Michigan State");
  await expect(page.locator("#btree-tooltip")).toContainText("Candidate 2: Michigan State");
  await expect(page.locator("#btree-tooltip")).not.toContainText("Candidate 1: Louisville");

  expect(errors).toEqual([]);
});

test("desktop dashboard workflow zones and graphical jumps stay actionable", async ({ page }, testInfo) => {
  const errors = collectBrowserErrors(page);
  await page.setViewportSize({ width: 1320, height: 960 });
  const htmlPath = generateDashboardHtml();

  await page.goto(pathToFileURL(htmlPath).href, { waitUntil: "domcontentloaded" });

  const zones = {
    build: page.locator("#build"),
    review: page.locator("#review-queue"),
    tree: page.locator("#bracket-tree"),
    evidence: page.locator("#evidence"),
    paths: page.locator("#paths"),
    appendix: page.locator("#technical-appendix"),
  };

  for (const zone of Object.values(zones)) {
    await expect(zone).toBeVisible();
  }

  const zoneOrder = await page.evaluate(() =>
    ["build", "review-queue", "bracket-tree", "evidence", "paths", "technical-appendix"].map((id) => {
      const node = document.getElementById(id);
      return node.getBoundingClientRect().top + window.scrollY;
    }),
  );
  expect(zoneOrder[0]).toBeLessThan(zoneOrder[1]);
  expect(zoneOrder[1]).toBeLessThan(zoneOrder[2]);
  expect(zoneOrder[2]).toBeLessThan(zoneOrder[3]);
  expect(zoneOrder[3]).toBeLessThan(zoneOrder[4]);
  expect(zoneOrder[4]).toBeLessThan(zoneOrder[5]);

  await expectElementBox(page.locator(".candidate-comparison-panel"), 700, 80);
  await expectElementBox(page.locator(".divergence-map-panel"), 700, 180);
  await expectSvgHasVisibleContent(page.locator("svg.btree-svg").first());
  await expectElementBox(page.locator(".watchlist-card__probability .prob-track__lane").first(), 100, 8);

  const divergenceCell = page.locator("[data-divergence-target-kind='evidence']").first();
  const divergenceEvidenceId = await divergenceCell.getAttribute("data-open-evidence");
  expect(divergenceEvidenceId).toBeTruthy();
  await divergenceCell.click();
  await page.waitForFunction((id) => {
    const panel = document.getElementById(id);
    return !!panel && panel.open === true;
  }, divergenceEvidenceId);
  await expect(divergenceCell).toHaveAttribute("aria-pressed", "true");
  await expect(page.locator(".watchlist-card:not(.is-hidden-by-bucket)").first()).toBeVisible();

  const reviewJump = page.locator(".watchlist-card:not(.is-hidden-by-bucket) .jump-button[data-open-evidence]").first();
  const reviewEvidenceId = await reviewJump.getAttribute("data-open-evidence");
  expect(reviewEvidenceId).toBeTruthy();
  await reviewJump.click();
  await page.waitForFunction((id) => {
    const panel = document.getElementById(id);
    return !!panel && panel.open === true;
  }, reviewEvidenceId);

  await page.locator(".entry-toggle[data-entry-target='candidate-2']").click();
  await expect(page.locator("[data-entry-panel='candidate-2']")).toBeVisible();
  await page.locator(".btree-toggle[data-btree-target='candidate-2']").click();
  await expect(page.locator("[data-btree-panel='candidate-2'] .btree-node--route-diff").first()).toBeVisible();

  const screenshot = await page.screenshot({
    path: testInfo.outputPath("desktop-dashboard-workflow.png"),
    fullPage: false,
  });
  expectUsefulScreenshot(screenshot);
  expect(errors).toEqual([]);
});

test("mobile bracket entry workflow stays visible before deep diagnostics", async ({ page }) => {
  await page.setViewportSize({ width: 390, height: 900 });
  const htmlPath = generateDashboardHtml();

  await page.goto(pathToFileURL(htmlPath).href, { waitUntil: "domcontentloaded" });

  await expect(page.locator("h1")).toContainText("Bracket entry workspace");
  await expect(page.locator("#build")).toBeVisible();
  await expect(page.locator(".next-action-panel")).toBeVisible();
  await expect(page.locator(".entry-workspace")).toBeVisible();
  await expect(page.locator("[data-entry-panel='candidate-1']")).toBeVisible();
  await expect(page.locator(".entry-pick").first()).toBeVisible();
  await expect(page.locator(".entry-review-button").first()).toBeVisible();

  const buildTop = await page.locator("#build").evaluate((node) => node.getBoundingClientRect().top);
  const reviewTop = await page.locator("#review-queue").evaluate((node) => node.getBoundingClientRect().top);
  expect(buildTop).toBeLessThan(reviewTop);
});

test("mobile navigation, review cards, evidence jumps, and screenshots are usable", async ({ page }, testInfo) => {
  const errors = collectBrowserErrors(page);
  await page.setViewportSize({ width: 390, height: 900 });
  const htmlPath = generateDashboardHtml();

  await page.goto(pathToFileURL(htmlPath).href, { waitUntil: "domcontentloaded" });

  const navBox = await page.locator(".jump-nav").boundingBox();
  expect(navBox).toBeTruthy();
  expect(navBox.height).toBeLessThan(80);
  const navLabels = await page.locator(".jump-nav a").allTextContents();
  expect(navLabels).toEqual(expect.arrayContaining(["Enter bracket", "Review picks", "Tree", "Evidence", "Reference"]));

  await expect(page.locator("h1")).toContainText("Bracket entry workspace");
  await expect(page.locator(".status-panel")).toBeVisible();
  await expect(page.locator("#build .next-action-panel")).toBeVisible();
  await expect(page.locator("#build .entry-workspace")).toBeVisible();
  await expect(page.locator("#build .section-note")).toBeVisible();

  const entryGuidance = await page.locator("#build .section-note").evaluate((node) => {
    const style = window.getComputedStyle(node);
    const box = node.getBoundingClientRect();
    return {
      fontSize: Number.parseFloat(style.fontSize),
      lineHeight: Number.parseFloat(style.lineHeight),
      width: box.width,
    };
  });
  expect(entryGuidance.fontSize).toBeGreaterThanOrEqual(14);
  expect(entryGuidance.lineHeight).toBeGreaterThanOrEqual(18);
  expect(entryGuidance.width).toBeGreaterThan(300);

  await page.locator(".jump-nav a[href='#review-queue']").click();
  await expect(page.locator("#review-queue")).toBeVisible();
  const reviewCard = page.locator(".watchlist-card").first();
  await expectElementBox(reviewCard, 300, 180);
  await expect(reviewCard.locator(".jump-button[data-open-evidence]")).toBeVisible();
  await expect(reviewCard.locator(".watchlist-card__probability .prob-track__lane")).toBeVisible();

  const reviewJump = reviewCard.locator(".jump-button[data-open-evidence]");
  const evidenceId = await reviewJump.getAttribute("data-open-evidence");
  expect(evidenceId).toBeTruthy();
  await reviewJump.click();
  await page.waitForFunction((id) => {
    const panel = document.getElementById(id);
    return !!panel && panel.open === true;
  }, evidenceId);
  await expect(page.locator(`details[id="${evidenceId}"] .evidence-panel__lede`)).toBeVisible();

  await page.locator(".jump-nav a[href='#bracket-tree']").click();
  const visibleTreeContainer = page.locator(".bracket-tree-container").first();
  await expect(visibleTreeContainer).toBeVisible();
  const treeScroll = await visibleTreeContainer.evaluate((node) => ({
    clientWidth: node.clientWidth,
    scrollWidth: node.scrollWidth,
  }));
  expect(treeScroll.scrollWidth).toBeGreaterThan(treeScroll.clientWidth);

  const screenshot = await page.screenshot({
    path: testInfo.outputPath("mobile-dashboard-workflow.png"),
    fullPage: false,
  });
  expectUsefulScreenshot(screenshot);
  expect(errors).toEqual([]);
});
