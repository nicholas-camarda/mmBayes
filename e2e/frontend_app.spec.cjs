const { test, expect } = require("@playwright/test");
const fs = require("node:fs");
const os = require("node:os");
const path = require("node:path");

const repoRoot = path.resolve(__dirname, "..");
const distDir = path.join(repoRoot, "frontend", "dist");
const COMPARE_WORKSPACE_NEAR_VIEWPORT_TOP = 1200;

function stageApp() {
  const stageDir = fs.mkdtempSync(path.join(os.tmpdir(), "mmbayes-frontend-e2e-"));
  fs.cpSync(distDir, stageDir, { recursive: true });
  const bracket = fs.readFileSync(
    path.join(repoRoot, "tests", "fixtures", "dashboard_payload_bracket.json"),
    "utf8",
  );
  const technical = fs.readFileSync(
    path.join(repoRoot, "tests", "fixtures", "dashboard_payload_technical.json"),
    "utf8",
  );
  const technicalPayload = JSON.parse(technical);
  technicalPayload.ensemble_diagnostics = technicalPayload.ensemble_diagnostics ?? {
    weight_stan_glm: 0.62,
    weight_bart: 0.38,
    intercept: -0.04,
    gate_passed: true,
    validation_summary: [
      {
        model: "learned_ensemble",
        mean_bracket_score: 85.4,
        mean_correct_picks: 42.1,
        mean_log_loss: 0.451,
        mean_brier: 0.192,
      },
    ],
    gate_conditions: [
      {
        condition: "Calibration guardrail",
        passed: true,
        observed: 0.041,
        threshold: 0.05,
      },
    ],
  };
  technicalPayload.championship_totals = technicalPayload.championship_totals ?? {
    candidate_summaries: [
      {
        candidate_id: 1,
        championship_matchup: "West_01_2025 vs Midwest_01_2025",
        recommended_tiebreaker_points: 143,
        predicted_total_median: 142.5,
      },
    ],
    championship_distribution: [
      { candidate_id: 1, total_points: 140, probability: 0.24 },
      { candidate_id: 1, total_points: 145, probability: 0.31 },
    ],
    scale: {
      min_total: 130,
      max_total: 155,
      max_probability: 0.31,
    },
  };
  fs.writeFileSync(
    path.join(stageDir, "dashboard_payloads.js"),
    `window.__MMBAYES_PAYLOADS__ = {"bracket": ${bracket}, "technical": ${JSON.stringify(technicalPayload)}};`,
  );
  return stageDir;
}

async function expectElementBox(locator, minimumWidth = 20, minimumHeight = 20) {
  await expect(locator).toBeVisible();
  const box = await locator.boundingBox();
  expect(box).toBeTruthy();
  expect(box.width).toBeGreaterThan(minimumWidth);
  expect(box.height).toBeGreaterThan(minimumHeight);
}

async function expectInFirstViewport(locator, maxBottom = 960) {
  await expect(locator).toBeVisible();
  const box = await locator.boundingBox();
  expect(box).toBeTruthy();
  expect(box.y + box.height).toBeLessThanOrEqual(maxBottom);
}

async function expectStartsBefore(locator, maxTop) {
  await expect(locator).toBeVisible();
  const box = await locator.boundingBox();
  expect(box).toBeTruthy();
  expect(box.y).toBeLessThanOrEqual(maxTop);
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

test.describe("static frontend dashboards", () => {
  test.skip(!fs.existsSync(distDir), "frontend/dist missing; run `npm run build` in frontend/ first");

  test("bracket page renders fixture payload offline over file://", async ({ page }) => {
    const stageDir = stageApp();
    const errors = collectBrowserErrors(page);
    await page.goto(`file://${path.join(stageDir, "index.html")}`);
    await expect(page.getByRole("heading", { name: /bracket entry workspace/i })).toBeVisible();
    await expect(page.locator(".entry-workspace")).toBeVisible();
    await expect(page.getByTestId("decision-sheet")).toBeVisible();
    expect(errors).toEqual([]);
  });

  test("technical page renders fixture payload offline over file://", async ({ page }) => {
    const stageDir = stageApp();
    await page.goto(`file://${path.join(stageDir, "technical.html")}`);
    await expect(page.getByRole("heading", { name: /technical bracket dashboard/i })).toBeVisible();
    await page.locator("details.collapsible-panel summary", { hasText: "Decision summary" }).click();
    await expect(page.getByTestId("decision-summary")).toBeVisible();
    await expect(page.getByTestId("compare-workspace")).toBeVisible();
    await expect(page.getByTestId("backtest-panel")).toBeVisible();
    await expect(page.getByTestId("calibration-chart")).toBeVisible();
    await expect(page.getByTestId("ranked-decisions-board")).toBeVisible();
    const boardBox = await page.getByTestId("ranked-decisions-board").boundingBox();
    expect(boardBox).toBeTruthy();
    expect(boardBox.height).toBeGreaterThan(280);
    await expect(page.getByTestId("live-performance-panel")).toBeVisible();
  });

  test("bracket React app covers the legacy entry and evidence workflow jobs", async ({ page }) => {
    const errors = collectBrowserErrors(page);
    await page.setViewportSize({ width: 1320, height: 960 });
    const stageDir = stageApp();
    await page.goto(`file://${path.join(stageDir, "index.html")}`, { waitUntil: "domcontentloaded" });

    await expect(page.getByRole("heading", { name: /bracket entry workspace/i })).toBeVisible();
    await expect(page.locator("#build .entry-workspace")).toBeVisible();
    await expect(page.locator("#review-queue")).toBeVisible();
    await expect(page.locator("#bracket-tree svg.btree-svg").first()).toBeVisible();
    await expect(page.locator("#evidence")).toBeVisible();
    await expect(page.locator("#paths")).toBeVisible();
    await expect(page.locator("#technical-appendix")).toBeVisible();

    await expect(page.getByText(/Candidate 2 changes/i)).toBeVisible();
    await expect(page.getByText(/Favorite probability/i).first()).toBeVisible();
    await expect(page.locator(".watchlist-card").first()).toBeVisible();

    const evidenceJump = page.locator("[data-open-evidence]").first();
    const evidenceId = await evidenceJump.getAttribute("data-open-evidence");
    expect(evidenceId).toBeTruthy();
    await evidenceJump.click();
    await expect(page.locator(`details[id="${evidenceId}"]`)).toBeVisible();
    await expect(page.getByTestId("evidence-card")).toBeVisible();
    await expect(page.getByText(/Overall strength/i).first()).toBeVisible();

    expect(errors).toEqual([]);
  });

  test("technical React app covers compare, calibration, backtest, and ensemble diagnostics", async ({
    page,
  }) => {
    const errors = collectBrowserErrors(page);
    await page.setViewportSize({ width: 1320, height: 960 });
    const stageDir = stageApp();
    await page.goto(`file://${path.join(stageDir, "technical.html")}`, { waitUntil: "domcontentloaded" });

    await expect(page.getByRole("heading", { name: /technical bracket dashboard/i })).toBeVisible();
    await expect(page.getByTestId("compare-workspace")).toBeVisible();
    await expect(page.getByTestId("ranked-decisions-board")).toBeVisible();
    await expect(page.getByTestId("candidate-differences-board")).toBeVisible();
    await expect(page.getByTestId("candidate-differences-table")).toHaveCount(1);
    await expect(page.getByTestId("live-performance-panel")).toBeVisible();
    await expect(page.getByTestId("backtest-panel")).toBeVisible();
    await expect(page.getByTestId("calibration-chart")).toBeVisible();
    await expect(page.getByTestId("ensemble-diagnostics")).toBeVisible();
    await page
      .locator("details.collapsible-panel summary", { hasText: "Championship tiebreaker comparison" })
      .click();
    await expect(page.getByTestId("championship-totals")).toBeVisible();

    expect(errors).toEqual([]);
  });

  test("synced output app renders the real checked-in payload bundle", async ({ page }) => {
    const outputAppDir = path.join(repoRoot, "output", "app");
    for (const requiredFile of ["index.html", "technical.html", "dashboard_payloads.js"]) {
      expect(fs.existsSync(path.join(outputAppDir, requiredFile))).toBeTruthy();
    }

    const errors = collectBrowserErrors(page);
    await page.goto(`file://${path.join(outputAppDir, "index.html")}`, { waitUntil: "domcontentloaded" });
    await expect(page.getByRole("heading", { name: /bracket entry workspace/i })).toBeVisible();
    await expect(page.locator("#build .entry-workspace")).toBeVisible();
    await expect(page.locator("#review-queue")).toBeVisible();

    await page.goto(`file://${path.join(outputAppDir, "technical.html")}`, { waitUntil: "domcontentloaded" });
    await expect(page.getByRole("heading", { name: /technical bracket dashboard/i })).toBeVisible();
    await expect(page.getByTestId("compare-workspace")).toBeVisible();

    expect(errors).toEqual([]);
  });

  test("payload load failures render visible errors instead of a blank app", async ({ page }) => {
    const stageDir = fs.mkdtempSync(path.join(os.tmpdir(), "mmbayes-bad-payload-"));
    fs.cpSync(distDir, stageDir, { recursive: true });
    fs.writeFileSync(
      path.join(stageDir, "dashboard_payloads.js"),
      'window.__MMBAYES_PAYLOADS__ = {"bracket":{"dashboard_schema_version":"9.0.0"},"technical":{"dashboard_schema_version":"9.0.0"}};',
    );

    await page.goto(`file://${path.join(stageDir, "index.html")}`, { waitUntil: "domcontentloaded" });
    await expect(page.getByRole("alert")).toContainText(/Failed to load dashboard payload/i);
    await expect(page.getByRole("heading", { name: /bracket dashboard/i })).toBeVisible();

    await page.goto(`file://${path.join(stageDir, "technical.html")}`, { waitUntil: "domcontentloaded" });
    await expect(page.getByRole("alert")).toContainText(/Failed to load dashboard payload/i);
    await expect(page.getByRole("heading", { name: /technical dashboard/i })).toBeVisible();
  });

  test("primary bracket controls are keyboard-operable and keep visible focus", async ({ page }) => {
    const stageDir = stageApp();
    await page.setViewportSize({ width: 1320, height: 960 });
    await page.goto(`file://${path.join(stageDir, "index.html")}`, { waitUntil: "domcontentloaded" });

    const candidateTwoToggle = page.locator(".entry-toggle[data-entry-target='candidate-2']");
    await candidateTwoToggle.focus();
    await expect(candidateTwoToggle).toBeFocused();
    await page.keyboard.press("Enter");
    await expect(page.locator("[data-entry-panel='candidate-2']")).toBeVisible();

    const firstEvidenceJump = page
      .locator(".watchlist-card:not(.is-hidden-by-bucket) .jump-button[data-open-evidence]")
      .first();
    await firstEvidenceJump.focus();
    await expect(firstEvidenceJump).toBeFocused();
    await page.keyboard.press("Enter");
    const evidenceId = await firstEvidenceJump.getAttribute("data-open-evidence");
    expect(evidenceId).toBeTruthy();
    await expect(page.locator(`details[id="${evidenceId}"]`)).toBeVisible();
  });

  test("bracket entry toggles and review markers work without console errors", async ({ page }) => {
    const errors = collectBrowserErrors(page);
    const stageDir = stageApp();
    await page.goto(`file://${path.join(stageDir, "index.html")}`, { waitUntil: "domcontentloaded" });

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

    const treeNode = page.locator("[data-btree-panel='candidate-1'] g.btree-node").first();
    await treeNode.locator("rect").hover();
    await expect(page.locator("#btree-tooltip")).toHaveClass(/is-visible/);

    const evidenceId = await treeNode.getAttribute("data-open-evidence");
    expect(evidenceId).toBeTruthy();
    await treeNode.click();
    await expect(page.locator(`details[id="${evidenceId}"]`)).toBeVisible();

    await page.locator(".btree-toggle[data-btree-target='candidate-2']").click();
    await expect(page.locator("[data-btree-panel='candidate-1']")).toBeHidden();
    await expect(page.locator("[data-btree-panel='candidate-2']")).toBeVisible();
    await expect(
      page.locator("[data-btree-panel='candidate-2'] .btree-node--route-diff").first(),
    ).toBeVisible();

    expect(errors).toEqual([]);
  });

  test("desktop workflow zones and graphical jumps stay actionable", async ({ page }, testInfo) => {
    const errors = collectBrowserErrors(page);
    await page.setViewportSize({ width: 1320, height: 960 });
    const stageDir = stageApp();
    await page.goto(`file://${path.join(stageDir, "index.html")}`, { waitUntil: "domcontentloaded" });

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
      ["build", "review-queue", "bracket-tree", "evidence", "paths", "technical-appendix"].map(
        (id) => {
          const node = document.getElementById(id);
          return node.getBoundingClientRect().top + window.scrollY;
        },
      ),
    );
    expect(zoneOrder[0]).toBeLessThan(zoneOrder[1]);
    expect(zoneOrder[1]).toBeLessThan(zoneOrder[2]);
    expect(zoneOrder[2]).toBeLessThan(zoneOrder[3]);
    expect(zoneOrder[3]).toBeLessThan(zoneOrder[4]);
    expect(zoneOrder[4]).toBeLessThan(zoneOrder[5]);

    await expectElementBox(page.locator(".candidate-comparison-panel"), 700, 80);
    await expectElementBox(page.locator(".divergence-map-panel"), 700, 180);
    await expectSvgHasVisibleContent(page.locator("svg.btree-svg").first());
    await expectElementBox(
      page.locator(".watchlist-card__probability .prob-track__lane").first(),
      100,
      28,
    );

    const divergenceCell = page.locator("[data-divergence-target-kind='evidence']").first();
    const divergenceEvidenceId = await divergenceCell.getAttribute("data-open-evidence");
    expect(divergenceEvidenceId).toBeTruthy();
    await divergenceCell.click();
    await expect(page.locator(`details[id="${divergenceEvidenceId}"]`)).toBeVisible();
    await expect(divergenceCell).toHaveAttribute("aria-pressed", "true");
    await expect(page.locator(".watchlist-card:not(.is-hidden-by-bucket)").first()).toBeVisible();

    const reviewJump = page
      .locator(".watchlist-card:not(.is-hidden-by-bucket) .jump-button[data-open-evidence]")
      .first();
    const reviewEvidenceId = await reviewJump.getAttribute("data-open-evidence");
    expect(reviewEvidenceId).toBeTruthy();
    await reviewJump.click();
    await expect(page.locator(`details[id="${reviewEvidenceId}"]`)).toBeVisible();

    const screenshot = await page.screenshot({
      path: testInfo.outputPath("desktop-dashboard-workflow.png"),
      fullPage: false,
    });
    expectUsefulScreenshot(screenshot);
    expect(errors).toEqual([]);
  });

  test("mobile bracket entry workflow stays visible before deep diagnostics", async ({ page }) => {
    await page.setViewportSize({ width: 390, height: 900 });
    const stageDir = stageApp();
    await page.goto(`file://${path.join(stageDir, "index.html")}`, { waitUntil: "domcontentloaded" });

    await expect(page.locator("h1")).toContainText("Bracket entry workspace");
    await expect(page.locator("#build")).toBeVisible();
    await expect(page.locator(".next-action-panel")).toBeVisible();
    await expect(page.locator(".entry-workspace")).toBeVisible();
    await expect(page.locator("[data-entry-panel='candidate-1']")).toBeVisible();
    await expect(page.locator(".entry-pick").first()).toBeVisible();
    await expect(page.locator(".entry-review-button").first()).toBeVisible();

    const buildTop = await page.locator("#build").evaluate((node) => node.getBoundingClientRect().top);
    const reviewTop = await page
      .locator("#review-queue")
      .evaluate((node) => node.getBoundingClientRect().top);
    expect(buildTop).toBeLessThan(reviewTop);
  });

  test("mobile navigation, review cards, evidence jumps, and screenshots are usable", async ({
    page,
  }, testInfo) => {
    const errors = collectBrowserErrors(page);
    await page.setViewportSize({ width: 390, height: 900 });
    const stageDir = stageApp();
    await page.goto(`file://${path.join(stageDir, "index.html")}`, { waitUntil: "domcontentloaded" });

    const navBox = await page.locator(".jump-nav").boundingBox();
    expect(navBox).toBeTruthy();
    expect(navBox.height).toBeLessThan(80);
    const navLabels = await page.locator(".jump-nav a").allTextContents();
    expect(navLabels).toEqual(
      expect.arrayContaining([
        "How to read",
        "Enter bracket",
        "Review picks",
        "Tree",
        "Evidence",
        "Reference",
      ]),
    );

    await expect(page.locator("h1")).toContainText("Bracket entry workspace");
    await expect(page.locator(".status-panel")).toBeVisible();
    await expect(page.locator("#build .next-action-panel")).toBeVisible();
    await expect(page.locator("#build .entry-workspace")).toBeVisible();
    await expect(page.locator("#build > .section-note")).toBeVisible();

    const entryGuidance = await page.locator("#build > .section-note").evaluate((node) => {
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
    await expectElementBox(reviewCard, 240, 160);
    await expect(reviewCard.locator(".jump-button[data-open-evidence]")).toBeVisible();
    await expect(reviewCard.locator(".watchlist-card__probability .prob-track__lane")).toBeVisible();

    const reviewJump = reviewCard.locator(".jump-button[data-open-evidence]");
    const evidenceId = await reviewJump.getAttribute("data-open-evidence");
    expect(evidenceId).toBeTruthy();
    await reviewJump.click();
    await expect(page.locator(`details[id="${evidenceId}"] .evidence-panel__lede`)).toBeVisible();
    await expect(page.getByTestId("advantage-chart")).toBeVisible();

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

  test("first desktop viewport keeps primary workflow visible", async ({ page }) => {
    const stageDir = stageApp();
    await page.setViewportSize({ width: 1320, height: 960 });

    await page.goto(`file://${path.join(stageDir, "index.html")}`, { waitUntil: "domcontentloaded" });
    await expect(page.locator("#build")).toBeVisible();
    await expectInFirstViewport(page.locator("#build > h2"));
    await expectInFirstViewport(page.locator(".next-action-panel"));

    await page.goto(`file://${path.join(stageDir, "technical.html")}`, { waitUntil: "domcontentloaded" });
    await expectInFirstViewport(page.getByTestId("technical-action-summary"));
    // Compare is a full analytical workspace, so the gate requires its start
    // near the first viewport after the action and orientation panels.
    await expectStartsBefore(
      page.getByTestId("compare-workspace"),
      COMPARE_WORKSPACE_NEAR_VIEWPORT_TOP,
    );
  });
});
