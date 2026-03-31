/** @type {import('@playwright/test').PlaywrightTestConfig} */
module.exports = {
  testDir: "./e2e",
  timeout: 10 * 60 * 1000,
  retries: 0,
  fullyParallel: false,
  use: {
    headless: true,
  },
};

