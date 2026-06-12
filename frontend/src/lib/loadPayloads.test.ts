import { afterEach, describe, expect, it } from "vitest";
import { loadBracketPayload, loadTechnicalPayload } from "./loadPayloads";
import bracketFixture from "../../../tests/fixtures/dashboard_payload_bracket.json";
import technicalFixture from "../../../tests/fixtures/dashboard_payload_technical.json";

declare global {
  interface Window {
    __MMBAYES_PAYLOADS__?: unknown;
  }
}

afterEach(() => {
  delete window.__MMBAYES_PAYLOADS__;
});

describe("payload loading", () => {
  it("loads the bracket payload from the injected window global", async () => {
    window.__MMBAYES_PAYLOADS__ = { bracket: bracketFixture, technical: technicalFixture };
    const payload = await loadBracketPayload();
    expect(payload.dashboard).toBe("bracket");
    expect(payload.candidates.length).toBeGreaterThan(0);
  });

  it("loads the technical payload from the injected window global", async () => {
    window.__MMBAYES_PAYLOADS__ = { bracket: bracketFixture, technical: technicalFixture };
    const payload = await loadTechnicalPayload();
    expect(payload.dashboard).toBe("technical");
  });

  it("rejects incompatible schema major versions", async () => {
    window.__MMBAYES_PAYLOADS__ = {
      bracket: { ...bracketFixture, dashboard_schema_version: "2.0.0" },
      technical: technicalFixture,
    };
    await expect(loadBracketPayload()).rejects.toThrow(/dashboard_schema_version/);
  });

  it("falls back to fetch when no window global is present", async () => {
    const originalFetch = globalThis.fetch;
    globalThis.fetch = (async (url: string) => {
      expect(String(url)).toContain("bracket_dashboard_payload.json");
      return new Response(JSON.stringify(bracketFixture), { status: 200 });
    }) as typeof fetch;
    try {
      const payload = await loadBracketPayload();
      expect(payload.dashboard).toBe("bracket");
    } finally {
      globalThis.fetch = originalFetch;
    }
  });
});
