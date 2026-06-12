import { describe, expect, it } from "vitest";
import { normalizePayloadRow } from "./normalizePayloadRow";

describe("normalizePayloadRow", () => {
  it("returns the first row when payload rows are serialized as arrays", () => {
    expect(normalizePayloadRow([{ mean_accuracy: 0.73 }])).toEqual({ mean_accuracy: 0.73 });
  });

  it("returns objects unchanged", () => {
    expect(normalizePayloadRow({ mean_accuracy: 0.73 })).toEqual({ mean_accuracy: 0.73 });
  });
});
