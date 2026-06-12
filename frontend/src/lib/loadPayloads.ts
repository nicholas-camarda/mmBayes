import {
  assertSchemaCompatible,
  type BracketPayload,
  type DashboardPayloads,
  type TechnicalPayload,
} from "../types/payload";

function injectedPayloads(): Partial<DashboardPayloads> | undefined {
  return (window as { __MMBAYES_PAYLOADS__?: Partial<DashboardPayloads> }).__MMBAYES_PAYLOADS__;
}

async function fetchPayload<T>(fileName: string): Promise<T> {
  const response = await fetch(`./${fileName}`);
  if (!response.ok) {
    throw new Error(`Failed to load ${fileName}: HTTP ${response.status}`);
  }
  return (await response.json()) as T;
}

export async function loadBracketPayload(): Promise<BracketPayload> {
  const payload =
    injectedPayloads()?.bracket ??
    (await fetchPayload<BracketPayload>("bracket_dashboard_payload.json"));
  assertSchemaCompatible(payload as BracketPayload);
  return payload as BracketPayload;
}

export async function loadTechnicalPayload(): Promise<TechnicalPayload> {
  const payload =
    injectedPayloads()?.technical ??
    (await fetchPayload<TechnicalPayload>("technical_dashboard_payload.json"));
  assertSchemaCompatible(payload as TechnicalPayload);
  return payload as TechnicalPayload;
}
