export function normalizePayloadRow<T>(
  value: T | T[] | null | undefined,
): T | null {
  if (value == null) return null;
  if (Array.isArray(value)) {
    return (value[0] as T | undefined) ?? null;
  }
  return value;
}
