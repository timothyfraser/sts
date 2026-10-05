// src/data/parse.js: turn API rows into clean rows, once (the serialization trap).
// JSON has no date type and R's NA arrives as null, so every field is converted here.
export function toNumber(v) {
  if (v === null || v === undefined || v === '') return null;
  const n = Number(v);
  return Number.isFinite(n) ? n : null;
}

// { data: [{ month: '2016-03', mean_solar_rate, total_solar, n_munis }] } -> sorted clean rows
export function parseTrend(body) {
  const rows = Array.isArray(body?.data) ? body.data : [];
  return rows
    .map((r) => {
      const m = /^(\d{4})-(\d{2})$/.exec(String(r.month ?? ''));
      return m && {
        label: r.month,
        year: Number(m[1]),
        month: Number(m[2]),
        rate: toNumber(r.mean_solar_rate),
        installs: toNumber(r.total_solar),
      };
    })
    .filter(Boolean)
    .sort((a, b) => a.label.localeCompare(b.label));
}

// Months -> one summary per year (used by the overview's one-query fix).
export function summarizeYears(rows) {
  const by = new Map();
  for (const r of rows) {
    const s = by.get(r.year) || { year: r.year, installs: 0, months: 0, rateSum: 0, rateN: 0 };
    s.months += 1;
    if (r.installs != null) s.installs += r.installs;
    if (r.rate != null) { s.rateSum += r.rate; s.rateN += 1; }
    by.set(r.year, s);
  }
  return [...by.values()]
    .map((s) => ({ ...s, meanRate: s.rateN ? s.rateSum / s.rateN : null }))
    .sort((a, b) => a.year - b.year);
}
