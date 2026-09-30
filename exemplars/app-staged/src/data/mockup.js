// src/data/mockup.js: STAGE 1, the mockup.
//
// Hand-typed constants in the SAME shape the real API returns, so the page can be
// built and reviewed before any API exists. A few months only: enough to lay out
// the cards, the chart and the table. These numbers are invented.
//
// This file must never reach a deployed build. `npm run ship-check` looks for the
// SOURCE_TAG string below in the built files and refuses if it finds it.
export const SOURCE_TAG = 'STS_MOCKUP_DATA';

const MOCKUP_MONTHS = [
  { month: '2015-01', mean_solar_rate: 0.10, total_solar: 300, n_munis: 147 },
  { month: '2015-06', mean_solar_rate: 0.20, total_solar: 600, n_munis: 147 },
  { month: '2016-01', mean_solar_rate: 0.12, total_solar: 350, n_munis: 147 },
  { month: '2016-06', mean_solar_rate: 0.22, total_solar: 650, n_munis: 147 },
];

// Same signature as real.js: getTrend(year) -> { measure, n, data: [rows] }.
export async function getTrend(year) {
  const data = MOCKUP_MONTHS.filter((r) => !year || r.month.startsWith(String(year)));
  return { measure: `mockup constants (${SOURCE_TAG})`, n: data.length, data };
}
