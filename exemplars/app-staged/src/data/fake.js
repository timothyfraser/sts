// src/data/fake.js: STAGE 2, fake generated rows.
//
// Rows GENERATED to the API's shape (every month, 2014-2019) so the page meets
// realistic volume, gaps and edge cases before the real API is wired: a month with
// zero installs, a month with a missing rate (R's NA arrives as null). A seeded
// random generator makes the fake rows identical on every load, so screenshots
// and bug reports are repeatable. These numbers are invented.
//
// This file must never reach a deployed build (see SOURCE_TAG and ship-check).
export const SOURCE_TAG = 'STS_FAKE_DATA';

// mulberry32: a tiny seeded random number generator (same seed -> same rows).
function seeded(seed) {
  return () => {
    seed |= 0; seed = (seed + 0x6d2b79f5) | 0;
    let t = Math.imul(seed ^ (seed >>> 15), 1 | seed);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

function generate() {
  const rand = seeded(5460);
  const rows = [];
  for (let y = 2014; y <= 2019; y++) {
    for (let m = 1; m <= 12; m++) {
      const installs = Math.round(200 + 400 * rand());
      rows.push({
        month: `${y}-${String(m).padStart(2, '0')}`,
        mean_solar_rate: m === 7 && y === 2016 ? null : Number((installs / 3000).toFixed(4)), // one NA on purpose
        total_solar: m === 1 ? 0 : installs,                                                  // January = 0 on purpose
        n_munis: 147,
      });
    }
  }
  return rows;
}
const FAKE_ROWS = generate();

export async function getTrend(year) {
  const data = FAKE_ROWS.filter((r) => !year || r.month.startsWith(String(year)));
  return { measure: `fake generated rows (${SOURCE_TAG})`, n: data.length, data };
}
