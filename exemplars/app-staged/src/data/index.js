// src/data/index.js: THE data-layer switch. The only module that decides the stage.
//
//   Stage 1 "mockup" -> src/data/mockup.js   hand-typed constants
//   Stage 2 "fake"   -> src/data/fake.js     generated rows, realistic volume and gaps
//   Stage 3 "real"   -> src/data/real.js     the Plumber API
//
// The stage comes from VITE_DATA_STAGE (.env.local, see .env.example); unset means
// "mockup", because every new app starts there. In `npm run dev` only, ?stage=fake
// in the address bar overrides it, so you can flip stages without a restart.
//
// Components NEVER import mockup.js / fake.js / real.js. They call getTrend() from
// here. That is what makes the switch a one-line change, and what lets the ship
// check prove no fake data is wired: in a production build, the ternary below is
// decided at build time and the unused stages are dropped from dist/ entirely.
const ENV_STAGE = import.meta.env.VITE_DATA_STAGE || 'mockup';

const loaders = import.meta.env.DEV
  ? { mockup: () => import('./mockup.js'), fake: () => import('./fake.js'), real: () => import('./real.js') }
  : import.meta.env.VITE_DATA_STAGE === 'real'
    ? { real: () => import('./real.js') }
    : import.meta.env.VITE_DATA_STAGE === 'fake'
      ? { fake: () => import('./fake.js') }
      : { mockup: () => import('./mockup.js') };

function pickStage() {
  if (import.meta.env.DEV) {
    const q = new URLSearchParams(window.location.search).get('stage');
    if (q && loaders[q]) return q;
  }
  return loaders[ENV_STAGE] ? ENV_STAGE : Object.keys(loaders)[0];
}
export const STAGE = pickStage();

// ---------------------------------------------------------------------------
// Logging (logger-instrumenter): one console line per fetch, tagged by stage,
// with method, URL, milliseconds and row count. Search the console for [FETCH:
// to see every request. Set VITE_DEBUG_FETCH=false to silence it.
// ---------------------------------------------------------------------------
const DEBUG = import.meta.env.VITE_DEBUG_FETCH !== 'false';
const flog = (...a) => { if (DEBUG) console.log(...a); };
const ferr = (...a) => { if (DEBUG) console.error(...a); };

// ---------------------------------------------------------------------------
// The request counter the page shows. Every getTrend() call counts as one
// request, in every stage, so the N+1 is visible even before the API exists.
// ---------------------------------------------------------------------------
let count = 0;
const listeners = new Set();
const emit = () => listeners.forEach((fn) => fn(count));
export function resetRequests() { count = 0; emit(); }
export function onRequests(fn) { listeners.add(fn); fn(count); return () => listeners.delete(fn); }

// getTrend(year?) -> { measure, n, data } in every stage.
export async function getTrend(year, opts = {}) {
  const source = await loaders[STAGE]();
  const url = `/trend${year ? `?year=${year}` : ''}`;
  const t0 = performance.now();
  count += 1; emit();
  try {
    const body = await source.getTrend(year, opts);
    const rows = Array.isArray(body?.data) ? body.data.length : 0;
    flog(`[FETCH:${STAGE}] GET ${url} ${Math.round(performance.now() - t0)}ms rows=${rows} (#${count})`);
    return body;
  } catch (err) {
    if (err.name !== 'AbortError') ferr(`[FETCH:${STAGE}] GET ${url} ${Math.round(performance.now() - t0)}ms ERROR ${err.message}`);
    throw err;
  }
}

// What the in-page stage indicator shows.
export const STAGE_INFO = {
  mockup: { n: 1, label: 'Stage 1: mockup constants', tone: 'warn' },
  fake: { n: 2, label: 'Stage 2: fake generated rows', tone: 'warn' },
  real: { n: 3, label: 'Stage 3: real API', tone: 'ok' },
}[STAGE];
