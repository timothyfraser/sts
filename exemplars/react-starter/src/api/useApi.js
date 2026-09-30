// src/api/useApi.js: the ONE place this app talks to the API.
//
// Every component that needs data calls useApi(path) instead of writing its own
// fetch(). That gives every panel the same three states (loading, error, data),
// the same error messages, and one line to change when the API moves.
import { useCallback, useEffect, useState } from 'react';

// Where requests go.
//   Development: VITE_API_URL is unset, so we use '/api' and vite.config.js
//   forwards it to the Plumber API that runme.R starts (port 8000).
//   Deployed: set VITE_API_URL (see .env.example) to the Plumber API's URL.
// The trailing-slash trim stops 'https://x/' + '/trend' becoming 'https://x//trend'.
export const API_BASE = (import.meta.env.VITE_API_URL || '/api').replace(/\/+$/, '');

// useApi('/trend?year=2016') returns { data, loading, error, ms, url, retry }.
//   data    - the parsed JSON body (or null until it arrives)
//   loading - true while a request is in flight
//   error   - a short sentence for people (or null)
//   ms      - how long the last successful request took, in milliseconds
//   url     - the exact URL requested, shown in the loading and error panels
//   retry   - call it to send the same request again (the error panel's button)
export function useApi(path) {
  const url = `${API_BASE}${path}`;
  const [state, setState] = useState({ data: null, loading: true, error: null, ms: null });
  const [attempt, setAttempt] = useState(0); // bumping this re-runs the effect below
  const retry = useCallback(() => setAttempt((n) => n + 1), []);

  useEffect(() => {
    // An AbortController lets us cancel the request. We cancel it when the
    // component unmounts, or when `url` changes before the old answer arrives,
    // so a slow answer for 2015 can never overwrite a fast answer for 2016.
    const controller = new AbortController();
    const started = performance.now();
    setState((s) => ({ ...s, loading: true, error: null }));

    fetch(url, { signal: controller.signal, headers: { Accept: 'application/json' } })
      .then(async (res) => {
        // fetch() only rejects when the network fails. A 404 or a 500 still
        // "succeeds", so we have to check res.ok ourselves.
        // In development, when Plumber is NOT running, the Vite proxy answers 500
        // for it (it could not connect), so a 5xx gets the same "is it running?" hint.
        if (!res.ok) {
          const hint = res.status >= 500 ? ' Is the Plumber API running on port 8000?' : '';
          throw new Error(`The API answered HTTP ${res.status}.${hint}`);
        }
        return res.json(); // the await matters: without it `data` would be a Promise
      })
      .then((data) => {
        setState({ data, loading: false, error: null, ms: Math.round(performance.now() - started) });
      })
      .catch((err) => {
        if (err.name === 'AbortError') return; // we cancelled it on purpose; not an error
        // "Failed to fetch" (Chrome) / "NetworkError" (Firefox) means nobody answered at all:
        // usually the Plumber API is not running, or CORS blocked the answer.
        const nobodyHome = err instanceof TypeError;
        setState({
          data: null,
          loading: false,
          ms: null,
          error: nobodyHome
            ? 'The API did not answer. Is the Plumber API running on port 8000?'
            : err.message,
        });
      });

    return () => controller.abort(); // cleanup: runs on unmount and before the next request
  }, [url, attempt]);

  return { ...state, url, retry };
}

// ---------------------------------------------------------------------------
// Parsing: the serialization trap.
//
// JSON has no date type, and R's NA arrives as null. So the API's rows look like
//   { "month": "2016-03", "mean_solar_rate": 0.1786, "total_solar": 533, "n_munis": 147 }
// where month is a STRING. Using it as a date without converting it gives an
// axis sorted like text, or "Invalid Date". We convert every field explicitly,
// once, here, so no component ever sees raw API values.
// ---------------------------------------------------------------------------

// '2016-03' -> a Date for the 1st of March 2016 (local time, so it never shifts a month in any timezone).
export function parseMonth(value) {
  const m = /^(\d{4})-(\d{2})$/.exec(String(value ?? ''));
  return m ? new Date(Number(m[1]), Number(m[2]) - 1, 1) : null;
}

// A number from JSON, or null for R's NA (null), a missing field, or text that is not a number.
export function toNumber(value) {
  if (value === null || value === undefined || value === '') return null;
  const n = Number(value);
  return Number.isFinite(n) ? n : null;
}

// The /trend response -> clean rows. Rows without a valid month are dropped.
export function parseTrend(body) {
  const rows = Array.isArray(body?.data) ? body.data : []; // guard: column-oriented JSON would not be an array
  return rows
    .map((r) => ({
      month: parseMonth(r.month),
      label: String(r.month),
      rate: toNumber(r.mean_solar_rate),  // new installs per 1,000 residents, averaged over municipalities
      installs: toNumber(r.total_solar),  // new installs, summed over municipalities
      munis: toNumber(r.n_munis),         // municipalities reporting that month
    }))
    .filter((r) => r.month !== null)
    .sort((a, b) => a.month - b.month);
}
