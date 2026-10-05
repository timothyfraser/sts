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
// The trailing-slash trim stops 'https://x/' + '/geo' becoming 'https://x//geo'.
export const API_BASE = (import.meta.env.VITE_API_URL || '/api').replace(/\/+$/, '');

// useApi('/geo') returns { data, loading, error, ms, url, retry }.
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
    // so a slow old answer can never overwrite a newer one.
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
// Parsing: the serialization and CRS traps.
//
// /geo answers a GeoJSON FeatureCollection. Each feature's properties look like
//   { "ward_precinct": "0113", "voter_turnout": 65.5 }
// ward_precinct is a STRING on purpose ("0113" as a number would lose its zero),
// and R's NA arrives as null. We check both, once, here.
//
// The CRS trap: GeoJSON coordinates must be WGS84 longitude, latitude
// (EPSG:4326). If the API forgot st_transform(), coordinates arrive in metres
// (e.g. 236000, 900000) and MapLibre silently draws nothing, or draws Boston
// in the ocean. looksLikeLonLat() catches that and parseGeo() refuses loudly.
// ---------------------------------------------------------------------------

// A number from JSON, or null for R's NA (null), a missing field, or text that is not a number.
export function toNumber(value) {
  if (value === null || value === undefined || value === '') return null;
  const n = Number(value);
  return Number.isFinite(n) ? n : null;
}

// The first coordinate pair of a (Multi)Polygon, however deeply it is nested.
function firstPosition(coords) {
  let c = coords;
  while (Array.isArray(c) && Array.isArray(c[0])) c = c[0];
  return Array.isArray(c) ? c : null;
}

// True when a position is plausibly [longitude, latitude] in degrees.
export function looksLikeLonLat(pos) {
  return Array.isArray(pos) && Math.abs(pos[0]) <= 180 && Math.abs(pos[1]) <= 90;
}

// The /geo response -> { geojson, values }. geojson is a clean copy MapLibre can
// draw (numbers cleaned, a numeric id for hover state); values are the non-null
// measures, used for the breaks. Throws a sentence for people on a bad CRS.
export function parseGeo(body) {
  const features = Array.isArray(body?.features) ? body.features : [];
  const first = features.find((f) => f?.geometry);
  if (first && !looksLikeLonLat(firstPosition(first.geometry.coordinates))) {
    throw new Error('The coordinates are not longitude/latitude. The API must st_transform(x, 4326) before writing GeoJSON.');
  }
  const clean = features
    .filter((f) => f?.geometry)
    .map((f, i) => ({
      type: 'Feature',
      id: i, // MapLibre's feature-state (the hover outline) needs a numeric id
      geometry: f.geometry,
      properties: {
        ward_precinct: String(f.properties?.ward_precinct ?? ''),
        voter_turnout: toNumber(f.properties?.voter_turnout), // percent of registered voters
      },
    }));
  const values = clean.map((f) => f.properties.voter_turnout).filter((v) => v !== null);
  return { geojson: { type: 'FeatureCollection', features: clean }, values };
}

// Quantile breaks, computed in the browser: k classes with (about) the same
// number of precincts in each. Returns the k-1 inner cut points, ascending and
// de-duplicated (tied values can collapse two classes into one).
export function quantileBreaks(values, k = 5) {
  const v = [...values].sort((a, b) => a - b);
  if (v.length === 0) return [];
  const cuts = [];
  for (let i = 1; i < k; i++) {
    const pos = (v.length - 1) * (i / k); // linear interpolation between order statistics (R's type 7)
    const lo = Math.floor(pos);
    const hi = Math.ceil(pos);
    cuts.push(v[lo] + (v[hi] - v[lo]) * (pos - lo));
  }
  return [...new Set(cuts.map((c) => Math.round(c * 10) / 10))];
}
