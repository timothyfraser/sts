// src/data/real.js: STAGE 3, the real API (exemplars/api-plumber, GET /trend).
//
// Development: VITE_API_URL is unset, so requests go to /api and vite.config.js
// forwards them to the Plumber API on http://localhost:8000 (no CORS needed).
// Deployed: set VITE_API_URL (see .env.example) to the deployed Plumber API's URL.
export const SOURCE_TAG = 'real';
export const API_BASE = (import.meta.env.VITE_API_URL || '/api').replace(/\/+$/, '');

export async function getTrend(year, { signal } = {}) {
  const url = `${API_BASE}/trend${year ? `?year=${year}` : ''}`;
  const res = await fetch(url, { signal, headers: { Accept: 'application/json' } });
  // fetch() only rejects when the network fails; a 404 or a 500 still "succeeds".
  if (!res.ok) throw new Error(`GET ${url} answered HTTP ${res.status}. Is the Plumber API running?`);
  return res.json();
}
