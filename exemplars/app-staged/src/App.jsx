// src/App.jsx: the shell. A header with the stage indicator and the request
// counter, then whichever route the hash names.
import { useEffect, useState } from 'react';
import { STAGE, STAGE_INFO, onRequests } from './data/index.js';
import { useRoute } from './router.js';
import Overview from './components/Overview.jsx';
import Detail from './components/Detail.jsx';

function useRequestCount() {
  const [n, setN] = useState(0);
  useEffect(() => onRequests(setN), []);
  return n;
}

export default function App() {
  const route = useRoute();
  const requests = useRequestCount();
  // Start the overview in the N+1 mode when the address bar says ?load=n1 (for the demo).
  const [mode, setMode] = useState(() => (new URLSearchParams(window.location.search).get('load') === 'n1' ? 'n1' : 'one'));

  return (
    <main className="wrap">
      <div className="stack">
        <h1>Rooftop solar, built in three stages</h1>
        <p className="lede">Mockup constants, then fake generated rows, then the real Plumber API. The page does not change; only the data layer does.</p>
      </div>

      <div className="shell" role="region" aria-label="Staged solar dashboard">
        <header>
          <b>{route.name === 'detail' ? `Solar installs in ${route.year}` : 'Solar installs by year'}</b>
          <span className="row">
            {/* The stage indicator: never ship while this is amber. */}
            <span className={`stage stage-${STAGE_INFO.tone}`} data-stage={STAGE} title="Set by VITE_DATA_STAGE (src/data/index.js)">{STAGE_INFO.label}</span>
            {/* The request counter: every getTrend() call in this view. */}
            <span className="reqs" aria-live="polite" data-requests={requests}>
              <b>{requests}</b> {requests === 1 ? 'request' : 'requests'}
            </span>
          </span>
        </header>
        <div className="body">
          {route.name === 'detail' ? <Detail year={route.year} /> : <Overview mode={mode} setMode={setMode} />}
        </div>
      </div>

      <p className="note">Open the browser console: every fetch logs a <code>[FETCH:{STAGE}]</code> line with method, URL, milliseconds and rows.</p>
    </main>
  );
}
