// src/App.jsx: the page. One request (/trend?year=...), then a KPI row, a chart and a table.
//
// Data flows one way:
//   year (state) -> useApi('/trend?year=YYYY') -> parseTrend() -> rows -> KPIs, chart, table
// The chart and the table share ONE request: App fetches, the children only draw.
import { useCallback, useMemo, useState } from 'react';
import { useApi, parseTrend } from './api/useApi.js';
import TrendChart from './components/TrendChart.jsx';
import TrendTable from './components/TrendTable.jsx';
import { Loading, Empty, ErrorState } from './components/States.jsx';

const YEARS = [2014, 2015, 2016, 2017]; // the year chips
const fmtInt = new Intl.NumberFormat('en-US');
const fmtMonth = new Intl.DateTimeFormat('en-US', { month: 'long' });

// Start on ?year=... from the address bar if it is there (handy for sharing a view), else 2016.
function initialYear() {
  const y = Number(new URLSearchParams(window.location.search).get('year'));
  return Number.isInteger(y) && y > 0 ? y : 2016;
}

export default function App() {
  const [year, setYear] = useState(initialYear);
  const [selected, setSelected] = useState(null); // the month label the reader clicked, e.g. "2016-03"

  const path = `/trend?year=${year}`;
  const { data, loading, error, ms, url, retry } = useApi(path);

  // Parse once per response, not on every render (useMemo caches it).
  const rows = useMemo(() => parseTrend(data), [data]);

  // Clicking the selected month again clears it. useCallback keeps the function
  // identity stable, so the chart does not redraw just because App re-rendered.
  const toggle = useCallback((label) => setSelected((s) => (s === label ? null : label)), []);

  // The KPI numbers, computed from the same rows the chart and table show.
  const kpi = useMemo(() => {
    const withInstalls = rows.filter((r) => r.installs != null);
    const withRate = rows.filter((r) => r.rate != null);
    const total = withInstalls.reduce((sum, r) => sum + r.installs, 0);
    const peak = withInstalls.reduce((best, r) => (best && best.installs >= r.installs ? best : r), null);
    const meanRate = withRate.length ? withRate.reduce((s, r) => s + r.rate, 0) / withRate.length : null;
    return { total, peak, meanRate };
  }, [rows]);

  function chooseYear(y) {
    setYear(y);
    setSelected(null);
  }

  // Exactly one of: loading, error, empty, or the real content.
  let body;
  if (loading && !data) body = <Loading what="the solar trend" url={url} />;
  else if (error) body = <ErrorState message={error} url={url} onRetry={retry} />;
  else if (rows.length === 0) body = <Empty hint={`The API has no months for ${year}. Pick another year above.`} />;
  else body = (
    <>
      <div className="k4">
        <div className="card kpi"><span className="l">New installs ({year})</span><span className="v">{fmtInt.format(kpi.total)}</span><span className="d">{rows.length} months</span></div>
        <div className="card kpi"><span className="l">Peak month</span><span className="v sm">{kpi.peak ? fmtMonth.format(kpi.peak.month) : '–'}</span><span className="d">{kpi.peak ? `${fmtInt.format(kpi.peak.installs)} installs` : ''}</span></div>
        <div className="card kpi"><span className="l">Mean installs / 1,000 residents</span><span className="v">{kpi.meanRate == null ? '–' : kpi.meanRate.toFixed(3)}</span><span className="d">per municipality-month</span></div>
        <div className="card kpi"><span className="l">Last fetch</span><span className="v sm">{ms == null ? '…' : `${ms} ms`}</span><span className="d"><code>GET {path}</code></span></div>
      </div>
      <div className="two">
        <div className="card stack">
          <h3>Installs by month</h3>
          <TrendChart rows={rows} selected={selected} onSelect={toggle} />
          <div className="legend"><span><i className="k-data" />installs</span><span><i className="k-accent" />selected month</span></div>
        </div>
        <div className="card stack">
          <h3>The same months as numbers</h3>
          <TrendTable rows={rows} selected={selected} onSelect={toggle} />
        </div>
      </div>
    </>
  );

  return (
    <main className="wrap">
      <div className="stack">
        <h1>Rooftop solar in 147 Japanese municipalities</h1>
        <p className="lede">New installs per month, from the course's Plumber API. Pick a year; hover or tab to a bar to read it.</p>
      </div>

      <div className="shell" role="region" aria-label="Solar trend dashboard">
        <header>
          <b>Solar installs by month</b>
          <span className="row" role="group" aria-label="Year">
            {YEARS.map((y) => (
              <button key={y} type="button" className={y === year ? 'chip on' : 'chip'}
                      aria-pressed={y === year} onClick={() => chooseYear(y)}>{y}</button>
            ))}
          </span>
        </header>
        {/* aria-busy tells screen readers the panel is updating while a new year loads */}
        <div className="body" aria-busy={loading}>{body}</div>
      </div>

      <p className="note">Data: <code>data/jp_solar.csv</code> via <code>exemplars/api-plumber</code>. Every number on this page comes from the API; nothing is typed in.</p>
    </main>
  );
}
