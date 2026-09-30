// src/App.jsx: the page. One request (/geo), then a KPI row, a map and a legend.
//
// Data flows one way:
//   useApi('/geo') -> parseGeo() -> { geojson, values } -> quantileBreaks() -> map + legend + KPIs
// The map and the legend share ONE response and ONE set of breaks: App computes, the children draw.
import { useMemo, useState } from 'react';
import { useApi, parseGeo, quantileBreaks } from './api/useApi.js';
import TurnoutMap, { rampColours } from './components/TurnoutMap.jsx';
import Legend from './components/Legend.jsx';
import { Loading, Empty, ErrorState } from './components/States.jsx';

const CLASSES = 5; // five colours in the ramp (app.css --ramp-1 .. --ramp-5)

export default function App() {
  const path = '/geo';
  const { data, loading, error, ms, url, retry } = useApi(path);
  const [hoverValue, setHoverValue] = useState(null);

  // Parse once per response. A bad CRS throws here and becomes the error panel.
  const parsed = useMemo(() => {
    if (!data) return { geojson: null, values: [], problem: null };
    try { return { ...parseGeo(data), problem: null }; }
    catch (e) { return { geojson: null, values: [], problem: e.message }; }
  }, [data]);

  const breaks = useMemo(() => quantileBreaks(parsed.values, CLASSES), [parsed.values]);
  const stats = useMemo(() => {
    const v = parsed.values;
    if (!v.length) return null;
    const counts = Array(breaks.length + 1).fill(0);
    v.forEach((x) => { counts[breaks.filter((b) => x >= b).length] += 1; });
    return { min: Math.min(...v), max: Math.max(...v), median: [...v].sort((a, b) => a - b)[Math.floor(v.length / 2)], counts };
  }, [parsed.values, breaks]);
  // Colours are read from CSS once the page has styles; recompute with the breaks.
  const colours = useMemo(() => rampColours(), [breaks]);

  const features = parsed.geojson?.features.length ?? 0;

  // Exactly one of: loading, error, empty, or the real content.
  let body;
  if (loading && !data) body = <Loading what="the precinct shapes" url={url} />;
  else if (error || parsed.problem) body = <ErrorState message={error || parsed.problem} url={url} onRetry={retry} />;
  else if (features === 0 || !stats) body = <Empty hint="The API answered, but with no precincts. Check data/boston_voting/ and the /geo route." />;
  else body = (
    <>
      <div className="k4">
        <div className="card kpi"><span className="l">Precincts</span><span className="v">{features}</span><span className="d">{parsed.values.length} with turnout</span></div>
        <div className="card kpi"><span className="l">Median turnout</span><span className="v">{stats.median.toFixed(1)}%</span><span className="d">range {stats.min.toFixed(1)} to {stats.max.toFixed(1)}%</span></div>
        <div className="card kpi"><span className="l">Last fetch</span><span className="v sm">{ms == null ? '…' : `${ms} ms`}</span><span className="d"><code>GET {path}</code></span></div>
      </div>
      <div className="two">
        <div className="card stack">
          <h3>Turnout by precinct</h3>
          <TurnoutMap geojson={parsed.geojson} breaks={breaks} onHover={setHoverValue} />
          <p className="note">Hover a precinct to read it. Scroll or pinch to zoom.</p>
        </div>
        <div className="card">
          <Legend breaks={breaks} colours={colours} min={stats.min} max={stats.max} hoverValue={hoverValue} counts={stats.counts} />
        </div>
      </div>
    </>
  );

  return (
    <main className="wrap">
      <div className="stack">
        <h1>Voter turnout in Boston's precincts</h1>
        <p className="lede">Precinct shapes and turnout from the course's Plumber API, drawn with MapLibre GL. Each shade is one quantile class; the legend says which.</p>
      </div>

      <div className="shell" role="region" aria-label="Turnout map">
        <header>
          <b>2020 turnout, percent of registered voters</b>
          <span className="note">{CLASSES} quantile classes</span>
        </header>
        <div className="body" aria-busy={loading}>{body}</div>
      </div>

      <p className="note">Data: <code>data/boston_voting/</code> via <code>exemplars/api-plumber</code> <code>/geo</code> (WGS84 GeoJSON). No basemap, no map API key.</p>
    </main>
  );
}
