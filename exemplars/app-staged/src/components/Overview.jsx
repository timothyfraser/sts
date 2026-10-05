// src/components/Overview.jsx: route 1. One card per year, each linking to its detail page.
//
// THE N+1, SHOWN AND FIXED. Two ways to fill the same cards:
//   "One request per year" (the N+1): ask /trend?year=2014, then ?year=2015, ...
//      one request per card: 6 cards = 6 requests, and it grows with every year.
//   "One query" (the fix): ask /trend once for every month and group by year here.
//      1 request, whatever the number of cards.
// The request counter in the header shows the difference. The cards are identical.
import { useEffect } from 'react';
import { getTrend, resetRequests } from '../data/index.js';
import { parseTrend, summarizeYears } from '../data/parse.js';
import { useData } from '../useData.js';
import { Loading, Empty, ErrorState } from './States.jsx';

const YEARS = [2014, 2015, 2016, 2017, 2018, 2019];
const fmtInt = new Intl.NumberFormat('en-US');

// BAD: one request per item. Promise.all runs them in parallel, but it is still N requests.
async function loadPerYear() {
  const bodies = await Promise.all(YEARS.map((y) => getTrend(y)));
  return bodies.flatMap((b) => summarizeYears(parseTrend(b)));
}

// FIX: one query for everything, grouped on the client.
async function loadOneQuery() {
  return summarizeYears(parseTrend(await getTrend()));
}

export default function Overview({ mode, setMode }) {
  const loader = mode === 'n1' ? loadPerYear : loadOneQuery;
  useEffect(() => { resetRequests(); }, [mode]); // count requests for THIS view only
  const { data, loading, error } = useData(loader, mode);

  let body;
  if (loading && !data) body = <Loading what="the yearly summaries" url="/trend" />;
  else if (error) body = <ErrorState message={error} url="/trend" onRetry={() => setMode(mode)} />;
  else if (!data.length) body = <Empty hint="This stage has no rows. Try another stage." />;
  else body = (
    <div className="years">
      {data.map((s) => (
        <a key={s.year} className="card kpi year" href={`#/year/${s.year}`}>
          <span className="l">{s.year}</span>
          <span className="v">{fmtInt.format(s.installs)}</span>
          <span className="d">new installs over {s.months} months{s.meanRate != null ? `, mean rate ${s.meanRate.toFixed(3)}` : ''}</span>
        </a>
      ))}
    </div>
  );

  return (
    <div className="stack">
      <div className="row" role="group" aria-label="How the cards load">
        <span className="note">Load the cards with</span>
        <button type="button" className={mode === 'n1' ? 'chip on' : 'chip'} aria-pressed={mode === 'n1'} onClick={() => setMode('n1')}>one request per year (N+1)</button>
        <button type="button" className={mode === 'one' ? 'chip on' : 'chip'} aria-pressed={mode === 'one'} onClick={() => setMode('one')}>one query (fixed)</button>
      </div>
      {body}
    </div>
  );
}
