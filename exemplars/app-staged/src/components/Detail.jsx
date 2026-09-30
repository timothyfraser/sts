// src/components/Detail.jsx: route 2. Every month of one year: a bar chart and a table,
// both drawn from ONE request (/trend?year=YYYY). The parent fetches; children only draw.
import { useEffect, useMemo } from 'react';
import { scaleLinear } from 'd3';
import { getTrend, resetRequests } from '../data/index.js';
import { parseTrend } from '../data/parse.js';
import { useData } from '../useData.js';
import { Loading, Empty, ErrorState } from './States.jsx';

const fmtInt = new Intl.NumberFormat('en-US');

function Bars({ rows }) {
  const max = Math.max(1, ...rows.map((r) => r.installs ?? 0));
  const x = scaleLinear().domain([0, max]).range([0, 100]); // installs -> % of the row width
  return (
    <div className="hbars" role="list">
      {rows.map((r) => (
        <div key={r.label} className="hbar" role="listitem" title={`${r.label}: ${r.installs ?? 'no data'} installs`}>
          <span className="lab">{r.label}</span>
          <span className="track"><i style={{ width: `${x(r.installs ?? 0)}%` }} /></span>
          <span className="num">{r.installs == null ? '–' : fmtInt.format(r.installs)}</span>
        </div>
      ))}
    </div>
  );
}

export default function Detail({ year }) {
  useEffect(() => { resetRequests(); }, [year]);
  const { data, loading, error } = useData(() => getTrend(year), year);
  const rows = useMemo(() => parseTrend(data), [data]);
  const url = `/trend?year=${year}`;

  let body;
  if (loading && !data) body = <Loading what={`the months of ${year}`} url={url} />;
  else if (error) body = <ErrorState message={error} url={url} onRetry={() => window.location.reload()} />;
  else if (!rows.length) body = <Empty hint={`This stage has no months for ${year}.`} />;
  else body = (
    <div className="two">
      <div className="card stack"><h3>Installs by month</h3><Bars rows={rows} /></div>
      <div className="card stack">
        <h3>The same months as numbers</h3>
        <table>
          <thead><tr><th>Month</th><th className="tw">Installs</th><th className="tw">Mean rate</th></tr></thead>
          <tbody>{rows.map((r) => (
            <tr key={r.label}><td>{r.label}</td><td className="tw">{r.installs == null ? '–' : fmtInt.format(r.installs)}</td><td className="tw">{r.rate == null ? 'NA' : r.rate.toFixed(4)}</td></tr>
          ))}</tbody>
        </table>
      </div>
    </div>
  );

  return (
    <div className="stack">
      <p><a href="#/">&larr; All years</a></p>
      {body}
    </div>
  );
}
