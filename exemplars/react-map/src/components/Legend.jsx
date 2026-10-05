// src/components/Legend.jsx: the key for the map's five classes.
// It reads the SAME breaks and colours the fill expression uses, so the legend
// can never disagree with the map. The class under the pointer is highlighted.
const fmt = (v) => `${v.toFixed(1)}%`;

export default function Legend({ breaks, colours, min, max, hoverValue, counts }) {
  // Class i runs from edges[i] to edges[i+1].
  const edges = [min, ...breaks, max];
  const classOf = (v) => (v == null ? -1 : breaks.filter((b) => v >= b).length);
  const active = classOf(hoverValue);
  return (
    <div className="stack">
      <h3>Voter turnout, quantile classes</h3>
      <ol className="ramp" aria-label="Legend">
        {edges.slice(0, -1).map((lo, i) => (
          <li key={i} className={i === active ? 'on' : ''}>
            <i style={{ background: colours[i] }} />
            <span>{fmt(lo)} to {fmt(edges[i + 1])}</span>
            <span className="note">{counts[i] ?? 0} precincts</span>
          </li>
        ))}
        <li>
          <i className="k-nodata" />
          <span>No data</span>
        </li>
      </ol>
      <p className="note">Quantile breaks put about the same number of precincts in each class; the ranges are computed in your browser from the /geo response.</p>
    </div>
  );
}
