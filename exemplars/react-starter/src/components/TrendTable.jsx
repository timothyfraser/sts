// src/components/TrendTable.jsx: the same rows as the chart, as a table.
// A table is the accessible twin of a chart: exact numbers, readable by a screen reader.
// Number columns are right-aligned (class "n") so the digits line up (kit rule 4).
const fmtInt = new Intl.NumberFormat('en-US');
const fmtMonth = new Intl.DateTimeFormat('en-US', { month: 'short', year: 'numeric' });

export default function TrendTable({ rows, selected, onSelect }) {
  return (
    <div className="tw">{/* a wide table scrolls inside its card, never the whole page */}
      <table>
        <caption className="note">New rooftop solar installs per month, summed over municipalities</caption>
        <thead>
          <tr>
            <th scope="col">Month</th>
            <th scope="col" className="n">Installs</th>
            <th scope="col" className="n">Per 1,000</th>
            <th scope="col" className="n">Munis</th>
          </tr>
        </thead>
        <tbody>
          {rows.map((r) => (
            // key: React needs a stable id per row to update the table efficiently.
            <tr key={r.label} className={r.label === selected ? 'on' : undefined}
                onClick={() => onSelect(r.label)}>
              <td>{fmtMonth.format(r.month)}</td>
              {/* R's NA arrives as null: show a dash, never the word "null" */}
              <td className="n">{r.installs == null ? '–' : fmtInt.format(r.installs)}</td>
              <td className="n">{r.rate == null ? '–' : r.rate.toFixed(3)}</td>
              <td className="n">{r.munis ?? '–'}</td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}
