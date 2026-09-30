// src/components/TrendChart.jsx: one D3 bar chart of new solar installs per month.
//
// React and D3 share the job:
//   React owns the <svg> element and the tooltip <div> (they are in the JSX below).
//   D3 draws the bars and axes INSIDE the svg, in a useEffect, whenever the rows,
//   the width or the selected month change. useRef gives D3 a handle on the svg.
// Colours come from CSS classes in src/styles/app.css, which use the kit's tokens
// (--data for bars, --accent for the selected bar), so dark mode just works.
import { useEffect, useRef, useState } from 'react';
import * as d3 from 'd3';

const HEIGHT = 280;                                    // chart height in px; the width follows the card
const M = { top: 12, right: 8, bottom: 28, left: 48 }; // room for the axes
const fmtInt = d3.format(',');                         // 12345 -> "12,345"
const fmtMonth = d3.timeFormat('%b');                  // axis tick: "Mar"
const fmtLong = d3.timeFormat('%B %Y');                // tooltip: "March 2016"

export default function TrendChart({ rows, selected, onSelect }) {
  const wrapRef = useRef(null); // the card area: we measure its width
  const svgRef = useRef(null);  // the svg D3 draws into
  const [width, setWidth] = useState(600);
  const [tip, setTip] = useState(null); // { x, y, row } while a bar is hovered or focused

  // 1. Follow the card's width, so the chart fits a phone and a laptop.
  useEffect(() => {
    const ro = new ResizeObserver(([entry]) => setWidth(Math.max(280, entry.contentRect.width)));
    ro.observe(wrapRef.current);
    return () => ro.disconnect(); // stop watching when the chart unmounts
  }, []);

  // 2. Draw. Runs again whenever rows, width or the selection change.
  useEffect(() => {
    const svg = d3.select(svgRef.current);
    const innerW = width - M.left - M.right;
    const innerH = HEIGHT - M.top - M.bottom;

    // Scales turn data values into pixels.
    const x = d3.scaleBand().domain(rows.map((r) => r.label)).range([0, innerW]).padding(0.2);
    const y = d3.scaleLinear().domain([0, d3.max(rows, (r) => r.installs) || 1]).nice().range([innerH, 0]);

    // Three layers, made once and reused on every redraw. Reusing them (instead of
    // clearing the svg) keeps each bar the SAME element between draws, so a bar you
    // selected with the keyboard keeps its focus when the chart redraws.
    const layer = (name, dx, dy) => {
      let g = svg.select(`g.${name}`);
      if (g.empty()) g = svg.append('g').attr('class', `${name} axis`);
      return g.attr('transform', `translate(${dx},${dy})`);
    };
    const yAxis = layer('y-axis', M.left, M.top);
    const xAxis = layer('x-axis', M.left, M.top + innerH);
    const bars = layer('bars', M.left, M.top);

    // Y axis with light gridlines across the plot.
    yAxis.call(d3.axisLeft(y).ticks(5).tickFormat(fmtInt).tickSize(-innerW))
      .call((a) => a.select('.domain').remove());

    // X axis: one short month name per bar; on a phone, label every other month.
    const every = innerW / rows.length < 28 ? 2 : 1;
    xAxis.call(d3.axisBottom(x).tickSizeOuter(0)
      .tickFormat((label, i) => (i % every ? '' : fmtMonth(rows[i].month))));

    // Tooltip position: above the bar, clamped so it never pushes the page sideways on a phone.
    const show = (event, r) => {
      const box = wrapRef.current.getBoundingClientRect();
      const bar = event.currentTarget.getBoundingClientRect();
      const centre = bar.left - box.left + bar.width / 2;
      setTip({ x: Math.min(Math.max(centre, 90), box.width - 90), y: bar.top - box.top, row: r });
    };

    // The bars. join() adds, updates and removes rects to match the rows; the key
    // (r.label) tells D3 which rect belongs to which month.
    // Each bar is keyboard-focusable (tabindex 0), acts as a button, and has a spoken label.
    bars.selectAll('rect.mark')
      .data(rows, (r) => r.label)
      .join('rect')
      .attr('class', (r) => (r.label === selected ? 'mark on' : 'mark'))
      .attr('x', (r) => x(r.label))
      .attr('width', x.bandwidth())
      .attr('y', (r) => y(r.installs ?? 0))                  // ?? 0: an NA month draws as an empty bar
      .attr('height', (r) => innerH - y(r.installs ?? 0))
      .attr('rx', 2)
      .attr('tabindex', 0)
      .attr('role', 'button')
      .attr('aria-pressed', (r) => String(r.label === selected))
      .attr('aria-label', (r) => `${fmtLong(r.month)}: ${r.installs == null ? 'no data' : fmtInt(r.installs)} new installs`)
      .on('mouseenter focus', show)
      .on('mouseleave blur', () => setTip(null))
      .on('click', (event, r) => onSelect(r.label))
      .on('keydown', (event, r) => {
        if (event.key === 'Enter' || event.key === ' ') { event.preventDefault(); onSelect(r.label); }
      });
  }, [rows, width, selected, onSelect]);

  return (
    <div ref={wrapRef} className="chart">
      <svg ref={svgRef} width={width} height={HEIGHT} role="group"
           aria-label="Bar chart of new solar installs by month. Tab to a bar to read it; press Enter to select it." />
      {tip && (
        <div className="tip" role="tooltip" style={{ left: tip.x, top: tip.y }}>
          <b>{fmtLong(tip.row.month)}</b>
          <span>{tip.row.installs == null ? 'no data' : fmtInt(tip.row.installs)} installs</span>
          <span>{tip.row.rate == null ? 'no data' : tip.row.rate.toFixed(3)} per 1,000 residents</span>
        </div>
      )}
    </div>
  );
}
