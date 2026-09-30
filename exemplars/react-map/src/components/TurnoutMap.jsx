// src/components/TurnoutMap.jsx: the map. One MapLibre map, one GeoJSON source, two layers.
//
//   geojson (prop) -> source "precincts" -> layer "precinct-fill" (colour by class)
//                                        -> layer "precinct-line" (outline; thick on hover)
//
// React owns the <div>; MapLibre owns everything inside it. So we create the map
// ONCE (useEffect with [] deps) and afterwards only *update* it: setData() for new
// features, setPaintProperty() for new breaks. Re-creating the map on every
// render is the most common AI-written bug here: it flickers and leaks WebGL contexts.
import { useEffect, useRef, useState } from 'react';
import maplibregl from 'maplibre-gl';

// Read a colour token from the page's CSS (tokens.css / app.css), so the map
// follows the kit and the dark theme. MapLibre needs real colour strings, not var(--x).
function token(name) {
  return getComputedStyle(document.documentElement).getPropertyValue(name).trim();
}
export const rampColours = () => [1, 2, 3, 4, 5].map((i) => token(`--ramp-${i}`));

// The data-driven fill: a "step" expression. Below breaks[0] -> colour 0,
// between breaks[0] and breaks[1] -> colour 1, and so on. Missing turnout -> the no-data grey.
//   ['step', input, colour0, break1, colour1, break2, colour2, ...]
export function fillExpression(breaks, colours) {
  const step = ['step', ['get', 'voter_turnout'], colours[0]];
  breaks.forEach((b, i) => step.push(b, colours[i + 1]));
  return ['case', ['==', ['get', 'voter_turnout'], null], token('--nodata'), step];
}

// A basemap-free style: one background layer and nothing else. No tiles, no
// glyphs, no API key, no third-party requests. The precincts ARE the map.
function blankStyle() {
  return { version: 8, sources: {}, layers: [{ id: 'bg', type: 'background', paint: { 'background-color': token('--bg') } }] };
}

// The bounding box of a FeatureCollection, as [[west, south], [east, north]] for fitBounds().
function bbox(geojson) {
  let w = 180, s = 90, e = -180, n = -90;
  const visit = (c) => {
    if (!Array.isArray(c) || c.length === 0) return;             // an empty ring (simplify can leave one)
    if (typeof c[0] === 'number') { w = Math.min(w, c[0]); e = Math.max(e, c[0]); s = Math.min(s, c[1]); n = Math.max(n, c[1]); }
    else c.forEach(visit);
  };
  geojson.features.forEach((f) => visit(f.geometry.coordinates));
  return [[w, s], [e, n]];
}

export default function TurnoutMap({ geojson, breaks, onHover }) {
  const box = useRef(null);      // the <div> MapLibre draws into
  const mapRef = useRef(null);   // the maplibregl.Map, once created
  const [ready, setReady] = useState(false);
  const [tip, setTip] = useState(null); // { x, y, id, value } for the tooltip

  // 1. Create the map once; remove it when the component unmounts.
  useEffect(() => {
    const map = new maplibregl.Map({
      container: box.current,
      style: blankStyle(),
      center: [-71.06, 42.32], zoom: 10.4,   // Boston, in [longitude, latitude] order (not lat, lng!)
      attributionControl: false,
      dragRotate: false, pitchWithRotate: false,
    });
    map.addControl(new maplibregl.NavigationControl({ showCompass: false }), 'top-right');
    map.on('load', () => {
      map.addSource('precincts', { type: 'geojson', data: { type: 'FeatureCollection', features: [] } });
      map.addLayer({ id: 'precinct-fill', type: 'fill', source: 'precincts',
        paint: { 'fill-color': token('--nodata'), 'fill-opacity': 0.9 } });
      map.addLayer({ id: 'precinct-line', type: 'line', source: 'precincts',
        paint: {
          'line-color': ['case', ['boolean', ['feature-state', 'hover'], false], token('--fg'), token('--surface')],
          'line-width': ['case', ['boolean', ['feature-state', 'hover'], false], 2.5, 0.6],
        } });
      setReady(true);
    });

    // 2. Hover: outline the precinct under the pointer and show a tooltip.
    let hovered = null;
    const clear = () => {
      if (hovered !== null) map.setFeatureState({ source: 'precincts', id: hovered }, { hover: false });
      hovered = null; setTip(null); onHover?.(null); map.getCanvas().style.cursor = '';
    };
    map.on('mousemove', 'precinct-fill', (e) => {
      const f = e.features?.[0];
      if (!f) return;
      if (hovered !== f.id) {
        if (hovered !== null) map.setFeatureState({ source: 'precincts', id: hovered }, { hover: false });
        hovered = f.id;
        map.setFeatureState({ source: 'precincts', id: hovered }, { hover: true });
      }
      map.getCanvas().style.cursor = 'pointer';
      const value = f.properties.voter_turnout ?? null; // MapLibre drops null properties
      setTip({ x: e.point.x, y: e.point.y, id: f.properties.ward_precinct, value });
      onHover?.(value);
    });
    map.on('mouseleave', 'precinct-fill', clear);

    mapRef.current = map;
    return () => { map.remove(); mapRef.current = null; };
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  // 3. New features -> setData() and zoom to them. (Never re-create the map.)
  useEffect(() => {
    const map = mapRef.current;
    if (!ready || !map || !geojson) return;
    map.getSource('precincts').setData(geojson);
    if (geojson.features.length) map.fitBounds(bbox(geojson), { padding: 16, duration: 0 });
  }, [ready, geojson]);

  // 4. New breaks -> a new fill expression.
  useEffect(() => {
    const map = mapRef.current;
    if (!ready || !map) return;
    map.setPaintProperty('precinct-fill', 'fill-color', fillExpression(breaks, rampColours()));
  }, [ready, breaks]);

  const ward = tip ? tip.id.slice(0, 2) : '';
  const pct = tip ? tip.id.slice(2) : '';
  return (
    <div className="map" role="img" aria-label="Map of Boston precincts shaded by voter turnout">
      <div ref={box} className="map-canvas" />
      {tip && (
        <div className="tip" style={{ left: tip.x, top: tip.y }}>
          <b>Ward {ward}, precinct {pct}</b>
          <span>{tip.value == null ? 'No turnout data' : `${Number(tip.value).toFixed(1)}% turnout`}</span>
        </div>
      )}
    </div>
  );
}
