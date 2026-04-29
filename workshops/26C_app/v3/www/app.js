/* ============================================================
   app.js — 26C Network Analytics Dashboard
   Chart.js message handlers + UI interactions
   All charts receive data from Shiny via session$sendCustomMessage()
============================================================ */

'use strict';

// ---- Utility ------------------------------------------------

const charts = {};

function mkChart(id, config) {
  if (charts[id]) { charts[id].destroy(); delete charts[id]; }
  const el = document.getElementById(id);
  if (!el) return;
  charts[id] = new Chart(el, config);
}

function cssVar(name) {
  return getComputedStyle(document.documentElement).getPropertyValue(name).trim();
}

function hexToRgba(hex, alpha) {
  hex = (hex || '#000000').replace('#', '');
  if (hex.length === 3) hex = hex.split('').map(h => h + h).join('');
  const n = parseInt(hex, 16);
  return `rgba(${(n >> 16) & 255},${(n >> 8) & 255},${n & 255},${alpha})`;
}

const baseOpts = {
  plugins: { legend: { display: false } },
  scales: {
    x: { ticks: { font: { size: 10 } }, grid: { color: 'rgba(0,0,0,0.03)' } },
    y: { beginAtZero: true, ticks: { font: { size: 10 } }, grid: { color: 'rgba(0,0,0,0.04)' } }
  }
};

// ---- Tab switching ------------------------------------------

function switchTab(tabId, btn) {
  document.querySelectorAll('.tab-pane').forEach(p => p.classList.remove('active'));
  document.querySelectorAll('.tab-btn').forEach(b => b.classList.remove('active'));
  const pane = document.getElementById(tabId);
  if (pane) pane.classList.add('active');
  if (btn)  btn.classList.add('active');
  setTimeout(() => Object.values(charts).forEach(c => { try { c.resize(); } catch(e){} }), 80);
}

// ---- Geography filter pills ---------------------------------
// Writes to the hidden Shiny input so server.R can react on it

function setGeoFilter(geo, el) {
  document.querySelectorAll('#geo-filters .filter-pill')
    .forEach(p => p.classList.remove('active'));
  el.classList.add('active');

  // Update the hidden Shiny input
  const inp = document.getElementById('geoFilter');
  if (inp) {
    inp.value = geo;
    inp.dispatchEvent(new Event('change'));
    // Shiny 1.6+ also needs this:
    Shiny.setInputValue('geoFilter', geo);
  }
}

// ---- Chart.js Message Handlers ------------------------------

// TAB 1: Geography bar
Shiny.addCustomMessageHandler('updateGeoChart', function(data) {
  const sec = cssVar('--color-secondary') || '#2e6da4';
  mkChart('geoChart', {
    type: 'bar',
    data: {
      labels: data.labels,
      datasets: [{
        data: data.values,
        backgroundColor: hexToRgba(sec, 0.75),
        borderColor: sec, borderWidth: 1
      }]
    },
    options: { ...baseOpts, plugins: { legend: { display: false } } }
  });
});

// TAB 1: Committee type donut
Shiny.addCustomMessageHandler('updateTypeChart', function(data) {
  const sec = cssVar('--color-secondary') || '#2e6da4';
  const ac2 = cssVar('--color-accent2')   || '#3cb4c8';
  const acc = cssVar('--color-accent')    || '#e8a020';
  const pri = cssVar('--color-primary')   || '#1a3a5c';
  mkChart('typeChart', {
    type: 'doughnut',
    data: {
      labels: data.labels,
      datasets: [{
        data: data.values,
        backgroundColor: [sec, ac2, acc, hexToRgba(pri, 0.6)],
        borderWidth: 2, borderColor: '#fff'
      }]
    },
    options: {
      plugins: {
        legend: {
          display: true, position: 'right',
          labels: { font: { size: 10 }, boxWidth: 10 }
        }
      }
    }
  });
});

// TAB 2: Isolate degree distribution bar
Shiny.addCustomMessageHandler('updateIsolateDegChart', function(data) {
  const sec = cssVar('--color-secondary') || '#2e6da4';
  mkChart('isolateDegChart', {
    type: 'bar',
    data: {
      labels: data.labels,
      datasets: [{
        label: '# Committees',
        data: data.values,
        backgroundColor: hexToRgba(sec, 0.70),
        borderColor: sec, borderWidth: 1
      }]
    },
    options: {
      ...baseOpts,
      plugins: {
        legend: { display: false },
        title: { display: true, text: 'Degree distribution in gco', font: { size: 10 }, color: '#6b7a91' }
      },
      scales: {
        x: { title: { display: true, text: 'Degree', font: { size: 9 } }, ticks: { font: { size: 9 } } },
        y: { beginAtZero: true, ticks: { font: { size: 9 } } }
      }
    }
  });
});

// TAB 3: Degree density (smoothed line from sorted values)
Shiny.addCustomMessageHandler('updateDegDensityChart', function(data) {
  const sec = cssVar('--color-secondary') || '#2e6da4';
  const ac2 = cssVar('--color-accent2')   || '#3cb4c8';
  const degVals = (data.deg_values || []).map(Number).filter(Number.isFinite);
  const wdegVals = (data.wdeg_values || []).map(Number).filter(Number.isFinite);
  if (!degVals.length || !wdegVals.length) {
    mkChart('degDensityChart', {
      type: 'bar',
      data: {
        labels: ['No data'],
        datasets: [{ data: [0], backgroundColor: hexToRgba(sec, 0.4) }]
      },
      options: { responsive: true, maintainAspectRatio: false, plugins: { legend: { display: false } } }
    });
    return;
  }

  // Robust ECDF-style curves (avoid KDE numerical/rendering edge-cases)
  const toEcdf = (vals) => {
    const sorted = [...vals].sort((a, b) => a - b);
    const n = sorted.length;
    return sorted.map((x, i) => ({ x, y: (i + 1) / n }));
  };
  const degPts = toEcdf(degVals);
  const wdegPts = toEcdf(wdegVals);
  const labels = degPts.map(p => p.x);

  mkChart('degDensityChart', {
    type: 'line',
    data: {
      labels: labels,
      datasets: [
        {
          label: 'Degree (ECDF)',
          data: degPts.map(p => p.y),
          borderColor: sec,
          backgroundColor: hexToRgba(sec, 0.12),
          fill: true, tension: 0.4, pointRadius: 0, borderWidth: 2
        },
        {
          label: 'Weighted Degree (ECDF)',
          data: wdegPts.map(p => p.y),
          borderColor: ac2,
          backgroundColor: hexToRgba(ac2, 0.10),
          fill: true, tension: 0.4, pointRadius: 0, borderWidth: 2
        }
      ]
    },
    options: {
      responsive: true,
      maintainAspectRatio: false,
      plugins: {
        legend: { display: true, labels: { font: { size: 10 }, boxWidth: 12 } }
      },
      scales: {
        x: { title: { display: true, text: 'Degree', font: { size: 10 } }, ticks: { font: { size: 9 }, maxTicksLimit: 10 } },
        y: { min: 0, max: 1, title: { display: true, text: 'Cumulative share', font: { size: 10 } }, ticks: { font: { size: 9 } }, grid: { color: 'rgba(0,0,0,0.04)' } }
      }
    }
  });
});

// TAB 3: Distance-to-focal bar
Shiny.addCustomMessageHandler('updateDistanceChart', function(data) {
  const sec = cssVar('--color-secondary') || '#2e6da4';
  mkChart('distanceChart', {
    type: 'bar',
    data: {
      labels: data.labels,
      datasets: [{
        label: '# Committees',
        data: data.values,
        backgroundColor: hexToRgba(sec, 0.70),
        borderColor: sec, borderWidth: 1
      }]
    },
    options: {
      ...baseOpts,
      plugins: {
        legend: { display: false },
        title: { display: true, text: `Distance from ${data.focal}`, font: { size: 10 }, color: '#6b7a91' }
      },
      scales: {
        x: { ticks: { font: { size: 9 } } },
        y: { beginAtZero: true, ticks: { font: { size: 9 } } }
      }
    }
  });
});

// TAB 4: Community bar
Shiny.addCustomMessageHandler('updateCommunityBarChart', function(data) {
  const sec = cssVar('--color-secondary') || '#2e6da4';
  const ac2 = cssVar('--color-accent2')   || '#3cb4c8';
  const acc = cssVar('--color-accent')    || '#e8a020';
  const pri = cssVar('--color-primary')   || '#1a3a5c';
  const bgColors = [sec, ac2, acc, hexToRgba(pri, 0.55)];
  mkChart('communityBarChart', {
    type: 'bar',
    data: {
      labels: data.labels,
      datasets: [{
        label: 'N Committees',
        data: data.values,
        backgroundColor: data.labels.map((_, i) => bgColors[i % bgColors.length]),
        borderWidth: 1
      }]
    },
    options: { ...baseOpts }
  });
});

// TAB 5: Geo sub-graph grouped bar
Shiny.addCustomMessageHandler('updateGeoSubgraphChart', function(data) {
  const sec = cssVar('--color-secondary') || '#2e6da4';
  const acc = cssVar('--color-accent')    || '#e8a020';
  mkChart('geoSubgraphChart', {
    type: 'bar',
    data: {
      labels: data.labels,
      datasets: [
        {
          label: 'Members',
          data: data.members,
          backgroundColor: hexToRgba(sec, 0.75),
          borderColor: sec, borderWidth: 1
        },
        {
          label: 'Shared Seats (local coaff)',
          data: data.local_seats,
          backgroundColor: hexToRgba(acc, 0.65),
          borderColor: acc, borderWidth: 1
        }
      ]
    },
    options: {
      ...baseOpts,
      responsive: true,
      maintainAspectRatio: false,
      plugins: { legend: { display: true, labels: { font: { size: 10 }, boxWidth: 10 } } }
    }
  });
});

// ---- Resize charts when window changes ----------------------
$(window).on('shiny:connected', function() {
  setTimeout(() => Object.values(charts).forEach(c => { try { c.resize(); } catch(e){} }), 200);
});
$(window).on('resize', function() {
  Object.values(charts).forEach(c => { try { c.resize(); } catch(e){} });
});
