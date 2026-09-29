/* lab.js - the lab kit. Vanilla JS; D3 v7 is a page <script>; motion.dev is imported lazily and guarded.
   Global: Lab. Lab.create(...) is the only thing that sets window.__lab. */
(function () {
  'use strict';
  var MOTION_URL = 'https://cdn.jsdelivr.net/npm/motion@11.18.2/+esm';
  var reduced = function () { return !!(window.matchMedia && matchMedia('(prefers-reduced-motion: reduce)').matches); };
  var motionP = null;
  function loadMotion() {
    if (reduced()) return Promise.resolve(null);
    // A CDN that fails or hangs must never block a render: give up after 3 s and jump to end states.
    if (!motionP) motionP = Promise.race([
      import(MOTION_URL).catch(function () { return null; }),
      new Promise(function (r) { setTimeout(function () { r(null); }, 3000); })
    ]);
    return motionP;
  }
  // Every helper resolves when the element is in its end state.
  var motion = {
    reduced: reduced,
    load: loadMotion,
    animate: function (el, keyframes, opts) {
      return loadMotion().then(function (m) {
        var end = {}; Object.keys(keyframes).forEach(function (k) { var v = keyframes[k]; end[k] = Array.isArray(v) ? v[v.length - 1] : v; });
        if (!m || !el) { if (el) Object.assign(el.style, end); return; }
        return m.animate(el, keyframes, Object.assign({ duration: 0.28, ease: [0.22, 1, 0.36, 1] }, opts || {})).finished;
      });
    },
    // FLIP: measure, mutate, invert, play.
    flip: function (els, mutate) {
      var list = Array.prototype.slice.call(els || []);
      var first = new Map(list.map(function (e) { return [e, e.getBoundingClientRect()]; }));
      mutate();
      if (reduced()) return Promise.resolve();
      return loadMotion().then(function (m) {
        if (!m) return;
        return Promise.all(list.filter(function (e) { return e.isConnected; }).map(function (e) {
          var a = first.get(e), b = e.getBoundingClientRect(); var dy = a.top - b.top;
          if (!dy) return null;
          return m.animate(e, { transform: ['translateY(' + dy + 'px)', 'translateY(0)'] }, { duration: 0.35, ease: [0.22, 1, 0.36, 1] }).finished;
        }));
      });
    }
  };

  function fmt(v, f) {
    if (typeof f === 'function') return f(v);
    if (typeof v !== 'number') return String(v);
    if (!isFinite(v)) return '∞';
    return v.toLocaleString('en-US', { maximumFractionDigits: f == null ? 2 : f, minimumFractionDigits: f == null ? 0 : f });
  }

  // Sidebar metrics: spec = [{key,label,fmt?}] ; returns {update(values), values()}
  function metrics(el, spec) {
    var dl = document.createElement('dl'); dl.className = 'lab-metrics'; el.appendChild(dl);
    var rows = {}; var base = null; var cur = {};
    spec.forEach(function (s) {
      var d = document.createElement('div'); d.className = 'lab-metric'; d.dataset.key = s.key;
      d.innerHTML = '<dt></dt><dd><span class="v">—</span><span class="lab-delta" aria-hidden="true"></span></dd>';
      d.querySelector('dt').textContent = s.label; dl.appendChild(d); rows[s.key] = { el: d, s: s };
    });
    return {
      update: function (vals) {
        if (!base) base = Object.assign({}, vals);
        Object.keys(rows).forEach(function (k) {
          var r = rows[k], v = vals[k], b = base[k];
          var prev = cur[k]; cur[k] = v;
          r.el.querySelector('.v').textContent = fmt(v, r.s.fmt);
          var de = r.el.querySelector('.lab-delta');
          var changed = typeof v === 'number' && typeof b === 'number' && v !== b;
          r.el.classList.toggle('is-changed', changed);
          de.className = 'lab-delta' + (changed ? (v > b ? ' up' : ' down') : '');
          de.textContent = changed ? (v > b ? '+' : '−') + fmt(Math.abs(v - b), r.s.fmt) : '';
          if (prev !== undefined && prev !== v && !reduced()) { r.el.classList.remove('flash'); void r.el.offsetWidth; r.el.classList.add('flash'); }
        });
      },
      values: function () { return Object.assign({}, cur); },
      rebase: function () { base = null; }
    };
  }

  function status(el) {
    el.classList.add('lab-status'); el.setAttribute('role', 'status'); el.setAttribute('aria-live', 'polite');
    el.innerHTML = '<span><span class="dot" aria-hidden="true"></span><span class="msg">loading data</span></span><span class="extra"></span>';
    el.dataset.state = 'busy';
    return {
      set: function (state, msg, extra) { el.dataset.state = state; if (msg != null) el.querySelector('.msg').textContent = msg; if (extra != null) el.querySelector('.extra').textContent = extra; }
    };
  }

  // Option order: a fixed per-lab, per-LC shuffle (seed = body[data-lab] + ':' + data-lc), so the
  // position of the correct option carries no signal and authors need not vary it. Deterministic
  // (FNV-1a hash -> mulberry32 -> Fisher-Yates): same order on every load, stable screenshots.
  // No-op on any surprise: options only ever move within their own parent, never get dropped.
  function shuffleOptions(sec) {
    try {
      var opts = Array.prototype.slice.call(sec.querySelectorAll('.lc-option'));
      var parent = opts.length > 1 ? opts[0].parentNode : null;
      if (!parent || !opts.every(function (o) { return o.parentNode === parent; })) return;
      var s = ((document.body && document.body.dataset.lab) || '') + ':' + (sec.dataset.lc || ''), h = 2166136261 >>> 0;
      for (var i = 0; i < s.length; i++) { h ^= s.charCodeAt(i); h = Math.imul(h, 16777619) >>> 0; }
      var a = h, rnd = function () { a = a + 0x6D2B79F5 | 0; var t = Math.imul(a ^ a >>> 15, 1 | a); t = t + Math.imul(t ^ t >>> 7, 61 | t) ^ t; return ((t ^ t >>> 14) >>> 0) / 4294967296; };
      var idx = opts.map(function (_, k) { return k; });
      for (var j = idx.length - 1; j > 0; j--) { var r = Math.floor(rnd() * (j + 1)), x = idx[j]; idx[j] = idx[r]; idx[r] = x; }
      var frag = document.createDocumentFragment();
      idx.forEach(function (k) { frag.appendChild(opts[k]); });
      parent.appendChild(frag);
    } catch (e) { /* leave the authored order */ }
  }

  // Learning checks on the contract DOM.
  function lcs(root) {
    (root || document).querySelectorAll('.lab-lc[data-lc]').forEach(function (sec) {
      if (sec.dataset.wired) return; sec.dataset.wired = '1';
      shuffleOptions(sec);
      var opts = sec.querySelectorAll('.lc-option');
      opts.forEach(function (b) {
        b.setAttribute('aria-pressed', 'false');
        b.addEventListener('click', function () {
          opts.forEach(function (o) { o.classList.remove('is-chosen', 'is-correct', 'is-wrong'); o.setAttribute('aria-pressed', 'false'); });
          b.classList.add('is-chosen', b.dataset.correct === 'true' ? 'is-correct' : 'is-wrong'); b.setAttribute('aria-pressed', 'true');
        });
      });
      [['.lc-hint-btn', '.lc-hint'], ['.lc-reveal-btn', '.lc-answer']].forEach(function (p) {
        var btn = sec.querySelector(p[0]), pane = sec.querySelector(p[1]); if (!btn || !pane) return;
        btn.addEventListener('click', function () {
          var open = pane.hidden; pane.hidden = !open; btn.setAttribute('aria-expanded', String(open));
          if (open && p[1] === '.lc-answer') sec.querySelectorAll('.lc-option[data-correct=true]').forEach(function (o) { o.classList.add('is-correct'); });
        });
      });
    });
  }

  // Without D3 a component renders nothing and every call rejects; create() routes that to the status bar.
  function noD3(el) {
    var no = function () { return Promise.reject(new Error('D3 did not load (check the network)')); };
    if (el) el.setAttribute('data-lab-empty', '');
    return { update: no, render: no, ready: Promise.resolve(), el: el };
  }

  // Code panel: templates are functions of state -> string.
  function codePanel(el, tpl) {
    el.classList.add('lab-code'); el.setAttribute('data-lab-code', '');
    var langs = Object.keys(tpl).filter(function (k) { return typeof tpl[k] === 'function'; });
    var uid = (el.id || 'lab-code') + '-';
    var bar = '<div class="lab-code-bar"><div class="lab-code-tabs" role="tablist" aria-label="Code for the current state">' + langs.map(function (l, i) {
      return '<button role="tab" id="' + uid + 't-' + l + '" aria-controls="' + uid + 'p-' + l + '" aria-selected="' + (i === 0) + '" tabindex="' + (i === 0 ? 0 : -1) + '" data-code-tab="' + l + '">' + l.toUpperCase() + '</button>';
    }).join('') + '</div><button type="button" data-code-copy>Copy</button></div>';
    el.innerHTML = bar + langs.map(function (l, i) {
      return '<pre role="tabpanel" tabindex="0" id="' + uid + 'p-' + l + '" aria-labelledby="' + uid + 't-' + l + '" data-code-pane="' + l + '"' + (i ? ' hidden' : '') + '><code></code></pre>';
    }).join('');
    var active = langs[0], text = {};
    function select(l) {
      active = l;
      el.querySelectorAll('[data-code-tab]').forEach(function (b) { var on = b.dataset.codeTab === l; b.setAttribute('aria-selected', on); b.tabIndex = on ? 0 : -1; });
      el.querySelectorAll('[data-code-pane]').forEach(function (p) { p.hidden = p.dataset.codePane !== l; });
    }
    el.querySelectorAll('[data-code-tab]').forEach(function (b) {
      b.addEventListener('click', function () { select(b.dataset.codeTab); });
      b.addEventListener('keydown', function (e) {
        if (e.key !== 'ArrowRight' && e.key !== 'ArrowLeft') return;
        var i = (langs.indexOf(active) + (e.key === 'ArrowRight' ? 1 : langs.length - 1)) % langs.length;
        select(langs[i]); el.querySelector('[data-code-tab="' + langs[i] + '"]').focus();
      });
    });
    var copy = el.querySelector('[data-code-copy]');
    copy.addEventListener('click', function () {
      var t = text[active] || '';
      var done = function () { copy.textContent = 'Copied'; setTimeout(function () { copy.textContent = 'Copy'; }, 1200); };
      if (navigator.clipboard) navigator.clipboard.writeText(t).then(done, done); else done();
    });
    return {
      update: function (state) { langs.forEach(function (l) { text[l] = tpl[l](state); el.querySelector('[data-code-pane="' + l + '"] code').textContent = text[l]; }); },
      text: function () { return Object.assign({}, text); }
    };
  }

  // Linked table: keyed rows; update(rows, {hide:[cols]}) -> Promise (FLIP moves, fade-out removals).
  function table(el, rows, cols, opts) {
    if (!window.d3) return noD3(el);
    opts = opts || {};
    var key = opts.key || function (r) { return r.id; };
    el.classList.add('lab-table-wrap'); el.tabIndex = 0; el.setAttribute('role', 'region'); el.setAttribute('aria-label', opts.label || 'Data table');
    var t = document.createElement('table'); t.className = 'lab-table';
    if (opts.caption) { var c = t.createCaption(); c.textContent = opts.caption; c.className = 'lab-sr'; }
    t.innerHTML += '<thead><tr>' + cols.map(function (c) { return '<th scope="col" data-col="' + c.key + '"' + (c.num ? ' class="num"' : '') + '>' + c.label + '</th>'; }).join('') + '</tr></thead><tbody></tbody>';
    el.appendChild(t);
    var tb = t.tBodies[0], byKey = new Map();
    function rowEl(r) {
      var tr = byKey.get(key(r));
      if (!tr) { tr = document.createElement('tr'); tr.dataset.key = key(r); byKey.set(key(r), tr); }
      tr.innerHTML = cols.map(function (c) { var v = r[c.key]; return '<td data-col="' + c.key + '"' + (c.num ? ' class="num"' : '') + '>' + (v == null ? '' : fmt(v, c.fmt)) + '</td>'; }).join('');
      return tr;
    }
    function update(next, o) {
      o = o || {}; var hide = new Set(o.hide || []);
      var keep = new Set(next.map(key));
      var gone = []; byKey.forEach(function (tr, k) { if (!keep.has(k) && tr.isConnected) gone.push(tr); });
      var fade = reduced() ? Promise.resolve() : Promise.all(gone.map(function (tr) { return motion.animate(tr, { opacity: [1, 0] }, { duration: 0.2 }); }));
      return fade.then(function () {
        gone.forEach(function (tr) { tr.remove(); byKey.delete(tr.dataset.key); });
        var live = Array.prototype.slice.call(tb.rows);
        return motion.flip(live, function () {
          next.forEach(function (r) { var tr = rowEl(r); tr.style.opacity = ''; tb.appendChild(tr); });
          t.querySelectorAll('[data-col]').forEach(function (cell) { cell.classList.toggle('is-collapsed', hide.has(cell.dataset.col)); });
        });
      });
    }
    var ready = update(rows);
    return { update: update, ready: ready, el: t };
  }

  // Map: d3-geo. geojson FeatureCollection; opts {points:[{lon,lat,id}], highlight:fn(feature)->bool, label}
  function map(el, geojson, opts) {
    if (!window.d3) return noD3(el);
    opts = opts || {};
    var d3 = window.d3; if (!d3) throw new Error('Lab.map needs d3 v7 loaded first');
    var W = opts.width || 640, H = opts.height || 400;
    var svg = d3.select(el).append('svg').attr('class', 'lab-map').attr('viewBox', '0 0 ' + W + ' ' + H)
      .attr('role', 'img').attr('aria-label', opts.label || 'Map');
    var proj = d3.geoMercator().fitSize([W, H], geojson), path = d3.geoPath(proj);
    var gP = svg.append('g'), gT = svg.append('g');
    function render(o) {
      o = o || {}; var hl = o.highlight || opts.highlight || function () { return false; };
      gP.selectAll('path.poly').data(geojson.features, function (f, i) { return (f.properties && f.properties.id) || i; })
        .join('path').attr('class', function (f) { return 'poly' + (hl(f) ? ' is-hl' : ''); }).attr('d', path);
      var pts = o.points || opts.points || [];
      var sel = gT.selectAll('circle.pt').data(pts, function (p) { return p.id; });
      var dur = reduced() ? 0 : 280;
      sel.exit().transition().duration(dur * 0.65).attr('r', 0).remove();
      var all = sel.enter().append('circle').attr('class', 'pt').attr('r', 0)
        .attr('cx', function (p) { return proj([p.lon, p.lat])[0]; }).attr('cy', function (p) { return proj([p.lon, p.lat])[1]; })
        .merge(sel);
      var tr = all.transition().duration(dur).attr('r', function (p) { return p.r || opts.pointR || Math.max(4, W / 110); })
        .attr('cx', function (p) { return proj([p.lon, p.lat])[0]; }).attr('cy', function (p) { return proj([p.lon, p.lat])[1]; });
      return dur ? tr.end().catch(function () {}) : Promise.resolve();
    }
    return { render: render, projection: proj, svg: svg };
  }

  // create: wires window.__lab. cfg = {id, states, initial, load?, render(state,data)->Promise|void, readouts(state,data)->obj, code:{r,sql?}, metrics?:{el,spec}, status?:el, codeEl}
  function create(cfg) {
    var data = null, state = Object.assign({}, cfg.initial || (cfg.states && cfg.states.baseline) || {});
    var m = cfg.metrics ? metrics(cfg.metrics.el, cfg.metrics.spec) : null;
    var st = cfg.status ? status(cfg.status) : null;
    var cp = cfg.codeEl ? codePanel(cfg.codeEl, cfg.code) : null;
    var reads = {};
    lcs(document);
    function apply() {
      if (st) st.set('busy', 'applying');
      return Promise.resolve(cfg.render(state, data)).then(function () {
        reads = cfg.readouts ? cfg.readouts(state, data) : {};
        if (m) m.update(reads);
        if (cp) cp.update(state);
        if (st) st.set('ready', 'ready', cfg.statusText ? cfg.statusText(state, reads) : '');
        api.error = null;
      }).catch(fail);
    }
    // Failures (no D3, data 404, render throw) go to the status bar; ready and setState still resolve,
    // with api.error set, so a harness never hangs and the visual keeps its last good render.
    function fail(e) {
      var msg = (e && e.message) || String(e);
      api.error = msg; if (st) st.set('error', 'error: ' + msg);
      return { error: msg };
    }
    var ready = Promise.resolve().then(function () {
      if (cfg.needsD3 !== false && typeof window.d3 === 'undefined') throw new Error('D3 did not load (check the network)');
      return cfg.load ? cfg.load() : null;
    }).then(function (d) { data = d; return apply(); }, fail)
      .then(function (r) { performance.mark('lab:first-render'); return r; });
    var api = {
      id: cfg.id, error: null, ready: ready, states: cfg.states || {},
      setState: function (obj) { return ready.then(function () { state = Object.assign({}, state, obj || {}); if (cfg.onState) cfg.onState(state); return apply(); }); },
      getState: function () { return JSON.parse(JSON.stringify(state)); },
      readouts: function () { return Object.assign({}, reads); },
      code: function () { return cp ? cp.text() : {}; }
    };
    window.__lab = api;
    return api;
  }

  window.Lab = { create: create, metrics: metrics, status: status, lcs: lcs, codePanel: codePanel, table: table, map: map, motion: motion, fmt: fmt };
})();
