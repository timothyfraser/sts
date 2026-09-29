/* site-text.js — STAND-IN for the full site-nav runtime until it is ported.
   Fills [data-site-text="a.b"] and [data-site-href="a.b"] from
   window.COURSE_CONTRACT.site (assets/contract.js, generated from
   contract/site.json), builds [data-site-nav] from site.nav, sets the page
   <title>, hides [data-release] items before their release time, and wires
   the R | Python track toggle. Vanilla, no dependencies. */
(function () {
  var C = (window.COURSE_CONTRACT || {}).site || {};
  function get(path) {
    return path.split('.').reduce(function (o, k) { return o == null ? o : o[k]; }, C);
  }
  var me = document.currentScript || document.querySelector('script[src*="site-text.js"]');
  var root = me ? me.getAttribute('src').replace(/assets\/site-text\.js.*$/, '') : '';
  function run() {
    document.querySelectorAll('[data-site-text]').forEach(function (el) {
      var v = get(el.getAttribute('data-site-text'));
      if (v != null && v !== '') el.textContent = v;
    });
    document.querySelectorAll('[data-site-href]').forEach(function (el) {
      var v = get(el.getAttribute('data-site-href'));
      if (v && !/^</.test(v)) el.setAttribute('href', v);  /* unset contract field: keep the page's own href */
    });
    var t = document.querySelector('title[data-page-title]');
    if (t && C.book) document.title = (C.book.title || '') + ' · ' + t.getAttribute('data-page-title');
    var nav = document.querySelector('[data-site-nav]');
    if (nav && C.nav) {
      var here = location.pathname.split('/').pop() || 'index.html';
      nav.innerHTML = '';
      C.nav.forEach(function (item) {
        var a = document.createElement('a');
        a.href = root + item.href;
        a.textContent = item.label;
        if (item.href.split('/').pop() === here) a.setAttribute('aria-current', 'page');
        nav.appendChild(a);
      });
    }
    var now = new Date().toISOString().slice(0, 16);
    document.querySelectorAll('[data-release]').forEach(function (el) {
      if (el.getAttribute('data-release') > now) el.classList.add('is-locked');
    });
    document.querySelectorAll('.track-seg button[data-track]').forEach(function (b) {
      b.addEventListener('click', function () {
        var lang = b.getAttribute('data-track');
        document.querySelectorAll('.track-seg button').forEach(function (x) {
          x.setAttribute('aria-selected', String(x === b));
        });
        document.querySelectorAll('pre.chunk[data-lang]').forEach(function (p) {
          p.hidden = p.getAttribute('data-lang') !== lang;
        });
      });
    });
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', run); else run();
})();
