/* =========================================================================
   slides.js  (v2 navigation)
   The whole slide engine. Dependency-free, no build step, no framework.
   See specs/SLIDES.md §5.

   Responsibilities, and nothing else:
     · scale the fixed 1280×720 stage to the viewport (--sc) + letterbox
     · navigate: ← → PgUp PgDn Space Home End; tap/click the left 40% /
       right 60% of the viewport; horizontal touch swipe; ‹ › nav buttons
     · overview grid (`g`, tap the n / N counter; Esc closes) — every slide
       laid out live at thumbnail scale, click one to jump
     · progress bar pinned to the bottom of the viewport
     · direction-aware 180ms slide/fade transition (off under
       prefers-reduced-motion and in print)
     · opt-in bullet-by-bullet build: `ul.build > li` / `.fragment` reveal
       one step at a time before the slide advances (`.` reveals the rest)
     · deep-link: #3 selects slide 3 on load; nav rewrites the hash
     · inject footer chrome (deck title • course number • n / N) on every
       non-title slide, plus an optional "📖 chapter" link chip
     · toggle a print/scroll view (?print in the URL, or the `p` key)
     · contents sidebar (`t` / `/` / ☰): nested TOC, slide thumbnails,
       and a links pane; desktop pushes the stage, mobile overlays

   Reads its configuration off <body>:
     data-deck-title   "Lesson 0: Introduction"
     data-deck-kind    lesson | workshop | recitation
     data-week         "1"
     data-chapters     "ch5r,ch5py"   (optional; first one wins for the chip)

   Contract with the CSS: a slide is `.slide`; the visible one carries
   `.is-active`; print view is `body.is-print`; overview is
   `body.is-overview`; nav chrome idles behind `body.is-nav-idle`; the
   transition classes are `.sg-in-next` / `.sg-in-prev`. Nothing else is
   shared.

   Rule: never throw. A deck that half-loads must still page through.
   ========================================================================= */
(function () {
  'use strict';

  var STAGE_W = 1280, STAGE_H = 720;
  var deck = document.querySelector('.deck');
  var slides = [].slice.call(document.querySelectorAll('.slide'));
  if (!slides.length) return;

  var body = document.body;
  var root = document.documentElement;
  /* A11Y F-05. The deck files carry <html lang="en"> as authored; this is the
     belt-and-braces for any deck that predates that edit. Never overwrite an
     author's explicit lang. */
  if (!root.getAttribute('lang')) root.setAttribute('lang', 'en');
  var deckTitle = body.getAttribute('data-deck-title') || document.title || '';
  var chapters = (body.getAttribute('data-chapters') || '').split(',')
    .map(function (s) { return s.trim(); }).filter(Boolean);

  var index = 0;
  var printing = /[?&]print\b/.test(location.search);
  var overview = false;
  var reduceMotion = false;
  try {
    reduceMotion = !!(window.matchMedia
      && window.matchMedia('(prefers-reduced-motion: reduce)').matches);
  } catch (e) { /* older engine: just animate */ }

  /* ------------------------------------------------------------ scaling --
     One transform for the whole stage. `min` keeps 16:9 and letterboxes on
     any other viewport aspect. In print/scroll view we render at 1:1.
     In overview we additionally publish --ovs, the thumbnail scale.
     -------------------------------------------------------------------- */
  function overviewScale() {
    var pad = 18, gap = 14;
    var w = Math.max(240, window.innerWidth);
    var cols = Math.max(2, Math.min(5, Math.round(w / 340)));
    var thumb = (w - pad * 2 - gap * (cols - 1)) / cols;
    return Math.max(0.06, Math.min(0.42, thumb / STAGE_W));
  }

  /* Desktop (≥900px): the contents sidebar *pushes* the stage instead of
     overlaying it, so --sc is computed against the leftover column. Mobile
     keeps the overlay drawer — shrinking a 16:9 stage into the leftover
     strip would crush it. tocOpen is declared with the sidebar; the
     functions below only run after boot. */
  var TOC_PUSH_MIN = 900;

  function tocPanelWidth() {
    var raw = '';
    try { raw = (getComputedStyle(root).getPropertyValue('--toc-w') || '').trim(); }
    catch (e) { raw = ''; }
    var n = parseFloat(raw);
    return (isFinite(n) && n > 0) ? n : 336;
  }

  function tocPushing() {
    return !!(tocOpen && !printing && !overview && window.innerWidth >= TOC_PUSH_MIN);
  }

  function syncTocPush() {
    var push = tocPushing();
    body.classList.toggle('is-toc-push', push);
    return push;
  }

  function rescale() {
    var w = window.innerWidth, h = window.innerHeight;
    if (tocPushing()) w = Math.max(1, w - tocPanelWidth());
    var sc = printing ? 1 : Math.min(w / STAGE_W, h / STAGE_H);
    root.style.setProperty('--sc', String(sc));
    root.style.setProperty('--ovs', String(overviewScale()));
    root.style.setProperty('--toc-thumb-sc', String(Math.max(120, tocPanelWidth() - 24) / STAGE_W));
  }

  /* ------------------------------------------------------------- chrome --
     Footer bar on every slide except the title slide (which has its own
     presenter block and red bar).
     -------------------------------------------------------------------- */
  function chapterChip() {
    if (!chapters.length) return null;
    var c = window.COURSE_CONTRACT;
    var entry = c && c.chapters ? c.chapters[chapters[0]] : null;
    if (!entry || !entry.slug) return null;      // degrade silently
    var a = document.createElement('a');
    a.className = 'df-chap';
    a.href = '../chapters/' + entry.slug + '.html';
    a.textContent = '📖 ' + (entry.title || 'chapter');
    return a;
  }


  /* course facts: fill [data-site-text] from COURSE_CONTRACT.site (assets/contract.js) */
  function fillSiteText() {
    var site = (window.COURSE_CONTRACT || {}).site || {};
    document.querySelectorAll('[data-site-text]').forEach(function (el) {
      var v = el.getAttribute('data-site-text').split('.').reduce(function (o, k) { return o == null ? o : o[k]; }, site);
      if (v != null && v !== '') el.textContent = v;
    });
  }

  function injectFooters() {
    var chip = chapterChip();
    slides.forEach(function (el, i) {
      if (el.classList.contains('s-title')) return;
      if (el.querySelector(':scope > .deckfoot')) return;
      var f = document.createElement('div');
      f.className = 'deckfoot';
      f.innerHTML =
        '<span class="df-title"></span>' +
        '<span class="df-sep">•</span>' +
        '<span class="df-course" data-site-text="course.number"></span>' +
        '<button type="button" class="df-count" aria-label="Slide overview"></button>';
      f.querySelector('.df-title').textContent = deckTitle;
      f.querySelector('.df-count').textContent = (i + 1) + ' / ' + slides.length;
      if (chip) f.insertBefore(chip.cloneNode(true), f.querySelector('.df-count'));
      el.appendChild(f);
    });
    fillSiteText();
  }

  /* ------------------------------------------------------- nav chrome v2 --
     Two chevron buttons and a progress bar, both fixed to the viewport (not
     to the scaled stage) so they keep a real 48px+ hit target at any scale.
     -------------------------------------------------------------------- */
  var progressFill = null;

  /* A11Y F-09. The chevrons and the progress bar used to be appended straight
     to <body>, which left them outside every landmark ("region" violation on
     10/10 deck scans). They now live in one labelled <nav>. The <nav> itself
     is a zero-height static box and all three children stay position:fixed —
     nothing about the layout, the hit targets or the z-order changes. */
  var navHost = null;
  function chromeHost() {
    if (navHost) return navHost;
    navHost = document.createElement('nav');
    navHost.className = 'sg-chrome';
    navHost.setAttribute('aria-label', 'Slide navigation');
    body.appendChild(navHost);
    return navHost;
  }

  function navButton(cls, glyph, label, delta) {
    var b = document.createElement('button');
    b.type = 'button';
    b.className = 'sg-nav ' + cls;
    b.setAttribute('aria-label', label);
    b.innerHTML = '<span aria-hidden="true">' + glyph + '</span>';
    b.addEventListener('click', function (e) {
      e.preventDefault();
      e.stopPropagation();
      wake();
      if (overview) { setOverview(false); return; }
      if (!printing) go(delta);
    });
    chromeHost().appendChild(b);
    return b;
  }

  function buildProgress() {
    var bar = document.createElement('div');
    bar.className = 'sg-progress';
    bar.setAttribute('role', 'progressbar');
    bar.setAttribute('aria-label', 'Deck progress');
    var fill = document.createElement('div');
    fill.className = 'sg-progress-fill';
    bar.appendChild(fill);
    chromeHost().appendChild(bar);
    progressFill = fill;
    return bar;
  }

  function updateProgress() {
    if (!progressFill) return;
    var pct = slides.length > 1
      ? ((index + 1) / slides.length) * 100
      : 100;
    progressFill.style.width = pct.toFixed(4) + '%';
    var bar = progressFill.parentNode;
    if (bar && bar.setAttribute) {
      bar.setAttribute('aria-valuenow', String(index + 1));
      bar.setAttribute('aria-valuemin', '1');
      bar.setAttribute('aria-valuemax', String(slides.length));
    }
  }

  /* Fine-pointer idle fade: chrome drops to 0.25 after 2.5s of no mouse. */
  var idleTimer = null;
  function wake() {
    body.classList.remove('is-nav-idle');
    if (idleTimer) clearTimeout(idleTimer);
    idleTimer = setTimeout(function () {
      body.classList.add('is-nav-idle');
    }, 2500);
  }

  /* ------------------------------------------------ per-slide focus (F-04) --
     Every slide is in the DOM at once. In the normal view only the active one
     is displayed, so the browser already keeps focus out of the other 33 — but
     that is a side effect of `display:none`, not a stated rule, and it stops
     being true the moment a slide is shown for any other reason (overview,
     print, a future presenter view). This makes the rule explicit and adds the
     two things `display:none` never gave us:

       · the active slide is a focus target (`tabindex="-1"`), so focus can be
         parked on it;
       · when the user navigates *while focused inside a slide*, focus moves to
         the incoming slide instead of being dropped on the floor — otherwise
         the next Tab restarts from the top of the document and the in-slide
         links are effectively unreachable a second time.

     Mouse and arrow-key users see no change: the focus hand-off only fires
     when focus was already inside the outgoing slide, which never happens
     unless somebody has been tabbing. Print and overview keep today's
     behaviour exactly — every slide is visible there, so every slide stays
     reachable.
     ------------------------------------------------------------------------ */
  var FOCUSABLE = 'a[href],button,input,select,textarea,summary,iframe,' +
    '[tabindex]:not([tabindex="-1"]),[contenteditable="true"]';

  function slideReachable(i) {
    return printing || overview || i === index;
  }

  function syncSlideAccess() {
    slides.forEach(function (el, i) {
      var on = slideReachable(i);
      el.setAttribute('tabindex', '-1');
      if (on) {
        el.removeAttribute('aria-hidden');
      } else {
        el.setAttribute('aria-hidden', 'true');
      }
      var kids;
      try { kids = el.querySelectorAll(FOCUSABLE); }
      catch (e) { return; }                      // never throw; deck still pages
      [].forEach.call(kids, function (k) {
        if (on) {
          if (k.hasAttribute('data-sg-ti')) {
            var was = k.getAttribute('data-sg-ti');
            k.removeAttribute('data-sg-ti');
            if (was === '') k.removeAttribute('tabindex');
            else k.setAttribute('tabindex', was);
          }
        } else if (!k.hasAttribute('data-sg-ti')) {
          k.setAttribute('data-sg-ti', k.getAttribute('tabindex') || '');
          k.setAttribute('tabindex', '-1');
        }
      });
    });
  }

  /* True when the keyboard is currently somewhere inside slide `el`. */
  function focusInside(el) {
    var a = document.activeElement;
    return !!(el && a && a !== document.body && el.contains(a));
  }

  /* ------------------------------------------------- builds (opt-in) --
     Bullet-by-bullet reveal, and NOTHING happens unless a deck asks for it.
     A slide's "fragments" are every direct `li` child of a `ul.build` /
     `ol.build`, plus any element carrying `.fragment`, in DOM order
     (querySelectorAll returns document order, so one selector is enough).

     State lives on the element as `data-revealed="yes" | "no"` — the CSS
     hides a "no" with `visibility`, never `display`, so the list keeps its
     box and the slide does not re-flow as bullets appear.

     Rules:
       · arriving forwards (or by a hash jump / TOC / overview click) the
         slide starts with every fragment hidden;
       · arriving BACKWARDS lands on a fully built slide — a back-step must
         never make the presenter re-click four bullets;
       · "next" reveals the next hidden fragment if there is one, otherwise
         it advances the slide; "previous" hides the last revealed fragment
         if there is one, otherwise it goes back. Keys, taps, swipes and the
         ‹ › chevrons all funnel through go(), so they all obey this;
       · `.` reveals the rest of the current slide in one go;
       · print, overview and the TOC thumbnails show everything (CSS).
     -------------------------------------------------------------------- */
  var BUILD_SEL = 'ul.build > li, ol.build > li, .fragment';

  function fragmentsOf(el) {
    if (!el || !el.querySelectorAll) return [];
    try { return [].slice.call(el.querySelectorAll(BUILD_SEL)); }
    catch (e) { return []; }                   // never throw; deck still pages
  }

  /* Reveal the first `shown` fragments of `el` and hide the rest. */
  function setBuild(el, shown) {
    var f = fragmentsOf(el);
    for (var i = 0; i < f.length; i++) {
      f[i].setAttribute('data-revealed', i < shown ? 'yes' : 'no');
    }
    return f.length;
  }

  function buildShown(el) {
    var f = fragmentsOf(el), n = 0;
    for (var i = 0; i < f.length; i++) {
      if (f[i].getAttribute('data-revealed') !== 'no') n++;
    }
    return n;
  }

  /* True when the step was consumed by the build — the deck must then NOT
     change slide. False for every slide that has no fragments, which is
     every slide in every deck that has not opted in. */
  function stepBuild(delta) {
    if (printing || overview) return false;
    var el = slides[index];
    var f = fragmentsOf(el);
    if (!f.length) return false;
    var shown = buildShown(el);
    if (delta > 0) {
      if (shown >= f.length) return false;
      f[shown].setAttribute('data-revealed', 'yes');
      return true;
    }
    if (shown <= 0) return false;
    f[shown - 1].setAttribute('data-revealed', 'no');
    return true;
  }

  function revealRest() {
    var el = slides[index];
    if (!el) return false;
    var f = fragmentsOf(el);
    if (!f.length) return false;
    setBuild(el, f.length);
    return true;
  }

  /* ---------------------------------------------------------- navigation */
  function animate(dir) {
    if (reduceMotion || printing || overview || !dir) return;
    var el = slides[index];
    if (!el) return;
    el.classList.remove('sg-in-next', 'sg-in-prev');
    // reflow so the animation restarts even on a rapid double-press
    void el.offsetWidth;
    el.classList.add(dir > 0 ? 'sg-in-next' : 'sg-in-prev');
  }

  function show(n, pushHash, dir) {
    var prev = index;
    var carryFocus = focusInside(slides[prev]);
    index = Math.max(0, Math.min(slides.length - 1, n));
    slides.forEach(function (el, i) {
      el.classList.toggle('is-active', i === index);
      if (i !== index) el.classList.remove('sg-in-next', 'sg-in-prev');
    });
    var d = (typeof dir === 'number') ? dir : (index > prev ? 1 : -1);
    if (index !== prev) animate(d);
    /* Builds: backwards lands fully built, everything else starts hidden. */
    setBuild(slides[index], d < 0 ? Infinity : 0);
    syncSlideAccess();
    if (carryFocus && index !== prev && slides[index] && slides[index].focus) {
      try { slides[index].focus({ preventScroll: true }); } catch (e) { /* ignore */ }
    }
    updateProgress();
    markTOC();
    attSync();                    // ATT.4: the console lives only on its slide
    if (pushHash !== false) {
      var want = '#' + (index + 1);
      if (location.hash !== want) {
        try { history.replaceState(null, '', want); }
        catch (e) { location.hash = want; }
      }
    }
  }
  function go(delta) {
    if (stepBuild(delta)) return;              // the slide built a step instead
    show(index + delta, true, delta > 0 ? 1 : -1);
  }

  function fromHash() {
    var n = parseInt((location.hash || '').replace('#', ''), 10);
    return (isFinite(n) && n >= 1 && n <= slides.length) ? n - 1 : 0;
  }

  /* ------------------------------------------------------ overview grid --
     No clone, no second DOM: a body class re-lays-out the real slides at
     --ovs. Everything the normal view depends on (.is-active, the s-agenda
     display:grid special case, the footers) is left exactly as it was, so
     closing the grid cannot leave the deck in a broken state.
     -------------------------------------------------------------------- */
  function setOverview(on) {
    on = !!on;
    if (on && printing) setPrint(false);
    if (on && tocOpen) setTOC(false, true);   // the grid owns the screen
    overview = on;
    body.classList.toggle('is-overview', overview);
    syncSlideAccess();
    rescale();
    if (overview) {
      slides.forEach(function (el) { el.classList.remove('sg-in-next', 'sg-in-prev'); });
      var cur = slides[index];
      if (cur && cur.scrollIntoView) {
        try { cur.scrollIntoView({ block: 'center' }); } catch (e) { /* ignore */ }
      }
    }
    attSync();                    // ATT.4: no live console in the grid
    wake();
  }

  /* --------------------------------------------------------- print view --
     Stacks every slide for PDF export (⌘P / Ctrl-P). Reversible.
     -------------------------------------------------------------------- */
  function setPrint(on) {
    printing = !!on;
    if (printing && overview) setOverview(false);
    if (printing && tocOpen) setTOC(false, true);
    body.classList.toggle('is-print', printing);
    if (printing) {
      slides.forEach(function (el) {
        el.classList.add('is-active');
        el.classList.remove('sg-in-next', 'sg-in-prev');
      });
    } else {
      show(index, true, 0);
    }
    syncSlideAccess();
    attSync();                    // ATT.4: no live console in the print stack
    rescale();
  }

  /* ------------------------------------------------------- download PDF --
     WK2.8. The print path already worked; nobody could find it. This is the
     discoverable front door: one button, top-right, that puts the deck into
     print view and opens the browser's print dialog, where the user picks
     "Save as PDF". A page cannot force a file download of its own rendering,
     so the label and the tooltip say what actually happens rather than
     promising a one-click file.
     -------------------------------------------------------------------- */
  var pdfBtn = null;

  function downloadPDF() {
    if (tocOpen) setTOC(false, true);       // the sidebar is not part of the deck
    if (overview) setOverview(false);
    if (!printing) setPrint(true);
    /* Let the is-print relayout settle before the (blocking) print dialog
       snapshots the page. A timer, deliberately, not requestAnimationFrame:
       rAF is throttled to nothing in a background or non-compositing tab and
       the dialog would simply never open. */
    setTimeout(function () {
      try { window.print(); } catch (e) { /* ignore */ }
    }, 60);
  }

  function buildPDFButton() {
    pdfBtn = document.createElement('button');
    pdfBtn.type = 'button';
    pdfBtn.className = 'sg-pdf-btn';
    pdfBtn.setAttribute('aria-label',
      'Download this deck as a PDF. Opens your browser print dialog — ' +
      'choose "Save as PDF" as the destination.');
    pdfBtn.title =
      'Download PDF — opens your browser’s print dialog; ' +
      'pick "Save as PDF" as the destination. One slide per page.';
    pdfBtn.innerHTML =
      '<span class="sg-pdf-ico" aria-hidden="true">⤓</span>' +
      '<span class="sg-pdf-lab">Download PDF</span>';
    pdfBtn.addEventListener('click', function (e) {
      e.preventDefault();
      e.stopPropagation();
      wake();
      downloadPDF();
    });
    chromeHost().appendChild(pdfBtn);
  }

  /* ------------------------------------------- contents sidebar + search --
     WK2.10 + sidebar UX: one toggleable sidebar per deck, built here so
     every deck gets it from one file. See specs/SLIDES.md §5.2.

     Nested titles: a slide's LABEL is no longer the breadcrumb `.topbar`
     (that name is the SECTION, and it used to smear onto every following
     item). Harvest order for the slide title is `.subhead` (minus pills)
     → the part of `.topbar` after a middot → h2/h3 / `.pillhead` → first
     list `<strong>` → section heading. Consecutive slides that share a
     section nest under it.

     Three panes: Contents (nested TOC), Thumbs (scaled clones), Links
     (the same nest of every slide + color-coded chips on slides that
     have `a[href]`; unlinked slides still appear, title only).

     Desktop (≥900px) PUSHES the stage into the leftover column; mobile
     keeps the overlay drawer. Search haystack is snapshotted BEFORE
     injectFooters() so chrome never matches.
     -------------------------------------------------------------------- */
  var tocOpen = false;
  var tocTab = 'toc';       // 'toc' | 'thumbs' | 'links'
  var tocEl = null, tocBtn = null, tocInput = null, tocList = null,
      tocLinksList = null, tocThumbs = null, tocStatus = null,
      tocTabsEl = null;
  var tocItems = [];        // contents-tab <li.sg-toc-item>, index-aligned
  var tocLinkItems = [];    // links-tab <li.sg-toc-item>, index-aligned
  var tocThumbBtns = [];    // thumbs-tab buttons, index-aligned
  var tocData = [];         // { title, section, hay, links } per slide
  var thumbsBuilt = false;

  function cleanText(s) { return String(s || '').replace(/\s+/g, ' ').trim(); }

  function tidyTitle(s) {
    s = cleanText(s);
    s = s.replace(/\s*\(([A-Z0-9]{2,8})\)\s*$/, '').trim();
    return s;
  }

  function textMinus(el, dropSel) {
    if (!el) return '';
    var c = el.cloneNode(true);
    try {
      [].forEach.call(c.querySelectorAll(dropSel), function (n) {
        if (n.parentNode) n.parentNode.removeChild(n);
      });
    } catch (e) { /* ignore */ }
    return tidyTitle(c.textContent);
  }

  function slideSection(el) {
    if (el.classList.contains('s-title')) {
      var th = el.querySelector('.titlebox h1, h1');
      return th ? tidyTitle(th.textContent) : 'Title';
    }
    if (el.classList.contains('s-section')) {
      var sh = el.querySelector('h1');
      return sh ? tidyTitle(sh.textContent) : 'Section';
    }
    var tb = el.querySelector('.topbar');
    if (tb) {
      var t = cleanText(tb.textContent);
      var parts = t.split(/\s*[·•]\s*/);
      return tidyTitle(parts[0] || t);
    }
    if (el.classList.contains('s-agenda')) {
      var ah = el.querySelector('h1');
      return ah ? tidyTitle(ah.textContent) : "Today's Class";
    }
    return '';
  }

  function slideOwnTitle(el, section, i) {
    var sub = el.querySelector('.subhead');
    if (sub) {
      var st = textMinus(sub, '.pill,.chip,.imgring');
      var extras = [];
      try {
        [].forEach.call(sub.querySelectorAll('.pill,.chip'), function (p) {
          var extra = tidyTitle(p.textContent);
          if (extra) extras.push(extra);
        });
      } catch (e) { /* ignore */ }
      if (st && extras.length) st = st + ' · ' + extras.join(' · ');
      else if (!st && extras.length) st = extras.join(' · ');
      if (st) return st;
    }
    var tb = el.querySelector('.topbar');
    if (tb) {
      var raw = cleanText(tb.textContent);
      var parts = raw.split(/\s*[·•]\s*/);
      if (parts.length > 1) {
        var rest = tidyTitle(parts.slice(1).join(' · '));
        if (rest && rest !== section) return rest;
      }
    }
    var h = el.querySelector('.content h2, .content h3, h2, h3');
    if (h) {
      var ht = tidyTitle(h.textContent);
      if (ht && ht !== section) return ht;
    }
    var ph = el.querySelector('.pillhead');
    if (ph) {
      var pt = tidyTitle(ph.textContent);
      if (pt) return pt;
    }
    if (el.classList.contains('s-section')
        || el.classList.contains('s-agenda')
        || el.classList.contains('s-title')) {
      var h1 = el.querySelector('h1');
      if (h1) return tidyTitle(h1.textContent);
    }
    var strong = el.querySelector('.content li > strong, .content p > strong');
    if (strong) {
      var sg = tidyTitle(strong.textContent).replace(/[:.]\s*$/, '');
      if (sg && sg !== section && sg.length < 80) return sg;
    }
    return section || ('Slide ' + (i + 1));
  }

  /* Homework = dark red; activity = red; everything else is the muted
     family. Ambiguous Canvas assignment URLs stay "canvas" (light red)
     — never invent a homework chip for a link that doesn't say so. */
  function classifyLink(href, text) {
    var h = String(href || '').toLowerCase();
    var t = String(text || '').toLowerCase();
    var blob = h + ' ' + t;
    if (/\bhomework\b|\bhw\s*\d|\bassignment\s*\d/.test(blob)
        || (/\bhomework\b|\bhw\b/.test(t) && /canvas|\/assignments\/|\/hw/.test(h))) {
      return 'hw';
    }
    if (/\bactivit/.test(blob) || /\/activities\//.test(h) || /in-class/.test(blob)
        || /live[\s-]?lab/.test(blob)) {
      return 'activity';
    }
    if (/canvas\.cornell\.edu|instructure\.com/.test(h)) return 'canvas';
    if (/\/chapters\/|\/labs\/|timothyfraser\.com/.test(h) || /^\.\.\//.test(href)) {
      return 'site';
    }
    return 'other';
  }

  /* Slide prose often appends a literal "[link]" after the name. Chips
     show the name only. */
  function stripLinkMarker(s) {
    return cleanText(String(s || '').replace(/\s*\[link\]\s*/gi, ' '));
  }

  /* "activity page" / "here" / "Canvas" are not names. After stripping a
     leading Open/Read/Submit, the leftover is still tested. */
  function isGenericLinkText(s) {
    s = stripLinkMarker(s).toLowerCase().replace(/[.:!?]+$/g, '');
    if (!s) return true;
    var core = s.replace(/^(open|read|see|follow(\s+steps(\s+on)?)?|submit(\s+answers)?(\s+on)?)\s+/i, '');
    core = core.replace(/^(the|this|our|full)\s+/i, '').trim();
    if (!core) return true;
    return /^(link|here|click(\s+here)?|canvas|activity(\s+page)?|assignment|page|site|url)$/i.test(core);
  }

  function isGenericContext(s) {
    s = stripLinkMarker(s).toLowerCase();
    return !s || /^(exercise|activity|in-class activity|section|title|today'?s class|slide \d+)$/i.test(s);
  }

  function slugToLabel(href) {
    var raw = String(href || '').split('#')[0].split('?')[0];
    var path = raw;
    try {
      if (/^https?:\/\//i.test(raw)) {
        var u = document.createElement('a');
        u.href = raw;
        path = u.pathname || '';
      }
    } catch (e) { /* ignore */ }
    path = path.replace(/\/+$/, '');
    var segs = path.split('/').filter(Boolean);
    var skip = /^(activities|chapters|labs|assignments|courses|pages|files|d|edit|spreadsheets)$/i;
    var acronym = /^(fmea|spc|rsm|doe|hw|ai|rpn|mocus|ci)$/i;
    var i;
    for (i = segs.length - 1; i >= 0; i--) {
      var seg = segs[i].replace(/\.(html|htm|php|aspx)$/i, '');
      if (!seg || skip.test(seg) || /^\d+$/.test(seg)) continue;
      if (seg.length > 48 && !/[-_]/.test(seg)) continue;
      var words = seg.split(/[-_]+/).filter(Boolean);
      if (!words.length) continue;
      return words.map(function (w) {
        if (acronym.test(w)) return w.toUpperCase();
        return w.charAt(0).toUpperCase() + w.slice(1).toLowerCase();
      }).join(' ');
    }
    return '';
  }

  function nearbyLinkName(a, slideEl) {
    var attrs = [a.getAttribute('aria-label'), a.getAttribute('title')];
    var i;
    for (i = 0; i < attrs.length; i++) {
      var named = stripLinkMarker(attrs[i] || '');
      if (named && !isGenericLinkText(named) && !isGenericContext(named)) return named;
    }
    var sub = slideEl.querySelector('.subhead');
    if (sub) {
      var pills = [];
      try {
        [].forEach.call(sub.querySelectorAll('.pill,.chip'), function (p) {
          var extra = tidyTitle(p.textContent);
          if (extra && !/^(in class|live|new)$/i.test(extra)) pills.push(extra);
        });
      } catch (e) { /* ignore */ }
      var st = textMinus(sub, '.pill,.chip,.imgring');
      if (st && !isGenericLinkText(st) && !isGenericContext(st)) {
        return pills.length ? st + ' · ' + pills.join(' · ') : st;
      }
    }
    var h = slideEl.querySelector('.content h2, .content h3, h2, h3, h1');
    if (h) {
      var ht = tidyTitle(h.textContent);
      if (ht && !isGenericLinkText(ht) && !isGenericContext(ht)) return ht;
    }
    var subline = slideEl.querySelector('.sub');
    if (subline) {
      var su = stripLinkMarker(textMinus(subline, 'a,.pill,.chip') || '');
      if (su && !isGenericLinkText(su) && !isGenericContext(su)) return su;
    }
    return '';
  }

  /* Prefer the <a> text when it names the thing. Generic labels
     ("activity page", "here", "Canvas") fall through to nearby
     heading / slide title / pill / aria-label / title, then the URL
     path slug (`fmea-exercise` → "FMEA Exercise"). Classification
     still uses the original href + raw text so color stays honest. */
  function chipLabel(a, slideEl) {
    var raw = stripLinkMarker(textMinus(a, '.pill,.chip') || '');
    if (raw && !isGenericLinkText(raw)) return raw;
    var href = a.getAttribute('href') || '';
    var slug = slugToLabel(href);
    var near = nearbyLinkName(a, slideEl);
    var namedPath = /\/(activities|chapters|labs)\//i.test(href);
    if (namedPath && slug && !isGenericLinkText(slug) && !isGenericContext(slug)) return slug;
    if (near) return near;
    if (slug && !isGenericLinkText(slug) && !isGenericContext(slug)) return slug;
    return raw || slug || 'Link';
  }

  function harvestLinks(el) {
    var out = [], seen = {};
    var nodes;
    try { nodes = el.querySelectorAll('a[href]'); }
    catch (e) { return out; }
    [].forEach.call(nodes, function (a) {
      if (a.closest && a.closest('.deckfoot')) return;
      var href = a.getAttribute('href') || '';
      if (!href || href.charAt(0) === '#' || /^\s*javascript:/i.test(href)) return;
      var raw = textMinus(a, '.pill,.chip') || href;
      var text = chipLabel(a, el);
      var key = href + '\0' + text;
      if (seen[key]) return;
      seen[key] = true;
      out.push({ href: href, text: text, kind: classifyLink(href, raw) });
    });
    return out;
  }

  function sameSection(a, b) {
    a = String(a || '').toLowerCase();
    b = String(b || '').toLowerCase();
    if (!a || !b) return false;
    if (a === b) return true;
    /* "Training: Visualization with ggplot" vs "... in R" — keep one parent. */
    if (a.length >= 12 && b.length >= 12 && (a.indexOf(b) === 0 || b.indexOf(a) === 0)) return true;
    return false;
  }

  function harvestTOC() {
    var prev = '';
    tocData = slides.map(function (el, i) {
      var raw = slideSection(el) || prev || ('Slide ' + (i + 1));
      var section;
      if (el.classList.contains('s-section') || el.classList.contains('s-title')) {
        section = raw;
      } else if (sameSection(raw, prev)) {
        section = prev;
      } else {
        section = raw;
      }
      var title = slideOwnTitle(el, section, i);
      var links = harvestLinks(el);
      var text = '';
      try { text = cleanText(el.textContent); } catch (e2) { text = ''; }
      prev = section;
      return {
        title: title,
        section: section,
        links: links,
        hay: (title + ' ' + section + ' ' + text).toLowerCase()
      };
    });
  }

  function firstMatch() {
    var items = tocTab === 'links' ? tocLinkItems : tocItems;
    for (var i = 0; i < items.length; i++) {
      if (items[i] && !items[i].hidden) return i;
    }
    return -1;
  }

  function itemMatches(i, q) {
    if (!q) return true;
    if (tocData[i].hay.indexOf(q) >= 0) return true;
    var links = tocData[i].links || [];
    for (var k = 0; k < links.length; k++) {
      var blob = (links[k].text + ' ' + links[k].href).toLowerCase();
      if (blob.indexOf(q) >= 0) return true;
    }
    return false;
  }

  function syncGroups(listEl) {
    if (!listEl) return;
    var groups = listEl.querySelectorAll('.sg-toc-group');
    [].forEach.call(groups, function (g) {
      var any = g.querySelector('.sg-toc-item:not([hidden])');
      g.hidden = !any;
    });
  }

  function filterTOC() {
    if (!tocEl) return;
    var raw = tocInput ? tocInput.value : '';
    var q = cleanText(raw).toLowerCase();
    var n = 0;
    tocItems.forEach(function (li, i) {
      var hit = itemMatches(i, q);
      li.hidden = !hit;
      if (hit) n++;
    });
    tocLinkItems.forEach(function (li, i) {
      var hit = itemMatches(i, q);
      li.hidden = !hit;
    });
    tocThumbBtns.forEach(function (b, i) {
      if (!b) return;
      b.hidden = !itemMatches(i, q);
    });
    syncGroups(tocList);
    syncGroups(tocLinksList);
    var shown = tocTab === 'links'
      ? tocLinkItems.filter(function (li) { return li && !li.hidden; }).length
      : n;
    tocEl.classList.toggle('is-empty', shown === 0);
    if (!tocStatus) return;
    if (!q) {
      tocStatus.textContent = slides.length + ' slides';
    } else if (!shown) {
      tocStatus.textContent = 'No slides match “' + cleanText(raw) + '”';
    } else {
      tocStatus.textContent = shown + (shown === 1 ? ' match' : ' matches');
    }
  }

  function markTOC() {
    function markList(items) {
      items.forEach(function (li, i) {
        if (!li) return;
        var b = li.querySelector('.sg-toc-link');
        var on = i === index;
        li.classList.toggle('is-current', on);
        if (!b) return;
        if (on) b.setAttribute('aria-current', 'true');
        else b.removeAttribute('aria-current');
      });
    }
    markList(tocItems);
    markList(tocLinkItems);
    tocThumbBtns.forEach(function (b, i) {
      if (!b) return;
      var on = i === index;
      b.classList.toggle('is-current', on);
      if (on) b.setAttribute('aria-current', 'true');
      else b.removeAttribute('aria-current');
    });
    if (!tocOpen) return;
    var cur = tocTab === 'thumbs' ? tocThumbBtns[index]
      : tocTab === 'links' ? tocLinkItems[index]
      : tocItems[index];
    if (cur && !cur.hidden && cur.scrollIntoView) {
      try { cur.scrollIntoView({ block: 'nearest' }); } catch (e) { /* ignore */ }
    }
  }

  /* `quiet` = do not move focus (used when another view takes over). */
  function setTOC(on, quiet) {
    on = !!on;
    if (on && overview) setOverview(false);
    tocOpen = on;
    body.classList.toggle('is-toc', tocOpen);
    if (!tocOpen) body.classList.remove('is-toc-push');
    else syncTocPush();
    if (tocEl) tocEl.setAttribute('aria-hidden', tocOpen ? 'false' : 'true');
    if (tocBtn) tocBtn.setAttribute('aria-expanded', tocOpen ? 'true' : 'false');
    rescale();
    if (tocOpen) {
      markTOC();
      if (tocTab === 'thumbs') ensureThumbs();
      if (!quiet && tocInput) {
        try { tocInput.focus({ preventScroll: true }); }
        catch (e) { try { tocInput.focus(); } catch (e2) { /* ignore */ } }
      }
    } else if (!quiet && tocBtn && tocBtn.focus) {
      try { tocBtn.focus({ preventScroll: true }); } catch (e) { /* ignore */ }
    }
    wake();
  }

  function tocJump(i) {
    if (i < 0 || i >= slides.length) return;
    if (printing) setPrint(false);
    if (overview) setOverview(false);
    show(i, true, 0);
    markTOC();
  }

  function setTocTab(name) {
    if (name !== 'toc' && name !== 'thumbs' && name !== 'links') name = 'toc';
    tocTab = name;
    if (tocEl) {
      tocEl.classList.toggle('tab-toc', name === 'toc');
      tocEl.classList.toggle('tab-thumbs', name === 'thumbs');
      tocEl.classList.toggle('tab-links', name === 'links');
    }
    if (tocTabsEl) {
      [].forEach.call(tocTabsEl.querySelectorAll('.sg-toc-tab'), function (b) {
        var on = b.getAttribute('data-tab') === name;
        b.classList.toggle('is-on', on);
        b.setAttribute('aria-selected', on ? 'true' : 'false');
      });
    }
    if (name === 'thumbs') ensureThumbs();
    filterTOC();
    markTOC();
  }

  function makeTocItem(i, withChips) {
    var d = tocData[i];
    var li = document.createElement('li');
    li.className = 'sg-toc-item';
    li.setAttribute('data-i', String(i));
    var b = document.createElement('button');
    b.type = 'button';
    b.className = 'sg-toc-link';
    b.innerHTML = '<span class="sg-toc-n"></span><span class="sg-toc-t"></span>';
    b.querySelector('.sg-toc-n').textContent = String(i + 1);
    b.querySelector('.sg-toc-t').textContent = d.title;
    b.addEventListener('click', function (e) {
      e.preventDefault();
      e.stopPropagation();
      tocJump(i);
    });
    li.appendChild(b);
    if (withChips && d.links && d.links.length) {
      var chips = document.createElement('div');
      chips.className = 'sg-toc-chips';
      d.links.forEach(function (L) {
        var a = document.createElement('a');
        a.className = 'sg-toc-chip kind-' + L.kind;
        a.href = L.href;
        a.target = '_blank';
        a.rel = 'noopener noreferrer';
        a.textContent = L.text;
        a.title = L.text;
        a.addEventListener('click', function (e) { e.stopPropagation(); });
        chips.appendChild(a);
      });
      li.appendChild(chips);
    }
    return li;
  }

  function fillNested(listEl, withChips, bucket) {
    var prev = null, group = null, sub = null;
    tocData.forEach(function (d, i) {
      var newGroup = !group || d.section !== prev;
      if (newGroup) {
        group = document.createElement('li');
        group.className = 'sg-toc-group';
        var head = document.createElement('button');
        head.type = 'button';
        head.className = 'sg-toc-sec';
        head.textContent = d.section;
        (function (start) {
          head.addEventListener('click', function (e) {
            e.preventDefault();
            e.stopPropagation();
            tocJump(start);
          });
        })(i);
        group.appendChild(head);
        sub = document.createElement('ol');
        sub.className = 'sg-toc-sub';
        group.appendChild(sub);
        listEl.appendChild(group);
        prev = d.section;
      }
      var li = makeTocItem(i, withChips);
      sub.appendChild(li);
      bucket[i] = li;
    });
  }

  function fillThumb(btn, i) {
    if (!btn || btn.getAttribute('data-filled') === '1') return;
    var stage = document.createElement('div');
    stage.className = 'sg-toc-thumb-stage';
    var clone;
    try { clone = slides[i].cloneNode(true); }
    catch (e) { return; }
    clone.classList.remove('is-active', 'sg-in-next', 'sg-in-prev');
    clone.classList.add('sg-thumb-slide');
    try {
      [].forEach.call(clone.querySelectorAll('iframe,script,video,audio,.deckfoot'), function (n) {
        if (!n.parentNode) return;
        if (n.tagName === 'IFRAME') {
          var ph = document.createElement('div');
          ph.className = 'sg-toc-thumb-ph';
          ph.textContent = 'embedded page';
          n.parentNode.replaceChild(ph, n);
        } else {
          n.parentNode.removeChild(n);
        }
      });
    } catch (e2) { /* ignore */ }
    stage.appendChild(clone);
    var badge = document.createElement('span');
    badge.className = 'sg-toc-thumb-n';
    badge.textContent = String(i + 1);
    btn.appendChild(stage);
    btn.appendChild(badge);
    btn.setAttribute('data-filled', '1');
  }

  function ensureThumbs() {
    if (thumbsBuilt || !tocThumbs) return;
    thumbsBuilt = true;
    var i = 0;
    function chunk() {
      var n = 0;
      while (i < slides.length && n < 6) {
        fillThumb(tocThumbBtns[i], i);
        i++;
        n++;
      }
      if (i < slides.length) {
        setTimeout(chunk, 0);
      } else {
        markTOC();
      }
    }
    chunk();
  }

  function buildThumbsPane() {
    tocData.forEach(function (d, i) {
      var b = document.createElement('button');
      b.type = 'button';
      b.className = 'sg-toc-thumb';
      b.setAttribute('aria-label', 'Slide ' + (i + 1) + ': ' + d.title);
      b.addEventListener('click', function (e) {
        e.preventDefault();
        e.stopPropagation();
        tocJump(i);
      });
      tocThumbs.appendChild(b);
      tocThumbBtns[i] = b;
    });
  }

  function buildTOC() {
    tocBtn = document.createElement('button');
    tocBtn.type = 'button';
    tocBtn.className = 'sg-toc-btn';
    tocBtn.setAttribute('aria-label', 'Table of contents (t)');
    tocBtn.setAttribute('aria-expanded', 'false');
    tocBtn.setAttribute('aria-controls', 'sg-toc');
    tocBtn.innerHTML = '<span aria-hidden="true">☰</span>';
    tocBtn.addEventListener('click', function (e) {
      e.preventDefault();
      e.stopPropagation();
      setTOC(!tocOpen);
    });
    chromeHost().appendChild(tocBtn);

    tocEl = document.createElement('aside');
    tocEl.className = 'sg-toc tab-toc';
    tocEl.id = 'sg-toc';
    tocEl.setAttribute('aria-label', 'Slide contents');
    tocEl.setAttribute('aria-hidden', 'true');
    tocEl.innerHTML =
      '<div class="sg-toc-head">' +
        '<label class="sg-toc-lab" for="sg-toc-q">Contents &amp; search</label>' +
        '<input id="sg-toc-q" class="sg-toc-q" type="text" autocomplete="off" ' +
          'spellcheck="false" placeholder="Filter slides…" ' +
          'aria-describedby="sg-toc-status">' +
        '<p class="sg-toc-status" id="sg-toc-status" role="status" aria-live="polite"></p>' +
        '<div class="sg-toc-tabs" role="tablist" aria-label="Sidebar view">' +
          '<button type="button" class="sg-toc-tab is-on" role="tab" data-tab="toc" aria-selected="true">Contents</button>' +
          '<button type="button" class="sg-toc-tab" role="tab" data-tab="thumbs" aria-selected="false">Thumbs</button>' +
          '<button type="button" class="sg-toc-tab" role="tab" data-tab="links" aria-selected="false">Links</button>' +
        '</div>' +
      '</div>' +
      '<ol class="sg-toc-list" data-pane="toc"></ol>' +
      '<div class="sg-toc-thumbs" data-pane="thumbs"></div>' +
      '<ol class="sg-toc-list sg-toc-links" data-pane="links"></ol>';
    body.appendChild(tocEl);
    tocInput = tocEl.querySelector('.sg-toc-q');
    tocStatus = tocEl.querySelector('.sg-toc-status');
    tocList = tocEl.querySelector('.sg-toc-list[data-pane="toc"]');
    tocLinksList = tocEl.querySelector('.sg-toc-list[data-pane="links"]');
    tocThumbs = tocEl.querySelector('.sg-toc-thumbs');
    tocTabsEl = tocEl.querySelector('.sg-toc-tabs');

    tocItems = new Array(slides.length);
    tocLinkItems = new Array(slides.length);
    tocThumbBtns = new Array(slides.length);
    fillNested(tocList, false, tocItems);
    fillNested(tocLinksList, true, tocLinkItems);
    buildThumbsPane();

    tocTabsEl.addEventListener('click', function (e) {
      var t = e.target;
      while (t && t !== tocTabsEl && !(t.classList && t.classList.contains('sg-toc-tab'))) {
        t = t.parentNode;
      }
      if (!t || t === tocTabsEl) return;
      e.preventDefault();
      e.stopPropagation();
      setTocTab(t.getAttribute('data-tab'));
    });

    /* The panel's own keys. NOT a second document listener — these fire on
       the input element only, and the document handler already bails on
       INPUT, so typing here can never page the deck. */
    tocInput.addEventListener('input', filterTOC);
    tocInput.addEventListener('keydown', function (e) {
      if (e.key === 'Escape' || e.key === 'Esc') {
        e.preventDefault();
        e.stopPropagation();
        setTOC(false);
        return;
      }
      if (e.key === 'Enter') {
        e.preventDefault();
        e.stopPropagation();
        tocJump(firstMatch());
      }
    });

    filterTOC();
  }

  /* ------------------------------------------------------------- wiring */
  var NAV_KEYS = {
    ArrowRight: 1, ArrowDown: 1, PageDown: 1, ' ': 1, Spacebar: 1, Enter: 1,
    ArrowLeft: -1, ArrowUp: -1, PageUp: -1
  };

  /* Enter and Space are the two nav keys that ALSO activate whatever the
     browser has focused, so they need an owner. Rules:
       - a link, a summary, a select, or any button that is not ours keeps its
         native activation — Enter on a slide link must follow the link;
       - our own chevrons (.sg-nav) keep Enter (a keyboard user who tabs to
         "previous" and hits Enter means previous), but never keep Space: a
         chevron the MOUSE left focused would otherwise swallow Space, or send
         the deck backwards on it. Space is always "next", from anywhere. */
  var ACTIVATES = 'a[href],button,select,summary';
  function activationTarget(node, key) {
    if (key !== 'Enter' && key !== ' ' && key !== 'Spacebar') return null;
    while (node && node !== document) {
      if (node.matches && node.matches(ACTIVATES)) return node;
      node = node.parentNode;
    }
    return null;
  }

  document.addEventListener('keydown', function (e) {
    if (e.metaKey || e.ctrlKey || e.altKey) return;
    var t = e.target;
    if (t && (t.tagName === 'INPUT' || t.tagName === 'TEXTAREA' || t.isContentEditable)) return;

    var act = activationTarget(t, e.key);
    var ownButton = act && act.classList
      && (act.classList.contains('sg-nav')
        || act.classList.contains('sg-toc-btn')
        || act.classList.contains('sg-toc-tab')
        || act.classList.contains('sg-pdf-btn'));
    if (act && !(e.key !== 'Enter' && ownButton)) return;

    if (e.key === 'Escape' || e.key === 'Esc') {
      if (overview) { e.preventDefault(); setOverview(false); return; }
      if (tocOpen) { e.preventDefault(); setTOC(false); }
      return;
    }
    if (e.key === 'g' || e.key === 'G') { e.preventDefault(); setOverview(!overview); return; }
    /* WK2.10 — `t` toggles the contents sidebar, `/` opens it focused on the
       search box. Neither collides: `g` is the overview grid, `p` is print,
       and both of these are swallowed by the INPUT guard above, so typing a
       "t" or a "/" into the search box never reaches here. */
    if (e.key === 't' || e.key === 'T') { e.preventDefault(); setTOC(!tocOpen); return; }
    if (e.key === '/') {
      e.preventDefault();
      if (!tocOpen) setTOC(true);
      else if (tocInput) { try { tocInput.select(); } catch (e2) { tocInput.focus(); } }
      return;
    }
    if (e.key === 'Home') { e.preventDefault(); setPrint(false); setOverview(false); show(0); return; }
    if (e.key === 'End') { e.preventDefault(); setPrint(false); setOverview(false); show(slides.length - 1); return; }
    if (e.key === 'p' || e.key === 'P') { e.preventDefault(); setPrint(!printing); return; }
    /* `.` — reveal the rest of a built slide at once. Falls through on any
       slide with no fragments, so no key is quietly eaten. */
    if (e.key === '.') {
      if (!printing && !overview && revealRest()) { e.preventDefault(); return; }
    }
    /* ATT.4 — `a` reloads the attendance console, and ONLY on an attendance
       slide: everywhere else it falls through so no key is quietly eaten. */
    if (e.key === 'a' || e.key === 'A') {
      if (attReload()) { e.preventDefault(); return; }
    }
    if (Object.prototype.hasOwnProperty.call(NAV_KEYS, e.key)) {
      if (printing) return;                    // let the page scroll normally
      if (overview) { e.preventDefault(); go(NAV_KEYS[e.key]); return; }
      e.preventDefault();
      go(NAV_KEYS[e.key]);
    }
  });

  /* -- click / tap: left 40% back, right 60% forward ---------------------
     Never steals a click from a link, a button, or a text selection.
     -------------------------------------------------------------------- */
  var NO_NAV = 'a,button,input,textarea,select,label,summary,iframe,[role="button"],' +
    '.sg-nav,.sg-toc,.sg-toc-btn,.sg-pdf-btn,.deckfoot,.lang-toggle,.lang-seg';
  var suppressClickUntil = 0;

  function interactiveAncestor(node) {
    while (node && node !== document) {
      if (node.matches && node.matches(NO_NAV)) return true;
      node = node.parentNode;
    }
    return false;
  }

  document.addEventListener('click', function (e) {
    if (printing || overview) return;
    if (Date.now() < suppressClickUntil) return;
    if (interactiveAncestor(e.target)) return;
    var sel = window.getSelection && window.getSelection();
    if (sel && String(sel) && String(sel).length > 1) return;   // user selected text
    var w = window.innerWidth || 1;
    go((e.clientX / w) >= 0.4 ? 1 : -1);
  });

  /* -- overview: click a thumbnail to jump there ------------------------- */
  if (deck) {
    deck.addEventListener('click', function (e) {
      if (!overview) return;
      e.stopPropagation();
      var node = e.target;
      while (node && node !== deck && !(node.classList && node.classList.contains('slide'))) {
        node = node.parentNode;
      }
      if (!node || node === deck) return;
      var i = slides.indexOf(node);
      if (i < 0) return;
      setOverview(false);
      show(i, true, 0);
    });
  }

  /* -- the n / N counter opens the grid ---------------------------------- */
  document.addEventListener('click', function (e) {
    var node = e.target;
    while (node && node !== document && !(node.classList && node.classList.contains('df-count'))) {
      node = node.parentNode;
    }
    if (!node || node === document) return;
    e.preventDefault();
    e.stopPropagation();
    setOverview(!overview);
  }, true);

  /* -- touch swipe -------------------------------------------------------
     ≥40px horizontal, clearly more horizontal than vertical, and either
     fast (velocity) or long. Passive listeners: we never call
     preventDefault, so scrolling in the overview grid stays native.
     -------------------------------------------------------------------- */
  var SWIPE_MIN = 40;            // px
  var SWIPE_V = 0.18;            // px/ms — a flick
  var tStart = null;

  function passive(target, type, fn) {
    try { target.addEventListener(type, fn, { passive: true }); }
    catch (e) { target.addEventListener(type, fn, false); }
  }

  passive(document, 'touchstart', function (e) {
    tStart = null;
    if (printing || overview) return;
    if (!e.touches || e.touches.length !== 1) return;
    if (interactiveAncestor(e.target)) return;
    var t = e.touches[0];
    tStart = { x: t.clientX, y: t.clientY, t: Date.now() };
  });

  passive(document, 'touchmove', function (e) {
    if (!tStart) return;
    if (e.touches && e.touches.length > 1) tStart = null;    // pinch — bail
  });

  passive(document, 'touchend', function (e) {
    var s = tStart;
    tStart = null;
    if (!s || printing || overview) return;
    var t = (e.changedTouches && e.changedTouches[0]) || null;
    if (!t) return;
    var dx = t.clientX - s.x, dy = t.clientY - s.y;
    var dt = Math.max(1, Date.now() - s.t);
    if (Math.abs(dx) < SWIPE_MIN) return;
    if (Math.abs(dx) < Math.abs(dy) * 1.2) return;           // vertical gesture
    if (Math.abs(dx) / dt < SWIPE_V && Math.abs(dx) < 90) return;
    suppressClickUntil = Date.now() + 500;                   // no ghost-click nav
    go(dx < 0 ? 1 : -1);
  });

  window.addEventListener('resize', function () {
    if (tocOpen) syncTocPush();
    rescale();
  });
  window.addEventListener('hashchange', function () {
    if (!printing) show(fromHash(), false, 0);
  });
  window.addEventListener('mousemove', wake);

  /* --------------------------------------------------------------- boot --
     contract.js is optional: if it 404s we simply never render the chip.
     -------------------------------------------------------------------- */
  /* -- R ⇄ Python on paired code blocks --------------------------------
     A .lang-pair wraps <pre data-lang="r"> and <pre data-lang="py">.
     We inject one R | Python pill per pair and persist the choice so the
     whole deck (and same-origin playground iframes) stay on one track.
     -------------------------------------------------------------------- */
  var TRACK_KEY = 'sts-slide-track';

  function readTrack() {
    try {
      var t = localStorage.getItem(TRACK_KEY);
      if (t === 'py' || t === 'r') return t;
    } catch (e) { /* private mode, quota, … */ }
    return 'r';
  }

  function applyTrack(track) {
    if (track !== 'py' && track !== 'r') track = 'r';
    try { localStorage.setItem(TRACK_KEY, track); } catch (e) { /* ignore */ }
    body.setAttribute('data-slide-track', track);
    [].forEach.call(document.querySelectorAll('.lang-seg'), function (btn) {
      var on = btn.getAttribute('data-track') === track;
      btn.classList.toggle('is-active', on);
      btn.setAttribute('aria-pressed', on ? 'true' : 'false');
    });
  }

  function wireLangPairs() {
    [].forEach.call(document.querySelectorAll('.lang-pair'), function (pair) {
      if (pair.querySelector('.lang-toggle')) return;
      var bar = document.createElement('div');
      bar.className = 'lang-toggle';
      bar.setAttribute('role', 'group');
      bar.setAttribute('aria-label', 'Language');
      bar.innerHTML =
        '<button type="button" class="lang-seg" data-track="r" aria-pressed="true">R</button>' +
        '<button type="button" class="lang-seg" data-track="py" aria-pressed="false">Python</button>';
      pair.insertBefore(bar, pair.firstChild);
    });
    applyTrack(readTrack());
  }

  document.addEventListener('click', function (e) {
    var node = e.target;
    while (node && node !== document && !(node.classList && node.classList.contains('lang-seg'))) {
      node = node.parentNode;
    }
    if (!node || node === document) return;
    e.preventDefault();
    e.stopPropagation();
    applyTrack(node.getAttribute('data-track'));
  }, true);

  window.addEventListener('storage', function (e) {
    if (e.key === TRACK_KEY && (e.newValue === 'r' || e.newValue === 'py')) {
      applyTrack(e.newValue);
    }
  });

  /* ============================================ attendance slide (ATT.4) ==
     <section class="slide s-attendance" data-attendance> shows the QR view
     of the attendance console, full-bleed, for an in-class attendance check.

     Three rules shape this and none of them are negotiable:
       1. The console is behind Connect login. A public "current QR" endpoint
          would hand the session token to anyone who opened the deck from
          home, so the deck iframes the SAME login-gated URL: it renders for
          the signed-in instructor projecting the deck, and shows a login
          page to anyone else. The deck NEVER carries a token.
       2. The iframe is lazy AND disposable — created when the slide becomes
          active, destroyed when it leaves. A deck must not hold a live
          console (and a live count) running behind 40 other slides.
       3. Under automation (navigator.webdriver) nothing is fetched, exactly
          like the WebR/Pyodide playground: the slide gate never reaches the
          network. `?attendance=1` in the deck URL forces the real iframe so
          a human (or a deliberate browser check) can prove the wiring.
     `a` on an attendance slide reloads the frame — for a session opened
     after the slide went up.
     ---------------------------------------------------------------------- */
  var ATT_URL = '';
  var attSlides = [].filter.call(slides, function (el) {
    return el.hasAttribute && el.hasAttribute('data-attendance');
  });

  function attSrc(el) { return el.getAttribute('data-attendance-src') || ATT_URL; }

  /* the same page without ?embed=1 — the full console, for the fallback link */
  function attOpenURL(el) {
    var raw = attSrc(el);
    var q = raw.indexOf('?');
    if (q < 0) return raw;
    var kept = raw.slice(q + 1).split('&').filter(function (p) {
      return p && p !== 'embed=1';
    });
    return kept.length ? raw.slice(0, q) + '?' + kept.join('&') : raw.slice(0, q);
  }

  function attAutomated() {
    try {
      if (/[?&]attendance=1(?:&|$)/.test(location.search)) return false;
      return !!navigator.webdriver;
    } catch (e) { return false; }
  }

  function attBuild() {
    attSlides.forEach(function (el) {
      if (el.querySelector('.att-stage')) return;
      var stage = document.createElement('div');
      stage.className = 'att-stage';

      var frame = document.createElement('div');
      frame.className = 'att-frame';

      var bar = document.createElement('div');
      bar.className = 'att-bar';

      var cap = document.createElement('div');
      cap.className = 'att-caption';
      cap.textContent = 'Attendance check — scan the code';

      var link = document.createElement('a');
      link.className = 'att-open';
      link.href = attOpenURL(el);
      link.target = '_blank';
      link.rel = 'noopener';
      link.textContent = 'Open the attendance console';

      var hint = document.createElement('div');
      hint.className = 'att-hint';
      hint.textContent = 'press a to refresh';

      bar.appendChild(cap);
      bar.appendChild(link);
      bar.appendChild(hint);
      stage.appendChild(frame);
      stage.appendChild(bar);
      el.appendChild(stage);
    });
  }

  function attMount(el) {
    var host = el.querySelector('.att-frame');
    if (!host || host.querySelector('iframe')) return;
    if (attAutomated()) {
      if (!host.querySelector('.att-offline')) {
        var note = document.createElement('div');
        note.className = 'att-offline';
        note.textContent = 'The attendance console loads here in class.';
        host.appendChild(note);
      }
      return;
    }
    var f = document.createElement('iframe');
    f.className = 'att-iframe';
    f.setAttribute('title', 'Attendance check — QR code');
    f.setAttribute('allow', 'fullscreen');
    f.setAttribute('loading', 'lazy');
    f.setAttribute('src', attSrc(el));
    host.appendChild(f);
  }

  function attUnmount(el) {
    var host = el.querySelector('.att-frame');
    if (!host) return;
    var f = host.querySelector('iframe');
    if (f && f.parentNode) f.parentNode.removeChild(f);
  }

  function attSync() {
    if (!attSlides.length) return;
    attSlides.forEach(function (el) {
      var live = !printing && !overview && el === slides[index];
      if (live) attMount(el); else attUnmount(el);
    });
  }

  /* `a` — throw the frame away and build a fresh one (a session just opened) */
  function attReload() {
    var el = slides[index];
    if (!el || !el.hasAttribute || !el.hasAttribute('data-attendance')) return false;
    if (printing || overview) return false;
    attUnmount(el);
    attMount(el);
    return true;
  }

  function boot() {
    harvestTOC();                 // BEFORE injectFooters: keep chrome out of the haystack
    injectFooters();
    attBuild();                   // after harvestTOC: the fallback link is chrome
    navButton('sg-prev', '‹', 'Previous slide', -1);
    navButton('sg-next', '›', 'Next slide', 1);
    buildProgress();
    buildTOC();
    buildPDFButton();
    wireLangPairs();
    show(fromHash(), false, 0);
    rescale();
    wake();
    if (printing) setPrint(true);
    // expose a tiny handle for the verifier / console driving
    window.COURSE_SLIDES = {
      get index() { return index; },
      get count() { return slides.length; },
      go: go, show: show,
      get printing() { return printing; },
      setPrint: setPrint,
      get overview() { return overview; },
      setOverview: setOverview,
      get toc() { return tocOpen; },
      setTOC: setTOC,
      get tocTab() { return tocTab; },
      setTocTab: setTocTab,
      get tocTitles() { return tocData.map(function (d) { return d.title; }); },
      get tocTree() {
        return tocData.map(function (d, i) {
          return { i: i, title: d.title, section: d.section, links: d.links.slice() };
        });
      },
      get track() { return body.getAttribute('data-slide-track') || 'r'; },
      setTrack: applyTrack,
      attendanceReload: attReload,
      downloadPDF: downloadPDF
    };
  }

  if (chapters.length && !window.COURSE_CONTRACT) {
    var s = document.createElement('script');
    s.src = '../assets/contract.js';
    s.onload = boot;
    s.onerror = function () { boot(); };          // silent degrade, no console noise
    document.head.appendChild(s);
  } else {
    boot();
  }

  if (deck) deck.setAttribute('data-slides', String(slides.length));
})();
