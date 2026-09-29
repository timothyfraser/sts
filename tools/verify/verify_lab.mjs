#!/usr/bin/env node
// verify_lab.mjs - the lab gate: the scripted definition of done for one interactive concept lab.
//
//   node tools/verify/verify_lab.mjs <id> [<id> ...]   check labs by golden id
//   node tools/verify/verify_lab.mjs --all             every golden file whose page exists under the site root's labs/
//   node tools/verify/verify_lab.mjs --self-test       run the fixture labs and assert each passes/fails as it should
//   options: --golden-dir DIR (default tests/labs/golden)  --site-root DIR (default docs-v3)
//            --shots-dir DIR (default tests/labs/shots)    --no-impeccable (skip check i; it then FAILS, never passes)
//
// Checks (see tests/labs/README.md):
//   a  loads with no console errors; performance mark lab:first-render under 1500 ms
//   b  every golden state: __lab.setState(state) then __lab.readouts() equals golden (abs tolerance)
//   c  every golden state: code panel text (DOM pane and __lab.code()) equals golden code
//   d  exactly three learning checks; each: 3 options, exactly one correct, choosing marks it, Hint and Reveal toggle
//   e  screenshots 390/1280 x light/dark + 390 light reduced-motion saved to <shots>/<id>/
//   f  axe: 0 serious or critical violations (390 and 1280, light and dark)
//   g  no horizontal page scroll at 390 (light and dark)
//   h  data from labs/data/ <= 500 KB; lab JS <= 150 KB excluding D3 (cdnjs) and motion (jsdelivr)
//   i  impeccable@4.1.0 detect --json report saved; 0 findings outside the golden ack list
//
// Output: one line per check result, `PASS|FAIL <id> <check> <state|-> <expected vs got>`,
// plus <shots>/<id>/gate.json. Exit 0 only if every check of every lab passes.
import http from 'node:http';
import fs from 'node:fs';
import path from 'node:path';
import { execFileSync } from 'node:child_process';
import { createRequire } from 'node:module';
import { fileURLToPath } from 'node:url';
import { chromium } from 'playwright';
import { cdnRoute } from './_serve.mjs';

const require = createRequire(import.meta.url);
const HERE = path.dirname(fileURLToPath(import.meta.url));
const REPO = path.resolve(HERE, '..', '..');
const AXE = fs.readFileSync(require.resolve('axe-core/axe.min.js'), 'utf8');

const LIMITS = { firstRenderMs: 1500, dataBytes: 500 * 1024, jsBytes: 150 * 1024 };
const EXCLUDED_JS = [/^https:\/\/cdnjs\.cloudflare\.com\/ajax\/libs\/d3\//, /^https:\/\/cdn\.jsdelivr\.net\/npm\/(motion|framer-motion|motion-dom|motion-utils)@/];
const SHOTS = [
  { name: '390-light', w: 390, theme: 'light' },
  { name: '390-dark', w: 390, theme: 'dark' },
  { name: '1280-light', w: 1280, theme: 'light' },
  { name: '1280-dark', w: 1280, theme: 'dark' },
  { name: '390-light-reduced', w: 390, theme: 'light', reduced: true },
];
const TYPES = { '.html': 'text/html; charset=utf-8', '.css': 'text/css', '.js': 'text/javascript', '.mjs': 'text/javascript',
  '.json': 'application/json', '.csv': 'text/csv', '.svg': 'image/svg+xml', '.png': 'image/png', '.jpg': 'image/jpeg',
  '.woff2': 'font/woff2', '.ico': 'image/x-icon' };

// ---------- helpers ----------
function serve(root) {
  return new Promise((resolve) => {
    const srv = http.createServer((req, res) => {
      const u = decodeURIComponent(new URL(req.url, 'http://x').pathname);
      let f = path.join(root, u);
      if (!f.startsWith(root)) { res.writeHead(403); return res.end(); }
      if (fs.existsSync(f) && fs.statSync(f).isDirectory()) f = path.join(f, 'index.html');
      if (!fs.existsSync(f)) { res.writeHead(404); return res.end('not found'); }
      res.writeHead(200, { 'content-type': TYPES[path.extname(f)] || 'application/octet-stream' });
      fs.createReadStream(f).pipe(res);
    });
    srv.listen(0, '127.0.0.1', () => resolve({ srv, base: `http://127.0.0.1:${srv.address().port}/` }));
  });
}
function launch() {
  const executablePath = process.env.CHROMIUM_PATH || (fs.existsSync('/opt/pw-browsers/chromium-1194/chrome-linux/chrome') ? '/opt/pw-browsers/chromium-1194/chrome-linux/chrome' : undefined);
  return chromium.launch({ executablePath, args: ['--no-sandbox'] });
}
const kb = (n) => (n < 1024 ? `${n} B` : `${(n / 1024).toFixed(1)} KB`);
const normCode = (s) => String(s ?? '').replace(/\r\n?/g, '\n').split('\n').map((l) => l.replace(/\s+$/, '')).join('\n').replace(/\n+$/, '');
const short = (v) => { const s = JSON.stringify(v); return s && s.length > 160 ? s.slice(0, 157) + '...' : s; };

// Resolve the golden `page` (repo-relative like docs-v3/labs/x.html, or site-relative like labs/x.html; may carry ?query).
function resolvePage(page, siteRoot) {
  const [p, q] = String(page).split(/(?=\?)/);
  for (const abs of [path.resolve(REPO, p), path.resolve(siteRoot, p)]) {
    if (abs.startsWith(siteRoot + path.sep) && fs.existsSync(abs)) return { abs, url: path.relative(siteRoot, abs).split(path.sep).join('/') + (q || '') };
  }
  return null;
}

// ---------- one lab ----------
export async function runLab(id, o) {
  const results = [];
  const rec = (ok, check, state, msg) => { results.push({ ok, check, state: state || '-', msg }); if (!o.quiet) console.log(`${ok ? 'PASS' : 'FAIL'} ${id} ${check} ${state || '-'} ${msg}`); };
  const outDir = path.join(o.shotsDir, id);
  fs.mkdirSync(outDir, { recursive: true });
  const finish = () => { fs.writeFileSync(path.join(outDir, 'gate.json'), JSON.stringify({ lab: id, ok: results.every((r) => r.ok), checked_at: new Date().toISOString(), results }, null, 2) + '\n'); return results; };

  const gPath = path.join(o.goldenDir, `${id}.json`);
  let golden;
  try { golden = JSON.parse(fs.readFileSync(gPath, 'utf8')); } catch (e) { rec(false, 'golden', null, `expected a readable JSON golden file at ${path.relative(REPO, gPath)} vs got ${e.message}`); return finish(); }
  const probs = [];
  if (!golden.page) probs.push('page');
  if (!Array.isArray(golden.states) || !golden.states.length) probs.push('states[] (non-empty)');
  for (const s of golden.states || []) if (!s.name || !s.state || !s.readouts || !s.code) probs.push(`state ${s.name || '?'} needs name/state/readouts/code`);
  if (probs.length) { rec(false, 'golden', null, `expected golden fields vs missing: ${probs.join(', ')}`); return finish(); }
  const tol = typeof golden.tolerance === 'number' ? golden.tolerance : 1e-6;
  const pg = resolvePage(golden.page, o.siteRoot);
  if (!pg) { rec(false, 'a', null, `expected page ${golden.page} to exist under ${path.relative(REPO, o.siteRoot)}/ vs got missing`); return finish(); }

  const { srv, base } = await serve(o.siteRoot);
  const browser = await launch();
  try {
    // ----- main context: a, b, c, d, h (1280 light, network log) -----
    const ctx = await browser.newContext({ viewport: { width: 1280, height: 900 }, colorScheme: 'light' });
    await cdnRoute(ctx);
    const page = await ctx.newPage();
    const errors = [];
    page.on('console', (m) => { if (m.type() === 'error') errors.push(`console: ${m.text()}${m.location() && m.location().url ? ` [${m.location().url}]` : ''}`); });
    page.on('pageerror', (e) => errors.push(`pageerror: ${e.message}`));
    const net = [];
    page.on('response', async (r) => {
      const entry = { url: r.url(), type: r.request().resourceType(), status: r.status(), bytes: 0 };
      net.push(entry);
      try { entry.bytes = (await r.body()).length; } catch { /* redirects / aborted */ }
    });
    await page.goto(base + pg.url, { waitUntil: 'load' });
    const hasLab = await page.waitForFunction(() => window.__lab && window.__lab.ready, null, { timeout: 10000 }).then(() => true, () => false);
    let readyErr = null;
    if (hasLab) readyErr = await page.evaluate(() => Promise.resolve(window.__lab.ready).then(() => null, (e) => String(e && e.message || e)));
    await page.waitForLoadState('networkidle').catch(() => {});
    const mark = await page.evaluate(() => { const m = performance.getEntriesByName('lab:first-render'); return m.length ? m[0].startTime : null; });

    // (a)
    if (!hasLab) rec(false, 'a', null, 'expected window.__lab with a ready promise within 10 s vs got none');
    else if (readyErr) rec(false, 'a', null, `expected __lab.ready to resolve vs got rejection: ${readyErr}`);
    if (errors.length) rec(false, 'a', null, `expected 0 console errors vs got ${errors.length}: ${short(errors.slice(0, 3))}`);
    if (mark == null) rec(false, 'a', null, 'expected performance mark lab:first-render vs got none');
    else if (mark >= LIMITS.firstRenderMs) rec(false, 'a', null, `expected first render < ${LIMITS.firstRenderMs} ms vs got ${mark.toFixed(0)} ms`);
    if (hasLab && !readyErr && !errors.length && mark != null && mark < LIMITS.firstRenderMs) rec(true, 'a', null, `0 console errors; first render ${mark.toFixed(0)} ms < ${LIMITS.firstRenderMs} ms`);

    // (b) + (c)
    for (const s of golden.states) {
      if (!hasLab) { rec(false, 'b', s.name, 'expected __lab vs got none'); rec(false, 'c', s.name, 'expected __lab vs got none'); continue; }
      const got = await page.evaluate(async (st) => {
        try {
          await window.__lab.setState(st);
          const panes = {};
          document.querySelectorAll('[data-lab-code] pre[data-code-pane]').forEach((p) => { const c = p.querySelector('code'); panes[p.dataset.codePane] = c ? c.textContent : null; });
          return { readouts: window.__lab.readouts(), code: window.__lab.code(), panes };
        } catch (e) { return { error: String(e && e.message || e) }; }
      }, s.state);
      if (got.error) { rec(false, 'b', s.name, `expected setState/readouts to run vs got error: ${got.error}`); rec(false, 'c', s.name, 'expected code panel vs got setState error'); continue; }
      const bad = [];
      for (const [k, want] of Object.entries(s.readouts)) {
        const have = got.readouts ? got.readouts[k] : undefined;
        if (typeof want === 'number') {
          if (typeof have !== 'number' || !Number.isFinite(have) || Math.abs(have - want) > tol) bad.push(`${k}: expected ${want} (tol ${tol}) vs got ${short(have)}${typeof have === 'number' ? ` (off by ${Math.abs(have - want).toExponential(2)})` : ''}`);
        } else if (have !== want) bad.push(`${k}: expected ${short(want)} vs got ${short(have)}`);
      }
      if (bad.length) for (const b of bad) rec(false, 'b', s.name, b);
      else rec(true, 'b', s.name, `${Object.keys(s.readouts).length} readouts match (tol ${tol})`);
      const cbad = [];
      for (const [lang, want] of Object.entries(s.code)) {
        const w = normCode(want);
        const pane = got.panes[lang];
        const api = got.code ? got.code[lang] : undefined;
        if (pane == null) cbad.push(`${lang}: expected pre[data-code-pane="${lang}"] > code vs got none`);
        else if (normCode(pane) !== w) cbad.push(`${lang} pane: expected ${short(w)} vs got ${short(normCode(pane))}`);
        if (api == null || normCode(api) !== w) cbad.push(`${lang} __lab.code(): expected ${short(w)} vs got ${short(api == null ? api : normCode(api))}`);
      }
      if (cbad.length) for (const b of cbad) rec(false, 'c', s.name, b);
      else rec(true, 'c', s.name, `code panel matches golden (${Object.keys(s.code).join(', ')})`);
    }

    // (d)
    const lc = await page.evaluate(async () => {
      const tick = () => new Promise((r) => setTimeout(r, 60));
      const out = [];
      const secs = [...document.querySelectorAll('section.lab-lc[data-lc]')];
      if (secs.length !== 3) out.push(`expected exactly 3 section.lab-lc[data-lc] vs got ${secs.length}`);
      for (const s of secs) {
        const n = `LC${s.dataset.lc}`;
        const opts = [...s.querySelectorAll('.lc-option')];
        const right = opts.filter((b) => b.dataset.correct === 'true');
        if (opts.length !== 3) out.push(`${n}: expected 3 .lc-option vs got ${opts.length}`);
        if (right.length !== 1) { out.push(`${n}: expected exactly 1 option with data-correct="true" vs got ${right.length}`); continue; }
        const wrong = opts.find((b) => b.dataset.correct !== 'true');
        if (wrong) { wrong.click(); await tick(); if (!(wrong.classList.contains('is-chosen') && wrong.classList.contains('is-wrong'))) out.push(`${n}: expected wrong option to get .is-chosen.is-wrong vs got class="${wrong.className}"`); }
        right[0].click(); await tick();
        if (!(right[0].classList.contains('is-chosen') && right[0].classList.contains('is-correct'))) out.push(`${n}: expected correct option to get .is-chosen.is-correct vs got class="${right[0].className}"`);
        for (const [btnSel, boxSel, label] of [['.lc-hint-btn', '.lc-hint', 'Hint'], ['.lc-reveal-btn', '.lc-answer', 'Reveal']]) {
          const btn = s.querySelector(btnSel), box = s.querySelector(boxSel);
          if (!btn || !box) { out.push(`${n}: expected ${btnSel} and ${boxSel} vs got ${btn ? '' : 'no button '}${box ? '' : 'no panel'}`); continue; }
          if (!box.hidden) out.push(`${n}: expected ${boxSel} hidden before ${label} vs got visible`);
          btn.click(); await tick();
          if (box.hidden || btn.getAttribute('aria-expanded') !== 'true') out.push(`${n}: expected ${label} to show ${boxSel} and set aria-expanded="true" vs got hidden=${box.hidden} aria-expanded=${btn.getAttribute('aria-expanded')}`);
          btn.click(); await tick();
          if (!box.hidden || btn.getAttribute('aria-expanded') !== 'false') out.push(`${n}: expected second ${label} click to hide ${boxSel} vs got hidden=${box.hidden} aria-expanded=${btn.getAttribute('aria-expanded')}`);
        }
      }
      return out;
    });
    if (lc.length) for (const m of lc) rec(false, 'd', null, m);
    else rec(true, 'd', null, '3 learning checks: options mark chosen/correct/wrong, exactly one correct each, Hint and Reveal toggle');

    // (h)
    await page.waitForTimeout(100);
    const inlineJs = await page.evaluate(() => [...document.querySelectorAll('script:not([src])')].reduce((a, s) => a + new Blob([s.textContent]).size, 0));
    const data = net.filter((r) => /\/labs\/data\//.test(new URL(r.url).pathname));
    const js = net.filter((r) => (r.type === 'script' || /\.m?js(\?|$)/.test(r.url)) && !EXCLUDED_JS.some((re) => re.test(r.url)) && !/\/labs\/data\//.test(r.url));
    const dataBytes = data.reduce((a, r) => a + r.bytes, 0);
    const jsBytes = js.reduce((a, r) => a + r.bytes, 0) + inlineJs;
    const hOk = dataBytes <= LIMITS.dataBytes && jsBytes <= LIMITS.jsBytes;
    if (dataBytes > LIMITS.dataBytes) rec(false, 'h', null, `expected data payload <= ${kb(LIMITS.dataBytes)} vs got ${kb(dataBytes)} over ${data.length} response(s)`);
    if (jsBytes > LIMITS.jsBytes) rec(false, 'h', null, `expected lab JS <= ${kb(LIMITS.jsBytes)} (excl. d3, motion) vs got ${kb(jsBytes)} (${js.length} file(s) + ${kb(inlineJs)} inline)`);
    if (hOk) rec(true, 'h', null, `data ${kb(dataBytes)} <= ${kb(LIMITS.dataBytes)}; JS ${kb(jsBytes)} <= ${kb(LIMITS.jsBytes)}`);
    fs.writeFileSync(path.join(outDir, 'network.json'), JSON.stringify(net, null, 2) + '\n');
    await ctx.close();

    // ----- (e) (f) (g): one context per shot -----
    const shotsMissing = [];
    for (const sh of SHOTS) {
      const c = await browser.newContext({ viewport: { width: sh.w, height: 900 }, colorScheme: sh.theme, reducedMotion: sh.reduced ? 'reduce' : 'no-preference' });
      await cdnRoute(c);
      const p = await c.newPage();
      await p.goto(base + pg.url, { waitUntil: 'load' });
      await p.evaluate((t) => document.documentElement.setAttribute('data-theme', t), sh.theme);
      await p.evaluate(() => (window.__lab && window.__lab.ready) || null).catch(() => {});
      await p.waitForTimeout(sh.reduced ? 50 : 300);
      const file = path.join(outDir, `${sh.name}.png`);
      await p.screenshot({ path: file, fullPage: true });
      if (!fs.existsSync(file) || fs.statSync(file).size === 0) shotsMissing.push(sh.name);
      if (!sh.reduced) {
        await p.addScriptTag({ content: AXE });
        const res = await p.evaluate(async () => await window.axe.run(document, { resultTypes: ['violations'] }));
        const bad = res.violations.filter((v) => v.impact === 'serious' || v.impact === 'critical');
        if (bad.length) for (const v of bad) rec(false, 'f', sh.name, `expected 0 serious/critical axe issues vs got ${v.impact} ${v.id}: ${v.help} (${v.nodes.length} node(s), e.g. ${v.nodes[0] && v.nodes[0].target.join(' ')})`);
        else rec(true, 'f', sh.name, '0 serious/critical axe issues');
      }
      if (sh.w <= 390 && !sh.reduced) {
        const sc = await p.evaluate(() => ({ sw: document.documentElement.scrollWidth, cw: document.documentElement.clientWidth }));
        if (sc.sw > sc.cw + 1) rec(false, 'g', sh.name, `expected no horizontal scroll at 390 vs got scrollWidth ${sc.sw} > clientWidth ${sc.cw}`);
        else rec(true, 'g', sh.name, `scrollWidth ${sc.sw} <= clientWidth ${sc.cw}`);
      }
      await c.close();
    }
    if (shotsMissing.length) rec(false, 'e', null, `expected 5 screenshots in ${path.relative(REPO, outDir)}/ vs missing ${shotsMissing.join(', ')}`);
    else rec(true, 'e', null, `5 screenshots in ${path.relative(REPO, outDir)}/ (${SHOTS.map((s) => s.name).join(', ')})`);
  } finally {
    await browser.close(); srv.close();
  }

  // ----- (i) impeccable -----
  if (o.noImpeccable) rec(false, 'i', null, 'expected impeccable report vs got skipped (--no-impeccable)');
  else {
    const reportPath = path.join(outDir, 'impeccable.json');
    let findings = null, err = null;
    const local = path.join(HERE, 'node_modules', '.bin', 'impeccable');
    const [cmd, args] = fs.existsSync(local) ? [local, []] : ['npx', ['-y', 'impeccable@4.1.0']];
    let stdout = '';
    try { stdout = execFileSync(cmd, [...args, 'detect', '--json', pg.abs], { cwd: REPO, encoding: 'utf8', stdio: ['ignore', 'pipe', 'pipe'], timeout: 180000 }); }
    catch (e) { if (e.status === 2 && e.stdout) stdout = e.stdout; else err = `impeccable exited ${e.status}: ${String(e.stderr || e.message).trim().split('\n')[0]}`; }
    if (!err) { try { findings = JSON.parse(stdout); } catch (e) { err = `unparseable JSON from impeccable: ${e.message}`; } }
    if (err) rec(false, 'i', null, `expected an impeccable report vs got ${err}`);
    else {
      const list = Array.isArray(findings) ? findings : (findings.findings || []);
      const ack = new Set((golden.ack || []).map((a) => a.rule));
      const counted = list.filter((f) => !(f.advisory === true || f.severity === 'advisory'));
      const unack = counted.filter((f) => !ack.has(f.antipattern || f.rule || f.id));
      fs.writeFileSync(reportPath, JSON.stringify({ tool: 'impeccable@4.1.0 detect --json', page: path.relative(REPO, pg.abs), ack: golden.ack || [], findings: list }, null, 2) + '\n');
      if (unack.length) for (const f of unack) rec(false, 'i', null, `expected 0 unacknowledged impeccable findings vs got ${f.antipattern || f.rule || f.id} (${f.name || ''}: ${f.snippet || ''}); add it to the golden ack with a reason, or fix it`);
      else rec(true, 'i', null, `${list.length} finding(s), ${counted.length - unack.length} acknowledged, 0 unacknowledged; report ${path.relative(REPO, reportPath)}`);
    }
  }
  return finish();
}

// ---------- CLI ----------
const isMain = process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url);
if (isMain) {
  const argv = process.argv.slice(2);
  const opt = (name, dflt) => { const i = argv.indexOf(name); if (i < 0) return dflt; const v = argv[i + 1]; argv.splice(i, 2); return v; };
  const flag = (name) => { const i = argv.indexOf(name); if (i < 0) return false; argv.splice(i, 1); return true; };
  const selfTest = flag('--self-test');
  const all = flag('--all');
  const noImpeccable = flag('--no-impeccable');
  const o = {
    goldenDir: path.resolve(REPO, opt('--golden-dir', selfTest ? 'tests/labs/fixtures/golden' : 'tests/labs/golden')),
    siteRoot: path.resolve(REPO, opt('--site-root', selfTest ? 'tests/labs/fixtures/site' : 'docs-v3')),
    shotsDir: path.resolve(REPO, opt('--shots-dir', selfTest ? 'tests/labs/shots/_fixtures' : 'tests/labs/shots')),
    noImpeccable,
  };
  const ids = () => (fs.existsSync(o.goldenDir) ? fs.readdirSync(o.goldenDir) : []).filter((f) => f.endsWith('.json')).map((f) => f.slice(0, -5)).sort();

  if (selfTest) {
    // Each fixture golden carries expect_fail: [] (must pass every check) or [<check>] (must fail that check and only that one).
    let bad = 0;
    for (const id of ids()) {
      const g = JSON.parse(fs.readFileSync(path.join(o.goldenDir, `${id}.json`), 'utf8'));
      const want = new Set(g.expect_fail || []);
      const res = await runLab(id, { ...o, quiet: true });
      const failed = new Set(res.filter((r) => !r.ok).map((r) => r.check));
      const ok = [...want].every((c) => failed.has(c)) && [...failed].every((c) => want.has(c));
      if (!ok) bad++;
      const firstFail = res.find((r) => !r.ok);
      console.log(`${ok ? 'OK  ' : 'BAD '} self-test ${id}: expected fail [${[...want].join(',')}] vs got fail [${[...failed].join(',')}]${firstFail ? `  e.g. FAIL ${firstFail.check} ${firstFail.state} ${firstFail.msg}` : ''}`);
    }
    console.log(`verify_lab --self-test: ${ids().length - bad}/${ids().length} fixtures behave as expected`);
    process.exit(bad || !ids().length ? 1 : 0);
  }

  let list = argv.filter((a) => !a.startsWith('--'));
  let orphanCount = 0;
  if (all) {
    list = ids().filter((id) => {
      try { const g = JSON.parse(fs.readFileSync(path.join(o.goldenDir, `${id}.json`), 'utf8')); const pg = resolvePage(g.page, o.siteRoot); return pg && /(^|\/)labs\//.test(pg.url); }
      catch { return true; } // unreadable golden: include it so it FAILS loudly
    });
    // A lab page (body data-lab="...") under <site>/labs/ that no golden file points at fails: every lab is gated.
    const labsDir = path.join(o.siteRoot, 'labs');
    const covered = new Set(ids().map((id) => { try { const pg = resolvePage(JSON.parse(fs.readFileSync(path.join(o.goldenDir, `${id}.json`), 'utf8')).page, o.siteRoot); return pg && pg.abs; } catch { return null; } }));
    const orphans = (fs.existsSync(labsDir) ? fs.readdirSync(labsDir) : []).filter((f) => f.endsWith('.html'))
      .map((f) => path.join(labsDir, f)).filter((f) => /<body[^>]*\sdata-lab=/.test(fs.readFileSync(f, 'utf8')) && !covered.has(f));
    for (const f of orphans) console.log(`FAIL ${path.basename(f, '.html')} golden - expected ${path.relative(REPO, o.goldenDir)}/<id>.json pointing at ${path.relative(REPO, f)} vs got no golden file`);
    if (orphans.length && !list.length) { console.log(`verify_lab --all: ${orphans.length} lab page(s) without a golden file`); process.exit(1); }
    orphanCount = orphans.length;
    if (!list.length) { console.log(`verify_lab --all: no golden files with a page under ${path.relative(REPO, o.siteRoot)}/labs/ in ${path.relative(REPO, o.goldenDir)}/ (nothing to check)`); process.exit(0); }
  }
  if (!list.length) { console.error('usage: verify_lab.mjs <id> [...] | --all | --self-test  [--golden-dir D] [--site-root D] [--shots-dir D]'); process.exit(2); }
  let failedLabs = 0;
  for (const id of list) { const r = await runLab(id, o); if (r.some((x) => !x.ok)) failedLabs++; }
  console.log(`verify_lab: ${list.length - failedLabs}/${list.length} labs pass every check${orphanCount ? `; ${orphanCount} lab page(s) without a golden file` : ''}`);
  process.exit(failedLabs || orphanCount ? 1 : 0);
}
