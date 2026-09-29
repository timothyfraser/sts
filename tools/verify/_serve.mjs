// Shared helpers: a tiny static server over docs-v3/ and a Chromium launcher.
import http from 'node:http';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { chromium } from 'playwright';

export const REPO = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..', '..');
export const ROOT = path.join(REPO, 'docs-v3');
const TYPES = { '.html': 'text/html; charset=utf-8', '.css': 'text/css', '.js': 'text/javascript',
  '.json': 'application/json', '.svg': 'image/svg+xml', '.png': 'image/png', '.jpg': 'image/jpeg',
  '.woff2': 'font/woff2', '.ico': 'image/x-icon' };

export function serve() {
  return new Promise((resolve) => {
    const srv = http.createServer((req, res) => {
      const u = decodeURIComponent(new URL(req.url, 'http://x').pathname);
      let f = path.join(ROOT, u);
      if (!f.startsWith(ROOT)) { res.writeHead(403); return res.end(); }
      if (fs.existsSync(f) && fs.statSync(f).isDirectory()) f = path.join(f, 'index.html');
      if (!fs.existsSync(f)) { res.writeHead(404); return res.end('not found'); }
      res.writeHead(200, { 'content-type': TYPES[path.extname(f)] || 'application/octet-stream' });
      fs.createReadStream(f).pipe(res);
    });
    srv.listen(0, '127.0.0.1', () => resolve({ srv, base: `http://127.0.0.1:${srv.address().port}/` }));
  });
}

// Behind a TLS-intercepting proxy (cloud sessions) Chromium does not trust the proxy CA,
// while Node does (NODE_EXTRA_CA_CERTS). So when a proxy is set, CDN requests are fetched by
// Node and fulfilled into the page. With no proxy (CI, laptops) the browser fetches directly.
const CDN = /^https:\/\/(cdnjs\.cloudflare\.com|cdn\.jsdelivr\.net|unpkg\.com)\//;
export async function cdnRoute(ctx) {
  if (!(process.env.HTTPS_PROXY || process.env.https_proxy)) return;
  await ctx.route(CDN, async (route) => {
    try {
      const r = await fetch(route.request().url());
      const body = Buffer.from(await r.arrayBuffer());
      await route.fulfill({ status: r.status, body,
        headers: { 'content-type': r.headers.get('content-type') || 'application/octet-stream',
                   'access-control-allow-origin': '*' } });
    } catch (e) { await route.abort(); }
  });
}

export async function launch() {
  const executablePath = process.env.CHROMIUM_PATH || '/opt/pw-browsers/chromium-1194/chrome-linux/chrome';
  const browser = await chromium.launch({ executablePath, args: ['--no-sandbox'] });
  const newContext = browser.newContext.bind(browser);
  browser.newContext = async (opts) => { const ctx = await newContext(opts); await cdnRoute(ctx); return ctx; };
  browser.newPage = async (opts) => { const ctx = await browser.newContext(opts); const pg = await ctx.newPage(); pg.close = async (o) => { await ctx.close(o); }; return pg; };
  return browser;
}

// Accepts "docs-v3/x.html", "x.html" or an absolute path; returns the docs-v3-relative path.
export function rel(p) {
  const abs = path.resolve(REPO, p);
  const r = abs.startsWith(ROOT + path.sep) ? path.relative(ROOT, abs) : path.relative(ROOT, path.resolve(ROOT, p));
  if (!fs.existsSync(path.join(ROOT, r))) throw new Error(`no such page under docs-v3/: ${p}`);
  return r.split(path.sep).join('/');
}

// Every page under docs-v3 that carries an ARCHETYPE comment (or is the deck template).
export function archetypePages() {
  const out = [];
  (function walk(d) {
    for (const e of fs.readdirSync(d, { withFileTypes: true })) {
      const f = path.join(d, e.name);
      if (e.isDirectory()) { if (e.name !== 'assets') walk(f); continue; }
      if (!e.name.endsWith('.html')) continue;
      const t = fs.readFileSync(f, 'utf8');
      if (t.includes('<!-- ARCHETYPE:') || e.name === '_template.html') out.push(path.relative(ROOT, f).split(path.sep).join('/'));
    }
  })(ROOT);
  return out.sort();
}
