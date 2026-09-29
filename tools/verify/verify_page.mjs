#!/usr/bin/env node
// verify_page.mjs <page.html ...> | --all
// Headless Chromium gate for docs-v3 pages. For each page it checks:
//   P1 loads over http with status 200
//   P2 no console error and no uncaught page error
//   P3 every same-origin request succeeds (no 404 asset, image, script)
//   P4 archetype pages carry the <!-- ARCHETYPE: ... --> comment
//   P5 every [data-site-text] slot renders non-empty text from the contract
//   P6 every [data-site-href] resolves to a non-placeholder href
//   P7 the page source types no hex colour and no course number (tokens only)
//   P8 document.title is non-empty
// Exit 0 when every page passes; 1 otherwise, naming page, check and expectation.
import fs from 'node:fs';
import path from 'node:path';
import { serve, launch, rel, archetypePages, ROOT, REPO } from './_serve.mjs';

const args = process.argv.slice(2);
const pages = args.includes('--all') || !args.length ? archetypePages() : args.map(rel);
const site = JSON.parse(fs.readFileSync(path.join(REPO, 'contract', 'site.json'), 'utf8'));
const courseNumber = site.course && site.course.number;
const lookup = (k) => k.split('.').reduce((o, x) => (o == null ? o : o[x]), site);

const { srv, base } = await serve();
const browser = await launch();
let failed = 0;
for (const p of pages) {
  const fails = [];
  const ctx = await browser.newContext({ viewport: { width: 1280, height: 900 } });
  const page = await ctx.newPage();
  page.on('console', (m) => { if (m.type() === 'error') fails.push(`P2 console error: ${m.text()}`); });
  page.on('pageerror', (e) => fails.push(`P2 page error: ${e.message}`));
  page.on('response', (r) => {
    if (r.url().startsWith(base) && r.status() >= 400) fails.push(`P3 ${r.status()} for ${r.url().slice(base.length)}`);
  });
  page.on('requestfailed', (r) => { if (r.url().startsWith(base)) fails.push(`P3 request failed: ${r.url().slice(base.length)}`); });
  const resp = await page.goto(base + p, { waitUntil: 'load' });
  if (!resp || resp.status() !== 200) fails.push(`P1 expected 200, got ${resp && resp.status()}`);
  await page.waitForTimeout(300);
  const src = fs.readFileSync(path.join(ROOT, p), 'utf8');
  if (!p.endsWith('_template.html') && !src.includes('<!-- ARCHETYPE:')) fails.push('P4 missing <!-- ARCHETYPE: ... --> comment');
  const r = await page.evaluate(() => ({
    empty: [...document.querySelectorAll('[data-site-text]')].filter((e) => !e.textContent.trim()).map((e) => e.getAttribute('data-site-text')),
    hrefs: [...document.querySelectorAll('[data-site-href]')].filter((e) => { const h = e.getAttribute('href') || ''; return !h || h === '#' || /[<>]/.test(h); }).map((e) => e.getAttribute('data-site-href')),
    title: document.title,
  }));
  // A slot whose contract field is itself still empty is an OPEN course fact, reported
  // but not failed; a slot that stays empty although the contract has a value is a FAIL.
  const open = [];
  for (const s of r.empty) (lookup(s) ? fails : open).push(`P5 slot data-site-text="${s}" rendered empty${lookup(s) ? ' although contract/site.json has a value' : ' (contract/site.json field is empty)'}`);
  for (const s of new Set(r.hrefs)) (lookup(s) && !/^</.test(lookup(s)) ? fails : open).push(`P6 slot data-site-href="${s}" has no usable href${lookup(s) ? '' : ' (contract/site.json field is empty)'}`);
  for (const o of open) console.log(`  OPEN ${p}: ${o}`);
  const noComments = src.replace(/<!--[\s\S]*?-->/g, '').replace(/src="data:[^"]*"/g, '');
  const hex = noComments.match(/#[0-9a-fA-F]{6}\b/g);
  if (hex) fails.push(`P7 typed hex colour(s) ${[...new Set(hex)].join(', ')} (use var(--*) tokens)`);
  if (courseNumber && noComments.includes(courseNumber)) fails.push(`P7 course number "${courseNumber}" typed into the page (use data-site-text="course.number")`);
  if (!r.title.trim()) fails.push('P8 document.title is empty');
  await ctx.close();
  if (fails.length) { failed++; console.log(`FAIL ${p}`); for (const f of fails) console.log(`  ${p}: ${f}`); }
  else console.log(`PASS ${p}`);
}
await browser.close(); srv.close();
console.log(`verify_page: ${pages.length - failed}/${pages.length} pages pass`);
process.exit(failed ? 1 : 0);
