#!/usr/bin/env node
// verify_visual.mjs <page.html ...> | --all  [--widths 390,1280] [--themes light,dark] [--out DIR]
// For each page x width x theme: writes a full-page screenshot, runs axe-core and
// fails on any serious or critical violation, and (at widths <= 390) fails on
// horizontal page scroll. Screenshots default to tools/verify/shots/ (git-ignored).
import fs from 'node:fs';
import path from 'node:path';
import { createRequire } from 'node:module';
import { serve, launch, rel, archetypePages, REPO } from './_serve.mjs';

const require = createRequire(import.meta.url);
const AXE = fs.readFileSync(require.resolve('axe-core/axe.min.js'), 'utf8');
const argv = process.argv.slice(2);
function opt(name, dflt) { const i = argv.indexOf(name); if (i < 0) return dflt; const v = argv[i + 1]; argv.splice(i, 2); return v; }
const widths = opt('--widths', '390,1280').split(',').map(Number);
const themes = opt('--themes', 'light,dark').split(',');
const outDir = path.resolve(REPO, opt('--out', 'tools/verify/shots'));
const all = argv.includes('--all') || !argv.filter((a) => a !== '--all').length;
const pages = all ? archetypePages() : argv.filter((a) => a !== '--all').map(rel);
fs.mkdirSync(outDir, { recursive: true });

const { srv, base } = await serve();
const browser = await launch();
let failed = 0, runs = 0;
for (const p of pages) for (const w of widths) for (const theme of themes) {
  runs++;
  const tag = `${p} @${w} ${theme}`;
  const fails = [];
  const ctx = await browser.newContext({ viewport: { width: w, height: 900 }, colorScheme: theme });
  const page = await ctx.newPage();
  await page.goto(base + p, { waitUntil: 'load' });
  await page.evaluate((t) => document.documentElement.setAttribute('data-theme', t), theme);
  await page.waitForTimeout(250);
  const shot = path.join(outDir, `${p.replace(/[\/]/g, '__').replace(/\.html$/, '')}-${w}-${theme}.png`);
  await page.screenshot({ path: shot, fullPage: true });
  if (w <= 390) {
    const sc = await page.evaluate(() => ({ sw: document.documentElement.scrollWidth, cw: document.documentElement.clientWidth }));
    if (sc.sw > sc.cw + 1) fails.push(`horizontal scroll: scrollWidth ${sc.sw} > clientWidth ${sc.cw}`);
  }
  await page.addScriptTag({ content: AXE });
  const res = await page.evaluate(async () => await window.axe.run(document, { resultTypes: ['violations'] }));
  for (const v of res.violations.filter((v) => v.impact === 'serious' || v.impact === 'critical')) {
    fails.push(`axe ${v.impact} ${v.id}: ${v.help} (${v.nodes.length} node(s), e.g. ${v.nodes[0] && v.nodes[0].target.join(' ')})`);
  }
  await ctx.close();
  if (fails.length) { failed++; console.log(`FAIL ${tag}  shot=${path.relative(REPO, shot)}`); for (const f of fails) console.log(`  ${tag}: ${f}`); }
  else console.log(`PASS ${tag}  shot=${path.relative(REPO, shot)}`);
}
await browser.close(); srv.close();
console.log(`verify_visual: ${runs - failed}/${runs} runs pass (${pages.length} pages x ${widths.join(',')} x ${themes.join(',')})`);
process.exit(failed ? 1 : 0);
