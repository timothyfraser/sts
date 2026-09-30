// scripts/ship-check.mjs: `npm run ship-check`. Refuses (exit 1) while fake data could ship.
//
// Three checks, each printed PASS or FAIL:
//   1. The stage a production build would use is "real" (VITE_DATA_STAGE from the
//      environment, else .env.production.local, .env.local, .env.production, .env).
//   2. No file under src/ outside src/data/ imports mockup.js or fake.js directly
//      (the switch in src/data/index.js must stay the only way in).
//   3. A fresh production build contains neither STS_MOCKUP_DATA nor STS_FAKE_DATA,
//      the tags mockup.js and fake.js carry. This is the proof: it checks what
//      would actually be deployed, not what the code says.
import { readFileSync, readdirSync, existsSync, statSync } from 'node:fs';
import { join, relative } from 'node:path';
import { execSync } from 'node:child_process';

const root = new URL('..', import.meta.url).pathname;
let failed = 0;
const pass = (m) => console.log(`PASS  ${m}`);
const fail = (m) => { failed += 1; console.log(`FAIL  ${m}`); };

// 1. Resolve the production stage the way Vite does (process env wins).
function stageFromFiles() {
  for (const f of ['.env.production.local', '.env.local', '.env.production', '.env']) {
    const p = join(root, f);
    if (!existsSync(p)) continue;
    const m = /^\s*VITE_DATA_STAGE\s*=\s*["']?([a-z]+)/m.exec(readFileSync(p, 'utf8'));
    if (m) return { stage: m[1], from: f };
  }
  return null;
}
const resolved = process.env.VITE_DATA_STAGE
  ? { stage: process.env.VITE_DATA_STAGE, from: 'the environment' }
  : stageFromFiles() || { stage: 'mockup', from: 'the default (VITE_DATA_STAGE unset)' };
if (resolved.stage === 'real') pass(`stage is "real" (from ${resolved.from})`);
else fail(`stage is "${resolved.stage}" (from ${resolved.from}); set VITE_DATA_STAGE=real to ship`);

// 2. No direct mock imports outside the data layer.
function walk(dir) {
  return readdirSync(dir).flatMap((n) => {
    const p = join(dir, n);
    return statSync(p).isDirectory() ? walk(p) : [p];
  });
}
const offenders = walk(join(root, 'src'))
  .filter((p) => !p.includes(`${join('src', 'data')}`) && /\.(jsx?|tsx?)$/.test(p))
  .filter((p) => /from\s+['"][^'"]*(mockup|fake)(\.js)?['"]|import\(\s*['"][^'"]*(mockup|fake)/.test(readFileSync(p, 'utf8')));
if (offenders.length) fail(`mock imports outside src/data/: ${offenders.map((p) => relative(root, p)).join(', ')}`);
else pass('no mock imports outside src/data/');

// 3. Build with the resolved stage and search what would be deployed.
try {
  execSync('npx vite build --logLevel error', { cwd: root, stdio: 'inherit', env: { ...process.env, VITE_DATA_STAGE: resolved.stage } });
  const hits = walk(join(root, 'dist'))
    .filter((p) => /\.(js|html|json)$/.test(p))
    .filter((p) => /STS_(MOCKUP|FAKE)_DATA/.test(readFileSync(p, 'utf8')));
  if (hits.length) fail(`fake or mockup data in the build: ${hits.map((p) => relative(root, p)).join(', ')}`);
  else pass('the build contains no mockup or fake data');
} catch (e) {
  fail(`vite build failed: ${e.message}`);
}

console.log(failed ? `\nSHIP CHECK REFUSED (${failed} failing check${failed > 1 ? 's' : ''}). Do not deploy.` : '\nSHIP CHECK PASSED. Safe to deploy.');
process.exit(failed ? 1 : 0);
