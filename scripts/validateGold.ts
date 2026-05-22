import { loadFile } from '../loadFile.ts';

async function main() {
  const core = await import('/Users/minseongkim/Desktop/youlab/theseus/theseus-evaluation/node_modules/theseus-core/dist/index.js');
  const validateSpec = core.validateSpec;
  
  let pass = 0, fail = 0;
  const fails: string[] = [];
  
  // For pdf type: directly load the json files without loadFile's PDF check
  // by loading them via the module directly
  const fs = await import('node:fs');
  const path = await import('node:path');
  
  // default + method via loadFile
  for (const type of ['default', 'method']) {
    process.env.GOLD_STANDARD_DIR = 'public/newGoldStandard';
    const pairs = await loadFile(type.toUpperCase());
    for (const pair of pairs) {
      const r = validateSpec(pair.goldJson);
      if (r.ok) {
        pass++;
      } else {
        fail++;
        const errStr = r.errors.slice(0, 5).join('; ');
        fails.push(`${type}/${pair.name}: ${errStr}`);
      }
    }
  }
  
  // pdf via direct import (loadFile skips them due to missing papers/)
  const pdfDir = path.resolve(process.cwd(), 'public/newGoldStandard/pdf');
  for (const file of fs.readdirSync(pdfDir).filter((f: string) => f.endsWith('.ts'))) {
    const { pathToFileURL } = await import('node:url');
    const url = pathToFileURL(path.join(pdfDir, file)).href;
    const mod = await import(url);
    const allExports = mod.default ?? mod;
    const jsonKey = Object.keys(allExports).find((k: string) => k.startsWith('JSON'));
    if (!jsonKey) { console.warn(`skip pdf/${file}: no JSON* key`); continue; }
    const r = validateSpec(allExports[jsonKey]);
    if (r.ok) {
      pass++;
    } else {
      fail++;
      const errStr = r.errors.slice(0, 5).join('; ');
      fails.push(`pdf/${file}: ${errStr}`);
    }
  }
  
  console.log('PASS', pass, 'FAIL', fail);
  fails.slice(0, 20).forEach(x => console.log(x));
}

main().catch(e => { console.error(e); process.exit(1); });
