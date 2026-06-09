import { getProvider, MODEL_REGISTRY } from "theseus-core";
import { z } from "zod";
import dotenv from "dotenv";
dotenv.config();

const schema = z.object({ ok: z.string() });

const combos: Array<
  ["OPENAI" | "GEMINI" | "DEEPSEEK" | "CLAUDE", "FLAGSHIP" | "LIGHT"]
> = [
  ["OPENAI", "FLAGSHIP"],
  ["OPENAI", "LIGHT"],
  ["GEMINI", "FLAGSHIP"],
  ["GEMINI", "LIGHT"],
  ["CLAUDE", "FLAGSHIP"],
  ["CLAUDE", "LIGHT"],
  ["DEEPSEEK", "FLAGSHIP"],
  ["DEEPSEEK", "LIGHT"],
];

async function main() {
  let pass = 0;
  for (const [v, s] of combos) {
    const id = MODEL_REGISTRY[v][s].modelId;
    const t0 = Date.now();
    try {
      const out = await getProvider(v, s).generateStructured(schema, {
        prompt: 'Return JSON with field "ok" set to the string "OK".',
        maxRetries: 0,
      });
      pass++;
      console.log(`PASS  ${v}/${s}  [${id}]  ${Date.now() - t0}ms  -> ${JSON.stringify(out)}`);
    } catch (e: any) {
      const msg = (e?.message || String(e)).split("\n")[0].slice(0, 220);
      console.log(`FAIL  ${v}/${s}  [${id}]  ${Date.now() - t0}ms  -> ${msg}`);
    }
  }
  console.log(`\n${pass}/${combos.length} providers reachable.`);
}

main();
