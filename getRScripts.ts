import path from "node:path";
import fs from "node:fs/promises";

import { loadFile, type ModulePair } from "./loadFile";
import { StudyDTO } from "./flatten";
import { defaultDTO } from "./defaultDTO";
import { json2strategus } from "./json2strategus";

// --- parse CLI args ---
function parseArgs() {
  const args = process.argv.slice(2);
  const argMap: Record<string, string> = {};
  for (const a of args) {
    const [k, v] = a.split("=");
    if (k && v) argMap[k.replace(/^--/, "").toLowerCase()] = v.toUpperCase();
  }

  const vendor = argMap["vendor"];
  const size = argMap["size"];
  if (!vendor || !size) {
    console.error(
      "❌ Usage: ts-node src/getRScripts.ts --vendor=OPENAI|GEMINI|DEEPSEEK|CLAUDE --size=FLAGSHIP|LIGHT"
    );
    process.exit(1);
  }

  const supportedVendors = ["OPENAI", "GEMINI", "DEEPSEEK", "CLAUDE"];
  const supportedSizes = ["FLAGSHIP", "LIGHT"];
  if (!supportedVendors.includes(vendor) || !supportedSizes.includes(size)) {
    console.error(
      `❌ Invalid vendor/size. Supported vendors: ${supportedVendors.join(", ")} / sizes: ${supportedSizes.join(", ")}`
    );
    process.exit(1);
  }

  return { vendor, size };
}

const { vendor, size } = parseArgs();

// ✅ vendor/size별 저장 폴더 지정
const RS_DIR = path.resolve(process.cwd(), "public", `RScripts_${vendor}_${size}`);

// 파일명용 slug
function slugify(s: string) {
  return (
    s
      .trim()
      .toLowerCase()
      .replace(/[\s/\\]+/g, "-")
      .replace(/[^a-z0-9-_]/g, "")
      .replace(/-+/g, "-")
      .replace(/^-|-$/g, "") || "case"
  );
}

async function ensureDir(abs: string) {
  await fs.mkdir(abs, { recursive: true });
}

/** 기본값 병합 (fillWithDefaults) */
function fillWithDefaults<T>(base: T, gold: any): T {
  if (gold === undefined) return base;
  if (gold === null) return gold as T;

  if (Array.isArray(base)) {
    if (Array.isArray(gold)) return gold as T;
    return base;
  }

  if (typeof base === "object" && base !== null) {
    const out: any = {};
    const keys = new Set([...Object.keys(base as any), ...Object.keys(gold ?? {})]);
    for (const k of keys) {
      const bVal = (base as any)[k];
      const gHas = Object.prototype.hasOwnProperty.call(gold, k);
      const gVal = gHas ? (gold as any)[k] : undefined;
      if (gHas) {
        if (
          bVal &&
          typeof bVal === "object" &&
          !Array.isArray(bVal) &&
          gVal &&
          typeof gVal === "object" &&
          !Array.isArray(gVal)
        ) {
          out[k] = fillWithDefaults(bVal, gVal);
        } else if (Array.isArray(bVal)) {
          out[k] = Array.isArray(gVal) ? gVal : bVal;
        } else {
          out[k] = gVal;
        }
      } else {
        out[k] = bVal;
      }
    }
    return out as T;
  }

  return gold as T;
}


type PerCase = {
  name: string;
  fileName: string;
  createdAt: string;
  savedPath: string;
};

export async function getRScripts() {
  const { vendor, size } = parseArgs();

  const pairs: ModulePair[] = await loadFile();
  if (!pairs.length) console.warn("[WARN] No module pairs found.");

  await ensureDir(RS_DIR);

  const results: PerCase[] = [];

  for (const p of pairs) {
    const caseSlug = slugify(p.name);
    const outName = `${caseSlug}.R`;
    const outPath = path.join(RS_DIR, outName);

    try {
      const dto: StudyDTO = fillWithDefaults<StudyDTO>(defaultDTO, p.goldJson);
      dto.name = caseSlug;
      dto.cohortDefinitions = {
        targetCohort: { id: 1794126, name: "target1" },
        comparatorCohort: { id: 1794132, name: "comparator1" },
        outcomeCohort: [{ id: 1794131, name: "outcome1" }],
      };
      dto.negativeControlConceptSet = { id: 1888110, name: "negative" };

      const script = await json2strategus(JSON.stringify(dto, null, 2), { vendor:vendor as any , size:size as any });

      await fs.writeFile(outPath, script, "utf8");

      results.push({
        name: p.name,
        fileName: outName,
        createdAt: new Date().toISOString(),
        savedPath: path.relative(process.cwd(), outPath),
      });

      console.log(`[OK] Saved R script: ${outName}`);
    } catch (err) {
      console.error(`[ERROR] ${p.name}:`, err);
    }
  }

  const indexPath = path.join(RS_DIR, "_summary.index.json");
  await fs.writeFile(
    indexPath,
    JSON.stringify({ createdAt: new Date().toISOString(), totalCases: results.length, results }, null, 2),
    "utf8"
  );

  console.log(`[DONE] ${results.length} scripts saved. Summary → ${indexPath}`);
}

// 단독 실행
if (require.main === module) {
  getRScripts().catch((e) => {
    console.error(e);
    process.exit(1);
  });
}
