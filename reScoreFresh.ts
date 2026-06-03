// Offline re-score (NO API). Reloads CURRENT gold from .ts files, re-pairs with saved predJson,
// applies acceptable-answers layer + minDaysAtRisk exclusion, rewrites per-case + _summary.index.json.
// DRY_RUN=1 -> print before/after only, write nothing.
import fs from "node:fs";
import path from "node:path";
import { flattenStudy, StudyDTO } from "./flatten";
import { evaluateFlat } from "./evaluate";
import { loadFile } from "./loadFile";

const DRY = process.env.DRY_RUN === "1";
const vendors = ["openai", "gemini", "claude", "deepseek"];
const sizes = ["flagship", "light"];

type SecKey = "studyPeriods" | "timeAtRisks" | "propensityScoreAdjustment" | "outcomeModels";
const SECTIONS: SecKey[] = ["studyPeriods", "timeAtRisks", "propensityScoreAdjustment", "outcomeModels"];

// NOTE: method/pdf gold는 구식(legacy) cmAnalysis 구조(중첩 propensityScoreAdjustment.psSettings,
// 단수 fitOutcomeModelArgs.modelType, priorOutcomeLookBack)라 통합 flattenStudy/evaluateFlat로
// 채점하면 gold fact가 붕괴해 점수가 망가진다(예: 0.92->0.00). 이 통합 파이프라인은 신규 구조
// (default/primary/primary_augmented)만 지원하므로 method/pdf는 제외한다(기존 점수 보존).
const DIRS: { ohdsi: boolean; type: string; path: string }[] = [
  { ohdsi: true,  type: "DEFAULT",           path: "public/results/ohdsi/default" },
  { ohdsi: true,  type: "PRIMARY",           path: "public/results/ohdsi/primary" },
  { ohdsi: true,  type: "PRIMARY_AUGMENTED", path: "public/results/ohdsi/primary_augmented" },
  { ohdsi: false, type: "DEFAULT",           path: "public/results/non-ohdsi/default" },
  { ohdsi: false, type: "PRIMARY",           path: "public/results/non-ohdsi/primary" },
];

function sectionAgg(results: any[], key: SecKey) {
  let evaluated = 0, trueCount = 0;
  for (const c of results) {
    const v = c.sectionAccuracy?.[key] ?? null;
    if (v !== null) { evaluated++; if (v === true) trueCount++; }
  }
  return { evaluated, trueCount, falseCount: evaluated - trueCount, accuracy: evaluated ? trueCount / evaluated : null };
}

async function loadGoldMap(ohdsi: boolean, type: string): Promise<Map<string, any>> {
  const prev = process.env.GOLD_STANDARD_DIR;
  process.env.GOLD_STANDARD_DIR = ohdsi ? "public/goldStandard" : "public/goldStandardTest";
  try {
    const pairs = await loadFile(type);
    const m = new Map<string, any>();
    for (const p of pairs) m.set(p.name, p.goldJson);
    return m;
  } finally {
    if (prev === undefined) delete process.env.GOLD_STANDARD_DIR;
    else process.env.GOLD_STANDARD_DIR = prev;
  }
}

async function main() {
  for (const d of DIRS) {
    const goldMap = await loadGoldMap(d.ohdsi, d.type);
    for (const v of vendors) for (const s of sizes) {
      const dir = path.join(d.path, `${v}_${s}`);
      const idxPath = path.join(dir, "_summary.index.json");
      if (!fs.existsSync(idxPath)) continue;
      const idx = JSON.parse(fs.readFileSync(idxPath, "utf8"));
      const oldSP = idx.sectionAccuracySummary?.studyPeriods;
      const oldFL = idx.fieldLevelMetrics;

      const newResults: any[] = [];
      for (const c of idx.results) {
        let goldJson = goldMap.get(c.name);
        if (!goldJson) { console.warn(`[WARN] no fresh gold for ${c.name} in ${dir}; using saved goldJson`); goldJson = c.goldJson; }
        let ev: any;
        try {
          const flatGold = flattenStudy(goldJson as StudyDTO);
          const flatPred = flattenStudy(c.predJson as StudyDTO);
          ev = evaluateFlat(flatGold, flatPred, c.name);
        } catch (e) {
          console.warn(`[WARN] eval failed for ${c.name} in ${dir}: ${(e as Error).message}; keeping previous`);
          newResults.push(c);
          continue;
        }
        const one = {
          ...c,
          goldJson, // fresh gold 반영
          metrics: { jaccard: ev.jaccard, recall: ev.recall, precision: ev.precision },
          counts: ev.counts,
          details: ev.details,
          sectionAccuracy: ev.sectionAccuracy,
          sectionCounts: ev.sectionCounts,
        };
        newResults.push(one);
        if (!DRY) fs.writeFileSync(path.join(dir, c.fileName), JSON.stringify(one, null, 2), "utf8");
      }

      const totalCases = newResults.length;
      const sectionAccuracySummary: any = {};
      for (const k of SECTIONS) sectionAccuracySummary[k] = sectionAgg(newResults, k);
      let wholeTP = 0, wholeFP = 0, wholeFN = 0;
      for (const c of newResults) { wholeTP += c.counts?.both ?? c.counts?.intersection ?? 0; wholeFP += c.counts?.predOnly ?? 0; wholeFN += c.counts?.goldOnly ?? 0; }
      const fieldPrecision = wholeTP + wholeFP > 0 ? wholeTP / (wholeTP + wholeFP) : null;
      const fieldSensitivity = wholeTP + wholeFN > 0 ? wholeTP / (wholeTP + wholeFN) : null;
      const fieldFPperCase = totalCases > 0 ? wholeFP / totalCases : null;

      const out = {
        createdAt: new Date().toISOString(),
        totalCases,
        fieldLevelMetrics: { wholeTP, wholeFP, wholeFN, fieldPrecision, fieldSensitivity, fieldFPperCase },
        sectionAccuracySummary,
        results: newResults,
      };
      if (!DRY) fs.writeFileSync(idxPath, JSON.stringify(out, null, 2), "utf8");

      const nm = `${d.ohdsi ? "ohdsi" : "non-ohdsi"}/${d.path.split("/").pop()}/${v}_${s}`.padEnd(40);
      const spStr = oldSP ? `SP ${oldSP.trueCount}/${oldSP.evaluated}->${sectionAccuracySummary.studyPeriods.trueCount}/${sectionAccuracySummary.studyPeriods.evaluated}` : "SP n/a";
      const pStr = oldFL ? `P ${(oldFL.fieldPrecision * 100).toFixed(1)}->${(fieldPrecision! * 100).toFixed(1)}` : "P n/a";
      const sStr = oldFL ? `S ${(oldFL.fieldSensitivity * 100).toFixed(1)}->${(fieldSensitivity! * 100).toFixed(1)}` : "S n/a";
      console.log(`${nm} ${spStr.padEnd(20)} ${pStr.padEnd(16)} ${sStr}`);
    }
  }
  console.log(DRY ? "\n[DRY RUN] no files written." : "\n[WROTE] per-case files + _summary.index.json updated.");
}

main().catch((e) => { console.error(e); process.exit(1); });
