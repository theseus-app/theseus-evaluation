// Offline re-score from saved goldJson/predJson (NO API). Mirrors batchEvaluate's summary logic.
// DRY_RUN=1 -> compute + print before/after only, write nothing.
import fs from "node:fs";
import path from "node:path";
import { flattenStudy, StudyDTO } from "./flatten";
import { evaluateFlat } from "./evaluate";

const DRY = process.env.DRY_RUN === "1";
const bases = [
  "public/results/non-ohdsi/primary",
  "public/results/non-ohdsi/default",
  "public/results/ohdsi/primary_augmented",
];
const vendors = ["openai", "gemini", "claude", "deepseek"];
const sizes = ["flagship", "light"];

type SecKey = "studyPeriods" | "timeAtRisks" | "propensityScoreAdjustment" | "outcomeModels";
const SECTIONS: SecKey[] = ["studyPeriods", "timeAtRisks", "propensityScoreAdjustment", "outcomeModels"];

function sectionAgg(results: any[], key: SecKey) {
  let evaluated = 0, trueCount = 0;
  for (const c of results) {
    const v = c.sectionAccuracy?.[key] ?? null;
    if (v !== null) { evaluated++; if (v === true) trueCount++; }
  }
  return { evaluated, trueCount, falseCount: evaluated - trueCount, accuracy: evaluated ? trueCount / evaluated : null };
}

for (const base of bases) {
  for (const v of vendors) for (const s of sizes) {
    const dir = path.join(base, `${v}_${s}`);
    const idxPath = path.join(dir, "_summary.index.json");
    if (!fs.existsSync(idxPath)) { console.log(`skip (no summary): ${dir}`); continue; }
    const idx = JSON.parse(fs.readFileSync(idxPath, "utf8"));
    const oldSP = idx.sectionAccuracySummary?.studyPeriods;
    const oldFL = idx.fieldLevelMetrics;

    const newResults: any[] = [];
    for (const c of idx.results) {
      const flatGold = flattenStudy(c.goldJson as StudyDTO);
      const flatPred = flattenStudy(c.predJson as StudyDTO);
      const ev: any = evaluateFlat(flatGold, flatPred);
      const one = {
        ...c,
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
    for (const c of newResults) { wholeTP += c.counts.both ?? c.counts.intersection ?? 0; wholeFP += c.counts.predOnly ?? 0; wholeFN += c.counts.goldOnly ?? 0; }
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

    const nm = `${base.split("/").pop()}/${v}_${s}`.padEnd(26);
    const spStr = `SP ${oldSP.trueCount}/${oldSP.evaluated}->${sectionAccuracySummary.studyPeriods.trueCount}/${sectionAccuracySummary.studyPeriods.evaluated}`;
    const pStr = `P ${(oldFL.fieldPrecision * 100).toFixed(1)}->${(fieldPrecision! * 100).toFixed(1)}`;
    const sStr = `S ${(oldFL.fieldSensitivity * 100).toFixed(1)}->${(fieldSensitivity! * 100).toFixed(1)}`;
    console.log(`${nm} ${spStr.padEnd(20)} ${pStr.padEnd(16)} ${sStr}`);
  }
}
console.log(DRY ? "\n[DRY RUN] no files written." : "\n[WROTE] per-case files + _summary.index.json updated.");
