import { promises as fs } from "fs";
import path from "path";

type AnyObj = Record<string, any>;
const SRC = process.env.SRC_DIR ?? "public/goldStandard";
const DST = process.env.DST_DIR ?? "public/newGoldStandard";
const TYPES = ["default", "method", "pdf"]; // PRIMARY excluded (out of scope)

function migrate(old: AnyObj): AnyObj {
  const g = old.getDbCohortMethodDataArgs ?? {};
  const sp = old.createStudyPopArgs ?? {};
  const psa = old.propensityScoreAdjustment ?? { psSettings: old.psSettings, createPsArgs: old.createPsArgs };
  const fit = old.fitOutcomeModelArgs ?? {};

  const studyPeriods = (g.studyPeriods ?? []).map((p: AnyObj) => ({
    description: p.description ?? "",
    studyStartDate: p.studyStartDate ?? "",
    studyEndDate: p.studyEndDate ?? "",
  }));

  const getDb = {
    studyPeriods,
    firstExposureOnly: g.firstExposureOnly ?? sp.firstExposureOnly ?? false,
    removeDuplicateSubjects: g.removeDuplicateSubjects ?? sp.removeDuplicateSubjects ?? "keep all",
    restrictToCommonPeriod: g.restrictToCommonPeriod ?? sp.restrictToCommonPeriod ?? false,
    washoutPeriod: g.washoutPeriod ?? sp.washoutPeriod ?? 365,
    maxCohortSize: g.maxCohortSize ?? 0,
  };

  const createStudyPop = {
    removeSubjectsWithPriorOutcome: sp.removeSubjectsWithPriorOutcome ?? true,
    priorOutcomeLookback: sp.priorOutcomeLookback ?? sp.priorOutcomeLookBack ?? 99999,
    timeAtRisks: (sp.timeAtRisks ?? []).map((t: AnyObj) => ({
      description: t.description ?? "",
      minDaysAtRisk: t.minDaysAtRisk ?? 1,
      riskWindowStart: t.riskWindowStart ?? 1,
      startAnchor: t.startAnchor ?? "cohort start",
      riskWindowEnd: t.riskWindowEnd ?? 0,
      endAnchor: t.endAnchor ?? "cohort end",
    })),
    censorAtNewRiskWindow: sp.censorAtNewRiskWindow ?? false,
  };

  const oldIptw = fit.inversePtWeighting ?? false;
  const psSettings = (psa.psSettings ?? []).map((s: AnyObj) => ({
    description: s.description ?? "",
    trimByPsArgs: s.trimByPsArgs ?? null,
    matchOnPsArgs: s.matchOnPsArgs ?? null,
    stratifyByPsArgs: s.stratifyByPsArgs ?? null,
    inversePtWeighting: s.inversePtWeighting ?? oldIptw,
  }));

  const fitOut = {
    outcomeModels: fit.outcomeModels ?? [
      { description: "", modelType: fit.modelType ?? "cox", useCovariates: fit.useCovariates ?? false },
    ],
    stratified: fit.stratified ?? false,
    prior: fit.prior ?? null,
    control: fit.control ?? null,
  };

  return {
    ...(old.name !== undefined ? { name: old.name } : {}),
    ...(old.cohortDefinitions ? { cohortDefinitions: old.cohortDefinitions } : {}),
    ...(old.negativeControlConceptSet ? { negativeControlConceptSet: old.negativeControlConceptSet } : {}),
    ...(old.covariateSelection ? { covariateSelection: old.covariateSelection } : {}),
    getDbCohortMethodDataArgs: getDb,
    createStudyPopArgs: createStudyPop,
    psSettings,
    createPsArgs: psa.createPsArgs ?? null,
    fitOutcomeModelArgs: fitOut,
  };
}

async function run() {
  for (const type of TYPES) {
    const srcDir = path.join(SRC, type);
    let entries: string[] = [];
    try { entries = (await fs.readdir(srcDir)).filter(f => f.endsWith(".ts")); }
    catch { continue; }
    const dstDir = path.join(DST, type);
    await fs.mkdir(dstDir, { recursive: true });
    for (const file of entries) {
      const name = file.replace(/\.ts$/, "");
      const mod = await import(path.resolve(srcDir, file));
      const jsonKey = `JSON${name}`;
      const textKey = `TEXT${name}`;
      if (!(jsonKey in mod)) { console.warn(`skip ${file}: no ${jsonKey}`); continue; }
      const migrated = migrate(mod[jsonKey]);
      const text = mod[textKey] ?? "";
      const out =
        `export const ${textKey} = ${JSON.stringify(text)}\n\n` +
        `export const ${jsonKey} = ${JSON.stringify(migrated, null, 2)}\n`;
      await fs.writeFile(path.join(dstDir, file), out, "utf8");
      console.log(`migrated ${type}/${file}`);
    }
  }
}
run().catch(e => { console.error(e); process.exit(1); });
