import assert from "node:assert/strict";
import { evaluateFlat } from "./evaluate";

let n = 0;
const t = (name: string, fn: () => void) => { fn(); n++; console.log("ok -", name); };

const TAR = (o: any) => ({ description: "", startAnchor: "cohort start", endAnchor: "cohort start", minDaysAtRisk: 1, ...o });
const flat = (o: any): any => ({ studyPeriods: [], timeAtRisks: [], psSettings: [], outcomeModels: [], ...o });

// minDaysAtRisk must NOT affect scoring: gold mdar=1, pred mdar=0, same window -> TAR exact match
t("minDaysAtRisk ignored", () => {
  const gold = flat({ timeAtRisks: [TAR({ riskWindowStart: 1, riskWindowEnd: 99999, minDaysAtRisk: 1 })] });
  const pred = flat({ timeAtRisks: [TAR({ riskWindowStart: 1, riskWindowEnd: 99999, minDaysAtRisk: 0 })] });
  assert.equal(evaluateFlat(gold, pred).sectionAccuracy.timeAtRisks, true);
});

// existing equivalence still holds: riskWindowEnd 9999 ≡ 99999
t("9999 ≡ 99999", () => {
  const gold = flat({ timeAtRisks: [TAR({ riskWindowStart: 1, riskWindowEnd: 99999 })] });
  const pred = flat({ timeAtRisks: [TAR({ riskWindowStart: 1, riskWindowEnd: 9999 })] });
  assert.equal(evaluateFlat(gold, pred).sectionAccuracy.timeAtRisks, true);
});

// new equivalence: riskWindowEnd 999999 ≡ 99999
t("999999 ≡ 99999", () => {
  const gold = flat({ timeAtRisks: [TAR({ riskWindowStart: 1, riskWindowEnd: 99999 })] });
  const pred = flat({ timeAtRisks: [TAR({ riskWindowStart: 1, riskWindowEnd: 999999 })] });
  assert.equal(evaluateFlat(gold, pred).sectionAccuracy.timeAtRisks, true);
});

// accept rule fires only with caseName: Dabigatran 645 -> 630
t("accept off without caseName", () => {
  const gold = flat({ timeAtRisks: [TAR({ riskWindowStart: 1, riskWindowEnd: 630 })] });
  const pred = flat({ timeAtRisks: [TAR({ riskWindowStart: 1, riskWindowEnd: 645 })] });
  assert.equal(evaluateFlat(gold, pred).sectionAccuracy.timeAtRisks, false);
});
t("accept on with caseName", () => {
  const gold = flat({ timeAtRisks: [TAR({ riskWindowStart: 1, riskWindowEnd: 630 })] });
  const pred = flat({ timeAtRisks: [TAR({ riskWindowStart: 1, riskWindowEnd: 645 })] });
  assert.equal(evaluateFlat(gold, pred, "DabigatranRivaroxabanAF").sectionAccuracy.timeAtRisks, true);
});

console.log(`\n${n} passed`);
