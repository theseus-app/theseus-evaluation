import assert from "node:assert/strict";
import { canonEquivalence, snapPredScalar, applyAcceptToPredFlat } from "./acceptableAnswers";

let n = 0;
const t = (name: string, fn: () => void) => { fn(); n++; console.log("ok -", name); };

// riskWindowStart {0,1} -> 1
t("rwStart 0 -> 1", () => assert.equal(canonEquivalence("riskWindowStart", 0), 1));
t("rwStart 1 -> 1", () => assert.equal(canonEquivalence("riskWindowStart", 1), 1));
t("rwStart 29 unchanged", () => assert.equal(canonEquivalence("riskWindowStart", 29), 29));

// riskWindowEnd {9999,99999,999999} -> 99999
t("rwEnd 9999 -> 99999", () => assert.equal(canonEquivalence("riskWindowEnd", 9999), 99999));
t("rwEnd 999999 -> 99999", () => assert.equal(canonEquivalence("riskWindowEnd", 999999), 99999));
t("rwEnd 99999 -> 99999", () => assert.equal(canonEquivalence("riskWindowEnd", 99999), 99999));
t("rwEnd 630 unchanged", () => assert.equal(canonEquivalence("riskWindowEnd", 630), 630));

// maxRatio {0,100} -> 0
t("maxRatio 100 -> 0", () => assert.equal(canonEquivalence("maxRatio", 100), 0));
t("maxRatio 0 -> 0", () => assert.equal(canonEquivalence("maxRatio", 0), 0));
t("maxRatio 5 unchanged", () => assert.equal(canonEquivalence("maxRatio", 5), 5));

// null passthrough + unknown component
t("null -> null", () => assert.equal(canonEquivalence("riskWindowEnd", null), null));
t("unknown component passthrough", () => assert.equal(canonEquivalence("caliper", 7), 7));

// Dabigatran riskWindowEnd ranges (gold-gated)
t("dabigatran 645 in [630,651] -> 630", () =>
  assert.equal(snapPredScalar("DabigatranRivaroxabanAF", "timeAtRisks", "riskWindowEnd", [99999, 630, 420, 210], 645), 630));
t("dabigatran 425 -> 420", () =>
  assert.equal(snapPredScalar("DabigatranRivaroxabanAF", "timeAtRisks", "riskWindowEnd", [99999, 630, 420, 210], 425), 420));
t("dabigatran 700 unchanged (out of range)", () =>
  assert.equal(snapPredScalar("DabigatranRivaroxabanAF", "timeAtRisks", "riskWindowEnd", [99999, 630, 420, 210], 700), 700));
t("dabigatran 645 NOT snapped when gold lacks 630", () =>
  assert.equal(snapPredScalar("DabigatranRivaroxabanAF", "timeAtRisks", "riskWindowEnd", [99999], 645), 645));

// ACEiARB 1826 -> 1825
t("aceiarb 1826 -> 1825", () =>
  assert.equal(snapPredScalar("ACEiARBMortality", "timeAtRisks", "riskWindowEnd", [365, 1825], 1826), 1825));

// Ticagrelor riskWindowStart 28 -> 29 (case-insensitive)
t("ticagrelor 28 -> 29", () =>
  assert.equal(snapPredScalar("ticagrelorclopidogrel", "timeAtRisks", "riskWindowStart", [1, 29], 28), 29));

// Aug2 31 -> 30 family
t("antivegfaug2 30 -> 31", () =>
  assert.equal(snapPredScalar("AntiVEGFKidneyAug2", "timeAtRisks", "riskWindowStart", [31], 30), 31));
t("corazonaug2 30 -> 31", () =>
  assert.equal(snapPredScalar("CORAZONAug2", "timeAtRisks", "riskWindowStart", [31], 30), 31));

// TicagrelorAug2 studyEndDate: gold null, accept "20190331"
t("ticagrelorAug2 20190331 -> null", () =>
  assert.equal(snapPredScalar("TicagrelorClopidogrelAug2", "studyPeriods", "studyEndDate", [null], "20190331"), null));

// unregistered study no-op
t("unregistered no-op", () =>
  assert.equal(snapPredScalar("SomethingElse", "timeAtRisks", "riskWindowStart", [31], 30), 30));

// applyAcceptToPredFlat: snaps pred TAR + studyPeriods
{
  const gold: any = {
    studyPeriods: [{ description: "", studyStartDate: "20111101", studyEndDate: null }],
    timeAtRisks: [
      { description: "", riskWindowStart: 1, startAnchor: "cohort start", riskWindowEnd: 99999, endAnchor: "cohort start", minDaysAtRisk: 1 },
      { description: "", riskWindowStart: 1, startAnchor: "cohort start", riskWindowEnd: 630, endAnchor: "cohort start", minDaysAtRisk: 1 },
    ],
    psSettings: [], outcomeModels: [],
  };
  const pred: any = {
    studyPeriods: [{ description: "", studyStartDate: "20111101", studyEndDate: "20190331" }],
    timeAtRisks: [
      { description: "", riskWindowStart: 1, startAnchor: "cohort start", riskWindowEnd: 99999, endAnchor: "cohort start", minDaysAtRisk: 0 },
      { description: "", riskWindowStart: 1, startAnchor: "cohort start", riskWindowEnd: 645, endAnchor: "cohort start", minDaysAtRisk: 0 },
    ],
    psSettings: [], outcomeModels: [],
  };
  const snappedTar = applyAcceptToPredFlat("DabigatranRivaroxabanAF", gold, pred);
  t("flat: pred end 645 -> 630", () => assert.equal(snappedTar.timeAtRisks[1].riskWindowEnd, 630));
  t("flat: pred unchanged when no date rule for case", () => assert.equal(snappedTar.studyPeriods[0].studyEndDate, "20190331"));

  const goldDate: any = { ...gold, timeAtRisks: [] };
  const snappedDate = applyAcceptToPredFlat("TicagrelorClopidogrelAug2", goldDate, pred);
  t("flat: pred date 20190331 -> null", () => assert.equal(snappedDate.studyPeriods[0].studyEndDate, null));
}

console.log(`\n${n} passed`);
