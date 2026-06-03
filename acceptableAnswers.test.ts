import assert from "node:assert/strict";
import { canonEquivalence } from "./acceptableAnswers";

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

console.log(`\n${n} passed`);
