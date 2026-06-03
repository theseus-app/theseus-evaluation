# Acceptable-Answers Layer + Offline Re-score Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a single-source "복수정답" (acceptable-answers) layer to the rule-based scorer, remove `minDaysAtRisk` from scoring, and re-score every existing result directory offline (no LLM) against freshly-loaded gold.

**Architecture:** A new `acceptableAnswers.ts` holds all tolerance rules — symmetric `equivalence` classes (applied to gold+pred) and study-specific `accept` rules (gold-anchored, snap pred→gold). `evaluate.ts` delegates its normalizers to this layer, drops `minDaysAtRisk`, and gains an optional `caseName` arg that triggers the accept snap on the predicted flat DTO. A new `reScoreFresh.ts` reloads current `.ts` gold, re-pairs with saved `predJson`, and rewrites metrics — no LLM calls.

**Tech Stack:** TypeScript run via `tsx`; tests are plain `tsx`-runnable scripts using `node:assert/strict` (repo has no test framework). Types from `theseus-core` via `flatten.ts`.

---

## Background facts (verified)

- Scorer is `evaluate.ts`: `toFacts()` builds canonical fact strings; `evaluateFlat(A,B)` does set intersection. Section helpers: `canonStudyPeriods`/`canonTimeAtRisks`/`canonPsSettings`/`canonOutcomeModels`.
- Existing hardcoded equivalences (`evaluate.ts:120-135`): riskWindowStart 0≡1→1, riskWindowEnd 9999≡99999→99999, maxRatio 100≡0→0. **Keep all**, plus add riskWindowEnd 999999.
- `minDaysAtRisk` is in the TAR fact at `evaluate.ts:73` (`toFacts`) and `:160` (`canonTimeAtRisks`). **Remove both.**
- `flatten.ts` `toYyyyMmDd("")` → `null`. So gold `studyEndDate:""` flattens to `null`; pred `"20190331"` stays `"20190331"`. The TicagrelorAug2 rule operates on these flattened values.
- Results: `public/results/{ohdsi,non-ohdsi}/<type>/<vendor>_<size>/{<case>.json,_summary.index.json}`; each case has `name`,`fileName`,`goldJson`,`predJson`. `c.name` == gold export suffix (e.g. `ACEiARBMortality`, `AntiVEGFKidneyAug2`).
- `loadFile(type)` reads gold from `process.env.GOLD_STANDARD_DIR ?? "public/goldStandard"` + folder map (DEFAULT→default, PRIMARY→primary, PRIMARY_AUGMENTED→primary_augmented, METHOD→method, PDF→pdf). Non-OHDSI gold lives in `public/goldStandardTest`.
- Working branch: `feature/acceptable-answers-layer` (gold corrections already merged from dr-you/main).

## File Structure

- **Create `acceptableAnswers.ts`** — all tolerance rules + functions. One responsibility: define what values are interchangeable/acceptable and snap pred accordingly.
- **Create `acceptableAnswers.test.ts`** — unit tests for the layer.
- **Modify `evaluate.ts`** — delegate normalizers to the layer, drop `minDaysAtRisk`, add `caseName` + accept snap.
- **Create `evaluate.test.ts`** — regression + new-behavior tests with light fixtures.
- **Modify `batchEvaluate.ts`** — pass case name into `evaluateFlat` (so future online runs also use accept rules).
- **Create `reScoreFresh.ts`** — offline re-score across all result dirs with fresh gold.
- **Modify `package.json`** — add `test` script.

---

### Task 1: `acceptableAnswers.ts` — equivalence layer

**Files:**
- Create: `acceptableAnswers.ts`
- Create: `acceptableAnswers.test.ts`

- [ ] **Step 1: Write the failing test**

Create `acceptableAnswers.test.ts`:

```ts
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
```

- [ ] **Step 2: Run test to verify it fails**

Run: `npx tsx acceptableAnswers.test.ts`
Expected: FAIL — `Cannot find module './acceptableAnswers'` (or `canonEquivalence is not a function`).

- [ ] **Step 3: Write minimal implementation**

Create `acceptableAnswers.ts`:

```ts
// acceptableAnswers.ts
// 단일 소스: 채점 허용 규칙(복수정답)을 한 곳에서 관리.
//  (A) equivalence: 대칭 동등 클래스 — gold·pred 양쪽에 적용 (canonical = 배열 첫 원소)
//  (B) accept: study별 gold-기준 비대칭 허용 — pred를 gold 값으로 snap

export type Scalar = number | string | null;

// ---- (A) 공통 equivalence (대칭) ----
const EQUIVALENCE: Record<string, Scalar[][]> = {
  riskWindowStart: [[1, 0]],
  riskWindowEnd: [[99999, 9999, 999999]],
  maxRatio: [[0, 100]],
};

export const canonEquivalence = (component: string, v: Scalar): Scalar => {
  if (v === null || v === undefined) return null;
  const classes = EQUIVALENCE[component];
  if (!classes) return v;
  for (const cls of classes) {
    if (cls.includes(v)) return cls[0]; // canonical = 첫 원소
  }
  return v;
};
```

- [ ] **Step 4: Run test to verify it passes**

Run: `npx tsx acceptableAnswers.test.ts`
Expected: PASS — `13 passed`.

- [ ] **Step 5: Commit**

```bash
git add acceptableAnswers.ts acceptableAnswers.test.ts
git commit -m "feat(eval): equivalence layer (riskWindowStart/End/maxRatio, +999999)"
```

---

### Task 2: `acceptableAnswers.ts` — study-specific accept (snap)

**Files:**
- Modify: `acceptableAnswers.ts`
- Modify: `acceptableAnswers.test.ts`

- [ ] **Step 1: Write the failing test**

Append to `acceptableAnswers.test.ts` (before the final `console.log`):

```ts
import { snapPredScalar, applyAcceptToPredFlat } from "./acceptableAnswers";

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

// applyAcceptToPredFlat: snaps pred TAR + studyPeriods, leaves gold logic to caller
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
  // case has both a Dabigatran-style end rule and a studyEndDate rule? Use Dabigatran for TAR, Ticagrelor for date — test separately:
  const snappedTar = applyAcceptToPredFlat("DabigatranRivaroxabanAF", gold, pred);
  t("flat: pred end 645 -> 630", () => assert.equal(snappedTar.timeAtRisks[1].riskWindowEnd, 630));
  t("flat: pred unchanged when no date rule for case", () => assert.equal(snappedTar.studyPeriods[0].studyEndDate, "20190331"));

  const goldDate: any = { ...gold, timeAtRisks: [] };
  const snappedDate = applyAcceptToPredFlat("TicagrelorClopidogrelAug2", goldDate, pred);
  t("flat: pred date 20190331 -> null", () => assert.equal(snappedDate.studyPeriods[0].studyEndDate, null));
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `npx tsx acceptableAnswers.test.ts`
Expected: FAIL — `snapPredScalar is not a function`.

- [ ] **Step 3: Write minimal implementation**

Append to `acceptableAnswers.ts`:

```ts
import type { FlattenStudyDTO } from "./flatten";

// ---- (B) study별 accept (gold 기준 snap) ----
export type AcceptRule =
  | { kind: "set"; values: Scalar[] }
  | { kind: "range"; min: number; max: number }; // range는 숫자 전용

type AcceptSection = "timeAtRisks" | "studyPeriods";
type AcceptComponent = "riskWindowStart" | "riskWindowEnd" | "studyEndDate";

type AcceptEntry = {
  caseName: string; // lowercase 정규화 후 비교 (c.name 기준)
  section: AcceptSection;
  component: AcceptComponent;
  goldValue: Scalar; // gold가 이 값일 때만 발동
  rule: AcceptRule;
};

const ACCEPT: AcceptEntry[] = [
  // DabigatranRivaroxabanAF: riskWindowEnd 범위 (양끝 포함)
  { caseName: "dabigatranrivaroxabanaf", section: "timeAtRisks", component: "riskWindowEnd", goldValue: 630, rule: { kind: "range", min: 630, max: 651 } },
  { caseName: "dabigatranrivaroxabanaf", section: "timeAtRisks", component: "riskWindowEnd", goldValue: 420, rule: { kind: "range", min: 420, max: 434 } },
  { caseName: "dabigatranrivaroxabanaf", section: "timeAtRisks", component: "riskWindowEnd", goldValue: 210, rule: { kind: "range", min: 210, max: 217 } },
  // ACEiARBMortality (non-OHDSI): riskWindowEnd 1825 ≡ 1826
  { caseName: "aceiarbmortality", section: "timeAtRisks", component: "riskWindowEnd", goldValue: 1825, rule: { kind: "set", values: [1826] } },
  // TicagrelorClopidogrel (OHDSI default·method TAR 4-6): riskWindowStart 29 ≡ 28
  { caseName: "ticagrelorclopidogrel", section: "timeAtRisks", component: "riskWindowStart", goldValue: 29, rule: { kind: "set", values: [28] } },
  // Aug2 5종: riskWindowStart 31 ≡ 30
  { caseName: "antivegfkidneyaug2", section: "timeAtRisks", component: "riskWindowStart", goldValue: 31, rule: { kind: "set", values: [30] } },
  { caseName: "corazonaug2", section: "timeAtRisks", component: "riskWindowStart", goldValue: 31, rule: { kind: "set", values: [30] } },
  { caseName: "covid19ppiandh2raaug2", section: "timeAtRisks", component: "riskWindowStart", goldValue: 31, rule: { kind: "set", values: [30] } },
  { caseName: "semaglutideandnaionaug2", section: "timeAtRisks", component: "riskWindowStart", goldValue: 31, rule: { kind: "set", values: [30] } },
  { caseName: "tramadolcodeinaug2", section: "timeAtRisks", component: "riskWindowStart", goldValue: 31, rule: { kind: "set", values: [30] } },
  // TicagrelorClopidogrelAug2: studyEndDate gold "" (flatten->null) ≡ "20190331"
  { caseName: "ticagrelorclopidogrelaug2", section: "studyPeriods", component: "studyEndDate", goldValue: null, rule: { kind: "set", values: ["20190331", null] } },
];

const lc = (s: string) => s.toLowerCase();

const matchRule = (rule: AcceptRule, predV: Scalar): boolean => {
  if (rule.kind === "set") return rule.values.some((v) => v === predV);
  return typeof predV === "number" && predV >= rule.min && predV <= rule.max;
};

export const snapPredScalar = (
  caseName: string,
  section: AcceptSection,
  component: AcceptComponent,
  goldValues: Scalar[],
  predV: Scalar
): Scalar => {
  const cn = lc(caseName);
  for (const e of ACCEPT) {
    if (e.caseName !== cn || e.section !== section || e.component !== component) continue;
    if (!goldValues.some((g) => g === e.goldValue)) continue; // gold 게이트
    if (matchRule(e.rule, predV)) return e.goldValue;
  }
  return predV;
};

// pred flat의 TAR/studyPeriods 값을 gold 기준으로 snap (gold는 건드리지 않음).
// studyEndDate는 단일 period(augmented) 가정 — 같은 case의 모든 period에 동일 규칙 적용.
export const applyAcceptToPredFlat = (
  caseName: string,
  gold: FlattenStudyDTO,
  pred: FlattenStudyDTO
): FlattenStudyDTO => {
  if (!caseName) return pred;
  const cn = lc(caseName);
  if (!ACCEPT.some((e) => e.caseName === cn)) return pred; // 빠른 종료

  const goldStart = (gold.timeAtRisks ?? []).map((t) => t.riskWindowStart as Scalar);
  const goldEnd = (gold.timeAtRisks ?? []).map((t) => t.riskWindowEnd as Scalar);
  const goldEndDates = (gold.studyPeriods ?? []).map((s) => s.studyEndDate as Scalar);

  const timeAtRisks = (pred.timeAtRisks ?? []).map((t) => ({
    ...t,
    riskWindowStart: snapPredScalar(cn, "timeAtRisks", "riskWindowStart", goldStart, t.riskWindowStart) as number,
    riskWindowEnd: snapPredScalar(cn, "timeAtRisks", "riskWindowEnd", goldEnd, t.riskWindowEnd) as number,
  }));
  const studyPeriods = (pred.studyPeriods ?? []).map((s) => ({
    ...s,
    studyEndDate: snapPredScalar(cn, "studyPeriods", "studyEndDate", goldEndDates, s.studyEndDate) as string | null,
  }));

  return { ...pred, timeAtRisks, studyPeriods };
};
```

- [ ] **Step 4: Run test to verify it passes**

Run: `npx tsx acceptableAnswers.test.ts`
Expected: PASS — all `ok -` lines, final count printed.

- [ ] **Step 5: Commit**

```bash
git add acceptableAnswers.ts acceptableAnswers.test.ts
git commit -m "feat(eval): study-specific accept rules + applyAcceptToPredFlat"
```

---

### Task 3: `evaluate.ts` — wire layer, drop minDaysAtRisk, add caseName

**Files:**
- Modify: `evaluate.ts` (imports; `:69,71` and `:156,158` normalizers already exist; remove `minDaysAtRisk` at `:73` and `:160`; `evaluateFlat` signature `:195`)
- Create: `evaluate.test.ts`

- [ ] **Step 1: Write the failing test**

Create `evaluate.test.ts`:

```ts
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
```

- [ ] **Step 2: Run test to verify it fails**

Run: `npx tsx evaluate.test.ts`
Expected: FAIL — "minDaysAtRisk ignored" throws (gold/pred TAR facts differ by minDaysAtRisk), and the caseName test fails (3rd arg unused).

- [ ] **Step 3: Write minimal implementation**

In `evaluate.ts`:

(a) Add imports near the top (after the existing `import { FlattenStudyDTO } from "./flatten";`):

```ts
import { canonEquivalence, applyAcceptToPredFlat } from "./acceptableAnswers";
```

(b) Replace the three normalizer bodies (`evaluate.ts:120-135`) with delegations:

```ts
const normalizeRiskWindowEnd = (n: number | null | undefined): number | null =>
  canonEquivalence("riskWindowEnd", n ?? null) as number | null;

const normalizeRiskWindowStart = (n: number | null | undefined): number | null =>
  canonEquivalence("riskWindowStart", n ?? null) as number | null;

const normalizeMaxRatio = (n: number | null | undefined): number | null =>
  canonEquivalence("maxRatio", n ?? null) as number | null;
```

(c) Remove the `minDaysAtRisk` line in `toFacts` (`evaluate.ts:73`) so the TAR fact becomes:

```ts
  for (const t of flat.timeAtRisks ?? []) {
    facts.add(
      canonicalizeObject("timeAtRisks", {
        riskWindowStart: normalizeRiskWindowStart(t.riskWindowStart),
        startAnchor: t.startAnchor,
        riskWindowEnd: normalizeRiskWindowEnd(t.riskWindowEnd),
        endAnchor: t.endAnchor,
      })
    );
  }
```

(d) Remove the `minDaysAtRisk` line in `canonTimeAtRisks` (`evaluate.ts:160`) so it becomes:

```ts
const canonTimeAtRisks = (arr: Flat["timeAtRisks"]) =>
  new Set(
    (arr ?? []).map((t) =>
      canonicalizeObject("timeAtRisks", {
        riskWindowStart: normalizeRiskWindowStart(t.riskWindowStart),
        startAnchor: t.startAnchor,
        riskWindowEnd: normalizeRiskWindowEnd(t.riskWindowEnd),
        endAnchor: t.endAnchor,
      })
    )
  );
```

(e) Change `evaluateFlat` signature and snap pred at the top (`evaluate.ts:195`):

```ts
export const evaluateFlat = (A: Flat, Braw: Flat, caseName?: string) => {
  // 복수정답 accept: pred를 gold 기준으로 snap (gold는 불변). caseName 없으면 미적용.
  const B = caseName ? applyAcceptToPredFlat(caseName, A, Braw) : Braw;

  const AF = toFacts(A);
  const BF = toFacts(B);
  // ...rest of the function unchanged (already uses `B`)...
```

(The rest of the body already references `B`, so no further edits are needed inside the function.)

- [ ] **Step 4: Run tests to verify they pass**

Run: `npx tsx evaluate.test.ts && npx tsx acceptableAnswers.test.ts`
Expected: both PASS (`5 passed` for evaluate; acceptableAnswers count unchanged).

- [ ] **Step 5: Commit**

```bash
git add evaluate.ts evaluate.test.ts
git commit -m "refactor(eval): delegate normalizers to layer, drop minDaysAtRisk, add caseName accept snap"
```

---

### Task 4: `batchEvaluate.ts` — pass case name to evaluateFlat

**Files:**
- Modify: `batchEvaluate.ts` (the `evaluateFlat(flatGold, flatPred)` call)

- [ ] **Step 1: Locate the call**

Run: `grep -n "evaluateFlat(" batchEvaluate.ts`
Expected: one call like `const ev = evaluateFlat(flatGold, flatPred);` with a per-case name in scope (the loop variable from `loadFile`, e.g. `pair.name` / `name`).

- [ ] **Step 2: Add the case name argument**

Edit that line to pass the case name used elsewhere in the loop (confirm the exact identifier from context — it is the `name` field of the current `ModulePair`):

```ts
const ev = evaluateFlat(flatGold, flatPred, name);
```

If the in-scope identifier is `pair.name`, use `evaluateFlat(flatGold, flatPred, pair.name)`. Do not invent a new variable — use the one already destructured for `name`/`fileName`.

- [ ] **Step 3: Type-check compiles**

Run: `npx tsc --noEmit batchEvaluate.ts 2>&1 | head` (informational; repo may have pre-existing errors — confirm no NEW error references this line).
Expected: no error pointing at the edited `evaluateFlat(...)` call.

- [ ] **Step 4: Commit**

```bash
git add batchEvaluate.ts
git commit -m "feat(eval): pass case name into evaluateFlat for online runs"
```

---

### Task 5: `reScoreFresh.ts` — offline re-score all dirs with fresh gold

**Files:**
- Create: `reScoreFresh.ts`

- [ ] **Step 1: Write the script**

Create `reScoreFresh.ts`:

```ts
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

const DIRS: { ohdsi: boolean; type: string; path: string }[] = [
  { ohdsi: true,  type: "DEFAULT",           path: "public/results/ohdsi/default" },
  { ohdsi: true,  type: "PRIMARY",           path: "public/results/ohdsi/primary" },
  { ohdsi: true,  type: "PRIMARY_AUGMENTED", path: "public/results/ohdsi/primary_augmented" },
  { ohdsi: true,  type: "METHOD",            path: "public/results/ohdsi/method" },
  { ohdsi: true,  type: "PDF",               path: "public/results/ohdsi/pdf" },
  { ohdsi: false, type: "DEFAULT",           path: "public/results/non-ohdsi/default" },
  { ohdsi: false, type: "PRIMARY",           path: "public/results/non-ohdsi/primary" },
  { ohdsi: false, type: "METHOD",            path: "public/results/non-ohdsi/method" },
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
```

- [ ] **Step 2: Verify it loads & runs in DRY mode (no writes)**

Run: `DRY_RUN=1 npx tsx reScoreFresh.ts 2>&1 | tail -40`
Expected: one line per existing `<dir>/<vendor>_<size>`, ending with `[DRY RUN] no files written.`; no uncaught errors. (Some `[WARN]` lines are acceptable if a result's `c.name` has no gold — note them for Task 6.)

- [ ] **Step 3: Commit the script**

```bash
git add reScoreFresh.ts
git commit -m "feat(eval): reScoreFresh — offline re-score all dirs against fresh gold (no LLM)"
```

---

### Task 6: Verify expected score changes, then apply for real

**Files:**
- Modify (data): `public/results/**/_summary.index.json` and per-case `.json`

- [ ] **Step 1: Sanity-check the corrected-gold cases in DRY mode**

Confirm the three gold corrections + accept rules move scores in the expected direction. Inspect a couple of affected cases without writing:

Run:
```bash
DRY_RUN=1 npx tsx reScoreFresh.ts > /tmp/rescore_dry.txt 2>&1
grep -E "non-ohdsi/default|primary_augmented" /tmp/rescore_dry.txt
grep -i "WARN" /tmp/rescore_dry.txt | sort | uniq -c
```
Expected: non-ohdsi/default and ohdsi/primary_augmented lines show precision/sensitivity ≥ previous (gold fixes + accept + minDaysAtRisk removal can only help or keep equal). Review any `[WARN] no fresh gold` lines — these indicate a `c.name` not present in the loaded gold folder; investigate before the real run (likely a name mismatch or a result dir whose gold lives elsewhere).

- [ ] **Step 2: Resolve any WARN name-mismatches**

For each warned `c.name`, check the gold folder actually contains it:
Run: `ls public/goldStandard/<type> public/goldStandardTest/<type>` for the relevant type and confirm the export-suffix name. If a result dir legitimately has no fresh gold (e.g. an orphan), the script falls back to saved `goldJson` — acceptable, but note it in the commit message.

- [ ] **Step 3: Apply the re-score for real**

Run: `npx tsx reScoreFresh.ts | tee /tmp/rescore_apply.txt`
Expected: ends with `[WROTE] per-case files + _summary.index.json updated.`

- [ ] **Step 4: Spot-verify a corrected case on disk**

Run:
```bash
node -e '
const fs=require("fs");
const p="public/results/non-ohdsi/default/openai_flagship/_summary.index.json";
const idx=JSON.parse(fs.readFileSync(p,"utf8"));
const c=idx.results.find(r=>r.name==="DPP4iPulmonaryTB");
console.log("DPP4i studyPeriods acc:", c.sectionAccuracy.studyPeriods, "| gold endDate now:", c.goldJson.getDbCohortMethodDataArgs.studyPeriods.map(s=>s.studyEndDate));
'
```
Expected: `c.goldJson` reflects `20201231` (fresh gold embedded), and section accuracy recomputed.

- [ ] **Step 5: Commit the updated results**

```bash
git add public/results
git commit -m "data(eval): offline re-score — fresh gold, minDaysAtRisk excluded, acceptable-answers applied"
```

---

### Task 7: Add `test` script

**Files:**
- Modify: `package.json`

- [ ] **Step 1: Update the script**

Replace the `"test"` line in `package.json` `scripts`:

```json
"test": "tsx acceptableAnswers.test.ts && tsx evaluate.test.ts"
```

- [ ] **Step 2: Run it**

Run: `npm test`
Expected: both test files print their `ok -` lines and pass counts; exit code 0.

- [ ] **Step 3: Commit**

```bash
git add package.json
git commit -m "chore(eval): add test script for layer + evaluate tests"
```

---

## Self-Review

**Spec coverage:**
- 공통 equivalence (0≡1, 9999/999999≡99999, 0≡100) → Task 1. ✅
- study별 accept (Dabigatran ranges, ACEiARB, Ticagrelor 29→28, Aug2 5종 31→30, TicagrelorAug2 studyEndDate) → Task 2. ✅
- minDaysAtRisk 제외 → Task 3 (c,d). ✅
- caseName wiring (offline + online) → Task 3(e) / Task 4. ✅
- fresh-gold offline re-score, all dirs, no LLM → Task 5. ✅
- gold 수정 반영 검증 + 실적용 → Task 6. ✅
- DRY_RUN safety → Task 5/6. ✅

**Placeholder scan:** No TBD/TODO; all code blocks complete; the only runtime ambiguity (`name` vs `pair.name` in Task 4) is resolved by instruction to use the existing in-scope identifier.

**Type consistency:** `canonEquivalence(component, Scalar)`, `snapPredScalar(caseName, section, component, goldValues, predV)`, `applyAcceptToPredFlat(caseName, gold, pred)`, `evaluateFlat(A, Braw, caseName?)` — names/arities consistent across Tasks 1–5. `Scalar = number|string|null` shared. Section/component string literals match between ACCEPT entries and `applyAcceptToPredFlat` calls (`"timeAtRisks"`/`"studyPeriods"`, `"riskWindowStart"`/`"riskWindowEnd"`/`"studyEndDate"`).

**Open confirmations (non-blocking):** `done/` dirs excluded (not in DIRS). studyEndDate snap assumes single augmented period (documented in code comment).
