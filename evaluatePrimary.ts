// evaluatePrimary.ts

import { FlattenStudyDTOPRIMARY } from "./flattenPrimary";

type FlatPRIMARY = FlattenStudyDTOPRIMARY;

// --- 안정 직렬화(키 정렬) ---
const stableStringify = (v: any): string => {
  const recur = (x: any): any => {
    if (x === null || x === undefined) return null;
    if (Array.isArray(x)) return x.map(recur);
    if (typeof x === "object") {
      const entries = Object.entries(x)
        .map(([k, v]) => [k, recur(v)] as const)
        .sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0));
      const o: Record<string, any> = {};
      for (const [k, v] of entries) o[k] = v;
      return o;
    }
    return x;
  };
  return JSON.stringify(recur(v));
};

// 값 정규화
const normVal = (v: unknown): string => {
  if (v === null || v === undefined) return "null";
  if (typeof v === "string") return v.trim();
  if (typeof v === "number" || typeof v === "boolean") return String(v);
  return stableStringify(v);
};

// 객체 항목 캐논화
const canonicalizeObject = (field: string, obj: Record<string, unknown>): string => {
  const entries = Object.entries(obj)
    .map(([k, v]) => [k, normVal(v)] as const)
    .sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0));
  const body = entries.map(([k, v]) => `${k}=${v}`).join("|");
  return `${field}:{${body}}`;
};

// =======================
// 1-depth facts (PRIMARY)
// =======================
export const toFactsPRIMARY = (flatPRIMARY: FlatPRIMARY): Set<string> => {
  const factsPRIMARY = new Set<string>();

  // 🔹 스칼라들 (키 이름은 기존이랑 동일하게 유지)
  factsPRIMARY.add(`maxCohortSize=${normVal(flatPRIMARY.maxCohortSizePRIMARY)}`);
  factsPRIMARY.add(
    `restrictToCommonPeriod=${normVal(flatPRIMARY.restrictToCommonPeriodPRIMARY)}`
  );
  factsPRIMARY.add(`firstExposureOnly=${normVal(flatPRIMARY.firstExposureOnlyPRIMARY)}`);
  factsPRIMARY.add(`washoutPeriod=${normVal(flatPRIMARY.washoutPeriodPRIMARY)}`);
  factsPRIMARY.add(
    `removeDuplicateSubjects=${normVal(flatPRIMARY.removeDuplicateSubjectsPRIMARY)}`
  );
  factsPRIMARY.add(
    `censorAtNewRiskWindow=${normVal(flatPRIMARY.censorAtNewRiskWindowPRIMARY)}`
  );
  factsPRIMARY.add(
    `removeSubjectsWithPriorOutcome=${normVal(
      flatPRIMARY.removeSubjectsWithPriorOutcomePRIMARY
    )}`
  );
  factsPRIMARY.add(
    `priorOutcomeLookBack=${normVal(flatPRIMARY.priorOutcomeLookBackPRIMARY)}`
  );

  // 🔹 studyPeriods: 단일 객체 (있으면 1개 fact)
  if (flatPRIMARY.studyPeriodsPRIMARY) {
    factsPRIMARY.add(
      canonicalizeObject("studyPeriods", {
        studyStartDate: flatPRIMARY.studyPeriodsPRIMARY.studyStartDate,
        studyEndDate: flatPRIMARY.studyPeriodsPRIMARY.studyEndDate,
      })
    );
  }

  // 🔹 timeAtRisks: 단일 객체
  if (flatPRIMARY.timeAtRisksPRIMARY) {
    const t = flatPRIMARY.timeAtRisksPRIMARY;
    factsPRIMARY.add(
      canonicalizeObject("timeAtRisks", {
        riskWindowStart: t.riskWindowStart,
        startAnchor: t.startAnchor,
        riskWindowEnd: normalizeRiskWindowEnd(t.riskWindowEnd),
        endAnchor: t.endAnchor,
        minDaysAtRisk: t.minDaysAtRisk,
      })
    );
  }

  // 🔹 psSettings: 단일 객체
  if (flatPRIMARY.psSettingsPRIMARY) {
    const p = flatPRIMARY.psSettingsPRIMARY;
    factsPRIMARY.add(
      canonicalizeObject("psSettings", {
        maxRatio: normalizeMaxRatio(p.maxRatio),
        caliper: p.caliper,
        caliperScale: p.caliperScale,
        numberOfStrata: p.numberOfStrata,
        baseSelection: p.baseSelection,
      })
    );
  }

  // 🔹 createPsArgs: 단일 복합 항목
  factsPRIMARY.add(
    canonicalizeObject("createPsArgs", pickCreatePsArgsPRIMARY(flatPRIMARY))
  );

  return factsPRIMARY;
};

// =======================
// 공통 유틸들
// =======================

// 안전 나눗셈
const safeDiv = (num: number, den: number, whenZero: number): number =>
  den === 0 ? whenZero : num / den;

// fact 키 추출: "a=b" -> "a", "foo:{...}" -> "foo"
const factKey = (f: string): string => {
  const objIdx = f.indexOf(":{");
  if (objIdx !== -1) return f.slice(0, objIdx);
  const eqIdx = f.indexOf("=");
  return eqIdx === -1 ? f : f.slice(0, eqIdx);
};

// riskwindow 기간 normalize
const normalizeRiskWindowEnd = (n: number | null | undefined): number | null => {
  if (n === null || n === undefined) return null;
  return n === 9999 ? 99999 : n;
};

// maxRatio: 0 (no max) 와 100 을 동치로 취급
const normalizeMaxRatio = (n: number | null | undefined): number | null => {
  if (n === null || n === undefined) return null;
  return n === 100 ? 0 : n;
};

const setEq = (A: Set<string>, B: Set<string>) =>
  A.size === B.size && [...A].every((x) => B.has(x));

// =======================
// PRIMARY용 canonicalizer
// =======================

// studyPeriodsPRIMARY: 단일 객체 -> Set<string>(0 또는 1개)
const canonStudyPeriodsPRIMARY = (spPRIMARY: FlatPRIMARY["studyPeriodsPRIMARY"]) =>
  new Set(
    spPRIMARY
      ? [
          canonicalizeObject("studyPeriods", {
            studyStartDate: spPRIMARY.studyStartDate,
            studyEndDate: spPRIMARY.studyEndDate,
          }),
        ]
      : []
  );

// timeAtRisksPRIMARY: 단일 객체 -> Set<string>
const canonTimeAtRisksPRIMARY = (tarPRIMARY: FlatPRIMARY["timeAtRisksPRIMARY"]) =>
  new Set(
    tarPRIMARY
      ? [
          canonicalizeObject("timeAtRisks", {
            riskWindowStart: tarPRIMARY.riskWindowStart,
            startAnchor: tarPRIMARY.startAnchor,
            riskWindowEnd: normalizeRiskWindowEnd(tarPRIMARY.riskWindowEnd),
            endAnchor: tarPRIMARY.endAnchor,
            minDaysAtRisk: tarPRIMARY.minDaysAtRisk,
          }),
        ]
      : []
  );

// psSettingsPRIMARY: 단일 객체 -> Set<string>
const canonPsSettingsPRIMARY = (psPRIMARY: FlatPRIMARY["psSettingsPRIMARY"]) =>
  new Set(
    psPRIMARY
      ? [
          canonicalizeObject("psSettings", {
            maxRatio: normalizeMaxRatio(psPRIMARY.maxRatio),
            caliper: psPRIMARY.caliper,
            caliperScale: psPRIMARY.caliperScale,
            numberOfStrata: psPRIMARY.numberOfStrata,
            baseSelection: psPRIMARY.baseSelection,
          }),
        ]
      : []
  );

// createPsArgsPRIMARY 추출
const pickCreatePsArgsPRIMARY = (fPRIMARY: FlatPRIMARY) => ({
  maxCohortSizeForFitting:
    fPRIMARY.createPsArgsPRIMARY?.maxCohortSizeForFitting ?? null,
  errorOnHighCorrelation:
    fPRIMARY.createPsArgsPRIMARY?.errorOnHighCorrelation ?? null,
  Prior: fPRIMARY.createPsArgsPRIMARY?.PriorPRIMARY ?? null,
  Control: fPRIMARY.createPsArgsPRIMARY?.ControlPRIMARY ?? null,
});

const DEFAULT_CREATE_PS_ARGS_PRIMARY = {
  maxCohortSizeForFitting: 0,
  errorOnHighCorrelation: false,
  Prior: null,
  Control: null,
};

const isSpecifiedCreatePsArgsPRIMARY = (gPRIMARY: FlatPRIMARY) =>
  stableStringify(pickCreatePsArgsPRIMARY(gPRIMARY)) !==
  stableStringify(DEFAULT_CREATE_PS_ARGS_PRIMARY);

// Set 기반 diffMetrics (PRIMARY에서 사용)
const diffMetricsSets = (setA: Set<string>, setB: Set<string>) => {
  const both = [...setA].filter((x) => setB.has(x));
  const predOnly = [...setB].filter((x) => !setA.has(x));
  const goldOnly = [...setA].filter((x) => !setB.has(x));

  const TP = both.length;
  const FP = predOnly.length;
  const FN = goldOnly.length;

  const precision = TP + FP > 0 ? TP / (TP + FP) : null;
  const recall = TP + FN > 0 ? TP / (TP + FN) : null;
  const f1 =
    precision && recall && precision + recall > 0
      ? (2 * precision * recall) / (precision + recall)
      : null;

  const accuracy = TP + FP + FN > 0 ? TP / (TP + FP + FN) : null;

  return {
    bothCount: TP,
    predOnlyCount: FP,
    goldOnlyCount: FN,
    precision,
    recall,
    f1,
    accuracy,
  };
};

// =======================
// 메인: evaluateFlatPRIMARY
// =======================

export const evaluateFlatPRIMARY = (A_PRIMARY: FlatPRIMARY, B_PRIMARY: FlatPRIMARY) => {
  const AF_PRIMARY = toFactsPRIMARY(A_PRIMARY);
  const BF_PRIMARY = toFactsPRIMARY(B_PRIMARY);

  // gold에 존재하는 key만 스코프
  const goldKeysPRIMARY = new Set<string>();
  for (const f of AF_PRIMARY) goldKeysPRIMARY.add(factKey(f));

  const BF_scoped_PRIMARY = new Set<string>();
  for (const f of BF_PRIMARY) if (goldKeysPRIMARY.has(factKey(f))) BF_scoped_PRIMARY.add(f);

  // 교집합/차집합
  const interPRIMARY = new Set<string>();
  for (const f of AF_PRIMARY) if (BF_scoped_PRIMARY.has(f)) interPRIMARY.add(f);

  const unionPRIMARY = new Set<string>(AF_PRIMARY);
  for (const f of BF_scoped_PRIMARY) unionPRIMARY.add(f);

  const TP_PRIMARY = interPRIMARY.size;
  const FP_PRIMARY = [...BF_scoped_PRIMARY].filter((f) => !AF_PRIMARY.has(f)).length;
  const FN_PRIMARY = [...AF_PRIMARY].filter((f) => !BF_scoped_PRIMARY.has(f)).length;

  const jaccardPRIMARY = safeDiv(TP_PRIMARY, unionPRIMARY.size, 1);
  const recallPRIMARY = safeDiv(TP_PRIMARY, AF_PRIMARY.size, 1);
  const precisionPRIMARY = safeDiv(TP_PRIMARY, BF_scoped_PRIMARY.size, 1);

  // --- 섹션별 정확도 및 메트릭 계산 ---

  // studyPeriodsPRIMARY
  const spSpecifiedPRIMARY = !!A_PRIMARY.studyPeriodsPRIMARY;
  const spSetA = canonStudyPeriodsPRIMARY(A_PRIMARY.studyPeriodsPRIMARY);
  const spSetB = canonStudyPeriodsPRIMARY(B_PRIMARY.studyPeriodsPRIMARY);
  const spCountsPRIMARY = spSpecifiedPRIMARY
    ? diffMetricsSets(spSetA, spSetB)
    : null;
  const spAccPRIMARY: boolean | null = spSpecifiedPRIMARY
    ? setEq(spSetA, spSetB)
    : null;

  // timeAtRisksPRIMARY
  const tarSpecifiedPRIMARY = !!A_PRIMARY.timeAtRisksPRIMARY;
  const tarSetA = canonTimeAtRisksPRIMARY(A_PRIMARY.timeAtRisksPRIMARY);
  const tarSetB = canonTimeAtRisksPRIMARY(B_PRIMARY.timeAtRisksPRIMARY);
  const tarCountsPRIMARY = tarSpecifiedPRIMARY
    ? diffMetricsSets(tarSetA, tarSetB)
    : null;
  const tarAccPRIMARY: boolean | null = tarSpecifiedPRIMARY
    ? setEq(tarSetA, tarSetB)
    : null;

  // propensityScoreAdjustment (psSettingsPRIMARY + createPsArgsPRIMARY)
  const psSpecifiedPRIMARY = !!A_PRIMARY.psSettingsPRIMARY;
  const cpsSpecifiedPRIMARY = isSpecifiedCreatePsArgsPRIMARY(A_PRIMARY);
  const psaSpecifiedPRIMARY = psSpecifiedPRIMARY || cpsSpecifiedPRIMARY;

  const psSetA = canonPsSettingsPRIMARY(A_PRIMARY.psSettingsPRIMARY);
  const psSetB = canonPsSettingsPRIMARY(B_PRIMARY.psSettingsPRIMARY);

  const psEqPRIMARY = psSpecifiedPRIMARY ? setEq(psSetA, psSetB) : true;

  const cpsEqPRIMARY = cpsSpecifiedPRIMARY
    ? stableStringify(pickCreatePsArgsPRIMARY(A_PRIMARY)) ===
      stableStringify(pickCreatePsArgsPRIMARY(B_PRIMARY))
    : true;

  const psaAccPRIMARY: boolean | null = psaSpecifiedPRIMARY
    ? psEqPRIMARY && cpsEqPRIMARY
    : null;
  const psaCountsPRIMARY = psaSpecifiedPRIMARY
    ? diffMetricsSets(psSetA, psSetB)
    : null;

  // --- 결과 리턴 (기존 evaluate.ts와 동일한 형태) ---
  return {
    jaccard: jaccardPRIMARY,
    recall: recallPRIMARY,
    precision: precisionPRIMARY,
    counts: {
      gold: AF_PRIMARY.size,
      pred: BF_scoped_PRIMARY.size,
      intersection: TP_PRIMARY,
      union: unionPRIMARY.size,
      both: TP_PRIMARY,
      predOnly: FP_PRIMARY,
      goldOnly: FN_PRIMARY,
    },
    details: {
      bothJson: [...interPRIMARY],
      predJsonOnly: [...BF_scoped_PRIMARY].filter((f) => !AF_PRIMARY.has(f)),
      goldJsonOnly: [...AF_PRIMARY].filter((f) => !BF_scoped_PRIMARY.has(f)),
    },
    sectionAccuracy: {
      studyPeriods: spAccPRIMARY,
      timeAtRisks: tarAccPRIMARY,
      propensityScoreAdjustment: psaAccPRIMARY,
    },
    sectionCounts: {
      studyPeriods: spCountsPRIMARY,
      timeAtRisks: tarCountsPRIMARY,
      propensityScoreAdjustment: psaCountsPRIMARY,
    },
  };
};
