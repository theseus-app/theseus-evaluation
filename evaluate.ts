// evaluate.ts

import { FlattenStudyDTO } from "./flatten";

type Flat = FlattenStudyDTO;

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

// 1-depth facts로 변환 (완전일치 규칙 반영)
export const toFacts = (flat: Flat): Set<string> => {
    const facts = new Set<string>();

    // 스칼라
    facts.add(`maxCohortSize=${normVal(flat.maxCohortSize)}`);
    facts.add(`restrictToCommonPeriod=${normVal(flat.restrictToCommonPeriod)}`);
    facts.add(`firstExposureOnly=${normVal(flat.firstExposureOnly)}`);
    facts.add(`washoutPeriod=${normVal(flat.washoutPeriod)}`);
    facts.add(`removeDuplicateSubjects=${normVal(flat.removeDuplicateSubjects)}`);
    facts.add(`censorAtNewRiskWindow=${normVal(flat.censorAtNewRiskWindow)}`);
    facts.add(`removeSubjectsWithPriorOutcome=${normVal(flat.removeSubjectsWithPriorOutcome)}`);
    facts.add(`priorOutcomeLookBack=${normVal(flat.priorOutcomeLookBack)}`);

    facts.add(`modelType=${normVal(flat.modelType)}`);
    facts.add(`stratified=${normVal(flat.stratified)}`);
    facts.add(`useCovariates=${normVal(flat.useCovariates)}`);
    facts.add(`inversePtWeighting=${normVal(flat.inversePtWeighting)}`);

    // 배열 항목: 완전일치 항목으로 비교
    for (const sp of flat.studyPeriods ?? []) {
        facts.add(
            canonicalizeObject("studyPeriods", {
                studyStartDate: sp.studyStartDate,
                studyEndDate: sp.studyEndDate,
            })
        );
    }

    for (const t of flat.timeAtRisks ?? []) {
        facts.add(
            canonicalizeObject("timeAtRisks", {
                riskWindowStart: t.riskWindowStart,
                startAnchor: t.startAnchor,
                riskWindowEnd: t.riskWindowEnd,
                endAnchor: t.endAnchor,
                minDaysAtRisk: t.minDaysAtRisk,
            })
        );
    }

    for (const p of flat.psSettings ?? []) {
        facts.add(
            canonicalizeObject("psSettings", {
                maxRatio: p.maxRatio,
                caliper: p.caliper,
                caliperScale: p.caliperScale,
                numberOfStrata: p.numberOfStrata,
                baseSelection: p.baseSelection,
            })
        );
    }

    // createPsArgs: 단일 복합 항목
    facts.add(
        canonicalizeObject("createPsArgs", {
            maxCohortSizeForFitting: flat.createPsArgs?.maxCohortSizeForFitting ?? null,
            errorOnHighCorrelation: flat.createPsArgs?.errorOnHighCorrelation ?? null,
            Prior: flat.createPsArgs?.Prior ?? null,
            Control: flat.createPsArgs?.Control ?? null,
        })
    );

    // fitOutcomeModelArgs 복합 항목은 섹션 정확도 계산에서 직접 비교,
    // 여기서는 기존처럼 Prior/Control만 문자열로 포함(전체 지표용)
    facts.add(`fitOutcomeModel.Prior=${normVal(flat.Prior ?? null)}`);
    facts.add(`fitOutcomeModel.Control=${normVal(flat.Control ?? null)}`);

    return facts;
};

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

// --- 섹션 비교 헬퍼들 ---

const setEq = (A: Set<string>, B: Set<string>) =>
    A.size === B.size && [...A].every((x) => B.has(x));

const canonStudyPeriods = (arr: Flat["studyPeriods"]) =>
    new Set(
        (arr ?? []).map((sp) =>
            canonicalizeObject("studyPeriods", {
                studyStartDate: sp.studyStartDate,
                studyEndDate: sp.studyEndDate,
            })
        )
    );

const canonTimeAtRisks = (arr: Flat["timeAtRisks"]) =>
    new Set(
        (arr ?? []).map((t) =>
            canonicalizeObject("timeAtRisks", {
                riskWindowStart: t.riskWindowStart,
                startAnchor: t.startAnchor,
                riskWindowEnd: t.riskWindowEnd,
                endAnchor: t.endAnchor,
                minDaysAtRisk: t.minDaysAtRisk,
            })
        )
    );

const canonPsSettings = (arr: Flat["psSettings"]) =>
    new Set(
        (arr ?? []).map((p) =>
            canonicalizeObject("psSettings", {
                maxRatio: p.maxRatio,
                caliper: p.caliper,
                caliperScale: p.caliperScale,
                numberOfStrata: p.numberOfStrata,
                baseSelection: p.baseSelection,
            })
        )
    );

const pickCreatePsArgs = (f: Flat) => ({
    maxCohortSizeForFitting: f.createPsArgs?.maxCohortSizeForFitting ?? null,
    errorOnHighCorrelation: f.createPsArgs?.errorOnHighCorrelation ?? null,
    Prior: f.createPsArgs?.Prior ?? null,
    Control: f.createPsArgs?.Control ?? null,
});
const DEFAULT_CREATE_PS_ARGS = {
    maxCohortSizeForFitting: 0,
    errorOnHighCorrelation: false,
    Prior: null,
    Control: null,
};
const isSpecifiedCreatePsArgs = (g: Flat) =>
    stableStringify(pickCreatePsArgs(g)) !== stableStringify(DEFAULT_CREATE_PS_ARGS);

const pickFitOutcome = (f: Flat) => ({
    modelType: f.modelType,
    stratified: f.stratified,
    useCovariates: f.useCovariates,
    inversePtWeighting: f.inversePtWeighting,
    Prior: f.Prior ?? null,
    Control: f.Control ?? null,
});
const DEFAULT_FIT_OUTCOME = {
    modelType: "cox",
    stratified: false,
    useCovariates: false,
    inversePtWeighting: false,
    Prior: null,
    Control: null,
};
const isSpecifiedFitOutcome = (g: Flat) =>
    stableStringify(pickFitOutcome(g)) !== stableStringify(DEFAULT_FIT_OUTCOME);

// gold 스코프 키만으로 Jaccard/Recall/Precision + 섹션별 정확도
export const evaluateFlat = (A: Flat, B: Flat) => {
  const AF = toFacts(A);
  const BF = toFacts(B);

  // gold에 존재하는 key만 스코프
  const goldKeys = new Set<string>();
  for (const f of AF) goldKeys.add(factKey(f));

  const BF_scoped = new Set<string>();
  for (const f of BF) if (goldKeys.has(factKey(f))) BF_scoped.add(f);

  // 교집합/차집합
  const inter = new Set<string>();
  for (const f of AF) if (BF_scoped.has(f)) inter.add(f);

  const union = new Set<string>(AF);
  for (const f of BF_scoped) union.add(f);

  const TP = inter.size;
  const FP = [...BF_scoped].filter((f) => !AF.has(f)).length; // pred만(스코프 내)
  const FN = [...AF].filter((f) => !BF_scoped.has(f)).length; // gold만

  const jaccard = safeDiv(TP, union.size, 1);
  const recall = safeDiv(TP, AF.size, 1);
  const precision = safeDiv(TP, BF_scoped.size, 1);

  // --- 공통 헬퍼: 섹션별 diff + metric ---
  const diffMetrics = (
    goldArr: any[] | undefined,
    predArr: any[] | undefined,
    canonizer: (x: any) => Set<string>
  ) => {
    const setA = canonizer(goldArr);
    const setB = canonizer(predArr);
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

    const accuracy =
      TP + FP + FN > 0 ? TP / (TP + FP + FN) : null;

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

  // --- 섹션별 정확도 및 메트릭 계산 ---

  // studyPeriods
  const spSpecified = (A.studyPeriods?.length ?? 0) > 0;
  const spCounts = spSpecified
    ? diffMetrics(A.studyPeriods, B.studyPeriods, (arr) => canonStudyPeriods(arr))
    : null;
  const spAcc: boolean | null = spSpecified
    ? setEq(canonStudyPeriods(A.studyPeriods), canonStudyPeriods(B.studyPeriods))
    : null;

  // timeAtRisks
  const tarSpecified = (A.timeAtRisks?.length ?? 0) > 0;
  const tarCounts = tarSpecified
    ? diffMetrics(A.timeAtRisks, B.timeAtRisks, (arr) => canonTimeAtRisks(arr))
    : null;
  const tarAcc: boolean | null = tarSpecified
    ? setEq(canonTimeAtRisks(A.timeAtRisks), canonTimeAtRisks(B.timeAtRisks))
    : null;

  // propensityScoreAdjustment
  const psSpecified = (A.psSettings?.length ?? 0) > 0;
  const cpsSpecified = isSpecifiedCreatePsArgs(A);
  const psaSpecified = psSpecified || cpsSpecified;

  const psEq = psSpecified
    ? setEq(canonPsSettings(A.psSettings), canonPsSettings(B.psSettings))
    : true;

  const cpsEq = cpsSpecified
    ? stableStringify(pickCreatePsArgs(A)) === stableStringify(pickCreatePsArgs(B))
    : true;

  const psaAcc: boolean | null = psaSpecified ? psEq && cpsEq : null;
  const psaCounts = psaSpecified
    ? diffMetrics(A.psSettings, B.psSettings, (arr) => canonPsSettings(arr))
    : null;

  // fitOutcomeModelArgs
  const fitSpecified = isSpecifiedFitOutcome(A);
  const fitEq =
    stableStringify(pickFitOutcome(A)) === stableStringify(pickFitOutcome(B));
  const fitAcc: boolean | null = fitSpecified ? fitEq : null;

  const fitCounts = fitSpecified
    ? diffMetrics(
        [pickFitOutcome(A)],
        [pickFitOutcome(B)],
        (arr) => new Set(arr.map((x:any) => stableStringify(x)))
      )
    : null;

  // --- 결과 리턴 ---
  return {
    jaccard,
    recall,
    precision,
    counts: {
      gold: AF.size,
      pred: BF_scoped.size,
      intersection: TP,
      union: union.size,
      both: TP,
      predOnly: FP,
      goldOnly: FN,
    },
    details: {
      bothJson: [...inter], // TP
      predJsonOnly: [...BF_scoped].filter((f) => !AF.has(f)), // FP
      goldJsonOnly: [...AF].filter((f) => !BF_scoped.has(f)), // FN
    },
    sectionAccuracy: {
      studyPeriods: spAcc,
      timeAtRisks: tarAcc,
      propensityScoreAdjustment: psaAcc,
      fitOutcomeModelArgs: fitAcc,
    },
    sectionCounts: {
      studyPeriods: spCounts,
      timeAtRisks: tarCounts,
      propensityScoreAdjustment: psaCounts,
      fitOutcomeModelArgs: fitCounts,
    },
  };
};
