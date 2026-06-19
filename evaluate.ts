// evaluate.ts

import { FlattenStudyDTO } from "./flatten";
import { canonEquivalence, applyAcceptToPredFlat } from "./acceptableAnswers";

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

  // 평가 대상 = 4개 섹션만: studyPeriods / timeAtRisks / psSettings / outcomeModels.
  // 스칼라(washoutPeriod 등) · createPsArgs · description 은 평가에서 제외.
  // 배열 항목: 완전일치 항목으로 비교.
  // 빈 배열([])도 채점에 포함: 비어있으면 sentinel fact를 emit한다.
  //  gold 빈 + pred 빈     → sentinel끼리 매칭 → TP (정답)
  //  gold 빈 + pred 값있음 → gold sentinel은 FN, pred 값은 FP (오답)
  const EMPTY = (field: string) => canonicalizeObject(field, { __empty__: true });

  if ((flat.studyPeriods ?? []).length === 0) {
    facts.add(EMPTY("studyPeriods"));
  } else {
    for (const sp of flat.studyPeriods ?? []) {
      facts.add(
        canonicalizeObject("studyPeriods", {
          studyStartDate: sp.studyStartDate,
          studyEndDate: sp.studyEndDate,
        })
      );
    }
  }

  if ((flat.timeAtRisks ?? []).length === 0) {
    facts.add(EMPTY("timeAtRisks"));
  } else {
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
  }

  if ((flat.psSettings ?? []).length === 0) {
    facts.add(EMPTY("psSettings"));
  } else {
    for (const p of flat.psSettings ?? []) {
      facts.add(
        canonicalizeObject("psSettings", {
          maxRatio: normalizeMaxRatio(p.maxRatio),
          caliper: p.caliper,
          caliperScale: p.caliperScale,
          numberOfStrata: p.numberOfStrata,
          baseSelection: p.baseSelection,
          trimByPsArgs: p.trimByPsArgs ?? null,
          inversePtWeighting: p.inversePtWeighting,
        })
      );
    }
  }

  // outcomeModels[] — per-item modelType/useCovariates (C5)
  if ((flat.outcomeModels ?? []).length === 0) {
    facts.add(EMPTY("outcomeModels"));
  } else {
    for (const om of flat.outcomeModels ?? []) {
      facts.add(
        canonicalizeObject("outcomeModels", {
          modelType: om.modelType,
          useCovariates: om.useCovariates,
        })
      );
    }
  }

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

//riskwindow 기간 normalize
const normalizeRiskWindowEnd = (n: number | null | undefined): number | null =>
  canonEquivalence("riskWindowEnd", n ?? null) as number | null;

// riskWindowStart: 0과 1을 동치로 취급
const normalizeRiskWindowStart = (n: number | null | undefined): number | null =>
  canonEquivalence("riskWindowStart", n ?? null) as number | null;

// 2) maxRatio: 0 (no max) 와 100 을 동치로 취급
const normalizeMaxRatio = (n: number | null | undefined): number | null =>
  canonEquivalence("maxRatio", n ?? null) as number | null;

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
        riskWindowStart: normalizeRiskWindowStart(t.riskWindowStart),
        startAnchor: t.startAnchor,
        riskWindowEnd: normalizeRiskWindowEnd(t.riskWindowEnd),
        endAnchor: t.endAnchor,
      })
    )
  );

const canonPsSettings = (arr: Flat["psSettings"]) =>
  new Set(
    (arr ?? []).map((p) =>
      canonicalizeObject("psSettings", {
        maxRatio: normalizeMaxRatio(p.maxRatio),
        caliper: p.caliper,
        caliperScale: p.caliperScale,
        numberOfStrata: p.numberOfStrata,
        baseSelection: p.baseSelection,
        trimByPsArgs: p.trimByPsArgs ?? null,
        inversePtWeighting: p.inversePtWeighting,
      })
    )
  );

const canonOutcomeModels = (arr: Flat["outcomeModels"]) =>
  new Set(
    (arr ?? []).map((om) =>
      canonicalizeObject("outcomeModels", {
        modelType: om.modelType,
        useCovariates: om.useCovariates,
      })
    )
  );

// outcomeModels[] — per-item (C5); replaces old flat modelType/useCovariates/inversePtWeighting
const isSpecifiedOutcomeModels = (g: Flat) =>
  (g.outcomeModels?.length ?? 0) > 0;

// gold 스코프 키만으로 Jaccard/Recall/Precision + 섹션별 정확도
export const evaluateFlat = (A: Flat, Braw: Flat, caseName?: string) => {
  // 복수정답 accept: pred를 gold 기준으로 snap (gold는 불변). caseName 없으면 미적용.
  const B = caseName ? applyAcceptToPredFlat(caseName, A, Braw) : Braw;
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

  // studyPeriods — gold가 비어있어도(null/[]) 항상 채점.
  // gold 빈 + pred 빈 → 정답(true), gold 빈 + pred가 기간 생성 → 오답(false)
  const spCounts = diffMetrics(A.studyPeriods, B.studyPeriods, (arr) => canonStudyPeriods(arr));
  const spAcc: boolean | null = setEq(
    canonStudyPeriods(A.studyPeriods),
    canonStudyPeriods(B.studyPeriods)
  );

  // timeAtRisks — 빈 항목([])도 항상 채점 (studyPeriods와 동일 규칙)
  const tarCounts = diffMetrics(A.timeAtRisks, B.timeAtRisks, (arr) => canonTimeAtRisks(arr));
  const tarAcc: boolean | null = setEq(
    canonTimeAtRisks(A.timeAtRisks),
    canonTimeAtRisks(B.timeAtRisks)
  );

  // propensityScoreAdjustment — psSettings 만 (createPsArgs 제외) — 빈 항목도 항상 채점
  const psaAcc: boolean | null = setEq(
    canonPsSettings(A.psSettings),
    canonPsSettings(B.psSettings)
  );
  const psaCounts = diffMetrics(A.psSettings, B.psSettings, (arr) => canonPsSettings(arr));

  // outcomeModels[] (C5) — 빈 항목도 항상 채점
  const omCounts = diffMetrics(A.outcomeModels, B.outcomeModels, (arr) => canonOutcomeModels(arr));
  const omAcc: boolean | null = setEq(
    canonOutcomeModels(A.outcomeModels),
    canonOutcomeModels(B.outcomeModels)
  );

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
      outcomeModels: omAcc,
    },
    sectionCounts: {
      studyPeriods: spCounts,
      timeAtRisks: tarCounts,
      propensityScoreAdjustment: psaCounts,
      outcomeModels: omCounts,
    },
  };
};
