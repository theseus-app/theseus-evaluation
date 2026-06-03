// acceptableAnswers.ts
// 단일 소스: 채점 허용 규칙(복수정답)을 한 곳에서 관리.
//  (A) equivalence: 대칭 동등 클래스 — gold·pred 양쪽에 적용 (canonical = 배열 첫 원소)
//  (B) accept: study별 gold-기준 비대칭 허용 — pred를 gold 값으로 snap (다음 태스크에서 추가)

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
