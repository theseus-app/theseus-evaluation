//flatten accuracy: gold에 존재하는 항목만 평탄화해서 exact(0/1)로 평균
//array jaccard: 같은 “gold에 존재하는 배열 항목들” 각각에 대해 Jaccard 계산(경로별 상세)

import { PsSetting, StudyDTO } from "./types";

// evaluator-selective.ts
// flatten accuracy: gold에 존재하는 항목만 평탄화해서 exact(0/1)로 평균
// array jaccard: 같은 “gold에 존재하는 배열 항목들” 각각에 대해 Jaccard 계산(경로별 상세)

// === 평가 스펙 ===
type FieldKind = "scalar" | "array";
interface FieldSpec {
    path: string;               // lodash-like path
    kind: FieldKind;            // scalar | array
    projectItemKey?: (x: any) => string; // (선택) 정적 projector - shape projector가 우선
}

// SPEC은 "전체 항목"을 포함 (넓게 정의)
// 실제 채점은 gold에 존재하는 path만 대상으로 수행됨
const SPEC: FieldSpec[] = [
    // getDbCohortMethodDataArgs
    { path: "getDbCohortMethodDataArgs.maxCohortSize", kind: "scalar" },
    {
        path: "getDbCohortMethodDataArgs.studyPeriods",
        kind: "array",
        projectItemKey: (p) =>
            JSON.stringify({
                // description 등은 명시하지 않음; 어차피 shape 기반으로 gold에 있는 필드만 투영됨
                studyStartDate: normDate8(p?.studyStartDate),
                studyEndDate: normDate8(p?.studyEndDate),
            }),
    },

    // createStudyPopArgs (scalars)
    { path: "createStudyPopArgs.restrictToCommonPeriod", kind: "scalar" },
    { path: "createStudyPopArgs.firstExposureOnly", kind: "scalar" },
    { path: "createStudyPopArgs.washoutPeriod", kind: "scalar" },
    { path: "createStudyPopArgs.removeDuplicateSubjects", kind: "scalar" },
    { path: "createStudyPopArgs.censorAtNewRiskWindow", kind: "scalar" },
    { path: "createStudyPopArgs.removeSubjectsWithPriorOutcome", kind: "scalar" },
    { path: "createStudyPopArgs.priorOutcomeLookBack", kind: "scalar" },

    // createStudyPopArgs (array)
    {
        path: "createStudyPopArgs.timeAtRisks",
        kind: "array",
        projectItemKey: (t) =>
            JSON.stringify({
                // description 제외; shape projector가 최종적으로 gold 필드만 투영
                riskWindowStart: normNum(t?.riskWindowStart),
                startAnchor: normStr(t?.startAnchor),
                riskWindowEnd: normNum(t?.riskWindowEnd),
                endAnchor: normStr(t?.endAnchor),
                minDaysAtRisk: normNum(t?.minDaysAtRisk),
            }),
    },

    // propensityScoreAdjustment.psSettings (array)
    {
        path: "propensityScoreAdjustment.psSettings",
        kind: "array",
        projectItemKey: (s: PsSetting) =>
            JSON.stringify({
                // description 제외; shape projector가 최종적으로 gold 필드만 투영
                matchOnPsArgs: s?.matchOnPsArgs
                    ? {
                        maxRatio: normNum(s.matchOnPsArgs.maxRatio),
                        caliper: normNum(s.matchOnPsArgs.caliper),
                        caliperScale: normStr(s.matchOnPsArgs.caliperScale),
                    }
                    : null,
                stratifyByPsArgs: s?.stratifyByPsArgs
                    ? {
                        numberOfStrata: normNum(s.stratifyByPsArgs.numberOfStrata),
                        baseSelection: normStr(s.stratifyByPsArgs.baseSelection),
                    }
                    : null,
            }),
    },

    // propensityScoreAdjustment.createPsArgs.*
    { path: "propensityScoreAdjustment.createPsArgs.maxCohortSizeForFitting", kind: "scalar" },
    { path: "propensityScoreAdjustment.createPsArgs.errorOnHighCorrelation", kind: "scalar" },
    { path: "propensityScoreAdjustment.createPsArgs.prior.priorType", kind: "scalar" },
    { path: "propensityScoreAdjustment.createPsArgs.prior.useCrossValidation", kind: "scalar" },
    { path: "propensityScoreAdjustment.createPsArgs.control.tolerance", kind: "scalar" },
    { path: "propensityScoreAdjustment.createPsArgs.control.cvType", kind: "scalar" },
    { path: "propensityScoreAdjustment.createPsArgs.control.fold", kind: "scalar" },
    { path: "propensityScoreAdjustment.createPsArgs.control.cvRepetitions", kind: "scalar" },
    { path: "propensityScoreAdjustment.createPsArgs.control.noiseLevel", kind: "scalar" },
    { path: "propensityScoreAdjustment.createPsArgs.control.resetCoefficients", kind: "scalar" },
    { path: "propensityScoreAdjustment.createPsArgs.control.startingVariance", kind: "scalar" },

    // fitOutcomeModelArgs.*
    { path: "fitOutcomeModelArgs.modelType", kind: "scalar" },
    { path: "fitOutcomeModelArgs.stratified", kind: "scalar" },
    { path: "fitOutcomeModelArgs.useCovariates", kind: "scalar" },
    { path: "fitOutcomeModelArgs.inversePtWeighting", kind: "scalar" },
    { path: "fitOutcomeModelArgs.prior.priorType", kind: "scalar" },
    { path: "fitOutcomeModelArgs.prior.useCrossValidation", kind: "scalar" },
    { path: "fitOutcomeModelArgs.control.tolerance", kind: "scalar" },
    { path: "fitOutcomeModelArgs.control.cvType", kind: "scalar" },
    { path: "fitOutcomeModelArgs.control.fold", kind: "scalar" },
    { path: "fitOutcomeModelArgs.control.cvRepetitions", kind: "scalar" },
    { path: "fitOutcomeModelArgs.control.noiseLevel", kind: "scalar" },
    { path: "fitOutcomeModelArgs.control.resetCoefficients", kind: "scalar" },
    { path: "fitOutcomeModelArgs.control.startingVariance", kind: "scalar" },
];

// === 경로/값 유틸 ===
function getByPath(obj: any, path: string) {
    return path.split(".").reduce((o, k) => (o == null ? undefined : o[k]), obj);
}

// gold에 "키가 존재"하면 true (값이 null이더라도 true)
function hasAtPath(obj: any, path: string): boolean {
    const parts = path.split(".");
    let cur = obj;
    for (let i = 0; i < parts.length; i++) {
        const k = parts[i];
        if (cur == null) return false;
        if (Array.isArray(cur)) return true; // 배열 자체 존재
        if (!Object.prototype.hasOwnProperty.call(cur, k)) return false;
        cur = cur[k];
    }
    return true;
}

// 정규화
const normStr = (x: any) => (x == null ? null : String(x).trim().toLowerCase());
const normNum = (x: any) => {
    if (x == null || x === "") return null;
    const n = Number(x);
    return Number.isFinite(n) ? n : null;
};
const normDate8 = (x: any) => {
    if (x == null) return null;
    const d = String(x).replace(/\D/g, "").padEnd(8, "0").slice(0, 8);
    return d.length === 8 ? d : null;
};

// === gold 배열에서 "존재하는 필드만" 추출하는 shape 생성 ===
type Shape = { [k: string]: true | Shape }; // true = primitive/널 | Shape = nested object

function mergeShape(a: Shape, b: Shape): Shape {
    const out: Shape = { ...a };
    for (const [k, v] of Object.entries(b)) {
        if (out[k] === undefined) out[k] = v;
        else if (typeof out[k] === "object" && typeof v === "object") {
            out[k] = mergeShape(out[k] as Shape, v as Shape);
        } else {
            out[k] = true;
        }
    }
    return out;
}
function shapeFromItem(it: any): Shape {
    const s: Shape = {};
    if (it && typeof it === "object" && !Array.isArray(it)) {
        for (const [k, v] of Object.entries(it)) {
            if (v && typeof v === "object" && !Array.isArray(v)) s[k] = shapeFromItem(v);
            else s[k] = true;
        }
    }
    return s;
}
function shapeFromArray(arr: any[]): Shape {
    return (arr ?? []).reduce((acc: Shape, it: any) => mergeShape(acc, shapeFromItem(it)), {});
}

// === shape에 따라 item을 "gold에 있는 필드만" 투영 + 정규화 후 직렬화 ===
function projectItemWithShape(it: any, shape: Shape): string {
    function proj(obj: any, sh: Shape): any {
        const out: any = {};
        for (const [k, v] of Object.entries(sh)) {
            const val = obj?.[k];
            if (v === true) {
                if (typeof val === "string") out[k] = normStr(val);
                else if (typeof val === "number") out[k] = normNum(val);
                else if (typeof val === "boolean") out[k] = val;
                else if (val == null) out[k] = null;
                else out[k] = JSON.stringify(val ?? null); // 구조 불일치 보수 처리
            } else {
                out[k] = proj(val ?? {}, v as Shape);
            }
        }
        return out;
    }
    return JSON.stringify(proj(it ?? {}, shape));
}

// === 평탄화: 스칼라는 값 자체, 배열은 shape 기반 Set→정렬→JSON 문자열로 고정화 ===
function stableArrayKeyWithShape(arr: any[], shape: Shape, fallbackProject?: (x: any) => string): string {
    const S = new Set<string>();
    for (const it of arr ?? []) {
        const key =
            shape && Object.keys(shape).length > 0
                ? projectItemWithShape(it, shape)
                : (fallbackProject ? fallbackProject(it) : JSON.stringify(it));
        S.add(key);
    }
    const sorted = [...S].sort();
    return JSON.stringify(sorted);
}

/* ----------------------------------------------------------
   1) Flatten Accuracy (gold에 존재하는 항목만)
---------------------------------------------------------- */

export interface FlattenItem {
    path: string;
    kind: FieldKind;
    exact: 0 | 1;
    goldValue: any;
    predValue: any;
}

export interface FlattenAccuracyResult {
    items: FlattenItem[];
    accuracy: number;
}

/** gold에 존재하는 path만 평탄화해서 exact accuracy 측정 */
export function evaluateExactOnlyFlattened(gold: any, pred: StudyDTO): FlattenAccuracyResult {
    const items: FlattenItem[] = [];

    for (const spec of SPEC) {
        if (!hasAtPath(gold, spec.path)) continue; // gold에 없는 path는 스킵

        const g = getByPath(gold, spec.path);
        const p = getByPath(pred as any, spec.path);

        if (spec.kind === "scalar") {
            const gv =
                typeof g === "string" ? normStr(g) :
                    typeof g === "number" ? normNum(g) :
                        typeof g === "boolean" ? g : (g ?? null);
            const pv =
                typeof p === "string" ? normStr(p) :
                    typeof p === "number" ? normNum(p) :
                        typeof p === "boolean" ? p : (p ?? null);
            const exact: 0 | 1 = (gv === pv ? 1 : 0);
            items.push({ path: spec.path, kind: "scalar", exact, goldValue: gv, predValue: pv });
        } else {
            // 배열: gold shape 기준으로 안정 키 생성
            const gArr = Array.isArray(g) ? g : [];
            const pArr = Array.isArray(p) ? p : [];
            const shape = shapeFromArray(gArr);
            const gv = stableArrayKeyWithShape(gArr, shape, spec.projectItemKey);
            const pv = stableArrayKeyWithShape(pArr, shape, spec.projectItemKey);
            const exact: 0 | 1 = (gv === pv ? 1 : 0);
            items.push({ path: spec.path, kind: "array", exact, goldValue: gv, predValue: pv });
        }
    }

    const accuracy = items.length ? items.reduce((s, it) => s + it.exact, 0) / items.length : 0;
    return { items, accuracy };
}