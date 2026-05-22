// ===== Types sourced from theseus-core (new cmAnalysis structure) =====
export type {
    StudyDTO,
    Anchor,
    RemoveDuplicate,
    CaliperScale,
    BaseSelection,
    ModelType,
    CvType,
    NoiseLevel,
    TrimByPsArgs,
    MatchOnPsArgs,
    StratifyByPsArgs,
    Prior,
    Control,
    PsSetting,
    OutcomeModel,
} from "theseus-core";

import type {
    StudyDTO,
    Anchor,
    RemoveDuplicate,
    CaliperScale,
    BaseSelection,
    ModelType,
    Prior,
    Control,
    TrimByPsArgs,
} from "theseus-core";

// ===== 유틸: yyyyMMdd 정규화 (숫자/문자/널 모두 허용) =====
const toYyyyMmDd = (v: string | number | null | undefined): string | null => {
    if (v === null || v === undefined) return null;
    const s = String(v).trim();
    // 숫자 8자리 또는 yyyy-mm-dd 같은 경우도 허용 후 yyyyMMdd로 정제
    if (/^\d{8}$/.test(s)) return s; // 이미 yyyyMMdd
    const onlyDigits = s.replace(/\D/g, "");
    return onlyDigits.length === 8 ? onlyDigits : null;
};

// ===== Flattened representation type (eval-specific, NOT in theseus-core) =====
export type FlattenStudyDTO = {
    /////// getDbCohortMethodDataArgs ///////
    studyPeriods: { description: string; studyStartDate: string | null; studyEndDate: string | null }[]; // yyyyMMdd
    maxCohortSize: number;

    /////// getDbCohortMethodDataArgs — moved fields (C1) ///////
    firstExposureOnly: boolean;
    removeDuplicateSubjects: RemoveDuplicate;
    restrictToCommonPeriod: boolean;
    washoutPeriod: number;

    /////// createStudyPopArgs ///////
    censorAtNewRiskWindow: boolean;
    removeSubjectsWithPriorOutcome: boolean;
    priorOutcomeLookback: number;
    timeAtRisks: {
        description: string;
        riskWindowStart: number;
        startAnchor: Anchor;
        riskWindowEnd: number;
        endAnchor: Anchor;
        minDaysAtRisk: number;
    }[];

    /////// psSettings (top-level, C3) + trimByPsArgs/inversePtWeighting per item (C4) ///////
    psSettings: {
        description: string;
        trimByPsArgs: TrimByPsArgs | null;
        maxRatio: number | null;
        caliper: number | null;
        caliperScale: CaliperScale | null;
        numberOfStrata: number | null;
        baseSelection: BaseSelection | null;
        inversePtWeighting: boolean;
    }[];

    /////// createPsArgs (top-level, C3) ///////
    createPsArgs: {
        maxCohortSizeForFitting: number;
        errorOnHighCorrelation: boolean;
        Prior: Prior | null;
        Control: Control | null;
    };

    /////// fitOutcomeModelArgs — outcomeModels per item (C5); modelType/useCovariates/inversePtWeighting removed from top-level ///////
    outcomeModels: {
        description: string;
        modelType: ModelType;
        useCovariates: boolean;
    }[];
    stratified: boolean;
    Prior: Prior | null;
    Control: Control | null;
};


// ===== 메인: flatten 함수 =====
export function flattenStudy(dto: Partial<StudyDTO>): FlattenStudyDTO {
    const g = dto.getDbCohortMethodDataArgs ?? {
        studyPeriods: [],
        firstExposureOnly: false,
        removeDuplicateSubjects: "keep all" as RemoveDuplicate,
        restrictToCommonPeriod: false,
        washoutPeriod: 0,
        maxCohortSize: 0,
    };
    const c = dto.createStudyPopArgs ?? {
        removeSubjectsWithPriorOutcome: false,
        priorOutcomeLookback: 0,
        timeAtRisks: [],
        censorAtNewRiskWindow: false,
    };
    const psSettings = dto.psSettings ?? [];
    const createPsArgs = dto.createPsArgs ?? {
        maxCohortSizeForFitting: 0,
        errorOnHighCorrelation: false,
        prior: null,
        control: null,
    };
    const fom = dto.fitOutcomeModelArgs ?? {
        outcomeModels: [],
        stratified: false,
        prior: null,
        control: null,
    };

    // studyPeriods — now includes description (C2)
    const flatStudyPeriods = (g.studyPeriods ?? []).map(sp => ({
        description: sp?.description ?? "",
        studyStartDate: toYyyyMmDd(sp?.studyStartDate ?? null),
        studyEndDate: toYyyyMmDd(sp?.studyEndDate ?? null),
    }));

    // timeAtRisks — now includes description
    const timeAtRisks = (c.timeAtRisks ?? []).map(t => ({
        description: t.description ?? "",
        riskWindowStart: t.riskWindowStart ?? 0,
        startAnchor: (t.startAnchor ?? "cohort start") as Anchor,
        riskWindowEnd: t.riskWindowEnd ?? 0,
        endAnchor: (t.endAnchor ?? "cohort end") as Anchor,
        minDaysAtRisk: t.minDaysAtRisk ?? 0,
    }));

    // psSettings — top-level (C3), now with trimByPsArgs + inversePtWeighting (C4)
    const flatPsSettings = (psSettings ?? []).map(s => {
        const m = s?.matchOnPsArgs ?? null;
        const st = s?.stratifyByPsArgs ?? null;
        return {
            description: s?.description ?? "",
            trimByPsArgs: s?.trimByPsArgs ?? null,
            maxRatio: m ? m.maxRatio ?? null : null,
            caliper: m ? m.caliper ?? null : null,
            caliperScale: m ? (m.caliperScale ?? null) : null,
            numberOfStrata: st ? st.numberOfStrata ?? null : null,
            baseSelection: st ? (st.baseSelection ?? null) : null,
            inversePtWeighting: s?.inversePtWeighting ?? false,
        };
    });

    // outcomeModels — per-item modelType/useCovariates (C5)
    const outcomeModels = (fom.outcomeModels ?? []).map(om => ({
        description: om.description ?? "",
        modelType: (om.modelType ?? "cox") as ModelType,
        useCovariates: om.useCovariates ?? false,
    }));

    return {
        // getDbCohortMethodDataArgs
        studyPeriods: flatStudyPeriods,
        maxCohortSize: g.maxCohortSize ?? 0,

        // moved from createStudyPopArgs → getDbCohortMethodDataArgs (C1)
        firstExposureOnly: g.firstExposureOnly ?? false,
        removeDuplicateSubjects: (g.removeDuplicateSubjects ?? "keep all") as RemoveDuplicate,
        restrictToCommonPeriod: g.restrictToCommonPeriod ?? false,
        washoutPeriod: g.washoutPeriod ?? 0,

        // createStudyPopArgs
        censorAtNewRiskWindow: c.censorAtNewRiskWindow ?? false,
        removeSubjectsWithPriorOutcome: c.removeSubjectsWithPriorOutcome ?? false,
        priorOutcomeLookback: c.priorOutcomeLookback ?? 0,
        timeAtRisks,

        // psSettings (top-level, C3) + trimByPsArgs/inversePtWeighting (C4)
        psSettings: flatPsSettings,
        createPsArgs: {
            maxCohortSizeForFitting: createPsArgs.maxCohortSizeForFitting ?? 0,
            errorOnHighCorrelation: createPsArgs.errorOnHighCorrelation ?? false,
            // 대소문자 키 유지 (요청사항)
            Prior: createPsArgs.prior ?? null,
            Control: createPsArgs.control ?? null,
        },

        // fitOutcomeModelArgs — outcomeModels per item (C5)
        outcomeModels,
        stratified: fom.stratified ?? false,
        // 대소문자 키 유지 (요청사항)
        Prior: fom.prior ?? null,
        Control: fom.control ?? null,
    };
}
