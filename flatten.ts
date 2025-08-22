// ===== 재사용 타입들 (네가 이미 갖고 있다면 중복 정의는 제거해도 됨) =====
export type Anchor = "cohort start" | "cohort end";
export type RemoveDuplicate = "keep all" | "keep first" | "remove all";
export type CaliperScale = "propensity score" | "standardized" | "standardized logit";
export type BaseSelection = "all" | "target" | "comparator";
export type ModelType = "logistic" | "poisson" | "cox";
export type CvType = "auto";
export type NoiseLevel = "silent" | "quiet" | "noisy";

export interface MatchOnPsArgs {
    maxRatio: number;     // 0 = no max
    caliper: number;      // 0 = off
    caliperScale: CaliperScale;
}
export interface StratifyByPsArgs {
    numberOfStrata: number;
    baseSelection: BaseSelection;
}
export interface Prior {
    priorType: "laplace";
    useCrossValidation: boolean;
}
export interface Control {
    tolerance: number;
    cvType: CvType;
    fold: number;
    cvRepetitions: number;
    noiseLevel: NoiseLevel;
    resetCoefficients: boolean;
    startingVariance: number; // -1 = auto
}
export interface PsSetting {
    description: string;
    matchOnPsArgs: MatchOnPsArgs | null;
    stratifyByPsArgs: StratifyByPsArgs | null;
}

export type StudyDTO = {
    name: string;
    cohortDefinitions: {
        targetCohort: { id: number | null; name: string };
        comparatorCohort: { id: number | null; name: string };
        outcomeCohort: { id: number | null; name: string }[];
    };
    negativeControlConceptSet: { id: number | null; name: string };
    covariateSelection: {
        conceptsToInclude: { id: number | null; name: string }[];
        conceptsToExclude: { id: number | null; name: string }[];
    };
    getDbCohortMethodDataArgs: {
        studyPeriods: { studyStartDate: string | number | null; studyEndDate: string | number | null }[]; // yyyyMMdd
        maxCohortSize: number; // 0 = no limit
    };
    createStudyPopArgs: {
        restrictToCommonPeriod: boolean;
        firstExposureOnly: boolean;
        washoutPeriod: number;
        removeDuplicateSubjects: RemoveDuplicate;
        censorAtNewRiskWindow: boolean;
        removeSubjectsWithPriorOutcome: boolean;
        priorOutcomeLookBack: number;
        timeAtRisks: {
            description?: string;
            riskWindowStart: number;
            startAnchor: Anchor;
            riskWindowEnd: number;
            endAnchor: Anchor;
            minDaysAtRisk: number;
        }[];
    };
    propensityScoreAdjustment: {
        psSettings: PsSetting[];
        createPsArgs: {
            maxCohortSizeForFitting: number; // 0 = no downsample
            errorOnHighCorrelation: boolean;
            prior: Prior | null;
            control: Control | null;
        };
    };
    fitOutcomeModelArgs: {
        modelType: ModelType;
        stratified: boolean;
        useCovariates: boolean;
        inversePtWeighting: boolean;
        prior: Prior | null;
        control: Control | null;
    };
};

// ===== 네가 원하는 최종 Flatten 타입 =====
export type FlattenStudyDTO = {
    /////// getDbCohortMethodDataArgs ///////
    studyPeriods: { studyStartDate: string | null; studyEndDate: string | null }[]; // yyyyMMdd
    maxCohortSize: number;

    /////// createStudyPopArgs ///////
    restrictToCommonPeriod: boolean;
    firstExposureOnly: boolean;
    washoutPeriod: number;
    removeDuplicateSubjects: RemoveDuplicate;
    censorAtNewRiskWindow: boolean;
    removeSubjectsWithPriorOutcome: boolean;
    priorOutcomeLookBack: number;
    timeAtRisks: {
        riskWindowStart: number;
        startAnchor: Anchor;
        riskWindowEnd: number;
        endAnchor: Anchor;
        minDaysAtRisk: number;
    }[];

    /////// propensityScoreAdjustment ///////
    psSettings: {
        maxRatio: number | null;
        caliper: number | null;
        caliperScale: CaliperScale | null;
        numberOfStrata: number | null;
        baseSelection: BaseSelection | null;
    }[];
    createPsArgs: {
        maxCohortSizeForFitting: number;
        errorOnHighCorrelation: boolean;
        Prior: Prior | null;
        Control: Control | null;
    };

    /////// fitOutcomeModelArgs ///////
    modelType: ModelType;
    stratified: boolean;
    useCovariates: boolean;
    inversePtWeighting: boolean;
    Prior: Prior | null;
    Control: Control | null;
};

// ===== 유틸: yyyyMMdd 정규화 (숫자/문자/널 모두 허용) =====
const toYyyyMmDd = (v: string | number | null | undefined): string | null => {
    if (v === null || v === undefined) return null;
    const s = String(v).trim();
    // 숫자 8자리 또는 yyyy-mm-dd 같은 경우도 허용 후 yyyyMMdd로 정제
    if (/^\d{8}$/.test(s)) return s; // 이미 yyyyMMdd
    const onlyDigits = s.replace(/\D/g, "");
    return onlyDigits.length === 8 ? onlyDigits : null;
};

// ===== 메인: flatten 함수 =====
export function flattenStudy(dto: Partial<StudyDTO>): FlattenStudyDTO {
    const g = dto.getDbCohortMethodDataArgs ?? { studyPeriods: [], maxCohortSize: 0 };
    const c = dto.createStudyPopArgs ?? {
        restrictToCommonPeriod: false,
        firstExposureOnly: false,
        washoutPeriod: 0,
        removeDuplicateSubjects: "keep all" as RemoveDuplicate,
        censorAtNewRiskWindow: false,
        removeSubjectsWithPriorOutcome: false,
        priorOutcomeLookBack: 0,
        timeAtRisks: [],
    };
    const psa = dto.propensityScoreAdjustment ?? {
        psSettings: [],
        createPsArgs: {
            maxCohortSizeForFitting: 0,
            errorOnHighCorrelation: false,
            prior: null,
            control: null,
        },
    };
    const fom = dto.fitOutcomeModelArgs ?? {
        modelType: "cox" as ModelType,
        stratified: false,
        useCovariates: false,
        inversePtWeighting: false,
        prior: null,
        control: null,
    };

    const studyPeriods = (g.studyPeriods ?? []).map(sp => ({
        studyStartDate: toYyyyMmDd(sp?.studyStartDate ?? null),
        studyEndDate: toYyyyMmDd(sp?.studyEndDate ?? null),
    }));

    const timeAtRisks = (c.timeAtRisks ?? []).map(t => ({
        riskWindowStart: t.riskWindowStart ?? 0,
        startAnchor: (t.startAnchor ?? "cohort start") as Anchor,
        riskWindowEnd: t.riskWindowEnd ?? 0,
        endAnchor: (t.endAnchor ?? "cohort end") as Anchor,
        minDaysAtRisk: t.minDaysAtRisk ?? 0,
    }));

    const psSettings = (psa.psSettings ?? []).map(s => {
        const m = s?.matchOnPsArgs ?? null;
        const st = s?.stratifyByPsArgs ?? null;
        return {
            maxRatio: m ? m.maxRatio ?? null : null,
            caliper: m ? m.caliper ?? null : null,
            caliperScale: m ? (m.caliperScale ?? null) : null,
            numberOfStrata: st ? st.numberOfStrata ?? null : null,
            baseSelection: st ? (st.baseSelection ?? null) : null,
        };
    });

    return {
        // getDbCohortMethodDataArgs
        studyPeriods,
        maxCohortSize: g.maxCohortSize ?? 0,

        // createStudyPopArgs
        restrictToCommonPeriod: c.restrictToCommonPeriod ?? false,
        firstExposureOnly: c.firstExposureOnly ?? false,
        washoutPeriod: c.washoutPeriod ?? 0,
        removeDuplicateSubjects: (c.removeDuplicateSubjects ?? "keep all") as RemoveDuplicate,
        censorAtNewRiskWindow: c.censorAtNewRiskWindow ?? false,
        removeSubjectsWithPriorOutcome: c.removeSubjectsWithPriorOutcome ?? false,
        priorOutcomeLookBack: c.priorOutcomeLookBack ?? 0,
        timeAtRisks,

        // propensityScoreAdjustment
        psSettings,
        createPsArgs: {
            maxCohortSizeForFitting: psa.createPsArgs?.maxCohortSizeForFitting ?? 0,
            errorOnHighCorrelation: psa.createPsArgs?.errorOnHighCorrelation ?? false,
            // 대소문자 키 유지 (요청사항)
            Prior: psa.createPsArgs?.prior ?? null,
            Control: psa.createPsArgs?.control ?? null,
        },

        // fitOutcomeModelArgs
        modelType: (fom.modelType ?? "cox") as ModelType,
        stratified: fom.stratified ?? false,
        useCovariates: fom.useCovariates ?? false,
        inversePtWeighting: fom.inversePtWeighting ?? false,
        // 대소문자 키 유지 (요청사항)
        Prior: fom.prior ?? null,
        Control: fom.control ?? null,
    };
}
