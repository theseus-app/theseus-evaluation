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

// ===== 유틸: yyyyMMdd 정규화 (숫자/문자/널 모두 허용) =====
const toYyyyMmDd = (v: string | number | null | undefined): string | null => {
    if (v === null || v === undefined) return null;
    const s = String(v).trim();
    // 숫자 8자리 또는 yyyy-mm-dd 같은 경우도 허용 후 yyyyMMdd로 정제
    if (/^\d{8}$/.test(s)) return s; // 이미 yyyyMMdd
    const onlyDigits = s.replace(/\D/g, "");
    return onlyDigits.length === 8 ? onlyDigits : null;
};

export type StudyDTOPRIMARY = {
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

    // PRIMARY: studyPeriods = 단일 객체 + 여기로 올라온 필드들 포함
    getDbCohortMethodDataArgs: {
        studyPeriods: {                                  // ← array 아님
            studyStartDate: string | number | null;
            studyEndDate: string | number | null;
        };
        restrictToCommonPeriod: boolean;
        firstExposureOnly: boolean;
        washoutPeriod: number;
        removeDuplicateSubjects: RemoveDuplicate;
        maxCohortSize: number;
    };

    // PRIMARY: 여기서는 censor/prior/timeAtRisks만
    createStudyPopArgs: {
        censorAtNewRiskWindow: boolean;
        removeSubjectsWithPriorOutcome: boolean;
        priorOutcomeLookBack: number;
        timeAtRisks: {                                   // ← array 아님
            riskWindowStart: number;
            startAnchor: Anchor;
            riskWindowEnd: number;
            endAnchor: Anchor;
            minDaysAtRisk: number;
        };
    };

    // PRIMARY: psSettings = 단일 객체
    propensityScoreAdjustment: {
        psSettings: {
            matchOnPsArgs: {
                maxRatio: number;
                caliper: number;
                caliperScale: CaliperScale;
            };
            stratifyByPsArgs: {
                numberOfStrata: number | null;
                baseSelection: BaseSelection | null;
            } | null;
        };
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
export type FlattenStudyDTOPRIMARY = {
    /////// getDbCohortMethodDataArgs ///////
    studyPeriodsPRIMARY: { studyStartDate: string | null; studyEndDate: string | null }; // yyyyMMdd
    maxCohortSizePRIMARY: number;
    restrictToCommonPeriodPRIMARY: boolean;
    firstExposureOnlyPRIMARY: boolean;
    washoutPeriodPRIMARY: number;
    removeDuplicateSubjectsPRIMARY: RemoveDuplicate;

    /////// createStudyPopArgs ///////
    censorAtNewRiskWindowPRIMARY: boolean;
    removeSubjectsWithPriorOutcomePRIMARY: boolean;
    priorOutcomeLookBackPRIMARY: number;
    timeAtRisksPRIMARY: {
        riskWindowStart: number;
        startAnchor: Anchor;
        riskWindowEnd: number;
        endAnchor: Anchor;
        minDaysAtRisk: number;
    };

    /////// propensityScoreAdjustment ///////
    psSettingsPRIMARY: {
        maxRatio: number | null;
        caliper: number | null;
        caliperScale: CaliperScale | null;
        numberOfStrata: number | null;
        baseSelection: BaseSelection | null;
    };
    createPsArgsPRIMARY: {
        maxCohortSizeForFitting: number;
        errorOnHighCorrelation: boolean;
        PriorPRIMARY: Prior | null;
        ControlPRIMARY: Control | null;
    };

    /////// fitOutcomeModelArgs ///////
    modelTypePRIMARY: ModelType;
    stratifiedPRIMARY: boolean;
    useCovariatesPRIMARY: boolean;
    inversePtWeightingPRIMARY: boolean;
    PriorPRIMARY: Prior | null;
    ControlPRIMARY: Control | null;
};


// ===== 메인: flatten 함수 =====
export function flattenStudyPRIMARY(dto: Partial<StudyDTOPRIMARY>): FlattenStudyDTOPRIMARY {
    const gPRIMARY = dto.getDbCohortMethodDataArgs ?? {
        studyPeriods: { studyStartDate: null, studyEndDate: null },
        restrictToCommonPeriod: false,
        firstExposureOnly: false,
        washoutPeriod: 0,
        removeDuplicateSubjects: "keep all" as RemoveDuplicate,
        maxCohortSize: 0,
    };

    const cPRIMARY = dto.createStudyPopArgs ?? {
        censorAtNewRiskWindow: false,
        removeSubjectsWithPriorOutcome: false,
        priorOutcomeLookBack: 0,
        timeAtRisks: {
            riskWindowStart: 0,
            startAnchor: "cohort start" as Anchor,
            riskWindowEnd: 0,
            endAnchor: "cohort end" as Anchor,
            minDaysAtRisk: 0,
        },
    };

    const psaPRIMARY = dto.propensityScoreAdjustment ?? {
        psSettings: {
            matchOnPsArgs: {
                maxRatio: 1,
                caliper: 0,
                caliperScale: "standardized logit" as CaliperScale,
            },
            stratifyByPsArgs: null,
        },
        createPsArgs: {
            maxCohortSizeForFitting: 0,
            errorOnHighCorrelation: false,
            prior: null,
            control: null,
        },
    };

    const fomPRIMARY = dto.fitOutcomeModelArgs ?? {
        modelType: "cox" as ModelType,
        stratified: false,
        useCovariates: false,
        inversePtWeighting: false,
        prior: null,
        control: null,
    };

    // --- studyPeriods: 단일 객체 처리 ---
    const sp = gPRIMARY.studyPeriods ?? { studyStartDate: null, studyEndDate: null };
    const studyPeriodsPRIMARY = {
        studyStartDate: toYyyyMmDd(sp.studyStartDate ?? null),
        studyEndDate: toYyyyMmDd(sp.studyEndDate ?? null),
    };

    // --- timeAtRisks: 단일 객체 처리 ---
    const tar = cPRIMARY.timeAtRisks ?? {
        riskWindowStart: 0,
        startAnchor: "cohort start" as Anchor,
        riskWindowEnd: 0,
        endAnchor: "cohort end" as Anchor,
        minDaysAtRisk: 0,
    };
    const timeAtRisksPRIMARY = {
        riskWindowStart: tar.riskWindowStart ?? 0,
        startAnchor: (tar.startAnchor ?? "cohort start") as Anchor,
        riskWindowEnd: tar.riskWindowEnd ?? 0,
        endAnchor: (tar.endAnchor ?? "cohort end") as Anchor,
        minDaysAtRisk: tar.minDaysAtRisk ?? 0,
    };

    // --- psSettings: 단일 객체 처리 ---
    const ps = psaPRIMARY.psSettings ?? {
        matchOnPsArgs: {
            maxRatio: 1,
            caliper: 0,
            caliperScale: "standardized logit" as CaliperScale,
        },
        stratifyByPsArgs: null,
    };

    const m = ps.matchOnPsArgs ?? null;
    const st = ps.stratifyByPsArgs ?? null;

    const psSettingsPRIMARY = {
        maxRatio: m ? m.maxRatio ?? null : null,
        caliper: m ? m.caliper ?? null : null,
        caliperScale: m ? (m.caliperScale ?? null) : null,
        numberOfStrata: st ? st.numberOfStrata ?? null : null,
        baseSelection: st ? (st.baseSelection ?? null) : null,
    };

    return {
        /////// getDbCohortMethodDataArgs ///////
        studyPeriodsPRIMARY,
        maxCohortSizePRIMARY: gPRIMARY.maxCohortSize ?? 0,
        restrictToCommonPeriodPRIMARY: gPRIMARY.restrictToCommonPeriod ?? false,
        firstExposureOnlyPRIMARY: gPRIMARY.firstExposureOnly ?? false,
        washoutPeriodPRIMARY: gPRIMARY.washoutPeriod ?? 0,
        removeDuplicateSubjectsPRIMARY: (gPRIMARY.removeDuplicateSubjects ?? "keep all") as RemoveDuplicate,

        /////// createStudyPopArgs ///////
        censorAtNewRiskWindowPRIMARY: cPRIMARY.censorAtNewRiskWindow ?? false,
        removeSubjectsWithPriorOutcomePRIMARY: cPRIMARY.removeSubjectsWithPriorOutcome ?? false,
        priorOutcomeLookBackPRIMARY: cPRIMARY.priorOutcomeLookBack ?? 0,
        timeAtRisksPRIMARY,

        /////// propensityScoreAdjustment ///////
        psSettingsPRIMARY,
        createPsArgsPRIMARY: {
            maxCohortSizeForFitting: psaPRIMARY.createPsArgs?.maxCohortSizeForFitting ?? 0,
            errorOnHighCorrelation: psaPRIMARY.createPsArgs?.errorOnHighCorrelation ?? false,
            PriorPRIMARY: psaPRIMARY.createPsArgs?.prior ?? null,
            ControlPRIMARY: psaPRIMARY.createPsArgs?.control ?? null,
        },

        /////// fitOutcomeModelArgs ///////
        modelTypePRIMARY: (fomPRIMARY.modelType ?? "cox") as ModelType,
        stratifiedPRIMARY: fomPRIMARY.stratified ?? false,
        useCovariatesPRIMARY: fomPRIMARY.useCovariates ?? false,
        inversePtWeightingPRIMARY: fomPRIMARY.inversePtWeighting ?? false,
        PriorPRIMARY: fomPRIMARY.prior ?? null,
        ControlPRIMARY: fomPRIMARY.control ?? null,
    };
}

