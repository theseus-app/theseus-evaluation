export type FieldKind = "scalar" | "array";
export interface FieldSpec {
    path: string;
    kind: FieldKind;
}

export interface FlattenItem {
    path: string;
    kind: FieldKind;
    exact: 0 | 1;
    goldValue: unknown;
    predValue: unknown;
}

export interface FlattenAccuracyResult {
    items: FlattenItem[];
    accuracy: number;
}

export type Anchor = "cohort start" | "cohort end";
export type RemoveDuplicate = "keep all" | "keep first" | "remove all";
export type CaliperScale = "propensity score" | "standardized" | "standardized logit";
export type BaseSelection = "all" | "target" | "comparator";
export type ModelType = "logistic" | "poisson" | "cox";
export type CvType = "auto";
export type NoiseLevel = "silent" | "quiet" | "noisy";

//////////PropensityScoroeAdjustment///////////////
export interface MatchOnPsArgs {
    maxRatio: number;     // 0 = no max
    caliper: number;      // 0 = off
    caliperScale: CaliperScale;
}

export interface StratifyByPsArgs {
    numberOfStrata: number;
    baseSelection: BaseSelection;
}
// regularization
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

// unified PS setting (one of match or stratify)
export interface PsSetting {
    description: string;
    // exactly one of these two must be non-null
    matchOnPsArgs: MatchOnPsArgs | null;
    stratifyByPsArgs: StratifyByPsArgs | null;
}
///////////////////////////////////////////////////

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
        studyPeriods: { studyStartDate: string | null; studyEndDate: string | null }[]; // yyyyMMdd
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
            description: string;
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