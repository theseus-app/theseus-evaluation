import { StudyDTO } from "./flatten";

export const yyyymmdd = (d: Date) => {
    const yyyy = d.getFullYear();
    const mm = String(d.getMonth() + 1).padStart(2, "0");
    const dd = String(d.getDate()).padStart(2, "0");
    return `${yyyy}${mm}${dd}`;
};

export const fromHtmlDate = (val: string) => val.replaceAll("-", "");
export const toHtmlDate = (yyyymmddStr: string | null) =>
    yyyymmddStr && yyyymmddStr.length === 8
        ? `${yyyymmddStr.slice(0, 4)}-${yyyymmddStr.slice(4, 6)}-${yyyymmddStr.slice(6, 8)}`
        : "";

export const defaultDTO: StudyDTO = {
    name: "",
    cohortDefinitions: {
        targetCohort: { id: null, name: "" },
        comparatorCohort: { id: null, name: "" },
        outcomeCohort: [{ id: null, name: "" }],
    },
    negativeControlConceptSet: { id: null, name: "" },
    covariateSelection: {
        conceptsToInclude: [{ id: null, name: "" }],
        conceptsToExclude: [{ id: null, name: "" }],
    },
    getDbCohortMethodDataArgs: {
        studyPeriods: [
            {
                studyStartDate: null,
                studyEndDate: null,
            },
        ],
        maxCohortSize: 0,
    },
    createStudyPopArgs: {
        restrictToCommonPeriod: false,
        firstExposureOnly: false,
        washoutPeriod: 0,
        removeDuplicateSubjects: "keep all",
        censorAtNewRiskWindow: false,
        removeSubjectsWithPriorOutcome: true,
        priorOutcomeLookBack: 99999,
        timeAtRisks: [
            {
                description: "",
                riskWindowStart: 1,
                startAnchor: "cohort start",
                riskWindowEnd: 0,
                endAnchor: "cohort end",
                minDaysAtRisk: 1,
            },
        ],
    },
    propensityScoreAdjustment: {
        psSettings: [
            {
                description: "PS 1",
                matchOnPsArgs: { maxRatio: 1, caliper: 0.2, caliperScale: "standardized logit" },
                stratifyByPsArgs: null,
            },
        ],
        createPsArgs: {
            maxCohortSizeForFitting: 250000,
            errorOnHighCorrelation: true,
            prior: { priorType: "laplace", useCrossValidation: true },
            control: {
                tolerance: 2e-7,
                cvType: "auto",
                fold: 10,
                cvRepetitions: 10,
                noiseLevel: "silent",
                resetCoefficients: true,
                startingVariance: 0.01,
            },
        },
    },
    fitOutcomeModelArgs: {
        modelType: "cox",
        stratified: false,
        useCovariates: false,
        inversePtWeighting: false,
        prior: { priorType: "laplace", useCrossValidation: true },
        control: {
            tolerance: 2e-7,
            cvType: "auto",
            fold: 10,
            cvRepetitions: 10,
            noiseLevel: "quiet",
            resetCoefficients: true,
            startingVariance: 0.01,
        },
    },
};
