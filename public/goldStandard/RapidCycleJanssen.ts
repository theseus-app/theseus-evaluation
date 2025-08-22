export const TEXTRapidCycleJanssen =
    `
Individuals were included in these COVID-19 vaccine exposure cohorts if they had at least 365 days of observation before and at least one day of observation after this first COVID-19 vaccination starting on or after January 1st, 2021.

This estimation involves separate comparisons of the first dose of Janssen vaccination (target exposure) to the first doses of the Moderna and the Pfizer Covid-19 vaccines, which serve as two separate comparator exposure cohorts.

To allow for comparative assessment, three COVID-19 vaccine exposure cohorts were defined based on the first COVID-19 vaccine exposure to either Janssen Ad26.COV2·S (target exposure), Moderna mRNA-1273 (comparator exposure), or Pfizer mRNA-BNT162b2 (comparator exposure).

Subjects were excluded if they had the outcome prior to the COVID-19 vaccine exposure (index date) and the index date was within the outcome clean window.

relative risks of outcomes of interest were assessed during four overlapping at-risk periods following vaccination (1–14, 1–28, 1–42 and 1–90 days) except for anaphylaxis, which used an at-risk window starting on the vaccination day (0–2 days).

We apply variable ratio matching on the propensity score

The comparative cohort analysis employed a Cox proportional hazards model to estimate the hazard ratio for a specified outcome.
`

export const JSONRapidCycleJanssen =
{
    getDbCohortMethodDataArgs: {
        studyPeriods: [
            {
                studyStartDate: 20210101,
                studyEndDate: null
            }
        ],
        maxCohortSize: 0, //default
    },
    createStudyPopArgs: {
        restrictToCommonPeriod: false, //default 설정
        firstExposureOnly: true,
        washoutPeriod: 365,
        removeDuplicateSubjects: "remove all",
        censorAtNewRiskWindow: false, //default
        removeSubjectsWithPriorOutcome: true,
        priorOutcomeLookBack: 99999,
        timeAtRisks: [
            {
                riskWindowStart: 1,
                startAnchor: "cohort start",
                riskWindowEnd: 14,
                endAnchor: "cohort start",
                minDaysAtRisk: 1, //default로 설정
            },
            {
                riskWindowStart: 1,
                startAnchor: "cohort start",
                riskWindowEnd: 28,
                endAnchor: "cohort start",
                minDaysAtRisk: 1, //default로 설정
            },
            {
                riskWindowStart: 1,
                startAnchor: "cohort start",
                riskWindowEnd: 42,
                endAnchor: "cohort start",
                minDaysAtRisk: 1, //default로 설정
            },
            {
                riskWindowStart: 1,
                startAnchor: "cohort start",
                riskWindowEnd: 90,
                endAnchor: "cohort start",
                minDaysAtRisk: 1, //default로 설정
            },
            {
                riskWindowStart: 0,
                startAnchor: "cohort start",
                riskWindowEnd: 2,
                endAnchor: "cohort start",
                minDaysAtRisk: 1, //default로 설정
            }
        ]
    },
    propensityScoreAdjustment: {
        psSettings: [
            {
                matchOnPsArgs: {
                    maxRatio: 0,
                    caliper: 0.2, //default로 설정
                    caliperScale: "propensity score"
                },
                stratifyByPsArgs: null,
            }
        ],
        createPsArgs: { //default
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
    fitOutcomeModelArgs: { //default except modelType
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
}