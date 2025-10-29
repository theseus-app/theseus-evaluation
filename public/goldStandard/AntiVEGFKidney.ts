export const TEXTAntiVEGFKidney =
  `
TAR 1: Patients were assumed at-risk of kidney failure after the third anti-VEGF exposure until the end of continuous drug exposure or the end of the study period.

PS Settings 1: We used the large-scale propensity score method to match patients in each target and comparator exposure cohort comparison using 1:1 propensity score matching. The propensity score model included a large number of baseline covariates  as potential confounders and used the L1-regularization technique to avoid model overfitting.

Outcome Model: Cox proportional hazards models were used to estimate the risk of kidney failure while on treatment
`

export const JSONAntiVEGFKidney = {
  maxCohortSize: 0, //default로 설정
  createStudyPopArgs: {
    restrictToCommonPeriod: false, //default로 설정
    firstExposureOnly: true,
    washoutPeriod: 365,
    removeDuplicateSubjects: "keep all", //default로 설정
    censorAtNewRiskWindow: false, //default로 설정
    removeSubjectsWithPriorOutcome: true,
    priorOutcomeLookBack: 99999,
    timeAtRisks: [
      {
        riskWindowStart: 0,
        startAnchor: "cohort start",
        riskWindowEnd: 0,
        endAnchor: "cohort end",
        minDaysAtRisk: 1 //default 설정
      },
    ],
  },
  propensityScoreAdjustment: {
    psSettings: [
      {
        matchOnPsArgs: {
          maxRatio: 1,
          caliper: 0.2, //default 설정
          caliperScale: "standardized logit" //default 설정
        },
        stratifyByPsArgs: null
      },
    ],
    createPsArgs: { //laplace 제외하고 전부 default 설정
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
      }
    }
  },
  fitOutcomeModelArgs: { //modelType제외 default 설정
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
  }
}
