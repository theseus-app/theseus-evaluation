export const TEXTAntiVEGFKidney = `
Study Period 1: The BSI-FOO observational study was a multicentre cohort study of 1,903 hospitalised patients with a BSI across seven NHS acute hospital trusts in England and Wales conducted between November 2010 and May 2012 with the primary aim of identifying modifiable risk factors for 28-day mortality.

TAR 1: 25-day mortality was analysed to ensure full follow-up was available for all patients.

PS Settings 1: The number of participants and deaths in each emulated intervention group was examined within strata defined by propensity score quantiles.

Outcome Model: Convergence was not achieved when fitting an adjusted generalised linear model and therefore outcomes were compared using logistic regression adjusted for the propensity score.
`

export const JSONAntiVEGFKidney = {
  getDbCohortMethodDataArgs: {
    studyPeriods: [
      {
        studyStartDate: "20101101",
        studyEndDate: "20120531",
      },
    ],
    restrictToCommonPeriod: true,
    firstExposureOnly: false,
    washoutPeriod: 0,
    removeDuplicateSubjects: "keep all",
    maxCohortSize: 0, //default
  },
  createStudyPopArgs: {
    censorAtNewRiskWindow: false, //default로 설정
    removeSubjectsWithPriorOutcome: true,
    priorOutcomeLookBack: 99999,
    timeAtRisks: [
      {
        riskWindowStart: 1,
        startAnchor: "cohort start",
        riskWindowEnd: 25,
        endAnchor: "cohort start",
        minDaysAtRisk: 1 //default 설정
      }
    ],
  },
  propensityScoreAdjustment: {
    psSettings: [
      {
        matchOnPsArgs: null,
        stratifyByPsArgs: {
            numberOfStrata: 10,
            baseSelection: "all" //default로 설정
        },
      }
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
    modelType: "logistic",
    stratified: true,
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
