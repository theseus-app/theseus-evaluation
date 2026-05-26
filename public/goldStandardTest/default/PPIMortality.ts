export const TEXTPPIMortality = `
Study Period 1: We selected new users of acid suppression drugs between 1 July 2002 and 30 June 2004 and followed them for up to 10 years to examine the associations between new use of PPIs and causes of death.

TAR 1: We applied an intention to treat design for new use of
acid suppressant drugs.

PS Settings 1: We applied inverse treatment probability weights based on the propensity scores to the cohort, which results in a weighted pseudo cohort where treatment assignment is independent of measured confounders.

Outcome Model 1: In the first stage, the residual between the observed and predicted probability of receiving the assigned treatment given instrumental variable was computed from logistic regression weighted by inverse treatment probability weights based on high
dimensional propensity scores.

Outcome Model 2: n the second stage, we used the residual as an independent variable indicating unmeasured confounders in the inverse treatment probability weighted cause specific Cox survival analyses.
`

export const JSONPPIMortality = {
  "getDbCohortMethodDataArgs": {
    "studyPeriods": [
      {
        "description": "",
        "studyStartDate": "20020701",
        "studyEndDate": "20140630"
      }
    ],
    "firstExposureOnly": false,
    "removeDuplicateSubjects": "keep all",
    "restrictToCommonPeriod": false,
    "washoutPeriod": 365,
    "maxCohortSize": 0
  },
  "createStudyPopArgs": {
    "removeSubjectsWithPriorOutcome": true,
    "priorOutcomeLookback": 99999,
    "timeAtRisks": [
      {
        "description": "",
        "minDaysAtRisk": 1,
        "riskWindowStart": 1,
        "startAnchor": "cohort start",
        "riskWindowEnd": 99999,
        "endAnchor": "cohort start"
      }
    ],
    "censorAtNewRiskWindow": false
  },
  "psSettings": [
    {
      "description": "",
      "trimByPsArgs": null,
      "matchOnPsArgs": null,
      "stratifyByPsArgs": null,
      "inversePtWeighting": true
    }
  ],
  "createPsArgs": {
    "maxCohortSizeForFitting": 250000,
    "errorOnHighCorrelation": true,
    "prior": {
      "priorType": "laplace",
      "useCrossValidation": true
    },
    "control": {
      "tolerance": 2e-07,
      "cvType": "auto",
      "fold": 10,
      "cvRepetitions": 10,
      "noiseLevel": "silent",
      "resetCoefficients": true,
      "startingVariance": 0.01
    }
  },
  "fitOutcomeModelArgs": {
    "outcomeModels": [
      {
        "description": "",
        "modelType": "logistic",
        "useCovariates": false
      },
      {
        "description": "",
        "modelType": "cox",
        "useCovariates": false
      }
    ],
    "stratified": false,
    "prior": {
      "priorType": "laplace",
      "useCrossValidation": true
    },
    "control": {
      "tolerance": 2e-07,
      "cvType": "auto",
      "fold": 10,
      "cvRepetitions": 10,
      "noiseLevel": "quiet",
      "resetCoefficients": true,
      "startingVariance": 0.01
    }
  }
}
