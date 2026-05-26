export const TEXTStrokeRiskAug1 = `
Study Period: Included data available from January 1, 2005 through December 31, 2015

TAR: "intention-to-treat” time at risk in the cohort for which they first qualified, starting from the day after they entered that cohort until the first of: having the study outcome (see in the following sections), reaching the end of insurance enrollment, or reaching the end of the study. For both cohorts being compared, patients were required to have at least 1 day of continuous observation after the time-at-risk start.

PS Settings: the large-scale regularized regression model involving PS estimation (adapted PS strategy) using predicted probability from a large-scale regularized logistic regression model, fit with a Laplace prior (LASSO) using all observed covariates, including a recent diagnosis of dementia as input and 1:1 matching with a caliper of 0.2 of the standard deviation of the logit of PS.

Outcome Model: The RRs for the outcome during the time-at-risk was estimated by applying a Poisson regression model conditioned on the PS matched sets.
`

export const JSONStrokeRiskAug1 = {
  "getDbCohortMethodDataArgs": {
    "studyPeriods": [
      {
        "description": "",
        "studyStartDate": "20050101",
        "studyEndDate": "20151231"
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
      "matchOnPsArgs": {
        "maxRatio": 1,
        "caliper": 0.2,
        "caliperScale": "standardized logit"
      },
      "stratifyByPsArgs": null,
      "inversePtWeighting": false
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
        "modelType": "poisson",
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
