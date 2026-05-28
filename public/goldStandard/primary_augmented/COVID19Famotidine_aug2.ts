export const TEXTCOVID19FamotidineAug2 = `
Study Period: The study period started February 1, 2020, and continued until the end of available observation,

TAR: Time at risk was defined based on the intention-to-treat principle starting 1 day after admission and continuing up until the first of outcome of interest, loss to follow up, or the end of available observation

PS Settings: To adjust for potential measured confounding and improve the balance between comparison cohorts, we built large-scale propensity score (PS) models for each comparison using regularized regression (13). We used a Laplace prior (LASSO) with the optimal hyperparameter to fit the model, determined through 10-fold cross validation in which the outcome is a binary indicator for the potential comparator. For the main analysis, we used variable-ratio PS matching

Outcome Model: used conditional logistic regression models to estimate odds ratios (ORs) between target and alternative comparator treatments for the risk of each outcome. The regression for the outcome models conditioned on the PS adjustment with treatment as the sole explanatory variable.
`

export const JSONCOVID19FamotidineAug2 = {
  "getDbCohortMethodDataArgs": {
    "studyPeriods": [
      {
        "description": "",
        "studyStartDate": "20200201",
        "studyEndDate": ""
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
        "maxRatio": 100,
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
        "modelType": "logistic",
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
