export const TEXTCOVID19Famotidine = "\nStudy Period 1: The study period started February 1, 2020, and ended May 30, 2020,\n\nTAR 1: Time at risk was defined based on the intention-to-treat principle starting 1 day after admission and continuing up until the first of outcome of interest, loss to follow up, or 30 days after admission\n\nPS Settings 1: To adjust for potential measured confounding and improve the balance between comparison cohorts, we built large-scale propensity score (PS) models for each comparison using regularized regression (13). We used a Laplace prior (LASSO) with the optimal hyperparameter to fit the model, determined through 10-fold cross validation in which the outcome is a binary indicator for the potential comparator. For the main analysis, we stratified into 5 PS strata\nPS Settings 2: As a sensitivity analysis, we used a 1:1 PS matching.\n\nOutcome Model: used conditional Cox proportional hazards models to estimate hazard ratios (HRs) between target and alternative comparator treatments for the risk of each outcome. The regression for the outcome models conditioned on the PS strata with treatment as the sole explanatory variable.\n"

export const JSONCOVID19Famotidine = {
  "getDbCohortMethodDataArgs": {
    "studyPeriods": [
      {
        "description": "",
        "studyStartDate": "20200201",
        "studyEndDate": "20200530"
      }
    ],
    "firstExposureOnly": true,
    "removeDuplicateSubjects": "remove all",
    "restrictToCommonPeriod": false,
    "washoutPeriod": 0,
    "maxCohortSize": 0
  },
  "createStudyPopArgs": {
    "removeSubjectsWithPriorOutcome": false,
    "priorOutcomeLookback": 99999,
    "timeAtRisks": [
      {
        "description": "",
        "minDaysAtRisk": 1,
        "riskWindowStart": 1,
        "startAnchor": "cohort start",
        "riskWindowEnd": 30,
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
      "stratifyByPsArgs": {
        "numberOfStrata": 5,
        "baseSelection": "all"
      },
      "inversePtWeighting": false
    },
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
      "tolerance": 2e-7,
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
        "modelType": "cox",
        "useCovariates": false
      }
    ],
    "stratified": true,
    "prior": {
      "priorType": "laplace",
      "useCrossValidation": true
    },
    "control": {
      "tolerance": 2e-7,
      "cvType": "auto",
      "fold": 10,
      "cvRepetitions": 10,
      "noiseLevel": "quiet",
      "resetCoefficients": true,
      "startingVariance": 0.01
    }
  }
}
