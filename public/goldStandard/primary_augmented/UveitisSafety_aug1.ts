export const TEXTUveitisSafetyAug1 = `
Study Period: The study period was from 1 January 2000 to 31 December 2020.

TAR: We defined the 'intention-to-treat' time-at-risk (TAR) as the day after index until 30 days after index.

PS Settings: Our primary PS adjustment strategy matched target to comparator patients using a 1:1 matching approach and used a greedy matching algorithm that applied a caliper of 0.2 of the standard deviation on the logit scale of the PS distribution [48]. The PS was calculated for each patient as the predicted probability of target exposure status from an L1 regularized logistic regression model, fit with a Laplace prior where the regularization hyperparameter was selected by optimizing the likelihood in a 10-fold cross validation with a starting variance of 0.01 and a tolerance of 2*10-7 [47].

Outcome Model: Within each database and study population, we fit a Poisson regression model conditioned on PS-matched sets with Remicade treatment status as the explanatory variable to model the first ever NIU occurrence relative to the comparator group.
`

export const JSONUveitisSafetyAug1 = {
  "getDbCohortMethodDataArgs": {
    "studyPeriods": [
      {
        "description": "",
        "studyStartDate": "20000101",
        "studyEndDate": "20201231"
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
