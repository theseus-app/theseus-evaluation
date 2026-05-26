export const TEXTRapidCycleJanssenAug1 = `
Study Period: Individuals were included in these COVID-19 vaccine exposure cohorts if they had at least 365 days of observation before and at least one day of observation after this first COVID-19 vaccination starting on or after March 1st, 2021 and on or before December 31st, 2021.

TAR: relative risks of outcomes of interest were assessed during delayed at-risk periods following vaccination (15-42 days), leaving the first 14 days after vaccination as a blank period

PS Settings: We apply 1:1 matching on the propensity score

Outcome Model: The comparative cohort analysis employed a logistic regression model to estimate the odds ratio for a specified outcome.
`

export const JSONRapidCycleJanssenAug1 = {
  "getDbCohortMethodDataArgs": {
    "studyPeriods": [
      {
        "description": "",
        "studyStartDate": "20210301",
        "studyEndDate": "20211231"
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
        "riskWindowStart": 15,
        "startAnchor": "cohort start",
        "riskWindowEnd": 42,
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
