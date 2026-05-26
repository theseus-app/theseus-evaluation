export const TEXTIUDEHRE = `
Study Period 1: The study window was restricted to all IUD placements that occurred on or after January 1, 2003.

TAR 1: The time at risk was from 30 days to 15 years after IUD placement.

TAR 2: We also performed a subgroup analysis including only those cervical neoplasm diagnoses that occurred at least 1 year after IUD placement.

PS Settings 1-2: we used propensity score models with a regularized logistic regression. Patients were matched on the propensity score by two methods: stratification and 1:1 matching.

Outcome Model 1: used a Cox proportional hazards model to determine the relative risk for cervical neoplasms between the Cu IUD and LNG-IUS cohorts
`

export const JSONIUDEHRE = {
  "getDbCohortMethodDataArgs": {
    "studyPeriods": [
      {
        "description": "",
        "studyStartDate": "20030101",
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
        "riskWindowStart": 30,
        "startAnchor": "cohort start",
        "riskWindowEnd": 5475,
        "endAnchor": "cohort start"
      },
      {
        "description": "",
        "minDaysAtRisk": 1,
        "riskWindowStart": 365,
        "startAnchor": "cohort start",
        "riskWindowEnd": 5475,
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
    },
    {
      "description": "",
      "trimByPsArgs": null,
      "matchOnPsArgs": null,
      "stratifyByPsArgs": {
        "numberOfStrata": 5,
        "baseSelection": "all"
      },
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
