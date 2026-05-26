export const TEXTCORAZON = `
Study Period: between 1 January 2010 and 31 December 2019.

TAR: Patients were followed from the index date until the occurrence of the study outcome, treatment discontinuation (allowing for 90-day gaps between consecutive prescriptions, with the date of treatment discontinuation being the end date of the last prescription [the “on-treatment” approach]), switching from the index medication to another oral anticoagulant (apixaban, dabigatran, edoxaban, rivaroxaban, or warfarin), death, or the end of the study period (31 December 2019), whichever came first.

PS Settings: Patients were stratified into 5 strata based on propensity score to estimate the average treatment effect.

Outcome Model: Cox proportional hazards regression conditioned on the propensity score strata was applied to estimate the hazard ratio (HR) of the risk for outcomes
`

export const JSONCORAZON = {
  "getDbCohortMethodDataArgs": {
    "studyPeriods": [
      {
        "description": "",
        "studyStartDate": "20100101",
        "studyEndDate": "20191231"
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
        "riskWindowEnd": 0,
        "endAnchor": "cohort end"
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
