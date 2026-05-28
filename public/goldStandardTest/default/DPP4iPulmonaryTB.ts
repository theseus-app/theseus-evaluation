export const TEXTDPP4iPulmonaryTB = `
Study Period 1: We extracted 2007 to 2019 data from Taiwan’s NHIRD.

TAR 1: Follow-up commenced on the index date, which marked the initiation of the assigned treatment strategy. Participants were monitored until the first occurrence of one of the following events: diagnosis of pulmonary TB, death, or the end of the study period on December 31, 2020, whichever occurred first.

PS Settings 1: To approximate randomization, propensity score matching (PSM) was employed to estimate the probability (propensity score) of receiving a specific treatment (e.g., DPP-4 inhibitors vs. non-DPP-4 inhibitors) based on baseline characteristics. Patients who started on DPP-4 inhibitors were matched 1:1 with contemporaneous initiators of non-DPP-4 inhibitors.

Outcome Model 1: Time-to-event analyses comparing DPP-4 inhibitor users with non-users were conducted using Cox proportional hazards models with robust sandwich standard error estimates.
`

export const JSONDPP4iPulmonaryTB = {
  "getDbCohortMethodDataArgs": {
    "studyPeriods": [
      {
        "description": "",
        "studyStartDate": "20070101",
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
