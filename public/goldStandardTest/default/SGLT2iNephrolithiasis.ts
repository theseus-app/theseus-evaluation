export const TEXTSGLT2iNephrolithiasis = `
Study Period 1: In the trials, we included adults aged ≥18 years with type 2 diabetes whose first ever dispensing for one of the comparison drugs was between 1 January 2014 and 30 June 2022, after at least one year of continuous enrolment with the provincial medical services plan (see supplementary figure S1).

TAR 1: In our primary analysis, follow-up started the day of first dispensing (target trial index date) and continued until end of the study period (30 June 2022),

PS Settings 1: To account for non-random allocation of patients to the treatment groups, we used stabilized inverse probability of treatment weighting for the average treatment effect in the whole population

Outcome Model 1-2: For each comparison, weighted Poisson or Cox proportional hazards models were used to estimate rate ratios or hazard ratios and corresponding 95% confidence intervals (CIs),62 while accounting for competing risk of death.
`

export const JSONSGLT2iNephrolithiasis = {
  "getDbCohortMethodDataArgs": {
    "studyPeriods": [
      {
        "description": "",
        "studyStartDate": "20140101",
        "studyEndDate": "20220630"
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
        "modelType": "cox",
        "useCovariates": false
      },
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
