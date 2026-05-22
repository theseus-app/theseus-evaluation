export const TEXTCORAZON = "\nStudy Period 1-2: between 1 January 2010 (2012 for Longitudinal Patients Database France) and 31 December 2019.\n\nTAR 1: Patients were followed from the index date until the occurrence of the study outcome, treatment discontinuation (allowing for 90-day gaps between consecutive prescriptions, with the date of treatment discontinuation being the end date of the last prescription [the “on-treatment” approach]), switching from the index medication to another oral anticoagulant (apixaban, dabigatran, edoxaban, rivaroxaban, or warfarin), death, or the end of the study period (31 December 2019), whichever came first.\nTAR 2: We did additional sensitivity analyses in which the time at risk was not censored if a patient discontinued the index medication therapy or switched to another anticoagulant (analogue to the “intention-to-treat” approach).\n\nPS Settings 1: Patients were stratified into 5 strata based on propensity score to estimate the average treatment effect.\nPS Settings 2: We also repeated our analyses using propensity score matching at a variable-matching ratio as sensitivity analyses to estimate the average treatment effect on treated patients.\n\nOutcome Model: Cox proportional hazards regression conditioned on the propensity score strata was applied to estimate the hazard ratio (HR) of the risk for outcomes"

export const JSONCORAZON = {
  "getDbCohortMethodDataArgs": {
    "studyPeriods": [
      {
        "description": "",
        "studyStartDate": "20100101",
        "studyEndDate": "20191231"
      },
      {
        "description": "",
        "studyStartDate": "20120101",
        "studyEndDate": "20191231"
      }
    ],
    "firstExposureOnly": false,
    "removeDuplicateSubjects": "keep all",
    "restrictToCommonPeriod": false,
    "washoutPeriod": 0,
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
      },
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
        "maxRatio": 0,
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
