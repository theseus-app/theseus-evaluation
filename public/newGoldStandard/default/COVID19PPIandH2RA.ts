export const TEXTCOVID19PPIandH2RA = "\nStudy Period 1: HIRA collected data on COVID-19 testing by the reverse transcriptase polymerase chain reaction method from 1 January to 15 May, 2020.\n\nTAR 1: Patients were censored if they were no longer observable in the database.\n\nPS Settings 1: propensity score unadjusted analysis\nPS Settings 2: one-to-four exact PSM with greedy nearest methods\nPS Settings 3: propensity score stratification with five strata\n\nOutcome Model: Cox proportional hazards regression models were used to model the time to the first outcome occurrence for the target relative to the comparator cohort in pairwise comparisons\n"

export const JSONCOVID19PPIandH2RA = {
  "getDbCohortMethodDataArgs": {
    "studyPeriods": [
      {
        "description": "",
        "studyStartDate": "20200101",
        "studyEndDate": "20200515"
      }
    ],
    "firstExposureOnly": true,
    "removeDuplicateSubjects": "keep first",
    "restrictToCommonPeriod": false,
    "washoutPeriod": 180,
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
      "inversePtWeighting": false
    },
    {
      "description": "",
      "trimByPsArgs": null,
      "matchOnPsArgs": {
        "maxRatio": 4,
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
