export const TEXTDabigatranRivaroxabanAF = `
Study Period 1: we first identified Medicare beneficiaries who filed the first prescription for either Rivaroxaban or Dabigatran with the standard dosages between April 01, 2012 and December 31, 2013.

Study Period 2-4: We then created three subsequent trials to examine temporal variations. The same criteria and procedures were applied to the three trials to ensure comparability. All the
trials had a 7-month recruitment period: April 01, 2012–December 31, 2012, Noveber 01, 2012–May 31, 2013, and June 01, 2013–December 31, 2013, respectively.

TAR 1: Follow-up information was extracted for each subject (to death, loss to follow-up, or end of the study –December 31, 2013).

TAR 2-4: different lengths of maximum follow-up: 21, 14, and 7 months

PS Settings 1: For each emulated trial and each outcome, we resorted to the propensity score and inverse probability treatment (IPT) weighting approach to achieve baseline balance as in a randomized clinical trial.

Outcome Model 1: Then the Cox proportional hazards model was fitted to derive the treatment effect from the weighted pseudo-population. To further adjust for the remaining confounding and improve the precision of estimation, we included the treatment indicator and all baseline confounders as covariates.
`

export const JSONDabigatranRivaroxabanAF = {
  "getDbCohortMethodDataArgs": {
    "studyPeriods": [
      {
        "description": "",
        "studyStartDate": "20120401",
        "studyEndDate": "20131231"
      },
      {
        "description": "",
        "studyStartDate": "20120401",
        "studyEndDate": "20121231"
      },
      {
        "description": "",
        "studyStartDate": "20121101",
        "studyEndDate": "20130531"
      },
      {
        "description": "",
        "studyStartDate": "20130601",
        "studyEndDate": "20131231"
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
      },
      {
        "description": "",
        "minDaysAtRisk": 1,
        "riskWindowStart": 1,
        "startAnchor": "cohort start",
        "riskWindowEnd": 630,
        "endAnchor": "cohort start"
      },
      {
        "description": "",
        "minDaysAtRisk": 1,
        "riskWindowStart": 1,
        "startAnchor": "cohort start",
        "riskWindowEnd": 420,
        "endAnchor": "cohort start"
      },
      {
        "description": "",
        "minDaysAtRisk": 1,
        "riskWindowStart": 1,
        "startAnchor": "cohort start",
        "riskWindowEnd": 210,
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
        "useCovariates": true
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
