export const TEXTIVIronAnaphylaxis = `
Study Period 1: We used HCPCS codes to identify a cohort of older adults receiving their rst administration of one of the following IV iron formulations between July 2013 and December 2018: ferric carboxymaltose, ferumoxytol, ferric gluconate, iron dextran, or iron sucrose.

Study Period 2: we did an analysis restricting the study period to after 2014, when the last available high-
molecular-weight iron dextran (that is, Dexferrum [American Regent]) was withdrawn from the U.S. market.

TAR 1: On the basis of the onset of such reactions observed in clinical trials and spontaneous adverse drug reaction reporting, cases of anaphylaxis were restricted to those occurring within 1 day of IV iron administration.

PS Settings 1: To emulate the randomization procedure in the target trial, we accounted for imbalances in patient characteristics across the IV iron formulations using a multinomial extension of the propensity score–based inverse probability of treatment weights (IPTW)

Outcome Model 1: We calculated the incidence rates (IRs) of anaphylaxis by formulation type (per 10 000 rst administrations) and estimated adjusted odds ratios (ORs) along with their corresponding 95% CIs using IPTW-weighted logistic regressions that modeled anaphylaxis as the outcome and individual IV iron formulations as the dependent variable.
`

export const JSONIVIronAnaphylaxis = {
  "getDbCohortMethodDataArgs": {
    "studyPeriods": [
      {
        "description": "",
        "studyStartDate": "20130701",
        "studyEndDate": "20181231"
      },
      {
        "description": "",
        "studyStartDate": "20150101",
        "studyEndDate": "20181231"
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
        "riskWindowStart": 0,
        "startAnchor": "cohort start",
        "riskWindowEnd": 1,
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
