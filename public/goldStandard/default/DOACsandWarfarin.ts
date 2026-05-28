export const TEXTDOACsandWarfarin = `
Study Period 1: The overall study period was from 19 October 2010 (coinciding with initial approval of dabigatran) to 31 December 2018.

TAR 1: The primary on-treatment time-at-risk period was defined as the time from 1 day after the anticoagulant index date to the end of inferred persistent exposure, the last day of observation, or the end of the study period, whichever came first. For DOACs, the final exposure record allowed for no more than a 3-day gap between successive exposure intervals (inferred by days’ supply) plus 5 days appended to the last exposure date. For warfarin, the final exposure record allowed for no more than a 7-day gap between successive exposure intervals (inferred by days’ supply) plus 5 days appended to the last exposure date.

TAR 2: the on-treatment time-at-risk period was similarly defined as in the primary analysis, but the final exposure record allowed for no more than a 30-day gap between successive exposure intervals (inferred by days’ supply) with no days appended to the last exposure date for DOACs and warfarin.

TAR 3: The second sensitivity analysis was an intent-to-treat (ITT) analysis, defined as the time from 1 day after the anticoagulant index date until the last day of observation or the end of the study period, whichever came first, regardless of the subsequent drug dispensing records.

PS Settings 1: Propensity scores (PSs) were used to reduce potential confounding due to an imbalance of observed patient characteristics at baseline and were calculated for each patient using the predicted probability from a regularized logistic regression model, fit with a Laplace prior (LASSO) and the regularization hyperparameter selected by optimizing the likelihood in a tenfold cross validation. The primary PS strategy matched the target to comparator patients in a 1:1 ratio

PS Settings 2: sensitivity PS strategy variably matched the target to comparator patients in a maximum of a 1:100 ratio

Outcome Model 1: Cox proportional hazards regression models were used to model the time to the first outcome occurrence for the target relative to the comparator cohort in pairwise comparisons
`

export const JSONDOACsandWarfarin = {
  "getDbCohortMethodDataArgs": {
    "studyPeriods": [
      {
        "description": "",
        "studyStartDate": "20101019",
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
        "riskWindowStart": 1,
        "startAnchor": "cohort start",
        "riskWindowEnd": 5,
        "endAnchor": "cohort end"
      },
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
      "matchOnPsArgs": {
        "maxRatio": 100,
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
