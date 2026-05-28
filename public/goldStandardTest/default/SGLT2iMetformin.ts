export const TEXTSGLT2iMetformin = `
Study Period 1-4: To mitigate the potential channeling bias due to the selective prescription of SGLT-2i that changed over time since market launch, the study period was strafied into 4 consecutive calendar time blocks (T1, April 2013 throughDecember 2014; T2, January 2015 through June 2016; T3, July 2016 through December 2017; and T4, January 2018 through March 2020)

TAR 1: Follow-up began on the day after cohort entry and continued until the occurrence of a study outcome, death, treatment discontinuation (with an interval between prescription refills >60 days) (29), disenrollment, or end of the study period, whichever occurred first.

TAR 2: an intention-to-treat analysis was conducted to address potential informative censoring by carrying forward the initial exposure for 365 days without considering treatment discontinuation or the initiation of the comparator drug

PS Settings 1: We chose the ratio of 1:2 for matching to improve statistical efficiency because the number of metformin initiators was much larger compared with SGLT-2i initiators.

Outcome Model 1: Within each database, time block–specific PS-matched cohorts were aggregated for outcome regression, and hazard ratios (HRs) with 95% CIs were estimated using proportional hazards models without further adjustment.
`

export const JSONSGLT2iMetformin = {
  "getDbCohortMethodDataArgs": {
    "studyPeriods": [
      {
        "description": "",
        "studyStartDate": "20130401",
        "studyEndDate": "20141231"
      },
      {
        "description": "",
        "studyStartDate": "20150101",
        "studyEndDate": "20160630"
      },
      {
        "description": "",
        "studyStartDate": "20160701",
        "studyEndDate": "20171231"
      },
      {
        "description": "",
        "studyStartDate": "20180101",
        "studyEndDate": "20200331"
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
      },
      {
        "description": "",
        "minDaysAtRisk": 1,
        "riskWindowStart": 1,
        "startAnchor": "cohort start",
        "riskWindowEnd": 365,
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
        "maxRatio": 2,
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
