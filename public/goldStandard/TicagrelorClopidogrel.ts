export const TEXTTicagrelorClopidogrel = 
`
from November 2011 to March 2019.
we conducted an additional analysis to apply an identical study period to all databases, namely March 2013 and December2016,b

To avoid left censoring, we excluded patients who had been enrolled in the database for less than 1 year before the index date.

The other exclusion criteria were a history of ischemic stroke, hemorrhagic stroke, or gastrointestinal bleeding anytime before the index date; and a prescription of prasugrel, clopidogrel (for the ticagrelor group), or ticagrelor (for the clopidogrel group) during the 30 days preceding the index date.

Patients were censored when they were no longer observed in the database, while they remained in the primary analysis if they discontinued the allocated drug or switched the drug within the first year.
2 more time-at-risk windows were applied—a 5-year period and an on-treatment period. The on-treatment period was defined as the time from 1 day after the index date until the end of persistent exposure to the drug, allowing a 7-day gap or the end of a patient’s record.

The study populations were matched using one-to-one greedy matching of the propensity score.

In addition to one-to-one propensity score matching, 2 additional propensity score adjustments were performed: (1) variable-ratio propensity score matching with a maximum ratio of 10; and (2) propensity score stratification using deciles of the propensity score distribution.

We used Cox proportional haz ard regression models to estimate the association of expo sures with outcomes
`

export const JSONTicagrelorClopidogrel = {
    getDbCohortMethodDataArgs: {
        studyPeriods: [
            {
                studyStartDate: "20111101",
                studyEndDate: "20190331",
            },
            {
                studyStartDate: "20130301",
                studyEndDate: "20161231",
            },
        ],
    },
    createStudyPopArgs: {
        washoutPeriod: 365,
        removeSubjectsWithPriorOutcome: true,
        priorOutcomeLookBack: 30,
        timeAtRisks: [
            {
                riskWindowStart: 0,
                startAnchor: "cohort start",
                riskWindowEnd: 9999,
                endAnchor: "cohort start",
            },
            {
                riskWindowStart: 0,
                startAnchor: "cohort start",
                riskWindowEnd: 1825,
                endAnchor: "cohort start",
            },
            {
                riskWindowStart: 1,
                startAnchor: "cohort start",
                riskWindowEnd: 0,
                endAnchor: "cohort end",
            },
        ],
    },
    propensityScoreAdjustment: {
        psSettings: [
            {
                matchOnPsArgs: {
                    maxRatio: 1,
                },
            },
            {
                matchOnPsArgs: {
                    maxRatio: 10,
                },
            },
            {
                stratifyByPsArgs: {
                    numberOfStrata: 10,
                },
            },
        ],
    },
    fitOutcomeModelArgs: {
        modelType: "cox",
    },
};
