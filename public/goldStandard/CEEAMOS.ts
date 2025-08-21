export const TEXTCEEAMOS = 
`
we excluded patients who were enrolled in the database for < 1 year before the index date.

We excluded patients without a diagnosis 1 year before the index date. The other exclusion criteria were as follows: (1) a history of exposure to any hypertension treatment (prevalent user), (2) schizophrenia diagnosis and heart failure diagnosis at any time before the index date, (3) prescription of other blood pressure lowering medications (non-thiazide diuretics, beta blockers, and calcium channel blockers) and (4) prescription of the opposite drug (ARBs or thiazide diuretics for the ACE inhibitor group and vice versa) during the 7 days after the index date for ascertaining first-line treatment.

Our analysis considered the time-to-first event and was followed up to the earliest date among last date of assigned treatment, date of last observation in the database, date of occurrence of the endpoint, and date of censoring (as-treated [AT] approach).

The study populations were matched using variable-ratio PS matching with a maximum ratio of 10 (caliper = 0.2).

Cox proportional hazard models were fitted to estimate the hazard ratios (HRs) and 95% CIs according to exposure using the CohortMethod21 R package for each data source.
`

export const JSONCEEAMOS = {
    createStudyPopArgs: {
        washoutPeriod: 365,
        removeDuplicateSubjects: "remove all",
        removeSubjectsWithPriorOutcome: true,
        priorOutcomeLookBack: 365,
        timeAtRisks: [
            {
                riskWindowStart: 0,
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
                    maxRatio: 10,
                    caliper: 0.2,
                    caliperScale: "standardized logit",
                },
            },
        ],
    },
    fitOutcomeModelArgs: {
        modelType: "cox",
    },
};
