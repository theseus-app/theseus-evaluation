export const TEXTTramadolCodein = 
`
We employed the target and comparator cohorts with 365 days of continuous observable time prior to the index date.

Additionally, subjects were excluded if they experienced hip fracture, cancer, or were diagnosed with opioid abuse in the 365 days prior.

For this study, two time-at-risk (TAR) definitions were used: on-treatment (OT) and intent-to-treat (ITT).

The OT TAR was calculated from the index date of the first exposure to the end of treatment, based on days’ supply, allowing for up to 30-day gaps between the end of days’ supply and the start of the next prescription. This end date was censored if a subject died, left the database, reached the age of 89 years, reached 365 days after the index date, was exposed to the other opioid of interest

The ITT TAR started on the index date and continued until the target or comparator subject died, left the database, or experienced the outcome of interest

A PS was estimated for each subject using the predicted probability from a regularized logistic regression model, fit with a Laplace prior (least absolute shrinkage and selection operator), and the regularization hyperparameter selected by optimizing the likelihood in a ten-fold cross validation, using a starting variance of 0.01 and a tolerance of 2e−7.

Subjects were matched on 1:1 ratio of target to comparator subjects. This approach used a greedy matching algorithm by applying a caliper of 0.2 of the standard deviation on the logit scale of the PS distribution

In this study, we compared target cohorts with the comparator cohorts for the hazards of O1 or O2 during the TAR by applying a Cox proportional hazards model conditioned on PSs.
`

export const JSONTramadolCodein = {
    createStudyPopArgs: {
        removeSubjectsWithPriorOutcome: true,
        priorOutcomeLookBack: 365,
        timeAtRisks: [
            {
                riskWindowStart: 0,
                startAnchor: "cohort start",
                riskWindowEnd: 0,
                endAnchor: "cohort end",
            },
            {
                riskWindowStart: 0,
                startAnchor: "cohort start",
                riskWindowEnd: 9999,
                endAnchor: "cohort start",
            },
        ],
    },
    propensityScoreAdjustment: {
        psSettings: [
            {
                matchOnPsArgs: {
                    maxRatio: 1,
                    caliper: 0.2,
                    caliperScale: "standardized logit",
                },
            },
        ],
        createPsArgs: {
            prior: {
                priorType: "laplace",
                useCrossValidation: true,
            },
            control: {
                tolerance: 2.0e-7,
                fold: 10,
                startingVariance: 0.01,
            },
        },
    },
    fitOutcomeModelArgs: {
        modelType: "cox",
    },
};
