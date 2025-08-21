export const TEXTRanitidineCancer = 
`
The study included adult patients aged 20 years or older who used ranitidine for more than 30 days with at least 1 year of exposure-free observation period prior to cohort entry.

Patients with a history of cancer, exposure to other H2RAs for up to 1 year prior to cohort entry (for the target cohort), or ranitidine use for up to 1 year prior to cohort entry (for the comparator cohort) were excluded.

The predefined setting for primary analysis was 1:1 PS matching with ITT and a 1-year lag.

Comparator cohorts were constructed by performing 1:1 PS matching with a caliper of 0.2 SDs of the logic of the PS. 

Database-specific PSs were estimated using L1 regularized logistic regression tuned by 10-fold cross-validation.

Cox proportional hazard models were fitted to estimate the hazard ratios (HRs) and 95% CIs according to exposure using the CohortMethod21 R package for each data source.
`

export const JSONRanitidineCancer =
{
    createStudyPopArgs: {
        washoutPeriod: 365,
        removeSubjectsWithPriorOutcome: true,
        priorOutcomeLookBack: 365,
        timeAtRisks: [
            {
                riskWindowStart: 365,
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
                fold: 10,
            },
        },
    },
    fitOutcomeModelArgs: {
        modelType: "cox",
    }
}