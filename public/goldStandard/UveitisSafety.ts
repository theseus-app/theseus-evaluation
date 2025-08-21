export const TEXTUveitisSafety = 
`
The target cohort population was limited to index exposures after the earliest date of approval by the FDA for the drugs included in the comparator cohort.

Within each study population we compared new users of a target exposure to new users of comparator exposures, that we refer to as the target and comparator cohorts.

Patients aged at least 18 years at the time of index and with at least 365 days of prior observation were eligible to participate in both the target and comparator cohorts.

Additionally, we right-censored ‘on-treatment’ TAR at an exposure to a comparison drug, adalimumab, or etanercept; for the target cohorts, exposure was censored on other TNF alpha inhibitor (TNFαi) or interleukin inhibitors and the comparator cohorts, exposure was censored at the exposures listed in Table 2.

We defined the ‘on-treatment’ time-at-risk (TAR) as the day after index until the end of a period of inferred persistent exposure.

Our primary PS adjustment strategy matched target to comparator patients using a 1:10 maximum variable ratio matching approach and used a greedy matching algorithm that applied a caliper of 0.2 of the standard deviation on the logit scale of the PS distribution [48].

The PS was calculated for each patient as the predicted probability of target exposure status from an L1 regularized logistic regression model, fit with a Laplace prior where the regularization hyperparameter was selected by optimizing the likelihood in a 10-fold cross validation with a starting variance of 0.01 and a tolerance of 2*10− 7 [47].
`

export const JSONUveitisSafety =
{
    createStudyPopArgs: {
        restrictToCommonPeriod: true,
        firstExposureOnly: true,
        washoutPeriod: 365,
        censorAtNewRiskWindow: true,
        removeSubjectsWithPriorOutcome: true,
        priorOutcomeLookBack: 99999,
        timeAtRisks: [
            {
                riskWindowStart: 1,
                startAnchor: "cohort start",
                riskWindowEnd: 0,
                endAnchor: "cohort end"
            }
        ]
    },
    propensityScoreAdjustment: {
        psSettings: [
            {
                matchOnPsArgs: {
                    maxRatio: 10,
                    caliper: 0.2,
                    caliperScale: "standardized logit"
                }
            }
        ],
        createPsArgs: {
            prior: {
                priorType: "laplace",
                useCrossValidation: true
            },
            control: {
                tolerance: 0.0000002,
                cvType: "auto",
                fold: 10,
                startingVariance: 0.01
            }
        }
    },
    fitOutcomeModelArgs: {
        modelType: "cox",
        stratified: true
    }
}