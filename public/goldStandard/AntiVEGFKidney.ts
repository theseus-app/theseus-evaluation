export const TEXTAntiVEGFKidney = 
`
If patients switched anti-VEGF medications, only the first exposure was included in the analysis, after which the patients were right-censored from the cohort

Adult patients aged ≥ 18 years who were newly treated with monthly intravitreal anti-VEGF medications (ranibizumab, aflibercept, or bevacizumab) for ≥ 3 months for a blinding disease with ≥ 365 days of prior observation were included in the study.

Patients with preexisting kidney failure (defined further below) were also excluded.

Patients were assumed at-risk of kidney failure after the third anti-VEGF exposure until the end of continuous drug exposure or the end of the study period.

We used the large-scale propensity score method to match patients in each target and comparator exposure cohort comparison using 1:1 propensity score matching.

The propensity score model included a large number of baseline covariates  as potential confounders and used the L1-regularization technique to avoid model overfitting.

Cox proportional hazards models were used to estimate the risk of kidney failure while on treatment
`

export const JSONAntiVEGFKidney = {
  createStudyPopArgs: {
    firstExposureOnly: true,
    washoutPeriod: 365,
    removeSubjectsWithPriorOutcome: true,
    priorOutcomeLookBack: 99999,
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
          maxRatio: 1
        }
      },
    ],
    createPsArgs: {
      prior: {
        priorType: "laplace",
      }
    }
  },
  fitOutcomeModelArgs: {
    modelType: "cox"
  }
}
