export const TEXTSemaglutideandNAION = 
`
Adults 18 years and older with T2D taking semaglutide (GLP-1RA), dulaglutide (GLP-1RA), exenatide (GLP-1RA), empagliflozin (sodium-glucose cotransporter 2 [SGLT2] inhibitor), sitagliptin (dipeptidyl peptidase 4 [DPP4] inhibitor), or glipizide (sulfonylurea) during the study period (December 1, 2017-December 31, 2023) were included.

In brief, adults with T2D taking metformin monotherapy were included if they had at least 1 year of prior observation, initiated treatment with one of the medications of interest, had no prior exposure to a comparator diabetes medication, and had at most 30 days of prior insulin use.

The time-at-risk period began with medication initiation until the end of continuous drug exposure, defined as a gap in exposure of more than 30 days or the end of the continuous observation period.

Patients in each target and comparator exposure comparison (eg, semaglutide vs dulaglutide) were matched 1:1 using propensity scores

Cox proportional hazards models estimated the HR of NAION from cohort entry to the outcome while taking treatment with each target and comparator T2D medication.
`

export const JSONSemaglutideandNAION =
{
    getDbCohortMethodDataArgs: {
        studyPeriods: [
            {
                studyStartDate: 20171201,
                studyEndDate: 20231231
            }
        ]
    },
    createStudyPopArgs: {
        washoutPeriod: 365,
        timeAtRisks: [
            {
                riskWindowStart: 0,
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
                    maxRatio: 1,
                    caliperScale: "propensity score"
                }
            },
            {
                stratifyByPsArgs: {
                    baseSelection: "all"
                }
            }
        ],
        createPsArgs: {
            errorOnHighCorrelation: false,
            prior: {
                priorType: "laplace",
                useCrossValidation: true
            },
            control: {
                cvType: "auto",
                noiseLevel: "quiet",
                resetCoefficients: false
            }
        }
    },
    fitOutcomeModelArgs: {
        modelType: "cox",
        stratified: true,
        useCovariates: true,
        inversePtWeighting: false
    }
}