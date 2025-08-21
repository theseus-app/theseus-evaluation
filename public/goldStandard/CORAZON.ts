export const TEXTCORAZON = 
`
between 1 January 2010 (2012 for  Longitudinal Patients Database France) and 31 December 2019.

Patients were followed from the index date  until the occurrence of the study outcome, treatment  discontinuation (allowing for 90-day gaps between consecutive prescriptions, with the date of treatment discontinuation being the end date of the last prescription [the “on-treatment” approach]), switching from the index medication to another oral anticoagulant (apixaban, dabigatran, edoxaban, rivaroxaban, or warfarin), death, or the end of the study period (31 December 2019), whichever came first.

We did additional sensitivity analyses in which the time at risk was not censored if a patient discontinued the index medication therapy or switched to another anticoagulant (analogue to the “intention-to-treat” approach).

Patients were stratified into 5 strata based on propensity score to estimate the average treatment effect.
We also repeated our analyses using propensity score matching at a variable-matching ratio as sensitivity analyses to estimate the average treatment effect on treated patients.

Cox proportional hazards regression conditioned on the propensity score strata was applied to estimate the hazard ratio (HR) of the risk for outcomes 
`

export const JSONCORAZON = {
    getDbCohortMethodDataArgs: {
        studyPeriods: [
            {
                studyStartDate: "20100101",
                studyEndDate: "20191231",
            },
            {
                studyStartDate: "20120101",
                studyEndDate: "20191231",
            },
        ],
    },
    createStudyPopArgs: {
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
                stratifyByPsArgs: {
                    numberOfStrata: 5,
                },
            },
            {
                matchOnPsArgs: {
                    maxRatio: 0,
                },
            },
        ],
    },
    fitOutcomeModelArgs: {
        modelType: "cox",
        stratified: true,
    },
};
