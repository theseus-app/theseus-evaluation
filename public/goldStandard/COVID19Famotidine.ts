export const TEXTCOVID19Famotidine = 
`
The study period started February 1, 2020, and ended May 30, 2020,

Patients with evidence of intensive services (i.e., mechanical ventilation, tracheostomy, or extracorporeal membrane oxygenation) at or within 30 days before admission were excluded.

Time at risk was defined based on the intention-to-treat principle starting 1 day after admission and continuing up until the first of outcome of interest, loss to follow up, or 30 days after admission

To adjust for potential measured confounding and improve the balance between comparison cohorts, we built large-scale propensity score (PS) models for each comparison using regularized regression (13). We used a Laplace prior (LASSO) with the optimal hyperparameter to fit the model, determined through 10-fold cross validation in which the outcome is a binary indicator for the potential comparator. For the main analysis, we stratified into 5 PS strata and used conditional Cox proportional hazards models to estimate hazard ratios (HRs) between target and alternative comparator treatments for the risk of each outcome. The regression for the outcome models conditioned on the PS strata with treatment as the sole explanatory variable.
 As a sensitivity analysis, we used a 1:1 PS matching.
`
export const JSONCOVID19Famotidine = {
    getDbCohortMethodDataArgs: {
        studyPeriods: [
            {
                studyStartDate: "20200201",
                studyEndDate: "20200530",
            },
        ],
    },
    createStudyPopArgs: {
        removeSubjectsWithPriorOutcome: true,
        priorOutcomeLookBack: 30,
        timeAtRisks: [
            {
                riskWindowStart: 1,
                startAnchor: "cohort start",
                riskWindowEnd: 30,
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
                    maxRatio: 1,
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
        stratified: true,
    },
};
