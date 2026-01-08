export const TEXTMERINO = `
Study design
Patient-level data from the two studies performed as part of the BSI-FOO Programme [14, 15] were used to emulate the MERINO trial eligibility criteria, treatment strategy, and statistical analysis. The two studies were part of the same NIHR research programme and data collection was similar. Therefore, to maximise the potential sample size of the emulated trial, data from both studies were used. One was the BSI-FOO observational study and the other the RAPIDO RCT (trial registration ISRCTN97107018). The BSI-FOO observational study was a multicentre cohort study of 1,903 hospitalised patients with a BSI across seven NHS acute hospital trusts in England and Wales conducted between November 2010 and May 2012 with the primary aim of identifying modifiable risk factors for 28-day mortality. Adults (≥18 years old) receiving in-patient NHS hospital care and having a clinically significant BSI caused by six key pathogens: 1) methicillin-resistant Staphyloccos aureus (MRSA); 2) methicillin-susceptible S. aureus (MSSA); 3) non-Extended-spectrum beta-lactamase (ESBL)-producing Escherichi coli; 4) any ESBL-producing member of the family Enterobacteriales; 5) Pseudomonas aeruginosa; 6) any species of Candida, were included. RAPIDO was a multicentre open parallel group (1:1) RCT comparing two approaches to the identification of the causative microorganism(s) of BSI in hospitalised adult patients. The RAPIDO trial took place in seven NHS acute hospital trusts in England and Wales between July 2012 and August 2014. Date and time 0 was the date and time of first positive blood sample confirming BSI in both the observational study and the RCT. Full details of the inclusion criteria and study designs are given in the study results publications [14, 15].

The research programme on which this work is based was approved by Southwest Research Ethics Committee (10/HO102/51). The National Information Governance Board approved the use of routinely-collected patient data without specific consent for the BSI-FOO observational study and collection of full data for patients who died before being approached for consent in RAPIDO. North Bristol NHS Trust acted as Sponsor.

Study population
The MERINO trial inclusion/exclusion criteria were applied to BSI-FOO and RAPIDO participants (Table 1). Patients were included if they had a BSI with E. coli or Klebsiella spp. that was resistant to Ceftriaxone and/or Cefotaxime and susceptible to both meropenem and piperacillin-tazobactam and treatment was started within 72 hours of blood culture (date and time 0 for BSI-FOO and RAPIDO). Patients that would have otherwise been eligible but did not start any of the study drugs (meropenem or piperacillin-tazobactam) within the 72 hours window were considered ineligible and excluded from the emulated trial population. All participants were aged 18 years and over. Informed consent was not required for the BSI-FOO observational study and RAPIDO patients who died before being approached for consent, so it was not possible to replicate consent for this population, but all surviving RAPIDO participants provided written informed consent to join the trial. Exclusion criteria are given in Table 1.
Intervention
The trial interventions in the MERINO trial were treatment with piperacillin-tazobactam or meropenem. These were to be administered for a minimum of 4 days and maximum of 14 days, with duration determined by the treating clinician. In the emulated trial population patients were assigned to a “emulated intervention” based on their treatment timeline and allocated to the first study drug received. The start of follow-up was defined as the date in which the patient started their first dose of their assigned intervention.

Outcome measures
The primary outcome of the MERINO trial was all-cause mortality at 30 days after randomisation. It was not possible to analyse 30-day mortality for the emulated trial as follow-up in BSI-FOO and RAPIDO was limited to 28 days, where start of follow-up was defined as the date the blood sample was taken. All patients started their emulated intervention within 3 days of blood sample; therefore, 25-day mortality was analysed to ensure full follow-up was available for all patients.

Statistical analyses
The MERINO trial analysis population was defined as any randomised participant receiving at least 1 dose of the allocated drug. This was supported by an analysis of the per-protocol population. By definition, in the emulated trial population, all participants received at least one dose of allocated drug and were therefore included in the primary analysis population. We did not emulate the per-protocol analysis as few participants received allocated treatment for the required four days.

Continuous data were summarised using mean and standard deviation (or median and interquartile range (IQR) if distributions were skewed) and categorical data as numbers and percentages. Demographics, comorbidities and medical history were summarised by emulated intervention. Standardised mean differences were calculated to quantify imbalances in baseline characteristics by the treatment group [16]. Mortality over 25-days was summarised by emulated intervention using inverse probability weighted survival curves (weighted according to the inverse probability of treatment received, see below for further details of propensity score) to show adjusted survival graphically [17].

To emulate the trial analyses, absolute risk differences were calculated using generalised linear models. As the emulated trial was not randomised, potential confounding factors needed to be accounted for in the analysis. Propensity score models were developed using a logistic regression model with emulated intervention as the outcome. Variables included in the propensity score model were age and sex and any potential confounders based on clinician expertise. These were specified a priori. Factors included in the propensity score model were: centre, age, sex, temperature at time 0, neutrophil count on day 0 or closest, systolic blood pressure on day 0 or closest, on IV fluids at day, on ventilation at day 0, cerebrovascular disease, Charlson score and source of infection. The number of participants and deaths in each emulated intervention group was examined within strata defined by propensity score quantiles. Participants in strata for which there were no participants or deaths in either group were excluded to ensure the analyses were restricted to participants eligible to receive either treatment strategy, ensuring the assumption of positivity was met.

Convergence was not achieved when fitting an adjusted generalised linear model and therefore outcomes were compared using logistic regression adjusted for the propensity score. Unadjusted odds ratios were calculated for the MERINO trial to provide a comparison. Propensity scores were modelled using restricted cubic splines with 3 knots at 10th, 50th and 90th percentiles to capture potential non-linear associations with the outcome. Missing values were imputed with age- and sex-adjusted averages.

Three sensitivity analyses were carried out: (a) imputing missing categorical values with worst case values i.e. disease present; (b) propensity score model using restricted cubic splines at 25th, 50th and 75th percentiles to assess the robustness of the results to the location of knots; (c) excluding participants that switch to the other intervention during follow-up.

Model fit was assessed using standard methods. Meropenem was the reference group in all analyses. Results are reported as effect estimates with 95% confidence intervals (CI). In all tables missing data are described in footnotes.

All analyses were performed in Stata version 16.0 (StataCorp, LP, College Station, TX, US).
`

export const JSONMERINO = {
  getDbCohortMethodDataArgs: {
    studyPeriods: [
      {
        studyStartDate: "20101101",
        studyEndDate: "20120531",
      },
    ],
    restrictToCommonPeriod: true,
    firstExposureOnly: false,
    washoutPeriod: 0,
    removeDuplicateSubjects: "keep all",
    maxCohortSize: 0, //default
  },
  createStudyPopArgs: {
    censorAtNewRiskWindow: false, //default로 설정
    removeSubjectsWithPriorOutcome: true,
    priorOutcomeLookBack: 99999,
    timeAtRisks: [
      {
        riskWindowStart: 1,
        startAnchor: "cohort start",
        riskWindowEnd: 25,
        endAnchor: "cohort start",
        minDaysAtRisk: 1 //default 설정
      }
    ],
  },
  propensityScoreAdjustment: {
    psSettings: [
      {
        matchOnPsArgs: null,
        stratifyByPsArgs: {
            numberOfStrata: 10,
            baseSelection: "all" //default로 설정
        },
      }
    ],
    createPsArgs: { //laplace 제외하고 전부 default 설정
      maxCohortSizeForFitting: 250000,
      errorOnHighCorrelation: true,
      prior: { priorType: "laplace", useCrossValidation: true },
      control: {
        tolerance: 2e-7,
        cvType: "auto",
        fold: 10,
        cvRepetitions: 10,
        noiseLevel: "silent",
        resetCoefficients: true,
        startingVariance: 0.01,
      }
    }
  },
  fitOutcomeModelArgs: { //modelType제외 default 설정
    modelType: "logistic",
    stratified: true,
    useCovariates: false,
    inversePtWeighting: false,
    prior: { priorType: "laplace", useCrossValidation: true },
    control: {
      tolerance: 2e-7,
      cvType: "auto",
      fold: 10,
      cvRepetitions: 10,
      noiseLevel: "quiet",
      resetCoefficients: true,
      startingVariance: 0.01,
    },
  }
}
