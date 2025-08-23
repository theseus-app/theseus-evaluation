################################################################################
# See the Create analysis specifications section
# of the UsingThisTemplate.md for more details.
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# This help page also contains links to the corresponding HADES package that
# further details.
################################################################################
library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from an ATLAS/WebAPI instance
# Note: Replace baseUrl and cohortIds with your actual study assets. The cohortIds
# below are placeholders as shown in the template.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# - The template shows three cohorts: Target, Comparator, and Outcome.
# - We renumber them locally to 1 (target), 2 (comparator), 3 (outcome) to enable
#   easier downstream referencing independent of original WebAPI IDs.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: 
    1794132, # Comparator: 
    1794131  # Outcome: 
  ),
  generateStats = TRUE
)

# Re-number cohorts (local IDs used by modules)
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes ----------------------------------------------------
# Analysis Specifications indicate null negative control concept set ID; therefore,
# we do NOT create a negative control shared resource. The code below demonstrates
# how it would be created when a valid conceptSetId is available.
# To activate, set 'negCtrlConceptSetId' to a valid WebAPI Concept Set ID.

negCtrlConceptSetId <- NA_integer_ # From Analysis Specifications: "id": null

negativeControlOutcomeCohortSet <- tibble() # initialize empty

if (!is.na(negCtrlConceptSetId) && is.finite(negCtrlConceptSetId)) {
  negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
    conceptSetId = negCtrlConceptSetId,
    baseUrl = baseUrl
  ) %>%
    ROhdsiWebApi::resolveConceptSet(baseUrl = baseUrl) %>%
    ROhdsiWebApi::getConcepts(baseUrl = baseUrl) %>%
    rename(
      outcomeConceptId = "conceptId",
      cohortName = "conceptName"
    ) %>%
    mutate(cohortId = dplyr::row_number() + 100) %>% # Ensure no overlap with 1,2,3 (T/C/O)
    select(cohortId, cohortName, outcomeConceptId)
}

# Ensure no duplicate cohort IDs across primary cohorts and (optional) negatives
if (nrow(negativeControlOutcomeCohortSet) > 0) {
  if (any(duplicated(c(cohortDefinitionSet$cohortId,
                       negativeControlOutcomeCohortSet$cohortId)))) {
    stop("*** Error: duplicate cohort IDs found ***")
  }
} else {
  if (any(duplicated(cohortDefinitionSet$cohortId))) {
    stop("*** Error: duplicate cohort IDs found ***")
  }
}

# Create some data frames to hold the cohorts we'll use in each analysis --------
# Outcomes: Use local outcome cohort id = 3. 'cleanWindow' can be used by some
# downstream diagnostics or QC steps.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis 
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target cohort name",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator cohort name"
)

# Covariate concept selections -------------------------------------------------
# Analysis Specifications:
# - conceptsToInclude: [{ id: null, name: "" }]
# - conceptsToExclude: [{ id: null, name: "" }]
# So, these are empty in practice. Define empty frames to keep the template shape.
includedCovariateConcepts <- data.frame(
  conceptId = numeric(0),
  conceptName = character(0),
  stringsAsFactors = FALSE
)

excludedCovariateConcepts <- data.frame(
  conceptId = numeric(0),
  conceptName = character(0),
  stringsAsFactors = FALSE
)

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet = cohortDefinitionSet
)

negativeControlsShared <- NULL
if (nrow(negativeControlOutcomeCohortSet) > 0) {
  negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
    negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
    occurrenceType = "first",
    detectOnDescendants = TRUE
  )
}

cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
  runInclusionStatistics = TRUE,
  runIncludedSourceConcepts = TRUE,
  runOrphanConcepts = TRUE,
  runTimeSeries = FALSE,
  runVisitContext = TRUE,
  runBreakdownIndexEvents = TRUE,
  runIncidenceRate = TRUE,
  runCohortRelationship = TRUE,
  runTemporalCohortCharacterization = TRUE,
  minCharacterizationMean = 0.01
)

# CohortMethodModule -----------------------------------------------------------
# Analysis Specifications applied below:
# - Study Period: 20200201 - 20200530
# - TAR: start=1 day after cohort start, end=30 days after cohort start, minDaysAtRisk=1
# - PS Adjustments: (1) Stratify by PS (5 strata, base=all), (2) 1:1 Match, caliper 0.2 standardized logit
# - Create PS Args: laplace prior with CV, control tolerance 2e-7, cvType auto, fold 10, cvRepetitions 10, noiseLevel silent, resetCoefficients TRUE, startingVariance 0.01
# - Study Population Args: restrictToCommonPeriod=FALSE, firstExposureOnly=FALSE, washoutPeriod=0, removeDuplicateSubjects="keep all", censorAtNewRiskWindow=FALSE, removeSubjectsWithPriorOutcome=TRUE, priorOutcomeLookback=30

# Study period(s) from Analysis Specifications
studyPeriods <- tibble::tibble(
  studyStartDate = c("20200201"),
  studyEndDate   = c("20200530")
)

# Time-at-risk(s) from Analysis Specifications
timeAtRisks <- tibble::tibble(
  label = c("Day1to30_fromCohortStart"),
  riskWindowStart  = c(1),
  startAnchor      = c("cohort start"),
  riskWindowEnd    = c(30),
  endAnchor        = c("cohort start"),
  minDaysAtRisk    = c(1)
)

# Propensity Score settings - stratify by PS (from Analysis Specifications)
stratifyByPsArgsList <- tibble::tibble(
  label = c("PS_Stratify_5Strata_BaseAll"),
  numberOfStrata  = c(5),
  baseSelection   = c("all") # "all" | "target" | "comparator"
)

# Propensity Score settings - match on PS (from Analysis Specifications)
matchOnPsArgsList <- tibble::tibble(
  label = c("PS_Match_1to1_Caliper0.2_StdLogit"),
  maxRatio  = c(1),
  caliper   = c(0.2),
  caliperScale  = c("standardized logit") # "propensity score" | "standardized" | "standardized logit"
)

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# Convert "match on PS" rows to configs
if (exists("matchOnPsArgsList") && nrow(matchOnPsArgsList) > 0) {
  for (i in seq_len(nrow(matchOnPsArgsList))) {
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "match",
      label  = matchOnPsArgsList$label[i],
      params = list(
        maxRatio     = matchOnPsArgsList$maxRatio[i],
        caliper      = matchOnPsArgsList$caliper[i],
        caliperScale = matchOnPsArgsList$caliperScale[i]
      )
    )
  }
}

# Convert "stratify by PS" rows to configs
if (exists("stratifyByPsArgsList") && nrow(stratifyByPsArgsList) > 0) {
  for (i in seq_len(nrow(stratifyByPsArgsList))) {
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "stratify",
      label  = stratifyByPsArgsList$label[i],
      params = list(
        numberOfStrata = stratifyByPsArgsList$numberOfStrata[i],
        baseSelection  = stratifyByPsArgsList$baseSelection[i]
      )
    )
  }
}

# Iterate through all combinations of study period x TAR x PS adjustment
cmAnalysisList <- list()
analysisId <- 1

# Precompute covariate settings honoring included/excluded concepts
covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
  addDescendantsToExclude = TRUE,
  includedCovariateConceptIds = unique(includedCovariateConcepts$conceptId),
  excludedCovariateConceptIds = unique(excludedCovariateConcepts$conceptId)
)

# Build standard and negative-control outcome lists
# - The primary outcome is cohortId == 3 (from oList)
# - Negative controls (if any) are appended as outcomeOfInterest = FALSE, trueEffectSize = 1
baseOutcomeList <- lapply(seq_len(nrow(oList)), function(i) {
  CohortMethod::createOutcome(
    outcomeId = oList$outcomeCohortId[i],
    outcomeOfInterest = TRUE,
    trueEffectSize = NA,
    priorOutcomeLookback = 99999
  )
})

negativeControlOutcomes <- list()
if (nrow(negativeControlOutcomeCohortSet) > 0) {
  negativeControlOutcomes <- lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
    CohortMethod::createOutcome(
      outcomeId = i,
      outcomeOfInterest = FALSE,
      trueEffectSize = 1
    )
  })
}

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # PS adjustment method specific arguments
      if (psCfg$method == "match") {
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psCfg$method == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      } else {
        stop("Unknown PS adjustment method specified.")
      }

      # Build list of outcomes for this analysis (primary + optional NCs)
      outcomeList <- c(baseOutcomeList, negativeControlOutcomes)

      # Target/comparator/outcomes bundle. Exclude any specified covariate concept IDs.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = unique(excludedCovariateConcepts$conceptId)
        )
      }

      # Data retrieval args (note: studyStartDate/studyEndDate enforce overall window)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Create PS Args – honoring Analysis Specifications
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # Helpful in batch settings to continue after PS failures
        prior = Cyclops::createPrior(
          priorType = "laplace",
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          tolerance = 2e-07,
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "silent",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # Balance computation args (defaults; complement PS diagnostics)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Outcome model args (Cox, stratified, no covariates, IPW off) per Analysis Specifications
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = TRUE,
        useCovariates = FALSE,
        inversePtWeighting = FALSE,
        prior = Cyclops::createPrior(
          priorType = "laplace",
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          tolerance = 2e-07,
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "quiet",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # Study population args per Analysis Specifications
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 30,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t]
      )

      # Append the settings to Analysis List
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          studyStartDate,
          studyEndDate,
          timeAtRisks$label[t],
          psCfg$label
        ),
        getDbCohortMethodDataArgs = getDbCohortMethodDataArgs,
        createStudyPopArgs = createStudyPopArgs,
        createPsArgs = createPsArgs,
        matchOnPsArgs = matchOnPsArgs,
        stratifyByPsArgs = stratifyByPsArgs,
        computeSharedCovariateBalanceArgs = computeSharedCovariateBalanceArgs,
        computeCovariateBalanceArgs = computeCovariateBalanceArgs,
        fitOutcomeModelArgs = fitOutcomeModelArgs
      )
      analysisId <- analysisId + 1
    }
  }
}

cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,  
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the analysis specifications ------------------------------------------
# - Start from an empty Strategus specification object
# - Add shared resources (cohort definitions, and optionally negative controls)
# - Add module specifications for CohortGenerator, CohortDiagnostics, CohortMethod
analysisSpecifications <- Strategus::createEmptyAnalysisSpecificiations() |>
  Strategus::addSharedResources(cohortDefinitionShared)

if (!is.null(negativeControlsShared)) {
  analysisSpecifications <- analysisSpecifications |>
    Strategus::addSharedResources(negativeControlsShared)
}

analysisSpecifications <- analysisSpecifications |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Persist to JSON --------------------------------------------------------------
# Using Analysis Specifications "name": "covid19famotidine" for output folder/file
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "covid19famotidine", "covid19famotidineAnalysisSpecification.json")
)