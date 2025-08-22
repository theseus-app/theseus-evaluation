################################################################################
# See the Create analysis specifications section
# of the UsingThisTemplate.md for more details.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# This help page also contains links to the corresponding HADES package that
# further details.
################################################################################

# Load required packages -------------------------------------------------------
# Notes:
# - Strategus orchestrates the analysis specification.
# - ROhdsiWebApi fetches cohorts and concept sets from an ATLAS/WebAPI server.
# - CohortMethod, FeatureExtraction, Cyclops supply the underlying HADES analysis APIs.
# - dplyr/tibble for simple data wrangling.
# - ParallelLogger to save the final JSON specifications.
suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(Strategus)
  library(ROhdsiWebApi)
  library(CohortMethod)
  library(FeatureExtraction)
  library(Cyclops)
  library(ParallelLogger)
})

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from ATLAS/WebAPI
# Replace with your own WebAPI if needed. The cohort and concept set IDs below are placeholders.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# IMPORTANT:
# - The three numeric IDs below are placeholders for:
#   0000000 = Target cohort id
#   1111111 = Comparator cohort id
#   2222222 = Outcome cohort id
# - The code re-numbers them locally to 1, 2, 3 (as often used in HADES workflows).
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    0000000, # Target:
    1111111, # Comparator:
    2222222  # Outcome:
  ),
  generateStats = TRUE
)

# Re-number cohorts locally to simplified IDs (1=Target, 2=Comparator, 3=Outcome)
cohortDefinitionSet[cohortDefinitionSet$cohortId == 0000000, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1111111, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 2222222, ]$cohortId <- 3

# Negative control outcomes ----------------------------------------------------
# IMPORTANT:
# - The numeric ID below (1234567) is a placeholder for a concept set in ATLAS
#   that represents your negative control outcome concepts.
# - The concept set is resolved (includes descendants) and then converted into
#   a list of negative control "cohorts" (concept-based outcomes) with new IDs.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1234567,
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(
    baseUrl = baseUrl
  ) %>%
  ROhdsiWebApi::getConcepts(
    baseUrl = baseUrl
  ) %>%
  dplyr::rename(outcomeConceptId = "conceptId",
                cohortName = "conceptName") %>%
  dplyr::mutate(
    # Target/Comparator/Outcome cohorts will be 1,2,3...
    # Negative control cohort IDs start at 101, 102, ...
    cohortId = dplyr::row_number() + 100
  ) %>%
  dplyr::select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no ID collisions between cohortDefinitionSet and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between main cohorts and negative control cohorts ***")
}

# Create lookup data frames for analysis components ---------------------------
# Outcomes (primary, plus window info used downstream)
oList <- cohortDefinitionSet %>%
  dplyr::filter(.data$cohortId == 3) %>%
  dplyr::mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  dplyr::select(outcomeCohortId, outcomeCohortName) %>%
  dplyr::mutate(cleanWindow = 365)

# Target and Comparator pairing for the CohortMethod analysis
# IMPORTANT:
# - Keep these names exactly as in the Template.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target cohort name",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator cohort name",
  stringsAsFactors = FALSE
)

# For CohortMethod PS model covariate construction, we typically exclude the
# drugs of interest (exposures) from the covariate space to avoid perfect prediction.
# Here we provide placeholder concept IDs/names for those exposures.
excludedCovariateConcepts <- data.frame(
  conceptId = c(2345678, 3456789),
  conceptName = c("target concept name", "comparator concept name"),
  stringsAsFactors = FALSE
)

# Optional: If you want to define covariates to include instead of including them all
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# Build Shared Resources for Strategus ----------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Cohort definitions shared resource (downloaded above from WebAPI)
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Negative control outcomes shared resource (concept-based outcomes)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# CohortGeneratorModule settings: generate cohort statistics during execution
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings --------------------------------------------
# Configure broad diagnostics across the cohorts of interest
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
# The following blocks translate the Analysis Specifications into settings
# used to create a set of CmAnalysis objects. Each unique combination of:
# - Study period
# - Time-at-risk (TAR)
# - Propensity score adjustment strategy
# becomes one entry in cmAnalysisList.

# Study periods (from <Analysis Specifications> getDbCohortMethodDataArgs.studyPeriods)
# IMPORTANT:
# - Keep these as strings "YYYYMMDD" as provided in the specifications.
studyPeriods <- tibble::tibble(
  studyStartDate = c("20010101", "20010101"),
  studyEndDate   = c("20171231", "20151130")
)

# Time-at-risk window(s) (from <Analysis Specifications> createStudyPopArgs.timeAtRisks)
# Here we include the single TAR requested; you can extend this list for more TARs.
timeAtRisks <- tibble::tibble(
  label = c("TAR 1"),
  riskWindowStart = c(1),
  startAnchor = c("cohort start"), # "cohort start" | "cohort end"
  riskWindowEnd = c(0),
  endAnchor = c("cohort end")      # "cohort start" | "cohort end"
)

# Propensity Score settings (from <Analysis Specifications> propensityScoreAdjustment.psSettings)
# We build a tidy list for both PS matching configurations.
matchOnPsArgsList <- tibble::tibble(
  label = c(
    "ps-m1: match 1:1 caliper=0.05 (propensity score)",
    "ps-m2: match 1:10 caliper=0.2 (standardized logit)"
  ),
  maxRatio = c(1, 10),
  caliper = c(0.05, 0.2),
  caliperScale = c("propensity score", "standardized logit") # allowed: "propensity score" | "standardized" | "standardized logit"
)

# We are not using "stratify by PS" in this specification (kept for completeness)
stratifyByPsArgsList <- tibble::tibble(
  label = character(0),
  numberOfStrata = integer(0),
  baseSelection = character(0) # "all" | "target" | "comparator"
)

# Build a consolidated PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# Add "match on PS" configurations
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

# Add "stratify by PS" configurations (none for this analysis)
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

# Covariate construction settings ---------------------------------------------
# You can incorporate include/exclude concepts (if any provided) into the FE settings.
# From <Analysis Specifications> covariateSelection, lists are effectively empty here,
# so we default to the FeatureExtraction default covariates and ensure excluded
# covariates are removed via excludedCovariateConceptIds in TCO below.
covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
  addDescendantsToExclude = TRUE
  # Example of how to include/exclude concept IDs at FE level:
  # includedCovariateConceptIds = includedCovariateConcepts$conceptId,
  # excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
)

# Outcomes list for CohortMethod ----------------------------------------------
# Compose outcomes of interest (primary) and negative controls (concept-based).
# Note:
# - primary outcome uses priorOutcomeLookback consistent with createStudyPopulation
# - negative controls are marked outcomeOfInterest=FALSE with trueEffectSize=1
outcomeList <- append(
  lapply(seq_len(nrow(oList)), function(i) {
    CohortMethod::createOutcome(
      outcomeId = oList$outcomeCohortId[i],
      outcomeOfInterest = TRUE,
      trueEffectSize = NA,
      priorOutcomeLookback = 99999
    )
  }),
  lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
    CohortMethod::createOutcome(
      outcomeId = i,
      outcomeOfInterest = FALSE,
      trueEffectSize = 1
    )
  })
)

# Target-Comparator-Outcomes (TCO) specification -------------------------------
# IMPORTANT:
# - We exclude the exposure concepts from the covariate space to avoid perfect prediction.
targetComparatorOutcomesList <- list()
for (i in seq_len(nrow(cmTcList))) {
  targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
    targetId = cmTcList$targetCohortId[i],
    comparatorId = cmTcList$comparatorCohortId[i],
    outcomes = outcomeList,
    excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
  )
}

# Iterate through all analysis setting combinations ----------------------------
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Propensity score adjustment method-specific arguments
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
        stop("Unknown PS configuration method: ", psCfg$method)
      }

      # GetDbCohortMethodData arguments (study period scoped)
      # Settings: from <Analysis Specifications> getDbCohortMethodDataArgs
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Create PS arguments (from <Analysis Specifications> propensityScoreAdjustment.createPsArgs)
      # - Using laplace prior with CV, control tuned as specified
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # Allow Strategus to continue even if PS fit fails; diagnostics will reflect issues
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

      # Covariate balance computations (standard Table 1 specs)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Outcome model arguments (from <Analysis Specifications> fitOutcomeModelArgs)
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

      # Study population/TAR arguments (from <Analysis Specifications> createStudyPopArgs)
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 99999,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1
        # Optionally set maxDaysAtRisk = 99999 if desired
      )

      # Append the analysis definition to the list
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

# Package settings for the CohortMethod module into Strategus specifications
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the full Strategus analysis specifications ----------------------------
# Order:
# 1) Shared resources (cohorts and negative controls)
# 2) Modules: CohortGenerator, CohortDiagnostics, CohortMethod
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the analysis specifications to JSON ------------------------------------
# IMPORTANT:
# - The study name should align with <Analysis Specifications> name: "strokerisk".
# - The file path below follows the template structure.
outputDir <- file.path("inst", "strokerisk")
if (!dir.exists(outputDir)) {
  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
}
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path(outputDir, "strokeriskAnalysisSpecification.json")
)

# End of script ----------------------------------------------------------------
# Notes:
# - Replace cohort IDs (0000000, 1111111, 2222222) and negative control conceptSetId (1234567)
#   with real IDs from your ATLAS instance.
# - All key analysis settings were mapped from <Analysis Specifications>:
#   study periods, TAR, PS creation/matching, and outcome model fitting.
# - The script produces a Strategus JSON that can be executed to run the study.