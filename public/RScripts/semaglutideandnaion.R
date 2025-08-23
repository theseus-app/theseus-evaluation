################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script builds a Strategus analysis specification JSON using settings
# provided in the <Analysis Specifications> block, following the structure
# of the given <Template>.
#
# Major elements:
# - Shared resources: cohort definitions and negative-control outcome concepts
# - Modules:
#   * CohortGenerator
#   * CohortDiagnostics
#   * CohortMethod (with study periods, TARs, PS adjustment options, etc.)
#
# Annotations are provided throughout to clarify how each setting from the
# <Analysis Specifications> is applied.
################################################################################

# Libraries --------------------------------------------------------------------
library(dplyr)
library(tibble)
library(ROhdsiWebApi)
library(FeatureExtraction)
library(CohortMethod)
library(Cyclops)
library(Strategus)
library(ParallelLogger)

# Shared Resources -------------------------------------------------------------
# ATLAS / WebAPI source for cohort and concept set definitions
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Per <Analysis Specifications>, use EXACT cohort IDs and then renumber locally:
# - Target:     id = 1794126, name = "target1"      -> internal id 1
# - Comparator: id = 1794132, name = "comparator1"  -> internal id 2
# - Outcome:    id = 1794131, name = "outcome1"     -> internal id 3
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: target1
    1794132, # Comparator: comparator1
    1794131  # Outcome: outcome1
  ),
  generateStats = TRUE
)

# Re-number cohorts to a compact local index used in the CohortMethod module
# (1=Target, 2=Comparator, 3=Outcome). Renumbering facilitates T/C/O mapping.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes ----------------------------------------------------
# Per <Analysis Specifications>, negativeControlConceptSet id = 1888110, name = "negative"
# We use the concept set to produce a set of negative-control outcome concept IDs.
# These will be used as outcome "cohorts" by the CohortMethod module, where each
# concept is assigned a unique synthetic ID >= 101.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110,
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(
    baseUrl = baseUrl
  ) %>%
  ROhdsiWebApi::getConcepts(
    baseUrl = baseUrl
  ) %>%
  rename(
    outcomeConceptId = "conceptId",
    cohortName = "conceptName"
  ) %>%
  mutate(
    cohortId = dplyr::row_number() + 100 # 101, 102, ...
  ) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Basic safety check: ensure no overlap between numbered cohort IDs and negative-control IDs
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found across cohort definitions and negative controls ***")
}

# Create simple data frames to reference cohorts in each analysis ---------------

# Outcomes: main outcome ("outcome1") + negative controls
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis
# Use EXACT names from <Analysis Specifications>
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target1",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator1"
)

# Covariate inclusion/exclusion lists (from <Analysis Specifications>)
# The provided lists have null/empty entries, so nothing to include/exclude.
includedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character()
)
excludedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character()
)

# CohortGeneratorModule --------------------------------------------------------
# This module will:
# - Store the cohort definitions (T/C/O) as a shared resource
# - Store negative-control outcome concepts as a shared resource
# - Generate cohorts (with stats) when Strategus executes the analysis
cgModuleSettingsCreator <- CohortGeneratorModule$new()

cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",         # First occurrence per subject
  detectOnDescendants = TRUE        # Include descendants for concept detection
)

cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE              # Generate inclusion rule stats, etc.
)

# CohortDiagnosticsModule ------------------------------------------------------
# Diagnostics for T/C/O cohorts.
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
# Assemble the CohortMethod analysis list from the settings in <Analysis Specifications>.

# 1) Study periods (from getDbCohortMethodDataArgs.studyPeriods)
# If not restricting to a specific time window, the template uses empty vectors.
# Here, the <Analysis Specifications> provides a single study window:
#   studyStartDate = 20171201, studyEndDate = 20231231
studyPeriods <- tibble(
  studyStartDate = c(20171201),
  studyEndDate   = c(20231231)
)

# 2) Time-at-risk windows (from createStudyPopArgs.timeAtRisks)
# A single TAR:
# - riskWindowStart = 0, startAnchor = "cohort start"
# - riskWindowEnd   = 0, endAnchor   = "cohort end"
# - minDaysAtRisk   = 1 (applied later when building createStudyPopArgs)
timeAtRisks <- tibble(
  label = c("TAR-IndexToEnd"),
  riskWindowStart  = c(0),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end")
)

# 3) Propensity Score adjustment configurations (from propensityScoreAdjustment.psSettings)
# We create two configurations: a PS match, and a PS stratification.
matchOnPsArgsList <- tibble(
  label = c("PS-match-1:1-0.2"),
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("propensity score") # options: "propensity score" | "standardized" | "standardized logit"
)

stratifyByPsArgsList <- tibble(
  label = c("PS-strata-5-all"),
  numberOfStrata  = c(5),
  baseSelection = c("all") # options: "all" | "target" | "comparator"
)

# Build a list of PS configurations; each element will either "match" or "stratify"
psConfigList <- list()

# Convert each row of matchOnPsArgsList into a config entry
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

# Convert each row of stratifyByPsArgsList into a config entry
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

# Iterate through all setting combinations to produce one or more CM analyses ---
cmAnalysisList <- list()
targetComparatorOutcomesList <- list()
analysisId <- 1

# Build the outcome list once: include the main outcome and all negative controls
# - main outcome: outcomeOfInterest = TRUE, priorOutcomeLookback = 99999 (from specification)
# - negative controls: outcomeOfInterest = FALSE, trueEffectSize = 1
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

# Build TargetComparatorOutcomes list (one row in cmTcList, but coded to scale)
# Note: Excluded covariate concept IDs are empty here (no concepts provided).
for (i in seq_len(nrow(cmTcList))) {
  targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
    targetId = cmTcList$targetCohortId[i],
    comparatorId = cmTcList$comparatorCohortId[i],
    outcomes = outcomeList,
    excludedCovariateConceptIds = c()
  )
}

# Loop over the single study period and single TAR, but two PS configurations
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {
    # Covariate settings:
    # Default settings; no concept-level include/exclude provided in <Analysis Specifications>.
    covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
      addDescendantsToExclude = TRUE
    )

    # getDbCohortMethodDataArgs (from <Analysis Specifications>.getDbCohortMethodDataArgs)
    # - studyStartDate = 20171201
    # - studyEndDate   = 20231231
    # - maxCohortSize  = 0
    # Note: We do NOT set restrictToCommonPeriod here (left as default).
    getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
      studyStartDate = studyStartDate,
      studyEndDate = studyEndDate,
      maxCohortSize = 0,
      covariateSettings = covariateSettings
    )

    # createPsArgs (from <Analysis Specifications>.propensityScoreAdjustment.createPsArgs)
    # - maxCohortSizeForFitting = 250000
    # - errorOnHighCorrelation = FALSE
    # - prior: laplace, useCrossValidation = TRUE
    # - control: tolerance=2e-7, cvType="auto", fold=10, cvRepetitions=10,
    #            noiseLevel="quiet", resetCoefficients=FALSE, startingVariance=0.01
    createPsArgs <- CohortMethod::createCreatePsArgs(
      maxCohortSizeForFitting = 250000,
      errorOnHighCorrelation = FALSE,
      stopOnError = FALSE, # keep FALSE to allow CM to continue even if PS fitting fails
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
        resetCoefficients = FALSE,
        startingVariance = 0.01
      )
    )

    # Covariate balance computations (not specified; use template defaults)
    computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
      maxCohortSize = 250000,
      covariateFilter = NULL
    )
    computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
      maxCohortSize = 250000,
      covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
    )

    # fitOutcomeModelArgs (from <Analysis Specifications>.fitOutcomeModelArgs)
    # - modelType = "cox"
    # - stratified = FALSE
    # - useCovariates = FALSE
    # - inversePtWeighting = FALSE
    # - prior: laplace, useCrossValidation = TRUE
    # - control: tolerance=2e-7, cvType="auto", fold=10, cvRepetitions=10,
    #            noiseLevel="quiet", resetCoefficients=TRUE, startingVariance=0.01
    fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
      modelType = "cox",
      stratified = FALSE,
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

    # createStudyPopArgs (from <Analysis Specifications>.createStudyPopArgs)
    # - restrictToCommonPeriod = FALSE
    # - firstExposureOnly = FALSE
    # - washoutPeriod = 365
    # - removeDuplicateSubjects = "keep all"
    # - censorAtNewRiskWindow = FALSE
    # - removeSubjectsWithPriorOutcome = TRUE
    # - priorOutcomeLookBack = 99999
    # - TAR (loop vars):
    #   riskWindowStart = timeAtRisks$riskWindowStart[t]
    #   startAnchor     = timeAtRisks$startAnchor[t]
    #   riskWindowEnd   = timeAtRisks$riskWindowEnd[t]
    #   endAnchor       = timeAtRisks$endAnchor[t]
    #   minDaysAtRisk   = 1
    createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
      restrictToCommonPeriod = FALSE,
      firstExposureOnly = FALSE,
      washoutPeriod = 365,
      removeDuplicateSubjects = "keep all",
      censorAtNewRiskWindow = FALSE,
      removeSubjectsWithPriorOutcome = TRUE,
      priorOutcomeLookback = 99999,
      riskWindowStart = timeAtRisks$riskWindowStart[t],
      startAnchor = timeAtRisks$startAnchor[t],
      riskWindowEnd = timeAtRisks$riskWindowEnd[t],
      endAnchor = timeAtRisks$endAnchor[t],
      minDaysAtRisk = 1,
      maxDaysAtRisk = 99999
    )

    # For each PS configuration, create one CohortMethod analysis
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

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

      # Append the analysis specification to the list
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

# Finalize CohortMethod module specifications
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
# Build the full Strategus analysis specification that includes:
# - Shared resources (cohort definitions, negative controls)
# - Modules (CohortGenerator, CohortDiagnostics, CohortMethod)
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save to JSON (use EXACT study name from <Analysis Specifications>)
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "semaglutideandnaion", "semaglutideandnaionAnalysisSpecification.json")
)