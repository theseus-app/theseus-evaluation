#!/usr/bin/env Rscript

################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script creates a Strategus analysis specification JSON for the study:
# semaglutideandnaion
#
# It is based on the Analysis Specifications provided and the Strategus template.
# NOTE: Several IDs (cohort definition ids, negative control concept set id,
#       covariate concept ids) are intentionally left as placeholders (NULL in
#       the original specifications). You MUST replace the placeholder IDs with
#       real ids from your Atlas / WebAPI instance before running the study.
#
# The script includes extensive annotations to explain how each setting in the
# Analysis Specifications maps to Strategus / CohortMethod / Cyclops settings.
################################################################################

# Required libraries -----------------------------------------------------------
# Strategus builds on several OHDSI packages. Load them here to ensure all
# helper functions are available.
library(dplyr)
library(tibble)
library(Strategus)
library(ROhdsiWebApi)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)
library(ParallelLogger)

# Study name (used for file paths and saving the specification)
studyName <- "semaglutideandnaion"

# Base WebAPI URL --------------------------------------------------------------
# Replace with the Atlas / WebAPI endpoint for your environment.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

################################################################################
# Cohort Definitions (target, comparator, outcomes)
#
# The Analysis Specifications provided did not include concrete cohort IDs or
# names. Populate the cohortIds below with real cohort definition IDs from
# your Atlas/WebAPI. The script will export the cohort definitions and then
# renumber them to 1 (target), 2 (comparator), 3 (outcome) for internal use.
#
# Example:
#   cohortIds = c( 12345, 23456, 34567 )
#
# Replace the 0000000 placeholders below with real IDs.
################################################################################

cohortDefinitionIds <- c(
  0000000, # Target cohort ID: REPLACE with real cohort definition id for target
  1111111, # Comparator cohort ID: REPLACE with real cohort definition id for comparator
  2222222  # Outcome cohort ID: REPLACE with real cohort definition id for outcome
)

# Export cohort definitions from WebAPI
# generateStats = TRUE will request some statistics for the cohorts from the WebAPI
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = cohortDefinitionIds,
  generateStats = TRUE
)

# Re-number cohorts so internal ids are 1 (target), 2 (comparator), 3 (outcome)
# This renumbering simplifies downstream code because we reference cohorts by
# small integer ids.
if (nrow(cohortDefinitionSet) >= 3) {
  cohortDefinitionSet[cohortDefinitionSet$cohortId == cohortDefinitionIds[1], ]$cohortId <- 1
  cohortDefinitionSet[cohortDefinitionSet$cohortId == cohortDefinitionIds[2], ]$cohortId <- 2
  cohortDefinitionSet[cohortDefinitionSet$cohortId == cohortDefinitionIds[3], ]$cohortId <- 3
} else {
  stop("CohortDefinitionSet does not contain at least three cohort definitions. Check cohortDefinitionIds.")
}

################################################################################
# Negative control outcomes
#
# The Analysis Specifications had null for the negative control concept set id.
# If you have a concept set id for negative controls, replace negativeControlConceptSetId
# with that id. If left NULL, the script will create an empty negative control set.
################################################################################

negativeControlConceptSetId <- NULL # REPLACE with concept set id for negative controls if available

if (!is.null(negativeControlConceptSetId)) {
  negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
    conceptSetId = negativeControlConceptSetId,
    baseUrl = baseUrl
  ) %>%
    ROhdsiWebApi::resolveConceptSet(
      baseUrl = baseUrl
    ) %>%
    ROhdsiWebApi::getConcepts(
      baseUrl = baseUrl
    ) %>%
    rename(outcomeConceptId = "conceptId",
           cohortName = "conceptName") %>%
    mutate(cohortId = row_number() + 100) %>% # negative control cohortIds start at 101
    select(cohortId, cohortName, outcomeConceptId)
} else {
  # No negative controls provided; create an empty tibble with the expected columns.
  negativeControlOutcomeCohortSet <- tibble(
    cohortId = integer(),
    cohortName = character(),
    outcomeConceptId = integer()
  )
}

# Check for duplicate cohort IDs between main cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between main cohorts and negative controls ***")
}

################################################################################
# Build lists used in the CohortMethod module
#
# - oList: outcomes with cleanWindow (prior outcome washout)
# - cmTcList: target/comparator row(s)
# - excludedCovariateConcepts: concepts to exclude from covariate construction
#
# These are populated from the JSON Analysis Specifications. Because many of
# concept ids were NULL in the provided JSON, placeholders or empty lists are
# used. Replace where needed.
################################################################################

# Outcomes: take cohort with internal id == 3 as the primary outcome
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId,
         outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # from createStudyPopArgs: washout for prior outcomes

# Target and Comparator for the CohortMethod analysis
# Use the renumbered cohort ids 1 and 2 (target and comparator)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = cohortDefinitionSet %>% filter(.data$cohortId == 1) %>% pull(.data$cohortName),
  comparatorCohortId = 2,
  comparatorCohortName = cohortDefinitionSet %>% filter(.data$cohortId == 2) %>% pull(.data$cohortName),
  stringsAsFactors = FALSE
)

# Excluded covariate concepts
# The JSON specified conceptsToExclude list with null entries; keep empty unless you
# populate concept ids here. This is used to avoid including the exposures themselves
# as covariates (drug exposures we are comparing).
excludedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character(),
  stringsAsFactors = FALSE
)

################################################################################
# Module: CohortGeneratorModule
#
# Creates shared resource specifications for CohortGenerator (cohorts and
# negative controls). These shared resources are added to the analysis specs.
################################################################################

cgModuleSettingsCreator <- CohortGeneratorModule$new()

cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet = cohortDefinitionSet
)

negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

################################################################################
# Module: CohortDiagnosticsModule
#
# Create diagnostics for the cohorts. Many diagnostics are enabled to help
# explore cohort definitions and characteristics.
################################################################################

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

################################################################################
# Module: CohortMethodModule
#
# Translate the propensity score and outcome model settings from the JSON
# Analysis Specifications into CohortMethod arguments. The JSON specified:
#
# - studyPeriods: 20171201 to 20231231
# - timeAtRisks: single TAR from cohort start to cohort end (0,0)
# - createStudyPopArgs per JSON
# - propensity score configurations: one matching config and one stratification config
# - createPsArgs (Cyclops prior/control params)
# - fitOutcomeModelArgs (cox model, stratified, use covariates)
################################################################################

# Study period from JSON: one row
studyPeriods <- tibble::tibble(
  studyStartDate = as.character(20171201), # format YYYYMMDD as string; empty string means no restriction
  studyEndDate   = as.character(20231231)
)

# Time-at-risk specification from JSON (risk window 0 - 0 anchored to cohort start/end)
timeAtRisks <- tibble::tibble(
  label = c("Main TAR: cohort start to cohort end"),
  riskWindowStart = c(0),
  startAnchor = c("cohort start"),
  riskWindowEnd = c(0),
  endAnchor = c("cohort end")
)

# Build PS configurations: one for matching and one for stratification from JSON
# First: match on PS: maxRatio = 1, caliper = 0.2, caliperScale = "propensity score"
matchOnPsArgsList <- tibble::tibble(
  label = c("match_ps_1to1"),
  maxRatio = c(1),
  caliper = c(0.2),
  caliperScale = c("propensity score")
)

# Second: stratify by PS: 5 strata, baseSelection = "all"
stratifyByPsArgsList <- tibble::tibble(
  label = c("stratify_ps_5"),
  numberOfStrata = c(5),
  baseSelection = c("all")
)

# Build a combined psConfigList that will be iterated when constructing analyses
psConfigList <- list()

# Add match config
if (nrow(matchOnPsArgsList) > 0) {
  for (i in seq_len(nrow(matchOnPsArgsList))) {
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "match",
      label = matchOnPsArgsList$label[i],
      params = list(
        maxRatio = matchOnPsArgsList$maxRatio[i],
        caliper = matchOnPsArgsList$caliper[i],
        caliperScale = matchOnPsArgsList$caliperScale[i]
      )
    )
  }
}

# Add stratify config
if (nrow(stratifyByPsArgsList) > 0) {
  for (i in seq_len(nrow(stratifyByPsArgsList))) {
    psConfigList[[length(psConfigList) + 1]] <- list(
      method = "stratify",
      label = stratifyByPsArgsList$label[i],
      params = list(
        numberOfStrata = stratifyByPsArgsList$numberOfStrata[i],
        baseSelection = stratifyByPsArgsList$baseSelection[i]
      )
    )
  }
}

# Covariate settings: default covariates, but exclude descendants of excluded concepts
# The JSON had empty lists for covariate selection; createDefaultCovariateSettings
# plus adding descendant exclusions if any excludedCovariateConcepts are provided.
covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
  addDescendantsToExclude = TRUE
)

# Prepare outcomes: primary outcomes (oList) + negative controls (if present)
# For each outcome, create a CohortMethod::createOutcome specification.
outcomeList <- c(
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

# Convert to a simple list
outcomeList <- unlist(outcomeList, recursive = FALSE)

# Build targetComparatorOutcomesList: one entry for the single target/comparator pair
# excludedCovariateConceptIds: include any excluded covariate concept ids (empty here)
targetComparatorOutcomesList <- list(
  CohortMethod::createTargetComparatorOutcomes(
    targetId = cmTcList$targetCohortId[1],
    comparatorId = cmTcList$comparatorCohortId[1],
    outcomes = outcomeList,
    excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
  )
)

# Utility: cyclops prior and control objects based on JSON createPsArgs
# JSON: maxCohortSizeForFitting = 250000, errorOnHighCorrelation = FALSE,
# prior = laplace with cross-validation, control includes tolerance, cvType, fold,
# cvRepetitions, noiseLevel = "quiet", resetCoefficients = FALSE, startingVariance = 0.01
createPsPrior <- Cyclops::createPrior(
  priorType = "laplace",
  exclude = c(0),
  useCrossValidation = TRUE
)

createPsControl <- Cyclops::createControl(
  tolerance = 2e-7,
  cvType = "auto",
  seed = 1,
  fold = 10,
  cvRepetitions = 10,
  noiseLevel = "quiet",
  resetCoefficients = FALSE,
  startingVariance = 0.01
)

# Fit outcome model prior/control - from JSON fitOutcomeModelArgs
fitOutcomePrior <- Cyclops::createPrior(
  priorType = "laplace",
  exclude = c(0),
  useCrossValidation = TRUE
)

fitOutcomeControl <- Cyclops::createControl(
  tolerance = 2e-7,
  cvType = "auto",
  seed = 1,
  fold = 10,
  cvRepetitions = 10,
  noiseLevel = "quiet",
  resetCoefficients = TRUE,
  startingVariance = 0.01
)

# Now iterate through study periods, time-at-risks, and PS configs to create
# one CohortMethod analysis per configuration.
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {
    tar <- timeAtRisks[t, ]

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Set matchOnPsArgs or stratifyByPsArgs depending on method
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

      # getDbCohortMethodDataArgs: restrictToCommonPeriod TRUE/ FALSE? The JSON
      # getDbCohortMethodDataArgs had restrictToCommonPeriod true in template and
      # getDbCohortMethodDataArgs in Analysis Specs is not explicit; follow the
      # template approach and use restrictToCommonPeriod = TRUE when studyPeriod
      # boundaries are provided.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs from JSON:
      #   maxCohortSizeForFitting = 250000,
      #   errorOnHighCorrelation = FALSE,
      #   prior = laplace with CV,
      #   control with specified settings
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = FALSE,
        stopOnError = FALSE,
        estimator = "att",
        prior = createPsPrior,
        control = createPsControl
      )

      # Covariate balance args
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )

      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs based on JSON:
      # modelType = "cox", stratified = TRUE, useCovariates = TRUE, inversePtWeighting = FALSE
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = TRUE,
        useCovariates = TRUE,
        inversePtWeighting = FALSE,
        prior = fitOutcomePrior,
        control = fitOutcomeControl
      )

      # createStudyPopArgs - from JSON createStudyPopArgs:
      # restrictToCommonPeriod = FALSE,
      # firstExposureOnly = FALSE,
      # washoutPeriod = 365,
      # removeDuplicateSubjects = "keep all",
      # censorAtNewRiskWindow = FALSE,
      # removeSubjectsWithPriorOutcome = TRUE,
      # priorOutcomeLookback = 99999,
      # time-at-risk from tar row
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 365,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 99999,
        riskWindowStart = tar$riskWindowStart,
        startAnchor = tar$startAnchor,
        riskWindowEnd = tar$riskWindowEnd,
        endAnchor = tar$endAnchor,
        minDaysAtRisk = 1,
        maxDaysAtRisk = 99999
      )

      # Compose a descriptive label for the analysis
      description <- sprintf(
        "Study: %s-%s; TAR: %s; PS: %s",
        studyStartDate,
        studyEndDate,
        tar$label,
        psCfg$label
      )

      # Append the CohortMethod analysis specification to the list
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = description,
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

# Create the CohortMethod module specification
cmModuleSettingsCreator <- CohortMethodModule$new()

cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

################################################################################
# Assemble the full analysis specifications and save to JSON
#
# The Strategus analysis specification contains:
# - Shared resources (cohort definitions, negative controls)
# - Module specifications (CohortGenerator, CohortDiagnostics, CohortMethod)
################################################################################

analysisSpecifications <- Strategus::createEmptyAnalysisSpecificiations() %>%
  Strategus::addSharedResources(cohortDefinitionShared) %>%
  Strategus::addSharedResources(negativeControlsShared) %>%
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Ensure the directory inst/<studyName> exists
outputDir <- file.path("inst", studyName)
if (!dir.exists(outputDir)) {
  dir.create(outputDir, recursive = TRUE)
}

# Save the analysis specification to JSON. The file name follows the pattern:
# inst/<studyName>/<studyName>AnalysisSpecification.json
outputFile <- file.path(outputDir, sprintf("%sAnalysisSpecification.json", studyName))

ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  outputFile
)

message(sprintf("Analysis specification saved to: %s", outputFile))

################################################################################
# End of CreateStrategusAnalysisSpecification.R
#
# Instructions / Notes:
#
# - IMPORTANT: Replace all placeholder cohort definition IDs and concept set IDs
#   used near the top of this script with real IDs from your Atlas/WebAPI.
#
# - If you want to include specific covariates only or to exclude particular
#   covariates beyond the exposures, populate 'excludedCovariateConcepts' and/or
#   modify 'covariateSettings' accordingly.
#
# - This script generates two CohortMethod analyses:
#     1) 1:1 matching on propensity score (caliper = 0.2 on propensity score)
#     2) Stratification on propensity score into 5 strata (baseSelection = "all")
#
# - The PS model uses a Laplace (L1) prior with cross-validation and the Cyclops
#   control settings specified in the Analysis Specifications.
#
# - The outcome model is a Cox model, stratified, using covariates, with a Laplace
#   prior and cross-validation as specified.
#
# - If anything fails in fitting (e.g., inability to fit a penalized model),
#   Strategus will still proceed; review module logs and diagnostics for details.
################################################################################