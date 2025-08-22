#!/usr/bin/env Rscript

################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script creates a Strategus analysis specification JSON for the study
# defined in the Analysis Specifications block (study name: "corazon").
#
# IMPORTANT:
# - Several cohort ids / concept set ids are intentionally left as NULL here.
#   You must populate those variables (targetCohortId, comparatorCohortId,
#   outcomeCohortIds, negativeControlConceptSetId, etc.) before running this
#   script against a real Atlas/WebAPI instance.
# - Extensive inline comments explain how the analysis specification is built
#   from the provided settings in the Analysis Specifications object.
#
# Dependencies:
# install.packages("devtools")
# devtools::install_github("OHDSI/Strategus")
# devtools::install_github("OHDSI/ROhdsiWebApi")
# devtools::install_github("OHDSI/CohortMethod")
# devtools::install_github("OHDSI/FeatureExtraction")
# devtools::install_github("OHDSI/Cyclops")
#
################################################################################

library(dplyr)
library(Strategus)
library(ROhdsiWebApi)     # used to export cohort definitions and concept sets
library(CohortMethod)     # for creating CohortMethod argument objects
library(FeatureExtraction)
library(Cyclops)
library(ParallelLogger)

# ---------------------------
# Study-level settings
# ---------------------------

# Study name (used for file names and directories)
studyName <- "corazon"

# Base WebAPI URL to export cohort definitions / concept sets.
# Change to your Atlas/WebAPI endpoint.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# ---------------------------
# Cohort IDs (USER MUST SET)
# ---------------------------
# The Analysis Specifications provided in the input contained null cohort ids.
# The script below expects you to replace the NULLs with real cohort IDs from
# your Atlas/WebAPI instance.

# Target cohort id from Atlas (integer). Set to an Atlas cohort definition id.
targetCohortId <- NULL   # e.g., 1234

# Comparator cohort id from Atlas (integer). Set to an Atlas cohort definition id.
comparatorCohortId <- NULL   # e.g., 5678

# Outcome cohort ids from Atlas (vector of integers). One or more outcomes.
outcomeCohortIds <- c()  # e.g., c(9012)

# Negative control concept set id (Atlas concept set id) if available.
# If left NULL, negative controls will be omitted.
negativeControlConceptSetId <- NULL  # e.g., 34567

# Validate that user has provided the minimal required cohorts
if (is.null(targetCohortId) || is.null(comparatorCohortId) || length(outcomeCohortIds) == 0) {
  stop(
    "ERROR: Please set targetCohortId, comparatorCohortId, and outcomeCohortIds in this script\n",
    "They are currently NULL or empty. See script comments for instructions."
  )
}

# ---------------------------
# Covariate selection - optional
# ---------------------------
# The Analysis Specifications included placeholders for conceptsToInclude and
# conceptsToExclude. If you want to explicitly include or exclude covariates,
# populate these data frames with columns: conceptId (integer) and conceptName (string).
#
# For example:
# conceptsToInclude <- data.frame(conceptId = c(1111,2222), conceptName = c("A","B"), stringsAsFactors = FALSE)
# conceptsToExclude <- data.frame(conceptId = c(3333), conceptName = c("C"), stringsAsFactors = FALSE)

conceptsToInclude <- data.frame()  # leave empty to use default covariate set
conceptsToExclude <- data.frame()  # leave empty if no explicit exclusions

# Build excluded covariate concepts data.frame (used later in excludedCovariateConceptIds)
if (nrow(conceptsToExclude) > 0 && all(c("conceptId", "conceptName") %in% names(conceptsToExclude))) {
  excludedCovariateConcepts <- conceptsToExclude %>% select(conceptId, conceptName)
} else {
  excludedCovariateConcepts <- data.frame(conceptId = integer(), conceptName = character())
}

# ---------------------------
# Export cohort definitions from Atlas/WebAPI
# ---------------------------
# Export the target, comparator and outcome cohort definitions at once.
cohortIdsToExport <- c(targetCohortId, comparatorCohortId, outcomeCohortIds)

cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = cohortIdsToExport,
  generateStats = TRUE
)

# Re-map cohortIds in the exported set to sequential small integers used by Strategus:
# - target -> 1
# - comparator -> 2
# - outcomes -> 3,4,5...
#
# This renumbering is convenient for the downstream CohortMethod module where we
# refer to cohort ids starting at 1 / 2 / 3...
cohortDefinitionSet <- cohortDefinitionSet %>%
  mutate(originalCohortId = .data$cohortId)

# Replace cohort ids: map target->1, comparator->2, outcomes incrementing from 3
cohortDefinitionSet[cohortDefinitionSet$originalCohortId == targetCohortId, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$originalCohortId == comparatorCohortId, ]$cohortId <- 2

# For outcomes, start numbering at 3
outcomeStartId <- 3
for (i in seq_along(outcomeCohortIds)) {
  originalId <- outcomeCohortIds[i]
  cohortDefinitionSet[cohortDefinitionSet$originalCohortId == originalId, ]$cohortId <- outcomeStartId + (i - 1)
}

# If any cohorts not present in the exported set, throw an error to alert user
expectedOriginalIds <- cohortIdsToExport
exportedOriginalIds <- unique(cohortDefinitionSet$originalCohortId)
missingIds <- setdiff(expectedOriginalIds, exportedOriginalIds)
if (length(missingIds) > 0) {
  stop(sprintf("ERROR: The following cohort IDs failed to export from WebAPI: %s", paste(missingIds, collapse = ", ")))
}

# ---------------------------
# Negative control outcomes (optional)
# ---------------------------
# If a negative control concept set id has been provided, retrieve it from WebAPI,
# resolve to concepts and map them to cohort ids that start after the study cohorts.
negativeControlOutcomeCohortSet <- NULL
if (!is.null(negativeControlConceptSetId)) {
  negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
    conceptSetId = negativeControlConceptSetId,
    baseUrl = baseUrl
  ) %>%
    ROhdsiWebApi::resolveConceptSet(baseUrl = baseUrl) %>%
    ROhdsiWebApi::getConcepts(baseUrl = baseUrl) %>%
    dplyr::rename(outcomeConceptId = "conceptId",
                  cohortName = "conceptName") %>%
    dplyr::mutate(
      # assign cohortId for negative controls so they do not clash with target/comparator/outcomes
      cohortId = dplyr::row_number() + 100L
    ) %>%
    dplyr::select(cohortId, cohortName, outcomeConceptId)
}

# Check for duplicate cohort IDs between cohortDefinitionSet and negative controls
if (!is.null(negativeControlOutcomeCohortSet)) {
  dupCheck <- intersect(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)
  if (length(dupCheck) > 0) {
    stop("*** Error: duplicate cohort IDs found between study cohorts and negative controls ***")
  }
}

# ---------------------------
# Build lists used by CohortMethod analyses
# ---------------------------

# Outcomes: create oList from exported cohortDefinitionSet
# We identify outcome cohorts in our renumbered cohortDefinitionSet by selecting
# the cohortIds that correspond to the outcomes (starting at outcomeStartId).
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId >= outcomeStartId) %>%
  mutate(outcomeCohortId = .data$cohortId,
         outcomeCohortName = .data$cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)  # cleanWindow used for CohortMethod diagnostics (example default)

# Target and Comparator mapping for CohortMethod analyses
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = cohortDefinitionSet %>% filter(.data$cohortId == 1) %>% pull(.data$cohortName),
  comparatorCohortId = 2,
  comparatorCohortName = cohortDefinitionSet %>% filter(.data$cohortId == 2) %>% pull(.data$cohortName),
  stringsAsFactors = FALSE
)

# If you want to exclude the target/comparator drug concept ids from covariates,
# you can provide them here. We will also append any additional excluded covariates
# provided by conceptsToExclude above.
# Example:
# cmTcList$targetConceptId <- 11111
# cmTcList$comparatorConceptId <- 22222
# For now, leave undefined (NA) and only use `excludedCovariateConcepts`.
if (!("targetConceptId" %in% names(cmTcList))) cmTcList$targetConceptId <- NA_integer_
if (!("comparatorConceptId" %in% names(cmTcList))) cmTcList$comparatorConceptId <- NA_integer_

# ---------------------------
# CohortGenerator Module - Shared resources & module specification
# ---------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet = cohortDefinitionSet
)

negativeControlsShared <- NULL
if (!is.null(negativeControlOutcomeCohortSet) && nrow(negativeControlOutcomeCohortSet) > 0) {
  negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
    negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
    occurrenceType = "first",
    detectOnDescendants = TRUE
  )
}

cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# ---------------------------
# CohortDiagnostics Module
# ---------------------------
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

# ---------------------------
# CohortMethod Module: Study periods, TARs, PS configurations, and analyses
# ---------------------------

# The input Analysis Specifications provided two study periods:
# 1) 20100101 - 20191231
# 2) 20120101 - 20191231
studyPeriods <- tibble::tibble(
  studyStartDate = c("20100101", "20120101"),
  studyEndDate   = c("20191231", "20191231")
)

# Time-at-risks (TARs) - the JSON specified two TARs. We set helpful labels:
# - TAR 1: riskWindowStart=0 anchored to cohort start; riskWindowEnd=0 anchored to cohort end
#          This is commonly the "on-treatment" or "during exposure" window if cohorts represent exposures.
# - TAR 2: riskWindowStart=0 anchored to cohort start; riskWindowEnd=9999 anchored to cohort start
#          This is commonly an "intention-to-treat" long follow-up window.
timeAtRisks <- tibble::tibble(
  label = c("on_treatment", "intent_to_treat"),
  riskWindowStart  = c(0L, 0L),
  startAnchor = c("cohort start", "cohort start"),
  riskWindowEnd  = c(0L, 9999L),
  endAnchor = c("cohort end", "cohort start"),
  minDaysAtRisk = c(1L, 1L)
)

# Propensity Score settings:
# The JSON specified two PS configurations:
# 1) stratify by PS with numberOfStrata = 5, baseSelection = "all"
# 2) match on PS with maxRatio = 0, caliper = 0.2, caliperScale = "standardized logit"
#
# We will convert these to the two tibbles used by the template: stratifyByPsArgsList and matchOnPsArgsList.
stratifyByPsArgsList <- tibble::tibble(
  label = c("stratify_5_all"),
  numberOfStrata = c(5L),
  baseSelection = c("all")
)

matchOnPsArgsList <- tibble::tibble(
  label = c("match_caliper0.2"),
  maxRatio = c(0L),  # 0 used in input JSON; keep as provided (depending on CohortMethod semantics this may be interpreted specially)
  caliper = c(0.2),
  caliperScale = c("standardized logit")
)

# Build a list of PS configurations (psConfigList) combining match and stratify configs.
psConfigList <- list()

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

# ---------------------------
# Assemble CohortMethod analyses
# ---------------------------
cmAnalysisList <- list()
analysisId <- 1L

# Loop over study periods, TARs and PS configurations to define the analyses
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create matchOnPsArgs or stratifyByPsArgs depending on the method
      if (psCfg$method == "match") {
        # Create a match-on-PS argument object using CohortMethod helper
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
        stop(sprintf("Unknown PS method: %s", psCfg$method))
      }

      # Covariate settings: we default to the standard covariate set and, if the
      # user supplied explicit 'conceptsToInclude', more advanced custom
      # FeatureExtraction specifications would be required. For most studies the
      # default covariate settings are appropriate.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build the outcome list for CohortMethod analyses.
      # - The 'oList' contains the study outcomes (renumbered to 3, 4, ...).
      # - Negative controls (if present) are appended and flagged as 'outcomeOfInterest = FALSE'
      outcomeList <- c(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999
          )
        }),
        if (!is.null(negativeControlOutcomeCohortSet) && nrow(negativeControlOutcomeCohortSet) > 0) {
          lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
            CohortMethod::createOutcome(
              outcomeId = i,
              outcomeOfInterest = FALSE,
              trueEffectSize = 1
            )
          })
        } else {
          list()
        }
      )

      # Create target-comparator-outcomes mapping for CohortMethod.
      # This object lists the target/comparator pair and the outcomes (and a set of
      # excluded covariate concept ids).
      excludedIdsForTc <- c()
      if (nrow(excludedCovariateConcepts) > 0) {
        excludedIdsForTc <- excludedCovariateConcepts$conceptId
      }
      # also append any target/comparator-specific concept ids if provided in cmTcList
      if (!is.na(cmTcList$targetConceptId[1])) excludedIdsForTc <- c(excludedIdsForTc, cmTcList$targetConceptId)
      if (!is.na(cmTcList$comparatorConceptId[1])) excludedIdsForTc <- c(excludedIdsForTc, cmTcList$comparatorConceptId)

      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = unique(excludedIdsForTc)
        )
      }

      # getDbCohortMethodDataArgs: set study window and cohort-size limits for data extraction
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # from Analysis Specifications: 0 means no cap
        covariateSettings = covariateSettings
      )

      # Create PS fitting arguments based on the JSON-provided createPsArgs
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # allow Strategus to keep running even if a PS model fails for a pair
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",
          exclude = c(0),
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          tolerance = 2e-7,
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "silent",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # Covariate balance computations
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Fit outcome model args per JSON: Cox, stratified = TRUE, no covariates (i.e. use PS-adjustment)
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = TRUE,
        useCovariates = FALSE,
        inversePtWeighting = FALSE,
        prior = Cyclops::createPrior(
          priorType = "laplace",
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          tolerance = 2e-7,
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "quiet",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # createStudyPopArgs: reflect JSON values (two TARs are iterated above)
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",   # from Analysis Specifications
        censorAtNewRiskWindow = FALSE,          # from Analysis Specifications
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 99999,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        maxDaysAtRisk = 99999
      )

      # Append the settings to the CM analysis list
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

      analysisId <- analysisId + 1L
    } # end psConfigList loop
  } # end timeAtRisks loop
} # end studyPeriods loop

# CohortMethod module specifications
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# ---------------------------
# Create the final Strategus analysisSpecifications object
# ---------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecificiations() %>%
  Strategus::addSharedResources(cohortDefinitionShared)

if (!is.null(negativeControlsShared)) {
  analysisSpecifications <- analysisSpecifications %>% Strategus::addSharedResources(negativeControlsShared)
}

analysisSpecifications <- analysisSpecifications %>%
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# ---------------------------
# Save to JSON file
# ---------------------------
outputDir <- file.path("inst", studyName)
if (!dir.exists(outputDir)) dir.create(outputDir, recursive = TRUE)

outputFile <- file.path(outputDir, sprintf("%sAnalysisSpecification.json", studyName))
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  outputFile
)

message(sprintf("Analysis specification JSON saved to: %s", outputFile))
message("Review the JSON and ensure all placeholder cohort/concept-set ids were replaced with real ids before executing the Strategus study.")