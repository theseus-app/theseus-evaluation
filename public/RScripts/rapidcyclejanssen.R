################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script creates a Strategus analysis specification for the
# "rapidcyclejanssen" study. It follows the structure expected by the
# OHDSI Strategus package and HADES CohortMethod/CohortGenerator modules.
#
# IMPORTANT:
# - Many places in this script contain placeholders (IDs, names, concept ids).
#   You MUST replace those placeholders with the actual values for your study
#   (cohort IDs from ATLAS, concept set IDs, concept ids to include/exclude, etc.)
# - Detailed comments are included to explain how each setting from the
#   Analysis Specifications JSON is applied.
#
################################################################################

library(dplyr)
library(Strategus)
library(ROhdsiWebApi)      # for cohort / concept set export
library(CohortMethod)      # for creating CM argument objects
library(FeatureExtraction) # covariate settings helpers
library(Cyclops)           # prior/control creation for PS / outcome model
library(ParallelLogger)    # to save the final JSON file

# ----------------------------
# Study-level metadata / URLs
# ----------------------------
studyName <- "rapidcyclejanssen"

# Base URL for ATLAS WebAPI - replace with your Atlas instance if needed
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# ----------------------------
# Cohort definitions
# ----------------------------
# Export cohort definitions from ATLAS / WebAPI.
# Replace the cohortIds vector below with the actual cohort definition IDs
# in your Atlas instance for:
#   - target cohort (will be re-numbered to 1)
#   - comparator cohort (will be re-numbered to 2)
#   - outcome cohort(s) (will be re-numbered starting at 3)
#
# Example placeholders below MUST be replaced:
atlasCohortIds <- c(
  1111111, # <-- REPLACE: ATLAS cohortDefinitionId for the target cohort
  2222222, # <-- REPLACE: ATLAS cohortDefinitionId for the comparator cohort
  3333333  # <-- REPLACE: ATLAS cohortDefinitionId for the outcome cohort
)

cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = atlasCohortIds,
  generateStats = TRUE
)

# Re-number cohorts to a compact set used by the modules:
# target -> 1, comparator -> 2, outcome(s) -> 3, 4, ...
# We assume the export order above corresponds to target, comparator, outcomes.
cohortDefinitionSet$cohortId <- seq_len(nrow(cohortDefinitionSet))

# ----------------------------
# Negative control outcomes
# ----------------------------
# If you have a concept set in Atlas that contains negative control concepts,
# provide its conceptSet ID below. If you do not have negative controls,
# leave conceptSetId <- NULL and negativeControlsShared will not be created.
negativeControlConceptSetId <- NULL # <-- REPLACE: conceptSetId for negative controls or NULL

negativeControlOutcomeCohortSet <- NULL
if (!is.null(negativeControlConceptSetId)) {
  # Export, resolve, and convert the concept set into a cohort-like table
  negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
    conceptSetId = negativeControlConceptSetId,
    baseUrl = baseUrl
  ) %>%
    ROhdsiWebApi::resolveConceptSet(baseUrl = baseUrl) %>%
    ROhdsiWebApi::getConcepts(baseUrl = baseUrl) %>%
    rename(
      outcomeConceptId = "conceptId",
      cohortName = "conceptName"
    ) %>%
    mutate(cohortId = row_number() + 100) %>% # offset to avoid collisions
    select(cohortId, cohortName, outcomeConceptId)
}

# Safety check for duplicate cohort IDs
if (!is.null(negativeControlOutcomeCohortSet)) {
  if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
    stop("*** Error: duplicate cohort IDs found between cohortDefinitionSet and negativeControlOutcomeCohortSet ***")
  }
}

# ----------------------------
# Build cohort lists for CM
# ----------------------------
# Outcomes (from cohortDefinitionSet): we assume the third entry is the primary outcome.
# If you have multiple outcome cohorts exported, adjust filtering accordingly.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId >= 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # This maps to priorOutcomeLookback behavior in the template

# Target / Comparator mapping used for CohortMethod module
# We re-numbered such that target = 1 and comparator = 2 (based on export order above)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "TARGET_COHORT_NAME",      # <-- REPLACE: human-readable name
  comparatorCohortId = 2,
  comparatorCohortName = "COMPARATOR_COHORT_NAME", # <-- REPLACE
  targetConceptId = NA_integer_,   # <-- OPTIONAL: concept id for excluded covariates related to target
  comparatorConceptId = NA_integer_ # <-- OPTIONAL: concept id for excluded covariates related to comparator
)

# Covariates to explicitly exclude from LSPS (e.g., drug ingredients representing exposures).
# Replace with real concept IDs if you need to exclude them.
excludedCovariateConcepts <- data.frame(
  conceptId = c(NA_integer_),   # <-- REPLACE with concept ids to exclude; can be vector()
  conceptName = c(NA_character_)
)

# If you prefer to include only a limited set of covariates, define includedCovariateConcepts similarly:
# includedCovariateConcepts <- data.frame(conceptId = c(...), conceptName = c(...))

# ----------------------------
# CohortGenerator Module - shared resources & specifications
# ----------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource: cohorts (generated from cohortDefinitionSet)
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Shared resource: negative controls (only if provided)
negativeControlsShared <- NULL
if (!is.null(negativeControlOutcomeCohortSet)) {
  negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
    negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
    occurrenceType = "first",
    detectOnDescendants = TRUE
  )
}

# Module specifications for cohort generation
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# ----------------------------
# CohortDiagnostics Module - basic configuration
# ----------------------------
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

# ----------------------------
# CohortMethod Module configuration
# ----------------------------

# getDbCohortMethodDataArgs -> study periods and maxCohortSize
# Per Analysis Specifications: studyPeriods contains one entry with studyStartDate=20210101
studyPeriods <- tibble::tibble(
  studyStartDate = c(20210101),
  studyEndDate   = c(NA_integer_) # NA means open-ended / no explicit end date
)

# Time-at-risks (TARs) — per Analysis Specifications we have five TARs
timeAtRisks <- tibble::tibble(
  label = c("1-14", "1-28", "1-42", "1-90", "0-2"),
  riskWindowStart  = c(1, 1, 1, 1, 0),
  startAnchor = c("cohort start", "cohort start", "cohort start", "cohort start", "cohort start"),
  riskWindowEnd  = c(14, 28, 42, 90, 2),
  endAnchor = c("cohort start", "cohort start", "cohort start", "cohort start", "cohort start")
)

# Propensity Score adjustment configuration
# Analysis Specifications define one PS setting: match on PS with maxRatio=0, caliper=0.2, caliperScale='propensity score'
matchOnPsArgsList <- tibble::tibble(
  label = c("match_ps"),
  maxRatio  = c(0),          # 0 per specification; interpret as needed (replace if you want standard 1:1)
  caliper = c(0.2),
  caliperScale  = c("propensity score")
)

# No stratifyByPs settings in the JSON
stratifyByPsArgsList <- tibble::tibble(
  label = character(0),
  numberOfStrata = integer(0),
  baseSelection = character(0)
)

# Convert match / stratify dataframes into a single list of PS configurations
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

# ----------------------------
# Build CohortMethod analyses by iterating combinations
# ----------------------------
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create the PS adjustment argument objects depending on method
      if (psCfg$method == "match") {
        # createMatchOnPsArgs parameters:
        #   maxRatio: maximum matching ratio (0 per specification - user may wish to change)
        #   caliper: matching caliper
        #   caliperScale: "propensity score" | "standardized" | "standardized logit"
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
        stop("Unknown PS method in configuration")
      }

      # Covariate settings:
      # The analysis JSON contains an empty list for conceptsToInclude / exclude,
      # so default covariates will be used. If you want to include/exclude specific
      # concepts, build covariateSettings via createCovariateSettingsFromConceptSet
      # or create custom covariateSettings here.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build the outcome list for CohortMethod.
      # Include primary outcomes followed by negative controls (if provided).
      outcomeList <- c(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999 # per Analysis Specifications
          )
        }),
        if (!is.null(negativeControlOutcomeCohortSet)) {
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

      # Build targetComparatorOutcomesList (one element per target-comparator pair)
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        # excludedCovariateConceptIds: combine supplied target/comparator concept ids and study-level exclusions
        excludedIds <- c()
        if (!is.na(cmTcList$targetConceptId[i])) excludedIds <- c(excludedIds, cmTcList$targetConceptId[i])
        if (!is.na(cmTcList$comparatorConceptId[i])) excludedIds <- c(excludedIds, cmTcList$comparatorConceptId[i])
        if (nrow(excludedCovariateConcepts) > 0 && !all(is.na(excludedCovariateConcepts$conceptId))) {
          excludedIds <- unique(c(excludedIds, excludedCovariateConcepts$conceptId[!is.na(excludedCovariateConcepts$conceptId)]))
        }

        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedIds
        )
      }

      # getDbCohortMethodDataArgs: how to fetch cohort-method data from CDM
      # Analysis Specifications provided: maxCohortSize = 0 (no limit)
      # and studyPeriods defined above.
      # We set restrictToCommonPeriod = TRUE to ensure person-time in common observation period,
      # but this can be changed if you prefer.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs: create PS model fitting arguments
      # Per Analysis Specifications:
      # - maxCohortSizeForFitting = 250000
      # - errorOnHighCorrelation = TRUE
      # - prior: laplace with useCrossValidation = TRUE
      # - control: tolerance 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10,
      #            noiseLevel = "silent", resetCoefficients = TRUE, startingVariance = 0.01
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # allow Strategus to continue even if model fitting fails in some comparisons
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

      # Compute covariate balance args (shared and specific)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs per Analysis Specifications
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = FALSE,            # JSON specified stratified = false
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

      # createStudyPopArgs: mapping time-at-risk and study population filters
      # Per Analysis Specifications:
      # - restrictToCommonPeriod = FALSE
      # - firstExposureOnly = TRUE
      # - washoutPeriod = 365
      # - removeDuplicateSubjects = "remove all"
      # - censorAtNewRiskWindow = FALSE
      # - removeSubjectsWithPriorOutcome = TRUE
      # - priorOutcomeLookBack = 99999
      # - timeAtRisks: uses timeAtRisks[t,]
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = TRUE,
        washoutPeriod = 365,
        removeDuplicateSubjects = "remove all", # per specification
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

      # Compose a descriptive label for this analysis configuration
      description <- sprintf(
        "Study: %s-%s; TAR: %s; PS: %s",
        ifelse(is.na(studyStartDate), "", studyStartDate),
        ifelse(is.na(studyEndDate), "", studyEndDate),
        timeAtRisks$label[t],
        psCfg$label
      )

      # Append the analysis to the list
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
    } # end ps loop
  } # end TAR loop
} # end study periods loop

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

# ----------------------------
# Combine into analysis specifications and save
# ----------------------------
# Create an empty analysis specifications object and add resources + modules.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() %>%
  Strategus::addSharedResources(cohortDefinitionShared)

if (!is.null(negativeControlsShared)) {
  analysisSpecifications <- analysisSpecifications %>%
    Strategus::addSharedResources(negativeControlsShared)
}

analysisSpecifications <- analysisSpecifications %>%
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Create output directory and save to JSON
outputFile <- file.path("inst", studyName, paste0(studyName, "AnalysisSpecification.json"))
dir.create(dirname(outputFile), recursive = TRUE, showWarnings = FALSE)

ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  outputFile
)

message("Strategus analysis specification saved to: ", outputFile)
# End of script.