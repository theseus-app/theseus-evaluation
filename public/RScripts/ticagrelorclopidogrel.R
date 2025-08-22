################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script creates a Strategus analysis specification JSON file that
# encodes the modules and settings to run a comparative cohort study using
# CohortMethod and CohortGenerator modules (OHDSI HADES ecosystem).
#
# NOTE: Several identifiers (cohort definition ids, concept set ids, and any
#       optional included/excluded covariate conceptIds) must be supplied by
#       the user before running the script.  Where a value must be supplied,
#       a clear placeholder and a stop() check are provided so you will be
#       prompted to replace it.
#
# How to use:
# 1) Replace the cohort id placeholders (targetCohortId, comparatorCohortId,
#    outcomeCohortId) with the CohortDefinition ids exported from your Atlas
#    WebAPI instance (see baseUrl).
# 2) If using negative control outcome concept set, replace
#    negativeControlConceptSetId with the concept set id from Atlas.
# 3) Optionally add excluded covariate concept ids (e.g. exposures to exclude
#    from covariates) in excludedCovariateConcepts.
# 4) Run this script to write the analysis specification JSON to
#    inst/<studyName>/<studyName>AnalysisSpecification.json which is the file
#    Strategus expects when executing a study repository.
#
# This script applies the settings provided in the accompanying JSON "Analysis
# Specifications" for the study named "ticagrelorclopidogrel".
#
################################################################################

# Required libraries
library(dplyr)
library(tibble)
library(Strategus)
library(ROhdsiWebApi)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)
library(ParallelLogger)

# ---------- Study name and webapi base URL ------------------------------------
studyName <- "ticagrelorclopidogrel"

# Set your Atlas/WebAPI URL here (change if you use a local or protected API)
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# ---------- Cohort definition ids (REQUIRED: replace placeholders) ------------
# Replace the 0 values with the Atlas cohort definition ids for:
# - targetCohortId: cohortId for ticagrelor (example)
# - comparatorCohortId: cohortId for clopidogrel (example)
# - outcomeCohortId: cohortId for the outcome of interest
#
# Example (replace with real ids):
# targetCohortId <- 12345
# comparatorCohortId <- 23456
# outcomeCohortId <- 34567

targetCohortId <- 0        # <-- REPLACE with your target cohort DefinitionId
comparatorCohortId <- 0    # <-- REPLACE with your comparator cohort DefinitionId
outcomeCohortId <- 0       # <-- REPLACE with your outcome cohort DefinitionId

# Negative control concept set id (if applicable)
# Replace with conceptSetId in Atlas that enumerates negative control outcomes.
negativeControlConceptSetId <- 0  # <-- REPLACE (or leave 0 if none)

# Basic check to ensure user replaced the placeholders
if (targetCohortId == 0 || comparatorCohortId == 0 || outcomeCohortId == 0) {
  stop("Please replace targetCohortId, comparatorCohortId and outcomeCohortId with real cohort definition ids exported from Atlas.")
}

# If you do not want negative controls, set negativeControlConceptSetId to NULL or 0
useNegativeControls <- negativeControlConceptSetId != 0

# ---------- Export cohort definitions from WebAPI -----------------------------
# This retrieves the cohort definitions (JSON and SQL) that will be
# used as shared resources by Strategus modules.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(targetCohortId, comparatorCohortId, outcomeCohortId),
  generateStats = TRUE
)

# Re-number the included cohortIds to a small contiguous set starting at 1
# Strategus workflows often assume target/comparator/outcome cohorts are
# mapped to ids 1, 2, 3 for convenience.
cohortDefinitionSet[cohortDefinitionSet$cohortId == targetCohortId, "cohortId"] <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == comparatorCohortId, "cohortId"] <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == outcomeCohortId, "cohortId"] <- 3

# ---------- Negative control outcome cohort set (optional) --------------------
# If a concept set id is provided, resolve it to constituent concepts and
# convert into a negative control cohort set that the CohortMethod module can use.
if (useNegativeControls) {
  negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
    conceptSetId = negativeControlConceptSetId,
    baseUrl = baseUrl
  ) %>%
    ROhdsiWebApi::resolveConceptSet(baseUrl = baseUrl) %>%
    ROhdsiWebApi::getConcepts(baseUrl = baseUrl) %>%
    rename(outcomeConceptId = "conceptId", cohortName = "conceptName") %>%
    mutate(cohortId = row_number() + 100) %>% # negative controls get ids starting at 101
    select(cohortId, cohortName, outcomeConceptId)
} else {
  negativeControlOutcomeCohortSet <- tibble::tibble(
    cohortId = integer(),
    cohortName = character(),
    outcomeConceptId = integer()
  )
}

# Check for duplicate cohortId values between cohortDefinitionSet and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between core cohorts and negative controls ***")
}

# ---------- Define outcomes list and cohort pairs -----------------------------
# Outcome list (the primary outcome(s) you added to cohortDefinitionSet)
# Here we prepare the outcomes data.frame expected downstream.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # cleanWindow defines prior outcome exclusion window

# Target and Comparator list for CohortMethod
cmTcList <- tibble::tibble(
  targetCohortId = 1,
  targetCohortName = cohortDefinitionSet %>% filter(.data$cohortId == 1) %>% pull(.data$cohortName),
  comparatorCohortId = 2,
  comparatorCohortName = cohortDefinitionSet %>% filter(.data$cohortId == 2) %>% pull(.data$cohortName)
)

# ---------- Excluded / included covariate concepts (optional) -----------------
# If you wish to exclude the drug concepts under study from the covariate set
# specify them here. Replace the example conceptIds with real conceptIds.
# The JSON provided had placeholders; by default we keep this empty.
excludedCovariateConcepts <- tibble::tibble(
  conceptId = integer(),
  conceptName = character()
)

# If you wish to define included covariates only, create includedCovariateConcepts
# includedCovariateConcepts <- tibble::tibble(conceptId = c(...), conceptName = c(...))

# ---------- CohortGeneratorModule shared resources --------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Negative controls as shared resources for CohortGenerator
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",          # use the first occurrence per subject
  detectOnDescendants = TRUE         # include descendant concepts when resolving concepts
)

cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# ---------- CohortDiagnosticsModule settings ----------------------------------
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

# ---------- CohortMethod module: STUDY PERIODS and TIME-AT-RISKS ---------------
# Study periods: as provided in Analysis Specifications JSON
# Format is YYYYMMDD strings; these will be passed to createGetDbCohortMethodDataArgs
studyPeriods <- tibble::tibble(
  studyStartDate = c("20111101", "20130301"),
  studyEndDate   = c("20190331", "20161231")
)

# Time-at-risks (TARs) as specified in Analysis Specifications JSON:
# 1) 0 to 365 days from cohort start
# 2) 0 to 1825 days (5 years) from cohort start
# 3) 1 day after cohort start to cohort end (exposure-anchored)
timeAtRisks <- tibble::tibble(
  label = c("0_to_365_from_index", "0_to_1825_from_index", "1_to_end_of_exposure"),
  riskWindowStart = c(0, 0, 1),
  startAnchor = c("cohort start", "cohort start", "cohort start"),
  riskWindowEnd = c(365, 1825, 0),
  endAnchor = c("cohort start", "cohort start", "cohort end"),
  minDaysAtRisk = c(1, 1, 1)
)

# ---------- Propensity score adjustment settings ------------------------------
# We will create three PS configs as in the JSON:
# 1) match: maxRatio 1, caliper 0.2 (standardized logit)
# 2) match: maxRatio 10, caliper 0.2 (standardized logit)
# 3) stratify: numberOfStrata 10, baseSelection "all"

matchOnPsArgsList <- tibble::tibble(
  label = c("1_to_1_match", "1_to_10_match"),
  maxRatio = c(1L, 10L),
  caliper = c(0.2, 0.2),
  caliperScale = c("standardized logit", "standardized logit")
)

stratifyByPsArgsList <- tibble::tibble(
  label = c("stratify_10_all"),
  numberOfStrata = c(10L),
  baseSelection = c("all") # "all" means strata are defined across both groups
)

# Build PS configuration list for iterating analysis combinations
psConfigList <- list()
if (exists("matchOnPsArgsList") && nrow(matchOnPsArgsList) > 0) {
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
if (exists("stratifyByPsArgsList") && nrow(stratifyByPsArgsList) > 0) {
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

# ---------- CohortMethod analysis list construction ---------------------------
cmAnalysisList <- list()
analysisId <- 1

# Loop through study periods, time-at-risks, and PS configurations to create
# a CohortMethod analysis per combination.
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Determine whether this PS config is a match or a stratify configuration,
      # and create appropriate CohortMethod args.
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
        stop("Unknown PS method encountered while building configurations.")
      }

      # Covariate settings - use default covariates but exclude any specified drug
      # concepts (excludedCovariateConcepts). If you want to include only specific
      # covariates, define includedCovariateConcepts and adjust the covariateSettings accordingly.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build the outcome objects for CohortMethod:
      # - primary (true) outcomes come from the oList
      # - negative control outcomes (if provided) are appended with trueEffectSize = 1
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

      # Build the targetComparatorOutcomesList. Note: If you have multiple
      # target-comparator pairs, extend cmTcList accordingly.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Provide excluded covariate concept ids (target/comparator concept ids
          # plus any additional excludedCovariateConcepts)
          excludedCovariateConceptIds = c(
            excludedCovariateConcepts$conceptId
          )
        )
      }

      # getDbCohortMethodDataArgs controls retrieval from the CDM and includes
      # the study period to restrict to for this analysis.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0, # 0 means no maximum; this matches the Analysis Specifications JSON
        covariateSettings = covariateSettings
      )

      # createPsArgs: settings for fitting the propensity score model. This
      # uses Cyclops regularized logistic regression with a Laplace prior and
      # cross-validation for penalty selection.
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # allow Strategus to continue even if a PS fit fails for a pair
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",
          exclude = c(0),
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          tolerance = 2e-07,
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "silent",
          resetCoefficients = TRUE,
          startingVariance = 0.01,
          seed = 1
        )
      )

      # Covariate balance / diagnostics args
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )

      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs: settings for outcome model estimation (Cox model)
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = FALSE,               # per Analysis Specifications JSON
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
          startingVariance = 0.01,
          seed = 1
        )
      )

      # Create study population args as specified in the JSON analysis spec:
      # - restrictToCommonPeriod: FALSE
      # - firstExposureOnly: FALSE
      # - washoutPeriod: 365
      # - removeDuplicateSubjects: "keep all"
      # - censorAtNewRiskWindow: FALSE
      # - removeSubjectsWithPriorOutcome: TRUE
      # - priorOutcomeLookback: 99999
      # - time-at-risk parameters from timeAtRisks table
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
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        maxDaysAtRisk = 99999
      )

      # Append the CohortMethod analysis configuration to the list
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "StudyPeriod: %s-%s; TAR: %s; PS: %s",
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

# ---------- CohortMethod module specifications --------------------------------
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# ---------- Assemble analysis specifications -----------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecificiations() %>%
  Strategus::addSharedResources(cohortDefinitionShared) %>%
  Strategus::addSharedResources(negativeControlsShared) %>%
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# ---------- Save specification to JSON ----------------------------------------
# The JSON will be written to inst/<studyName>/<studyName>AnalysisSpecification.json
outputDir <- file.path("inst", studyName)
if (!dir.exists(outputDir)) dir.create(outputDir, recursive = TRUE)

outputFile <- file.path(outputDir, paste0(studyName, "AnalysisSpecification.json"))

# ParallelLogger::saveSettingsToJson will write the R object to disk in a
# JSON format that Strategus can read when executing the study.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  outputFile
)

message("Analysis specification saved to: ", outputFile)
message("Please review and, if necessary, update cohort/concept ids and covariate exclusions before running Strategus.")