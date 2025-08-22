################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script creates a Strategus analysis specification JSON file for the
# "ceeamos" study. It uses the OHDSI Strategus framework and HADES modules
# to define cohort generation, cohort diagnostics and CohortMethod analysis
# settings based on the study-level settings provided in the Analysis
# Specifications JSON.
#
# IMPORTANT:
# - Many IDs are set as placeholders below (cohort IDs, concept set IDs,
#   covariate concept IDs). You MUST replace these placeholders with the
#   actual values used in your Atlas/WebAPI environment before running.
# - The script is heavily annotated to explain how each setting in the
#   Analysis Specifications is translated into Strategus/CohortMethod calls.
################################################################################

# Load required packages
library(dplyr)
library(Strategus)
library(ROhdsiWebApi)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)
library(ParallelLogger)

# ------------------------------------------------------------------------------
# Shared resources and cohort definitions
# ------------------------------------------------------------------------------
# Base WebAPI URL (Atlas instance). Update if using a different ATLAS/WebAPI.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# ------------------------------------------------------------------------------
# COHORT DEFINITIONS
# ------------------------------------------------------------------------------
# The Analysis Specifications included NULL ids for the cohorts. Replace the
# placeholder cohort IDs below with the actual cohort definition IDs exported
# / created in your Atlas instance.
#
# Order: target cohort, comparator cohort, outcome cohort(s)
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1000001, # <-- REPLACE: Target cohort definition ID (Atlas cohort id)
    1000002, # <-- REPLACE: Comparator cohort definition ID (Atlas cohort id)
    1000003  # <-- REPLACE: Outcome cohort definition ID (Atlas cohort id)
  ),
  generateStats = TRUE
)

# For convenience, re-number the exported cohort IDs so that:
#  - target = 1
#  - comparator = 2
#  - outcome(s) = 3, 4, ...
# This makes downstream references simpler (the cohortGenerator/CM templates
# commonly expect small sequential IDs).
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1000001, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1000002, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1000003, ]$cohortId <- 3

# ------------------------------------------------------------------------------
# NEGATIVE CONTROL OUTCOMES
# ------------------------------------------------------------------------------
# The Analysis Specifications had a negativeControlConceptSet with a NULL id.
# Replace the placeholder conceptSetId with the ID of the concept set in Atlas
# that contains the negative control outcome concepts.
negControlConceptSetId <- 9999999 # <-- REPLACE with your conceptSetId

negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = negControlConceptSetId,
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(baseUrl = baseUrl) %>%
  ROhdsiWebApi::getConcepts(baseUrl = baseUrl) %>%
  # rename to match the expected columns for negative control cohort set
  rename(outcomeConceptId = "conceptId",
         cohortName = "conceptName") %>%
  # assign cohortIds for negative controls that do not clash with study cohorts;
  # e.g. start at 101 to avoid overlap with 1,2,3...
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

# Validate there are no duplicate cohort IDs across included cohorts
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between cohortDefinitionSet and negativeControlOutcomeCohortSet. Fix cohort ids before proceeding. ***")
}

# ------------------------------------------------------------------------------
# Build small helper tables describing targets/comparators and outcomes
# ------------------------------------------------------------------------------
# Outcomes: use the re-numbered cohort id for the outcome(s) exported above.
# The Analysis Specifications included a single outcome cohort with NULL id.
# We assume the third cohort exported is the outcome; adjust as needed.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # "cleanWindow" corresponds to the prior outcome lookback / clean-window; the
  # Analysis Specifications set priorOutcomeLookBack = 365, so use that here.
  mutate(cleanWindow = 365)

# Target / Comparator specification for CohortMethod analyses.
# We re-numbered target/comparator to 1 and 2 above.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = cohortDefinitionSet %>% filter(.data$cohortId == 1) %>% pull(cohortName),
  comparatorCohortId = 2,
  comparatorCohortName = cohortDefinitionSet %>% filter(.data$cohortId == 2) %>% pull(cohortName),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# Covariate inclusion / exclusion
# ------------------------------------------------------------------------------
# The Analysis Specifications provided lists for conceptsToInclude / conceptsToExclude,
# but with NULL ids. Replace the placeholder concept IDs below with real conceptIds
# for covariates you want to include or exclude explicitely.
excludedCovariateConcepts <- data.frame(
  conceptId = c(2345678, 3456789), # <-- REPLACE with actual conceptIds to exclude
  conceptName = c("target concept name", "comparator concept name"),
  stringsAsFactors = FALSE
)

# If you prefer to include only specific covariates, uncomment and populate:
# includedCovariateConcepts <- data.frame(
#   conceptId = c(4567890), # <-- REPLACE
#   conceptName = c("Diagnostic X"),
#   stringsAsFactors = FALSE
# )

# ------------------------------------------------------------------------------
# Create Strategus Module Specifications for Cohort Generation and Diagnostics
# ------------------------------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource for cohort definitions (the HADES cohort generator uses this)
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Shared resource: negative control outcome cohort definitions
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",       # detect first occurrence
  detectOnDescendants = TRUE     # include descendants for the concept set
)

# Module specification to run the cohort generation
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# ------------------------------------------------------------------------------
# CohortDiagnostics Module Specifications
# ------------------------------------------------------------------------------
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

# ------------------------------------------------------------------------------
# CohortMethod Module: Build analysis configurations using the Analysis Specs
# ------------------------------------------------------------------------------
# STUDY PERIODS
# The Analysis Specifications had studyPeriods with NULL start/end. When NULL,
# we indicate no restriction by using empty strings which the CohortMethod
# convention in this template treats as "no restriction".
studyPeriods <- tibble::tibble(
  studyStartDate = c(""), # empty ==> no study start restriction
  studyEndDate   = c("")  # empty ==> no study end restriction
)

# TIME-AT-RISKS (TARs)
# The Analysis Specifications define a single TAR with:
# riskWindowStart = 0, startAnchor = "cohort start",
# riskWindowEnd = 0, endAnchor = "cohort end", minDaysAtRisk = 1
timeAtRisks <- tibble::tibble(
  label = c("primary"),
  riskWindowStart  = c(0),
  startAnchor = c("cohort start"), # "cohort start" | "cohort end"
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end")      # "cohort start" | "cohort end"
)

# PROPENSITY SCORE CONFIGURATION
# Build a match-on-PS tibble that reflects the settings in the Analysis Specifications:
# matchOnPsArgs: maxRatio = 10, caliper = 0.2, caliperScale = "standardized logit"
matchOnPsArgsList <- tibble::tibble(
  label = c("match_on_ps_1"),
  maxRatio  = c(10),
  caliper = c(0.2),
  caliperScale  = c("standardized logit") # "propensity score" | "standardized" | "standardized logit"
)

# No stratifyByPs settings in the provided Analysis Specifications; leave empty
stratifyByPsArgsList <- tibble::tibble(
  label = character(),
  numberOfStrata = integer(),
  baseSelection = character()
)

# Build the combined psConfigList for iteration
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

# ------------------------------------------------------------------------------
# Now iterate over study periods, TARs, and PS configs to build CM analyses
# ------------------------------------------------------------------------------
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Convert psCfg to CohortMethod match/stratify arguments.
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
        stop("Unsupported PS method: ", psCfg$method)
      }

      # Covariate settings for the CohortMethod analysis.
      # Here we start from the default covariate settings and will exclude
      # specific concepts (e.g. the target/comparator drug concepts).
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build the outcome list: one or more outcomes of interest (trueEffectSize=NA)
      # plus the negative control outcomes (trueEffectSize=1).
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

      # For each target-comparator pair, build a targetComparatorOutcomes object.
      # Note: We also exclude covariates corresponding to the target/comparator
      # drug concepts and any other explicit exclusions provided above.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        # If specific target/comparator drug concept IDs are known they can be
        # added here (e.g. targetConceptId, comparatorConceptId). The Analysis
        # Specifications had NULLs, so we rely on the explicit excludedCovariateConcepts table.
        excludedCovariateIds <- excludedCovariateConcepts$conceptId

        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateIds
        )
      }

      # getDbCohortMethodDataArgs: how to pull covariates & cohort data from the CDM.
      # The Analysis Specifications set:
      # - studyPeriods as potentially empty (no restriction)
      # - maxCohortSize = 0  (use full cohorts)
      # We apply covariateSettings created above.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,   # The analysis-specific "createStudyPopArgs" controls study period
        studyStartDate = ifelse(studyStartDate == "", NULL, studyStartDate),
        studyEndDate = ifelse(studyEndDate == "", NULL, studyEndDate),
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs: parameters to fit the propensity score model.
      # Map values from Analysis Specifications:
      # maxCohortSizeForFitting = 250000,
      # errorOnHighCorrelation = TRUE,
      # prior: laplace w/ cross-validation,
      # control: tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10,
      # noiseLevel = "silent", resetCoefficients = TRUE, startingVariance = 0.01
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # allow Strategus to continue; individual CM ops may fail
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
          seed = 1,
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # Covariate balance computation args
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )

      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs: map from Analysis Specifications:
      # modelType = "cox", stratified = FALSE, useCovariates = FALSE,
      # inversePtWeighting = FALSE, prior = Laplace + CV,
      # control: tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10,
      # noiseLevel = "quiet", resetCoefficients = TRUE, startingVariance = 0.01
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = FALSE,
        useCovariates = FALSE,
        inversePtWeighting = FALSE,
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
          noiseLevel = "quiet",
          seed = 1,
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # createStudyPopArgs: convert the Analysis Specifications parameters here.
      # The specification provided:
      # - restrictToCommonPeriod = false
      # - firstExposureOnly = false
      # - washoutPeriod = 365
      # - removeDuplicateSubjects = "remove all"
      # - censorAtNewRiskWindow = false
      # - removeSubjectsWithPriorOutcome = true
      # - priorOutcomeLookBack = 365
      # - time at risk: configured above (riskWindowStart/End & anchors)
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 365,
        removeDuplicateSubjects = "remove all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 365,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1,
        maxDaysAtRisk = 99999
      )

      # Append the CohortMethod analysis configuration to the list
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          ifelse(studyStartDate == "", "none", studyStartDate),
          ifelse(studyEndDate == "", "none", studyEndDate),
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
    } # end for psConfigList
  } # end for timeAtRisks
} # end for studyPeriods

# ------------------------------------------------------------------------------
# Create CohortMethod Module Specification
# ------------------------------------------------------------------------------
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# ------------------------------------------------------------------------------
# Combine module specifications into a single Strategus analysis specification
# ------------------------------------------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecificiations() %>%
  Strategus::addSharedResources(cohortDefinitionShared) %>%
  Strategus::addSharedResources(negativeControlsShared) %>%
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# ------------------------------------------------------------------------------
# Save the analysis specification to JSON
# The file will be written to inst/<studyName>/<studyName>AnalysisSpecification.json
# Replace "ceeamos" with another name if needed.
# ------------------------------------------------------------------------------
outputFolder <- file.path("inst", "ceeamos")
if (!dir.exists(outputFolder)) dir.create(outputFolder, recursive = TRUE)

outputFile <- file.path(outputFolder, "ceeamosAnalysisSpecification.json")

ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  outputFile
)

message("Strategus analysis specification JSON written to: ", outputFile)

# ------------------------------------------------------------------------------
# End of CreateStrategusAnalysisSpecification.R
# ------------------------------------------------------------------------------
# Notes / TODOs for users:
# - Replace all placeholder IDs (cohort IDs, conceptSet IDs, conceptIds) with
#   the valid IDs from your Atlas / vocabulary.
# - Review the covariate inclusion/exclusion lists to ensure correct
#   specification of targeted covariates.
# - If you want to add additional time-at-risk windows or PS stratifications,
#   expand the timeAtRisks, matchOnPsArgsList, or stratifyByPsArgsList tables.
# - After saving the JSON, you can run Strategus::execute() with appropriate
#   connection details (cdmDatabaseSchema, workDatabaseSchema, etc.) to run
#   the study on a database.
# ------------------------------------------------------------------------------