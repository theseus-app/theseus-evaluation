################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script creates an analysis specification JSON for use with the OHDSI
# Strategus framework. It encodes the study-level settings described in the
# provided Analysis Specifications JSON (study name: "ranitidinecancer").
#
# IMPORTANT:
# - Many cohort / concept IDs are placeholders in this file (NA or example IDs).
#   You MUST replace these placeholder IDs with the real cohort IDs and concept
#   set IDs exported from your ATLAS / WebAPI instance prior to running the
#   Strategus run. Comments show where to edit values.
#
# - The script is heavily annotated to make it clear how each of the settings
#   from the Analysis Specifications map to the produced Strategus settings.
#
# Required packages:
#  - Strategus (for building the analysis specification and modules)
#  - ROhdsiWebApi (for downloading cohort definitions / concept sets)
#  - CohortMethod, FeatureExtraction, Cyclops (used to build CM args)
#  - dplyr (data manipulation)
#  - ParallelLogger (to save JSON)
#
################################################################################

library(dplyr)
library(Strategus)
library(ROhdsiWebApi)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)
library(ParallelLogger)

# ---------------------------
# Shared Resources / Settings
# ---------------------------

# WebAPI base url: change if you are not using the Atlas demo server
# (recommended: point to your own ATLAS / WebAPI server)
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Study name used to create the output JSON path and labels
studyName <- "ranitidinecancer"

# ------------------------------------------------------------------------------
# COHORT DEFINITIONS
# ------------------------------------------------------------------------------

# NOTE: The cohort IDs below are placeholders. Replace with the actual cohort IDs
# exported from your Atlas / WebAPI. The three cohorts correspond to:
#   1) Target cohort (exposed group)
#   2) Comparator cohort (alternative exposed group)
#   3) Outcome cohort (the cancer outcome definition)
#
# Example placeholder IDs (replace these):
targetCohortWebApiId     <- 1000001  # <-- REPLACE with your target cohortId from WebAPI
comparatorCohortWebApiId <- 1000002  # <-- REPLACE with your comparator cohortId from WebAPI
outcomeCohortWebApiId    <- 1000003  # <-- REPLACE with your outcome cohortId from WebAPI

# Export the cohort definition set from WebAPI so Strategus can reference them.
# generateStats = TRUE will instruct WebAPI to include statistics (optional).
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    targetCohortWebApiId,
    comparatorCohortWebApiId,
    outcomeCohortWebApiId
  ),
  generateStats = TRUE
)

# Re-number the exported cohortIds to small integers used throughout the
# Strategus configuration. This is a common pattern to avoid conflicts and to
# express relationships succinctly (target = 1, comparator = 2, outcome = 3).
#
# We map the WebAPI cohortId -> internal cohortId (1, 2, 3)
cohortDefinitionSet[cohortDefinitionSet$cohortId == targetCohortWebApiId, ]$cohortId     <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == comparatorCohortWebApiId, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == outcomeCohortWebApiId, ]$cohortId    <- 3

# ------------------------------------------------------------------------------
# NEGATIVE CONTROLS (Concept Set)
# ------------------------------------------------------------------------------

# NOTE: The negative control concept set id below is a placeholder. Replace with
# the conceptSetId for your negative controls from WebAPI.
negativeControlConceptSetId <- 1234567 # <-- REPLACE with your negative-control conceptSetId

# Download, resolve and expand the concept set definition for negative controls.
# This yields a table of concepts which we convert into "cohort-like" rows and
# assign cohortIds starting from 101, 102, ...
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = negativeControlConceptSetId,
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(baseUrl = baseUrl) %>%
  ROhdsiWebApi::getConcepts(baseUrl = baseUrl) %>%
  rename(outcomeConceptId = "conceptId",
         cohortName = "conceptName") %>%
  mutate(cohortId = row_number() + 100) %>% # negative control cohort ids -> 101, 102, ...
  select(cohortId, cohortName, outcomeConceptId)

# Sanity check: ensure no duplicated cohort IDs across target/comparator/outcome
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between cohortDefinitionSet and negative controls ***")
}

# ------------------------------------------------------------------------------
# CREATE COHORT LISTS (used later to build CM analyses)
# ------------------------------------------------------------------------------

# Outcomes: create an outcomes list from the outcome cohort(s)
# The original Analysis Specifications specify one outcome with a clean-window
# of 365 days.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId,
         outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # per Analysis Specifications: priorOutcomeLookBack and clean window = 365

# Target / Comparator pairs for the CohortMethod analyses.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target cohort",       # edit name if desired
  comparatorCohortId = 2,
  comparatorCohortName = "comparator cohort" # edit name if desired
)

# Excluded covariates
# The Analysis Specifications defined "conceptsToExclude" but no concrete ids.
# If you want to exclude specific concept IDs (e.g., drug concepts that define
# the exposures), list them here. For now we leave this empty; replace with a
# non-empty df if needed.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(0),
  conceptName = character(0),
  stringsAsFactors = FALSE
)

# Optional: if you want to explicitly include only some covariates,
# create an includedCovariateConcepts data.frame similarly. Left commented.
# includedCovariateConcepts <- data.frame(conceptId = c(...), conceptName = c("..."))

# ------------------------------------------------------------------------------
# CohortGenerator Module (shared resources and module specifications)
# ------------------------------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Shared resource: the cohort definitions we downloaded above
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Shared resource: negative control outcome cohorts (expanded concept set)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",        # use the first occurrence for negative controls
  detectOnDescendants = TRUE      # include descendant concepts
)

# Module specification: instruct Strategus to create the cohorts and optional stats
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# ------------------------------------------------------------------------------
# CohortDiagnostics Module: optional diagnostics run for each cohort
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
# CohortMethod Module: build analysis list per the Analysis Specifications
# ------------------------------------------------------------------------------

# Study periods:
# The Analysis Specifications provided study periods with null start/end (meaning
# no restriction). Here we set an empty string pair to represent "no restriction"
# in the CohortMethod args (this is a common pattern in Strategus templates).
studyPeriods <- tibble(
  studyStartDate = c(""), # blank => no study start restriction
  studyEndDate   = c("")  # blank => no study end restriction
)

# Time-at-risk definition(s) (TARs).
# The Analysis Specifications include one TAR:
#   - riskWindowStart: 365
#   - startAnchor: "cohort start"
#   - riskWindowEnd: 9999
#   - endAnchor: "cohort start"
#   - minDaysAtRisk: 1
timeAtRisks <- tibble(
  label = c("365d_to_end"),
  riskWindowStart = c(365),
  startAnchor = c("cohort start"),
  riskWindowEnd = c(9999),
  endAnchor = c("cohort start")
)

# Propensity score adjustment settings:
# The Analysis Specifications request a match-on-PS configuration:
#   - maxRatio = 1
#   - caliper = 0.2
#   - caliperScale = "standardized logit"
matchOnPsArgsList <- tibble(
  label = c("match_ps_1_0.2"),
  maxRatio = c(1),
  caliper = c(0.2),
  caliperScale = c("standardized logit") # allowed: "propensity score" | "standardized" | "standardized logit"
)

# No stratify-by-PS configurations were provided, so leave empty:
stratifyByPsArgsList <- tibble(label = character(0), numberOfStrata = integer(0), baseSelection = character(0))

# Build psConfigList combining match and stratify configurations.
psConfigList <- list()

# Convert each match-on-PS row into a config object
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

# Convert each stratify-by-PS row into a config object (none here)
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

# Build the CohortMethod analyses (one analysis per combination of study period,
# time-at-risk and PS configuration).
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create matchOnPsArgs or stratifyByPsArgs depending on method
      if (psCfg$method == "match") {
        # Map the match parameters from the Analysis Specifications
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psCfg$params$maxRatio,
          caliper = psCfg$params$caliper,
          caliperScale = psCfg$params$caliperScale,
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psCfg$params$numberOfStrata,
          stratificationColumns = c(),
          baseSelection = psCfg$params$baseSelection
        )
      }

      # Covariate settings
      # The Analysis Specifications did not request any special covariate filters
      # other than excluding certain concepts. We use the default covariate
      # settings (FeatureExtraction default) and will pass excluded covariates to
      # createTargetComparatorOutcomes below.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings()

      # Outcome objects:
      # - The outcome(s) from the cohort definitions are considered outcomes of interest.
      # - Negative controls are added with trueEffectSize = 1 to support diagnostics.
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

      # Build the target-comparator-outcomes objects. This binds each TC pair to
      # the list of outcomes and specifies excluded covariates. The template in
      # the Analysis Specifications expects excludedCovariateConceptIds.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        # We pass in excluded covariates (if any). If you have explicit target/
        # comparator concept IDs that should be excluded from covariates, add
        # those to excludedCovariateConcepts$conceptId above.
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # getDbCohortMethodDataArgs:
      # The Analysis Specifications included an object getDbCohortMethodDataArgs
      # with studyPeriods and maxCohortSize = 0. We set restrictToCommonPeriod = TRUE
      # and pass the studyStartDate / studyEndDate (blanks mean no restriction).
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs:
      # The Analysis Specifications provide detailed Cyclops prior/control settings.
      # We map these values into Cyclops helper constructors here.
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,     # as specified
        errorOnHighCorrelation = TRUE,        # as specified
        stopOnError = FALSE,                  # do not stop whole analysis if PS fitting fails for a pair
        estimator = "att",                    # common choice for match
        prior = Cyclops::createPrior(
          priorType = "laplace",              # L1 (lasso) regularization per spec
          exclude = c(0),                     # do not penalize intercept
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          noiseLevel = "silent",              # per spec for PS fitting
          cvType = "auto",
          fold = 10,                          # per Analysis Specifications (fold)
          seed = 1,
          resetCoefficients = TRUE,
          tolerance = 2e-07,
          cvRepetitions = 10,                 # per Analysis Specifications
          startingVariance = 0.01
        )
      )

      # computeCovariateBalanceArgs: used to compute covariate balance diagnostics
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs: Map settings from Analysis Specifications:
      # - modelType = "cox"
      # - stratified = FALSE
      # - useCovariates = FALSE
      # - inversePtWeighting = FALSE
      # - prior and control parameters (laplace + CV; control includes fold=10, cvRepetitions=10)
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
          cvType = "auto",
          fold = 10,
          seed = 1,
          resetCoefficients = TRUE,
          startingVariance = 0.01,
          tolerance = 2e-07,
          cvRepetitions = 10,
          noiseLevel = "quiet" # per Analysis Specifications for outcome model
        )
      )

      # createStudyPopArgs: per Analysis Specifications
      # - restrictToCommonPeriod = FALSE
      # - firstExposureOnly = FALSE
      # - washoutPeriod = 365
      # - removeDuplicateSubjects = "keep all"
      # - censorAtNewRiskWindow = FALSE
      # - removeSubjectsWithPriorOutcome = TRUE
      # - priorOutcomeLookBack = 365
      # - Time-at-risk definitions taken from the timeAtRisks table above
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 365,
        removeDuplicateSubjects = "keep all",
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

      # Create the CohortMethod analysis object and append to the list.
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
    } # end psConfigList loop
  } # end timeAtRisks loop
} # end studyPeriods loop

# Build CohortMethod module specifications
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
# COMPOSE THE FINAL Strategus analysisSpecifications OBJECT
# ------------------------------------------------------------------------------

# Note: many Strategus templates use createEmptyAnalysisSpecificiations() (spelling
# used historically in documentation). We call the Strategic helper to create an
# empty specification and then add the shared resources & module specs.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecificiations() %>%
  Strategus::addSharedResources(cohortDefinitionShared) %>%
  Strategus::addSharedResources(negativeControlsShared) %>%
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# ------------------------------------------------------------------------------
# Save the JSON file
# ------------------------------------------------------------------------------

# Create directory path inst/<studyName>/ if it does not exist
outputDir <- file.path("inst", studyName)
if (!dir.exists(outputDir)) {
  dir.create(outputDir, recursive = TRUE)
}

# File name: <studyName>AnalysisSpecification.json
outputFile <- file.path(outputDir, sprintf("%sAnalysisSpecification.json", studyName))

# Save to JSON using ParallelLogger (consistent with many Strategus examples)
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  outputFile
)

# Informational message (will appear when script is run)
message(sprintf("Analysis specification JSON saved to: %s", outputFile))

# ------------------------------------------------------------------------------
# Additional notes / reminders for users (non-exhaustive)
# - Replace the placeholder WebAPI cohort IDs and conceptSetId with real IDs.
# - If you have covariates to exclude or include, populate excludedCovariateConcepts
#   and/or includedCovariateConcepts above.
# - Review the cyclops prior/control settings: cross-validation (CV) folds and
#   repetitions can be computationally expensive; adjust according to resource.
# - After saving the JSON you can launch Strategus run on this specification.
# ------------------------------------------------------------------------------