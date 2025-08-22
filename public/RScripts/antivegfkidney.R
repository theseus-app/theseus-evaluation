#!/usr/bin/env Rscript

################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script builds a Strategus analysis specification JSON for the study:
#   "antivegfkidney"
#
# Notes:
# - Many placeholders (cohort IDs, concept set IDs, excluded covariate IDs)
#   must be replaced with your study-specific values before running.
# - The script applies the settings provided in the Analysis Specifications
#   (see the top of the file and inline comments).
# - It uses CohortGenerator, CohortDiagnostics and CohortMethod modules.
#
# Required packages: dplyr, Strategus, ROhdsiWebApi, CohortMethod,
# FeatureExtraction, Cyclops, ParallelLogger
################################################################################

library(dplyr)
library(Strategus)
library(ROhdsiWebApi)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)
library(ParallelLogger)

# ------------------------------------------------------------------------------
# Study identifier used for output paths and the JSON filename
# ------------------------------------------------------------------------------
studyName <- "antivegfkidney"

# ------------------------------------------------------------------------------
# 1) Cohort definitions
# ------------------------------------------------------------------------------
# Replace the cohortIds below with the Atlas cohort definition IDs for:
#   - Target cohort
#   - Comparator cohort
#   - Outcome cohort(s)
#
# Example:
#   cohortIds = c(12345, 23456, 34567)
#
# Current placeholders MUST be replaced before executing the workflow against
# a real WebAPI.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1111111, # Target cohort ID -> REPLACE with your target cohort ID
    2222222, # Comparator cohort ID -> REPLACE with your comparator cohort ID
    3333333  # Outcome cohort ID  -> REPLACE with your outcome cohort ID
  ),
  generateStats = TRUE
)

# Renumber cohorts to compact IDs starting at 1, 2, 3...
# This script expects: target -> 1, comparator -> 2, outcome(s) -> 3, ...
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1111111, "cohortId"] <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 2222222, "cohortId"] <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 3333333, "cohortId"] <- 3

# ------------------------------------------------------------------------------
# Negative control outcomes: Optionally provide a Concept Set Definition ID
# that resolves to a set of concepts you will use as negative controls.
# Replace conceptSetId below with your concept set definition ID for negative controls.
# If you don't have negative controls, you can skip creating negativeControlsShared
# and don't include the negative control cohort set in the final shared resources.
# ------------------------------------------------------------------------------
# NOTE: set conceptSetId to an actual concept set definition id or comment out
# the block below if not using negative controls.
negativeControlConceptSetId <- 4444444 # REPLACE with your concept set definition id (or NA/null to skip)

if (!is.null(negativeControlConceptSetId) && !is.na(negativeControlConceptSetId)) {
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
    mutate(cohortId = row_number() + 100) %>% # put negative controls in a distinct ID range (e.g. 101,102,...)
    select(cohortId, cohortName, outcomeConceptId)
} else {
  negativeControlOutcomeCohortSet <- NULL
}

# safety check for duplicate cohort ids
if (!is.null(negativeControlOutcomeCohortSet) &&
    any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between cohortDefinitionSet and negativeControlOutcomeCohortSet ***")
}

# ------------------------------------------------------------------------------
# 2) Cohort lists for use in modules
# ------------------------------------------------------------------------------
# Outcomes: we will take cohorts with cohortId == 3 as our outcomes (per renumbering)
# Add a 'cleanWindow' column used by some modules (e.g. CohortDiagnostics)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId,
         outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # clean window = 365 days (example)

# Target and Comparator table for CohortMethod analyses:
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "Target (replace name)",
  comparatorCohortId = 2,
  comparatorCohortName = "Comparator (replace name)",
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# 3) Covariate inclusion / exclusion settings
# ------------------------------------------------------------------------------
# If you want to exclude the drugs of interest from the LSPS (recommended),
# provide their concept IDs here.
# Replace the example IDs with your target/comparator concept IDs to be excluded.
excludedCovariateConcepts <- data.frame(
  conceptId = c(5555555, 6666666), # REPLACE with concept IDs to exclude
  conceptName = c("target drug concept", "comparator drug concept"),
  stringsAsFactors = FALSE
)

# Optionally, you can provide a set of covariates to explicitly include
# includedCovariateConcepts <- data.frame(
#   conceptId = c(...),
#   conceptName = c(...),
#   stringsAsFactors = FALSE
# )

# ------------------------------------------------------------------------------
# 4) CohortGenerator module (shared resources and specification)
# ------------------------------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

negativeControlsShared <- NULL
if (!is.null(negativeControlOutcomeCohortSet)) {
  negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
    negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
    occurrenceType = "first",
    detectOnDescendants = TRUE
  )
}

cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# ------------------------------------------------------------------------------
# 5) CohortDiagnostics module specifications
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
# 6) CohortMethod module - build analyses using the Analysis Specifications
# ------------------------------------------------------------------------------
# Study periods: per Analysis Specifications, the studyPeriods are empty (no restriction).
# If you want to restrict by date, populate studyStartDate and studyEndDate (YYYYMMDD).
studyPeriods <- tibble(
  studyStartDate = c(), # leave empty to indicate no date restriction
  studyEndDate   = c()
)

# Time-at-risk (TAR)
# Analysis Specifications define a single TAR:
#  riskWindowStart = 0
#  startAnchor = "cohort start"
#  riskWindowEnd = 0
#  endAnchor = "cohort end"
#  minDaysAtRisk = 1
timeAtRisks <- tibble(
  label = c("cohortStart_to_cohortEnd"),
  riskWindowStart  = c(0),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end"),
  minDaysAtRisk = c(1)
)

# Propensity score settings: match-on-PS with:
#   maxRatio = 1, caliper = 0.2, caliperScale = "standardized logit"
matchOnPsArgsList <- tibble(
  label = c("match_ps_1_0.2"),
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")
)

# No stratify-by-PS configs in the provided specifications
stratifyByPsArgsList <- tibble(
  label = c(),
  numberOfStrata  = c(),
  baseSelection = c()
)

# Build a single PS configuration list (one element per PS adjustment)
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
# Build CohortMethod analyses combining study periods, TARs and PS configs
# ------------------------------------------------------------------------------
cmAnalysisList <- list()
analysisId <- 1

# Iterate studyPeriods (empty => single iteration with NA studyStart/studyEnd handled below)
if (nrow(studyPeriods) == 0) {
  # Provide one empty study period (no restriction)
  studyPeriods <- tibble(studyStartDate = NA_character_, studyEndDate = NA_character_)
}

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Build PS adjustment args
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
        stop("Unknown PS method: ", psCfg$method)
      }

      # Covariate settings - using default extraction unless you supply includedCovariateConcepts
      # The analysis specifications implied inclusion/exclusion of particular concepts;
      # here we add descendantsToExclude = TRUE to ensure excluded concept sets remove descendants.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build outcome list for CohortMethod
      outcomeList <- c(
        # True outcomes from oList
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999 # per Analysis Specifications
          )
        }),
        # Negative controls (if provided)
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
      ) %>% unlist(recursive = FALSE)

      # Build target-comparator-outcomes list
      # Exclude covariates corresponding to the drugs of interest (excludedCovariateConcepts)
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        # We only provide excludedCovariateConceptIds from the excludedCovariateConcepts
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      }

      # getDbCohortMethodDataArgs - controls the cohort to CDM mapping and covariate extraction
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = ifelse(is.na(studyStartDate), NULL, studyStartDate),
        studyEndDate = ifelse(is.na(studyEndDate), NULL, studyEndDate),
        maxCohortSize = 0, # Analysis Specifications: maxCohortSize = 0 -> no limit
        covariateSettings = covariateSettings
      )

      # createPsArgs - settings used to build the PS model (Cyclops regularized regression)
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,  # from Analysis Specifications
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # allow workflow to complete even if PS fitting fails for a pairing
        estimator = "att",   # ATT estimator for PS if used
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
          startingVariance = 0.01
        )
      )

      # Covariate balance computation settings
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs - Cox proportional hazards model configuration
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = FALSE,              # Analysis Specifications: stratified = false
        useCovariates = FALSE,
        inversePtWeighting = FALSE,
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
          noiseLevel = "quiet",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # createStudyPopArgs - defines study population and time-at-risk anchorings
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,                 # per Analysis Specifications
        firstExposureOnly = TRUE,                       # per Analysis Specifications
        washoutPeriod = 365,                            # per Analysis Specifications
        removeDuplicateSubjects = "keep all",           # per Analysis Specifications
        censorAtNewRiskWindow = FALSE,                  # per Analysis Specifications
        removeSubjectsWithPriorOutcome = TRUE,          # per Analysis Specifications
        priorOutcomeLookback = 99999,                   # per Analysis Specifications
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        maxDaysAtRisk = 99999
      )

      # Append the CohortMethod analysis definition to the list
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          ifelse(is.na(studyStartDate), "", studyStartDate),
          ifelse(is.na(studyEndDate), "", studyEndDate),
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

# Create the CohortMethod module specifications
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
# 7) Assemble Strategy: shared resources + module specifications
# ------------------------------------------------------------------------------
# Start with an empty analysis specification object and add shared resources
# and modules. If you are not using negative controls, do not add
# negativeControlsShared (it will be NULL).
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() %>%
  Strategus::addSharedResources(cohortDefinitionShared)

if (!is.null(negativeControlsShared)) {
  analysisSpecifications <- analysisSpecifications %>% Strategus::addSharedResources(negativeControlsShared)
}

analysisSpecifications <- analysisSpecifications %>%
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# ------------------------------------------------------------------------------
# 8) Save the analysis specification to JSON
# ------------------------------------------------------------------------------
outputFile <- file.path("inst", studyName, paste0(studyName, "AnalysisSpecification.json"))
dir.create(dirname(outputFile), recursive = TRUE, showWarnings = FALSE)

ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  outputFile
)

# Informative message (printed when run interactively)
message("Strategus analysis specification saved to: ", outputFile)
message("Please review and replace all placeholder cohort / concept IDs before use.")