################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script builds a Strategus analysis specification for the study:
#   strokerisk
#
# It uses the OHDSI Strategus package and related HADES modules to:
#  - retrieve cohort definitions from an Atlas/WebAPI instance,
#  - declare negative control outcome concept sets,
#  - define cohort method analyses (study periods, time-at-risk, PS adjustments),
#  - assemble module specifications (CohortGenerator, CohortDiagnostics, CohortMethod),
#  - and save the final analysis specification JSON under inst/strokerisk.
#
# NOTE: Several values in this script are intentionally left as placeholders
# (cohort ids, concept set ids, and concept ids). Replace them with the
# appropriate IDs for your study before executing the script.
################################################################################

library(dplyr)
library(tibble)
library(Strategus)
library(ROhdsiWebApi)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)
library(ParallelLogger)

# ------------------------------
# Settings that users MUST edit
# ------------------------------
# Base URL for Atlas/WebAPI from which cohort definitions and concept sets are exported.
# Replace with your Atlas/WebAPI endpoint.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Replace the cohort IDs below with the actual cohort definition IDs from Atlas:
# - The first id should be the target cohort definition id
# - The second id should be the comparator cohort definition id
# - The third id should be the outcome cohort definition id (primary outcome)
cohortIds <- c(
  1234567, # Target: REPLACE_WITH_TARGET_COHORT_ID
  2345678, # Comparator: REPLACE_WITH_COMPARATOR_COHORT_ID
  3456789  # Outcome: REPLACE_WITH_OUTCOME_COHORT_ID
)

# Replace with the concept set id that defines the negative control outcomes
# (if you use a concept set in Atlas to define negative controls).
negativeControlConceptSetId <- 4567890 # REPLACE_WITH_NEGATIVE_CONTROL_CONCEPTSET_ID

# Replace or populate excluded covariate concepts (concept IDs) that should
# be excluded from the LSPS (e.g., the target and comparator drug ingredients).
# If none, set to an empty data.frame.
excludedCovariateConcepts <- data.frame(
  conceptId = c( 5678901 ), # REPLACE_WITH_CONCEPT_IDS_TO_EXCLUDE (e.g., target drug concept id)
  conceptName = c( "example excluded concept name" )
)

# If you want to include only specific covariates (instead of default covariates),
# set includedCovariateConcepts to a data.frame of conceptId/conceptName.
# Otherwise, keep it commented out or empty.
# includedCovariateConcepts <- data.frame(conceptId = c(...), conceptName = c("..."))

# Study name used for file paths
studyName <- "strokerisk"

# ------------------------------
# Export cohort definitions from Atlas/WebAPI
# ------------------------------
# We fetch the cohort definitions and generate basic statistics (generateStats = TRUE)
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = cohortIds,
  generateStats = TRUE
)

# Re-number cohorts so that Target = 1, Comparator = 2, Outcome = 3
# This renumbering simplifies mapping to CohortMethod target/comparator/outcome ids
# Keep the cohortId mapping order consistent with the cohortIds vector above.
cohortDefinitionSet[cohortDefinitionSet$cohortId == cohortIds[1], ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == cohortIds[2], ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == cohortIds[3], ]$cohortId <- 3

# ------------------------------
# Negative control outcomes: export concept set and resolve to concepts
# ------------------------------
# This block loads a concept set definition (from Atlas/WebAPI), resolves descendants
# (if any) and converts to a table of outcome concepts. Each negative control is
# then assigned a distinct cohortId starting at 101 to avoid overlap with 1/2/3.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = negativeControlConceptSetId,
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(baseUrl = baseUrl) %>%
  ROhdsiWebApi::getConcepts(baseUrl = baseUrl) %>%
  dplyr::rename(outcomeConceptId = "conceptId",
                cohortName = "conceptName") %>%
  dplyr::mutate(cohortId = dplyr::row_number() + 100) %>% 
  dplyr::select(cohortId, cohortName, outcomeConceptId)

# Sanity check: ensure we have no cohortId duplication between main cohorts and negatives
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between main cohorts and negative controls ***")
}

# ------------------------------
# Prepare lists used for building analyses
# ------------------------------

# Outcomes table for CohortMethod: take cohortId 3 (renumbered) as the primary outcome
# and set a cleanWindow (used to define incident events) if desired.
oList <- cohortDefinitionSet %>%
  dplyr::filter(.data$cohortId == 3) %>%
  dplyr::mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  dplyr::select(outcomeCohortId, outcomeCohortName) %>%
  dplyr::mutate(cleanWindow = 365) # example: 365-day clean window for incident outcome

# Target and Comparator pair for CohortMethod analyses (using renumbered ids 1 and 2)
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target cohort",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator cohort",
  stringsAsFactors = FALSE
)

# ------------------------------
# CohortGenerator Module specification
# ------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Create negative control cohort shared resources for cohort generation
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# ------------------------------
# CohortDiagnostics Module specification
# ------------------------------
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

# ------------------------------
# CohortMethod Module specification
# ------------------------------
# Build study periods per Analysis Specifications:
# Specified study periods:
#  - 20010101 to 20171231
#  - 20010101 to 20151130
studyPeriods <- tibble::tibble(
  studyStartDate = c("20010101", "20010101"),
  studyEndDate   = c("20171231", "20151130")
)

# Time-at-risk(s) as specified in the Analysis Specifications
# There is a single TAR:
#  - riskWindowStart = 1 (startAnchor = "cohort start")
#  - riskWindowEnd   = 0 (endAnchor = "cohort end")
timeAtRisks <- tibble::tibble(
  label = c("1_day_after_start_to_cohort_end"),
  riskWindowStart  = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end")
)

# Propensity score settings (two match configurations provided)
#  - 1: match maxRatio=1 caliper=0.05 caliperScale="propensity score"
#  - 2: match maxRatio=10 caliper=0.2 caliperScale="standardized logit"
matchOnPsArgsList <- tibble::tibble(
  label = c("match_1_0.05_propensity", "match_10_0.2_std_logit"),
  maxRatio  = c(1, 10),
  caliper = c(0.05, 0.2),
  caliperScale  = c("propensity score", "standardized logit")
)

# Build psConfigList from matchOnPsArgsList (we have only matching configurations)
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

# ------------------------------
# Build CM analysis list
# ------------------------------
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create matching or stratification args depending on configuration
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
        stop("Unknown PS method in psConfigList")
      }

      # Covariate settings — use default covariate extraction unless user
      # explicitly provides includedCovariateConcepts or excludedCovariateConcepts.
      # Note: addDescendantsToExclude = TRUE ensures ingredients / descendant concepts
      # are excluded when requested.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Build outcome list for CohortMethod:
      # - primary outcomes (outcomeOfInterest = TRUE) come from oList
      # - negative controls (outcomeOfInterest = FALSE, trueEffectSize = 1) come from negativeControlOutcomeCohortSet
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

      # Build targetComparatorOutcomesList: for each target/comparator pair, specify
      # excluded covariate concepts (target/comparator themselves + domain-specific excludes)
      # If cmTcList contains additional columns for targetConceptId/comparatorConceptId,
      # they will be used; otherwise use the excludedCovariateConcepts data.frame.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        # Combine excluded covariate ids: the explicit excludedCovariateConcepts defined above
        excludedIds <- if (nrow(excludedCovariateConcepts) > 0) {
          excludedCovariateConcepts$conceptId
        } else {
          integer(0)
        }

        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedIds
        )
      }

      # Get DB cohort method data arguments: restrict to the specified study period
      # and use the covariate settings defined above. Set maxCohortSize per spec (0 = no limit).
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # restricting extraction to study period boundaries
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Create PS fitting arguments (Cyclops prior + control) per Analysis Specifications
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # allow Strategus to continue if PS fitting fails for a pair
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
          noiseLevel = "quiet",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # Covariate balance diagnostics arguments
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Fit outcome model arguments per Analysis Specifications
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

      # Create study population arguments as specified:
      # Note: removeDuplicateSubjects = "keep all" per the specification.
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
        minDaysAtRisk = 1,
        maxDaysAtRisk = 99999
      )

      # Add the CohortMethod analysis specification to the list
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study period: %s-%s; TAR: %s; PS: %s",
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

# Create the CohortMethod module specifications.
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# ------------------------------
# Assemble the overall analysis specifications
# ------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecificiations() %>%
  Strategus::addSharedResources(cohortDefinitionShared) %>%
  Strategus::addSharedResources(negativeControlsShared) %>%
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# ------------------------------
# Save the analysis specification JSON
# ------------------------------
# The JSON will be saved to inst/<studyName>/<studyName>AnalysisSpecification.json
outputDir <- file.path("inst", studyName)
if (!dir.exists(outputDir)) dir.create(outputDir, recursive = TRUE)

outputFile <- file.path(outputDir, sprintf("%sAnalysisSpecification.json", studyName))
ParallelLogger::saveSettingsToJson(analysisSpecifications, outputFile)

message(sprintf("Analysis specification saved to: %s", outputFile))

################################################################################
# End of CreateStrategusAnalysisSpecification.R
#
# Reminders:
#  - Replace all placeholder IDs (cohortIds, negative control concept set id,
#    excluded covariate concept ids) with the real IDs from your Atlas/WebAPI
#    or concept dictionaries.
#  - Review covariate inclusion/exclusion, time-at-risk definitions, and matching
#    specifications to ensure they align with your study protocol.
#  - After saving the JSON, use Strategus to execute the study per your
#    execution plan.
################################################################################