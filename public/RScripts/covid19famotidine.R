################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script creates an analysis specification JSON for use with the
# OHDSI Strategus package. It is based on the provided Analysis
# Specifications (see Analysis Specifications section in the header).
#
# NOTES / ACTION ITEMS FOR USER:
# - Replace the placeholder cohort IDs (cohortIds vector) with the Atlas
#   cohort definition IDs for your target, comparator, and outcome cohorts.
# - If you have a negative control concept set in Atlas, replace the
#   negativeControlConceptSetId placeholder with that concept set ID.
# - If you want to include/exclude specific covariate concept IDs,
#   populate the excludedCovariateConcepts and includedCovariateConcepts
#   data frames below.
#
# This file contains detailed inline annotations describing how values
# from the "Analysis Specifications" JSON are mapped into Strategus /
# CohortMethod / Cyclops / FeatureExtraction settings.
################################################################################

library(dplyr)
library(Strategus)
library(ROhdsiWebApi)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)
library(ParallelLogger)

# ---------------------------------------------------------
# Basic study-level settings (from Analysis Specifications)
# ---------------------------------------------------------
studyName <- "covid19famotidine" # analysis name from JSON "name" field

# Base Atlas/WebAPI URL - replace if you use a different Atlas instance
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# ---------------------------------------------------------
# Cohort definitions
# ---------------------------------------------------------
# The Analysis Specifications contained null placeholder cohort ids.
# Replace the cohortIds below with the actual Atlas cohort IDs for:
#   - target cohort (first entry)
#   - comparator cohort (second entry)
#   - outcome cohort(s) (third and beyond)
#
# Example:
#   cohortIds = c(1111111, 2222222, 3333333)
#
# IMPORTANT: Update this vector to match your Atlas cohort definition IDs.
cohortIds <- c(
  1111111, # Target cohort: REPLACE_WITH_TARGET_ATLAS_COHORT_ID
  2222222, # Comparator cohort: REPLACE_WITH_COMPARATOR_ATLAS_COHORT_ID
  3333333  # Outcome cohort: REPLACE_WITH_OUTCOME_ATLAS_COHORT_ID
)

# Export cohort definitions from WebAPI. generateStats = TRUE will request
# cohort statistics (helpful for diagnostics).
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = cohortIds,
  generateStats = TRUE
)

# Re-number cohorts to a 1-based convention used in the rest of this script:
# Target -> 1, Comparator -> 2, Outcome(s) -> 3, ...
# This renumbering simplifies referencing the cohorts in downstream modules.
originalIds <- cohortIds
for (i in seq_along(originalIds)) {
  cohortDefinitionSet[cohortDefinitionSet$cohortId == originalIds[i], ]$cohortId <- i
}

# ---------------------------------------------------------
# Negative control concept set (optional)
# ---------------------------------------------------------
# The JSON had a placeholder (null). If you have a concept set ID in Atlas
# that defines negative control outcomes, set negativeControlConceptSetId
# to that integer. If not, keep as NULL and the script will proceed without
# negative controls.
negativeControlConceptSetId <- NULL # REPLACE_WITH_CONCEPTSET_ID_IF_AVAILABLE

if (!is.null(negativeControlConceptSetId)) {
  negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
    conceptSetId = negativeControlConceptSetId,
    baseUrl = baseUrl
  ) %>%
    ROhdsiWebApi::resolveConceptSet(baseUrl = baseUrl) %>%
    ROhdsiWebApi::getConcepts(baseUrl = baseUrl) %>%
    rename(outcomeConceptId = "conceptId",
           cohortName = "conceptName") %>%
    mutate(cohortId = row_number() + 100) %>% # start negative control ids at 101
    select(cohortId, cohortName, outcomeConceptId)
} else {
  # Create an empty tibble with the same structure to simplify downstream code
  negativeControlOutcomeCohortSet <- tibble(
    cohortId = integer(0),
    cohortName = character(0),
    outcomeConceptId = integer(0)
  )
}

# Safety check: ensure no duplicate cohortIds between main cohorts and negative controls
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between cohortDefinitionSet and negativeControlOutcomeCohortSet ***")
}

# ---------------------------------------------------------
# Create lists used in the CohortMethod module
# ---------------------------------------------------------
# Build outcome list (primary outcomes from cohort definitions + negative controls)
# In the Analysis Specifications the 'outcomeCohort' entry had null placeholders.
# The template approach uses cohortId==3 as the main outcome; if you have multiple
# outcome cohorts, add them to cohortIds above and the re-numbering will handle them.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId >= 3) %>% # all cohorts with id >= 3 are treated as outcomes
  mutate(outcomeCohortId = cohortId,
         outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # The createOutcome function uses priorOutcomeLookback; we set a default cleanWindow
  mutate(cleanWindow = 365)

# Target and comparator mapping for CohortMethod
# Using the renumbered ids: target == 1, comparator == 2
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target cohort",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator cohort",
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------
# Covariate concept include / exclude (optional)
# ---------------------------------------------------------
# The Analysis Specifications included placeholders for conceptsToInclude / exclude.
# If you want to exclude specific concepts from the covariate construction (e.g.
# the drug ingredients for target/comparator), populate excludedCovariateConcepts below.
excludedCovariateConcepts <- tibble(
  conceptId = integer(0),
  conceptName = character(0)
)

# If you prefer to whitelist specific covariates rather than using the full set,
# populate includedCovariateConcepts (optional).
includedCovariateConcepts <- tibble(
  conceptId = integer(0),
  conceptName = character(0)
)

# ---------------------------------------------------------
# CohortGeneratorModule shared resources & module specs
# ---------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# ---------------------------------------------------------
# CohortDiagnosticsModule settings
# (run the diagnostics most often useful for cohort inspection)
# ---------------------------------------------------------
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

# ---------------------------------------------------------
# CohortMethod Module: Study periods, time-at-risk, PS configs
# Values below are taken from the Analysis Specifications section.
# ---------------------------------------------------------

# getDbCohortMethodDataArgs -> studyPeriods (Analysis Specs)
# A single study period: 20200201 - 20200530
studyPeriods <- tibble(
  studyStartDate = c("20200201"),
  studyEndDate = c("20200530")
)

# createStudyPopArgs -> time-at-risks (Analysis Specs)
# One TAR specified: start 1 day after cohort start, end 30 days after cohort start.
timeAtRisks <- tibble(
  label = c("1-30 days"),
  riskWindowStart = c(1),
  startAnchor = c("cohort start"),
  riskWindowEnd = c(30),
  endAnchor = c("cohort start")
)

# Propensity Score adjustment settings from Analysis Specifications:
# Two PS configs:
#  1) stratify by PS into 5 strata, baseSelection = "all"
#  2) match on PS with maxRatio = 1 and caliper = 0.2 (standardized logit)
stratifyByPsArgsList <- tibble(
  label = c("stratify_5_all"),
  numberOfStrata = c(5),
  baseSelection = c("all"),
  stringsAsFactors = FALSE
)

matchOnPsArgsList <- tibble(
  label = c("match_1_0.2_stdlogit"),
  maxRatio = c(1),
  caliper = c(0.2),
  caliperScale = c("standardized logit"),
  stringsAsFactors = FALSE
)

# Build a unified psConfigList that the loop below can iterate over.
psConfigList <- list()
# First add stratification configs
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
# Then add matching configs
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

# ---------------------------------------------------------
# Iterate through all desired analysis setting combinations
# and construct CohortMethod analyses (cmAnalysisList)
# ---------------------------------------------------------
cmAnalysisList <- list()
analysisId <- 1

# Covariate settings:
# We will use default covariate settings unless the user populates included/excluded concept lists.
covariateSettings <- FeatureExtraction::createDefaultCovariateSettings()

# If excludedCovariateConcepts provided, create a simple covariate filter to remove them.
# (CohortMethod accepts excludedCovariateConceptIds when creating target/comparator/outcomes.)
excludedCovariateConceptIds <- if (nrow(excludedCovariateConcepts) > 0) excludedCovariateConcepts$conceptId else integer(0)

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Build PS adjustment args depending on method
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

      # Create outcome definitions compatible with CohortMethod::createOutcome
      outcomeList <- c(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999
          )
        })
      )

      # Append negative controls if available (these were given cohortIds starting at 101)
      if (nrow(negativeControlOutcomeCohortSet) > 0) {
        negOutcomes <- lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
        outcomeList <- append(outcomeList, negOutcomes)
      }

      # Create targetComparatorOutcomes list (maps the target/comparator pair to the outcomes)
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        # Note: excludedCovariateConceptIds contains the manual excluded concepts (e.g., drug ingredients)
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # We include the manual excludedCovariateConceptIds; users can also add the
          # target/comparator concept ids here if needed.
          excludedCovariateConceptIds = excludedCovariateConceptIds
        )
      }

      # getDbCohortMethodDataArgs -> map from Analysis Specs
      # The JSON specified studyPeriods and maxCohortSize = 0
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE, # typically TRUE to align target/comparator eras
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs -> from Analysis Specifications createPsArgs block
      # maxCohortSizeForFitting = 250000, errorOnHighCorrelation = TRUE, prior = laplace w/ CV,
      # control has tolerance=2e-7, cvType="auto", fold=10, cvRepetitions=10, noiseLevel="silent",
      # resetCoefficients = TRUE, startingVariance = 0.01
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # allow process to continue even if a PS model fails for a pair
        estimator = "att",
        prior = Cyclops::createPrior(
          priorType = "laplace",
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

      # Covariate balance computation args (shared & per analysis)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs -> from Analysis Specifications
      # modelType = "cox", stratified = TRUE, useCovariates = FALSE, inversePtWeighting = FALSE
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
          tolerance = 2e-7,
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "quiet",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # createStudyPopArgs -> from Analysis Specifications
      # Map the single timeAtRisk row to CohortMethod createCreateStudyPopulationArgs
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,                         # Analysis Spec says false
        firstExposureOnly = FALSE,                              # Analysis Spec says false
        washoutPeriod = 0,                                      # Analysis Spec says 0
        removeDuplicateSubjects = "keep all",                   # Analysis Spec: "keep all"
        censorAtNewRiskWindow = FALSE,                          # Analysis Spec says false
        removeSubjectsWithPriorOutcome = TRUE,                  # Analysis Spec says true
        priorOutcomeLookback = 30,                              # Analysis Spec says 30 days
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1,
        maxDaysAtRisk = 99999
      )

      # Construct a readable description for this analysis configuration
      description <- sprintf(
        "Study: %s-%s; TAR: %s; PS: %s",
        studyStartDate,
        studyEndDate,
        timeAtRisks$label[t],
        psCfg$label
      )

      # Append the CohortMethod analysis configuration to cmAnalysisList
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

# ---------------------------------------------------------
# CohortMethod module specifications
# ---------------------------------------------------------
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# ---------------------------------------------------------
# Assemble the final analysisSpecifications object and save to JSON
# ---------------------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Destination file for the analysis specification JSON.
# The file path follows the pattern inst/<studyName>/<studyName>AnalysisSpecification.json
destinationFile <- file.path("inst", studyName, sprintf("%sAnalysisSpecification.json", studyName))

# Create directories if necessary
if (!dir.exists(dirname(destinationFile))) dir.create(dirname(destinationFile), recursive = TRUE)

# Save the specification to disk
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  destinationFile
)

message("Analysis specification JSON saved to: ", destinationFile)
message("Review the file and replace placeholder cohort IDs / concept set IDs as needed before execution.")
###############################################################################