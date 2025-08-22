################################################################################
# See the Create analysis specifications section
# of the UsingThisTemplate.md for more details.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# This help page also contains links to the corresponding HADES package that
# further details.
################################################################################

# This script builds a Strategus analysis specification using the
# settings provided in <Analysis Specifications> for the study "tramadolcodein".
# It follows the structure of the <Template> with detailed annotations showing
# how each setting is mapped to a HADES module argument.

# Libraries --------------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(Strategus)
  library(ROhdsiWebApi)
  library(CohortMethod)
  library(FeatureExtraction)
  library(Cyclops)
  library(ParallelLogger)
})

# Shared Resources -------------------------------------------------------------
# WebAPI base URL (update to your environment if needed)
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# Replace the numeric placeholders below (0000000, 1111111, 2222222)
# with actual ATLAS/WebAPI cohort IDs for:
# - Target cohort (slot 1)
# - Comparator cohort (slot 2)
# - Outcome cohort (slot 3)
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    0000000, # Target:
    1111111, # Comparator:
    2222222  # Outcome:
  ),
  generateStats = TRUE
)

# Re-number cohorts to local IDs 1 (target), 2 (comparator), 3 (outcome).
# This is a common practice to have predictable local IDs downstream.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 0000000, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1111111, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 2222222, ]$cohortId <- 3

# Negative control outcomes ----------------------------------------------------
# From <Analysis Specifications>, negativeControlConceptSet$id is null.
# We therefore create an empty negative control outcome cohort set.
# If you later provide a conceptSetId, this block will automatically fetch and
# resolve the concept set into negative-control outcome concept IDs.
negativeControlConceptSetId <- NA_integer_

if (!is.na(negativeControlConceptSetId)) {
  negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
    conceptSetId = negativeControlConceptSetId,
    baseUrl = baseUrl
  ) %>%
    ROhdsiWebApi::resolveConceptSet(baseUrl = baseUrl) %>%
    ROhdsiWebApi::getConcepts(baseUrl = baseUrl) %>%
    dplyr::rename(outcomeConceptId = "conceptId",
                  cohortName = "conceptName") %>%
    dplyr::mutate(
      # target/comparator/outcome cohorts use IDs 1, 2, 3...
      # negative controls start from 101, 102, ...
      cohortId = dplyr::row_number() + 100L
    ) %>%
    dplyr::select(cohortId, cohortName, outcomeConceptId)
} else {
  negativeControlOutcomeCohortSet <- tibble(
    cohortId = integer(),
    cohortName = character(),
    outcomeConceptId = integer()
  )
}

# Validate no overlap in cohort IDs between main cohorts and negative controls
if (nrow(negativeControlOutcomeCohortSet) > 0) {
  if (length(intersect(cohortDefinitionSet$cohortId,
                       negativeControlOutcomeCohortSet$cohortId)) > 0) {
    stop("*** Error: duplicate cohort IDs found ***")
  }
}

# Create lists for outcomes and T/C pairs --------------------------------------
# Outcomes list (from the outcome cohort set with local ID 3)
# cleanWindow is set to 365 by convention in the template.
oList <- cohortDefinitionSet %>%
  dplyr::filter(.data$cohortId == 3) %>%
  dplyr::mutate(
    outcomeCohortId = .data$cohortId,
    outcomeCohortName = .data$cohortName
  ) %>%
  dplyr::select(outcomeCohortId, outcomeCohortName) %>%
  dplyr::mutate(cleanWindow = 365)

# Target and Comparator specification (local IDs 1 and 2)
# Keep the EXACT names from the template for these fields.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target cohort name",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator cohort name",
  stringsAsFactors = FALSE
)

# Covariate selection (from <Analysis Specifications>) -------------------------
# conceptsToInclude and conceptsToExclude are provided but with null IDs/names.
# We instantiate empty data frames following the template field names.
# Add your concept IDs if/when known; they will be used to exclude covariates.
includedCovariateConcepts <- tibble(
  conceptId = integer(),
  conceptName = character()
)
excludedCovariateConcepts <- tibble(
  conceptId = integer(),
  conceptName = character()
)

# CohortGeneratorModule --------------------------------------------------------
# Create shared resource specifications for cohorts
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Create negative control outcome shared resource if any exist
if (nrow(negativeControlOutcomeCohortSet) > 0) {
  negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
    negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
    occurrenceType = "first",
    detectOnDescendants = TRUE
  )
} else {
  negativeControlsShared <- NULL
}

# Configure the CohortGenerator module (compute cohort-level statistics)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
# Comprehensive diagnostics per the template
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
# Study Period(s) from <Analysis Specifications>:
# getDbCohortMethodDataArgs.studyPeriods has one entry with null dates, which we
# represent as NA. Passing empty strings to the CM arg means "no restriction".
studyPeriods <- tibble(
  studyStartDate = NA_character_,  # YYYYMMDD or NA for no restriction
  studyEndDate   = NA_character_   # YYYYMMDD or NA for no restriction
)

# Time-at-risk windows from <Analysis Specifications>. We provide labels for
# description purposes only.
timeAtRisks <- tibble(
  label = c("TAR 1", "TAR 2"),
  riskWindowStart = c(0, 0),
  startAnchor = c("cohort start", "cohort start"),
  riskWindowEnd = c(0, 9999),
  endAnchor = c("cohort end", "cohort start")
)

# Propensity score adjustment (match on PS) from <Analysis Specifications>.
# Only "match" is specified; no stratify settings.
matchOnPsArgsList <- tibble(
  label = c("Match: caliper=0.2, maxRatio=1, scale=standardized logit"),
  maxRatio = c(1),
  caliper = c(0.2),
  caliperScale = c("standardized logit")
)
stratifyByPsArgsList <- tibble(
  label = character(),
  numberOfStrata = integer(),
  baseSelection = character()
)

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# If a data frame for "match on PS" exists and has rows, convert each row to a config
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

# If a data frame for "stratify by PS" exists and has rows, convert each row to a config
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

# Create outcome list used by all analyses:
# - Primary outcome(s) from oList flagged as outcomeOfInterest = TRUE
# - Optional negative control outcomes flagged as outcomeOfInterest = FALSE
outcomeList <- list()
if (nrow(oList) > 0) {
  outcomeList <- append(
    outcomeList,
    lapply(seq_len(nrow(oList)), function(i) {
      CohortMethod::createOutcome(
        outcomeId = oList$outcomeCohortId[i],
        outcomeOfInterest = TRUE,
        trueEffectSize = NA,
        # We align with template's large default to avoid excluding due to lookback
        priorOutcomeLookback = 99999
      )
    })
  )
}
if (nrow(negativeControlOutcomeCohortSet) > 0) {
  outcomeList <- append(
    outcomeList,
    lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
      CohortMethod::createOutcome(
        outcomeId = i,
        outcomeOfInterest = FALSE,
        trueEffectSize = 1
      )
    })
  )
}

# Covariate settings for FeatureExtraction. We keep the template default with
# descendants added to excludes. Specific include/exclude concepts are handled
# via excludedCovariateConceptIds when creating TCOs.
covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
  addDescendantsToExclude = TRUE
)

# Build TargetComparatorOutcomes once (does not depend on TAR or PS choice).
# We only use excludedCovariateConceptIds from excludedCovariateConcepts. If you
# want to exclude additional conceptIds (e.g., the exposure concepts), add them
# to excludedCovariateConcepts above.
targetComparatorOutcomesList <- list()
for (i in seq_len(nrow(cmTcList))) {
  targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
    targetId = cmTcList$targetCohortId[i],
    comparatorId = cmTcList$comparatorCohortId[i],
    outcomes = outcomeList,
    excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
  )
}

# Create PS args from <Analysis Specifications>
# Note: "fold" is listed in the specifications control block, but Cyclops control
# does not expose a "fold" parameter directly. We map the provided settings to
# their closest Cyclops equivalents (cvType, cvRepetitions, tolerance, etc.).
createPsArgs <- CohortMethod::createCreatePsArgs(
  maxCohortSizeForFitting = 250000,
  errorOnHighCorrelation = TRUE,
  stopOnError = FALSE, # Allow Strategus to continue even if PS fit fails
  prior = Cyclops::createPrior(
    priorType = "laplace",
    useCrossValidation = TRUE
  ),
  control = Cyclops::createControl(
    tolerance = 2e-07,
    cvType = "auto",
    cvRepetitions = 10,
    noiseLevel = "silent",
    resetCoefficients = TRUE,
    startingVariance = 0.01
  )
)

# Fit outcome model args from <Analysis Specifications>
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
    cvRepetitions = 10,
    noiseLevel = "quiet",
    resetCoefficients = TRUE,
    startingVariance = 0.01
  )
)

# Build the CM analysis list by iterating over study periods, TARs, and PS configs
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {

  # Per template: if you are not restricting your study to a specific time window,
  # pass empty strings for studyStartDate/studyEndDate to CM.
  studyStartDate <- ifelse(is.na(studyPeriods$studyStartDate[s]), "", studyPeriods$studyStartDate[s])
  studyEndDate   <- ifelse(is.na(studyPeriods$studyEndDate[s]), "", studyPeriods$studyEndDate[s])

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Map PS config to appropriate CM arguments
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

      # Data extraction args (align with <Analysis Specifications> maxCohortSize = 0)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Create study population args mapped from <Analysis Specifications>:
      # - restrictToCommonPeriod = false
      # - firstExposureOnly = false
      # - washoutPeriod = 0
      # - removeDuplicateSubjects = "keep all"
      # - censorAtNewRiskWindow = false
      # - removeSubjectsWithPriorOutcome = true
      # - priorOutcomeLookBack = 365
      # - TARs picked from timeAtRisks tibble
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 365,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1
      )

      # Compute covariate balance args (template-aligned)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Append the settings to Analysis List
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          ifelse(nchar(studyStartDate) > 0, studyStartDate, "noStart"),
          ifelse(nchar(studyEndDate) > 0, studyEndDate, "noEnd"),
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

# Create the analysis specifications ------------------------------------------
# Compose Strategus analysis specs by adding shared resources and module specs.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecificiations() |>
  Strategus::addSharedResources(cohortDefinitionShared)

if (!is.null(negativeControlsShared)) {
  analysisSpecifications <- analysisSpecifications |>
    Strategus::addSharedResources(negativeControlsShared)
}

analysisSpecifications <- analysisSpecifications |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Persist the finalized analysis specification JSON for the study "tramadolcodein"
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "tramadolcodein", "tramadolcodeinAnalysisSpecification.json")
)