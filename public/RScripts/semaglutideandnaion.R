################################################################################
# See the Create analysis specifications section
# of the UsingThisTemplate.md for more details.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# This help page also contains links to the corresponding HADES package that
# further details.
################################################################################
library(dplyr)
library(tibble)
library(Strategus)

# Note:
# - This script follows the provided Template structure and applies settings from
#   the supplied Analysis Specifications object.
# - Where the Analysis Specifications contained NULL or empty values (e.g., cohort IDs,
#   negative control concept set), this script keeps placeholders and documents
#   where the user must supply study-specific values.
# - The code is annotated to explain how each setting is applied.

# Shared Resources -------------------------------------------------------------
# Base URL of the OHDSI WebAPI instance that hosts the cohort definitions.
# Replace this with the appropriate WebAPI for your environment if needed.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# IMPORTANT: Replace the cohortIds below with real ATLAS/WebAPI cohort IDs
#   - Index 1 (0000000 -> 1) will be used as the Target cohort
#   - Index 2 (1111111 -> 2) will be used as the Comparator cohort
#   - Index 3 (2222222 -> 3) will be used as the Outcome cohort
# This follows the Template's structure of exporting three cohorts and re-numbering
# them to 1, 2, and 3 for use within Strategus/HADES modules.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    0000000, # Target: replace with your Target cohort ID
    1111111, # Comparator: replace with your Comparator cohort ID
    2222222  # Outcome: replace with your Outcome cohort ID
  ),
  generateStats = TRUE
)

# Re-number cohorts so they are 1, 2, 3 in the analysis specification:
# - 1 = Target
# - 2 = Comparator
# - 3 = Outcome
cohortDefinitionSet[cohortDefinitionSet$cohortId == 0000000, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1111111, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 2222222, ]$cohortId <- 3

# Negative control outcomes ----------------------------------------------------
# Analysis Specifications provide an empty negativeControlConceptSet (id = NULL, name = "").
# Therefore, we will not define any negative controls here.
# If you have a negative control concept set in ATLAS/WebAPI, uncomment and set conceptSetId below.
negativeControlOutcomeCohortSet <- tibble::tibble() # Intentionally empty per Analysis Specifications

# Example (commented out) if you later add a real conceptSetId:
# negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
#   conceptSetId = 1234567,  # replace with your negative control concept set ID
#   baseUrl = baseUrl
# ) %>%
#   ROhdsiWebApi::resolveConceptSet(baseUrl = baseUrl) %>%
#   ROhdsiWebApi::getConcepts(baseUrl = baseUrl) %>%
#   dplyr::rename(outcomeConceptId = "conceptId",
#                 cohortName = "conceptName") %>%
#   dplyr::mutate(
#     cohortId = dplyr::row_number() + 100  # reserve 1,2,3 for T, C, O; NCs start at 101
#   ) %>%
#   dplyr::select(cohortId, cohortName, outcomeConceptId)

# Sanity check: ensure no overlap between primary cohorts and NC outcome cohorts
if (nrow(negativeControlOutcomeCohortSet) > 0) {
  if (any(cohortDefinitionSet$cohortId %in% negativeControlOutcomeCohortSet$cohortId)) {
    stop("*** Error: duplicate cohort IDs found between primary cohorts and negative controls ***")
  }
}

# Construct lists/data frames for analysis inputs ------------------------------
# Outcomes: Take the single outcome cohort (ID = 3), and define a cleanWindow = 365 days
#   This matches the Template and is independent from the analysis TAR(s).
oList <- cohortDefinitionSet %>%
  dplyr::filter(.data$cohortId == 3) %>%
  dplyr::mutate(
    outcomeCohortId = cohortId,
    outcomeCohortName = cohortName
  ) %>%
  dplyr::select(outcomeCohortId, outcomeCohortName) %>%
  dplyr::mutate(cleanWindow = 365)

# Define Target and Comparator info for CohortMethod analysis
# NOTE: Provide human-readable names (placeholders used here).
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target cohort name",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator cohort name",
  # Including concept IDs for the drugs of interest to facilitate exclusion from covariates if desired
  targetConceptId = NA_integer_,     # replace with target concept ID if available
  comparatorConceptId = NA_integer_  # replace with comparator concept ID if available
)

# Covariate selection: include/exclude sets -----------------------------------
# Analysis Specifications define empty lists for conceptsToInclude and conceptsToExclude.
# We keep them empty here as well.
# If you have concepts to include or exclude, supply their conceptIds below.
includedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character()
)

excludedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character()
)

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Only create the NegativeControl shared resource if negative controls were defined
negativeControlsShared <- NULL
if (nrow(negativeControlOutcomeCohortSet) > 0) {
  negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
    negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
    occurrenceType = "first",
    detectOnDescendants = TRUE
  )
}

cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings --------------------------------------------
# These settings follow the Template defaults and are not specified in the Analysis Specifications.
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
# The following sections translate the Analysis Specifications to CohortMethod settings.

# Study period(s): from Analysis Specifications getDbCohortMethodDataArgs.studyPeriods
# We define a row per time window to allow expanding to multiple periods if needed.
studyPeriods <- tibble::tibble(
  studyStartDate = c(20171201),
  studyEndDate   = c(20231231)
)

# Time-at-risk (TAR): from Analysis Specifications createStudyPopArgs.timeAtRisks (single TAR)
# We add a label to help identify the TAR in the analysis description.
timeAtRisks <- tibble::tibble(
  label = c("TAR_1"),
  riskWindowStart  = c(0),
  startAnchor = c("cohort start"),
  riskWindowEnd  = c(0),
  endAnchor = c("cohort end")
)

# Propensity Score settings:
# Two PS adjustment strategies are specified in the Analysis Specifications:
# 1) Match on PS: maxRatio = 1, caliper = 0.2, caliperScale = "propensity score"
# 2) Stratify by PS: numberOfStrata = 5, baseSelection = "all"
matchOnPsArgsList <- tibble::tibble(
  label = c("MatchOnPs"),
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("propensity score")
)
stratifyByPsArgsList <- tibble::tibble(
  label = c("StratifyByPs"),
  numberOfStrata  = c(5),
  baseSelection = c("all")
)

# Build a unified PS configuration list to iterate over (each element = one PS method/setting)
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

# Create outcome objects: primary outcome plus (optionally) negative controls
outcomeList <- append(
  # Primary outcome(s) of interest
  lapply(seq_len(nrow(oList)), function(i) {
    CohortMethod::createOutcome(
      outcomeId = oList$outcomeCohortId[i],
      outcomeOfInterest = TRUE,
      trueEffectSize = NA,
      priorOutcomeLookback = 99999  # align with Analysis Specifications createStudyPopArgs.priorOutcomeLookBack
    )
  }),
  # Negative controls (if any were defined)
  if (nrow(negativeControlOutcomeCohortSet) > 0) {
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

# Create the Target-Comparator-Outcome (TCO) set.
# Exclude covariate concepts per covariateSelection (empty by specification here).
targetComparatorOutcomesList <- list()
for (i in seq_len(nrow(cmTcList))) {
  targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
    targetId = cmTcList$targetCohortId[i],
    comparatorId = cmTcList$comparatorCohortId[i],
    outcomes = outcomeList,
    excludedCovariateConceptIds = c(
      cmTcList$targetConceptId[i],
      cmTcList$comparatorConceptId[i],
      excludedCovariateConcepts$conceptId
    )
  )
}

# Covariate Settings -----------------------------------------------------------
# We use the default covariate settings, adding descendant exclusion logic.
# If conceptsToInclude/Exclude were provided, they would be applied here.
covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
  addDescendantsToExclude = TRUE
  # Optionally, to use concept inclusion/exclusion:
  # includedCovariateConceptIds = includedCovariateConcepts$conceptId,
  # excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
)

# Iterate through all analysis setting combinations ----------------------------
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # PS adjustment method: match or stratify
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

      # Database retrieval args (CohortMethod data)
      # - restrictToCommonPeriod: not specified in the Analysis Specifications -> set to FALSE
      # - studyStartDate / studyEndDate: from Analysis Specifications (20171201 - 20231231)
      # - maxCohortSize: 0 (from Analysis Specifications)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs: from Propensity Score Adjustment settings in Analysis Specifications
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = FALSE,
        # leave stopOnError, estimator at defaults as not specified
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
          resetCoefficients = FALSE,
          startingVariance = 0.01
        )
      )

      # Covariate balance settings (not specified in Analysis Specifications; using Template defaults)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Outcome model args: from fitOutcomeModelArgs in Analysis Specifications
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = TRUE,
        useCovariates = TRUE,
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

      # Study population args: from createStudyPopArgs in Analysis Specifications
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
        minDaysAtRisk = 1
        # maxDaysAtRisk left at default because Analysis Specifications did not set it
      )

      # Assemble the CM analysis specification for this combination
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
    }
  }
}

# Finalize CohortMethod module specifications ----------------------------------
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
# We add shared resources and module specifications to the Strategus analysis specification.
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  { if (!is.null(negativeControlsShared)) Strategus::addSharedResources(., negativeControlsShared) else . } |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save to JSON -----------------------------------------------------------------
# The Analysis Specifications name is "semaglutideandnaion" per the request.
# The output path mirrors the Template, customized for this study name.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "semaglutideandnaion", "semaglutideandnaionAnalysisSpecification.json")
)