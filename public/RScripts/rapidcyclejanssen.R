################################################################################
# CreateStrategusAnalysisSpecification.R
# 
# This script builds a Strategus Analysis Specification using the HADES modules
# following the provided Template and Analysis Specifications.
#
# Key elements:
# - Uses CohortGenerator, CohortDiagnostics, and CohortMethod modules
# - Applies study periods, time-at-risk windows, and PS/Outcome model settings
#   exactly as described in the Analysis Specifications
# - Includes detailed annotations to explain each step and how settings map
#
# Note: Replace the placeholder cohort IDs/conceptSet IDs with your own WebAPI ids.
#       This script focuses on constructing analysis specifications; execution
#       requires valid cohort definitions and (optionally) negative controls.
################################################################################

# Libraries ---------------------------------------------------------------------
library(dplyr)
library(tibble)
library(Strategus)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)

# Shared Resources --------------------------------------------------------------
# Base WebAPI URL (update to your environment if needed)
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Study name from Analysis Specifications
studyName <- "rapidcyclejanssen"

# ------------------------------------------------------------------------------
# Cohort Definitions
# - The Template demonstrates pulling 3 cohorts (Target, Comparator, Outcome).
# - The Analysis Specifications do not provide concrete IDs; placeholders are used.
# - Replace 0000000 / 1111111 / 2222222 with valid WebAPI cohort IDs.
# - We renumber locally to 1 (target), 2 (comparator), 3 (outcome) as required
#   by the module inputs and Template.
# ------------------------------------------------------------------------------
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    0000000, # Target:    <-- Replace with your Target cohort ID
    1111111, # Comparator: <-- Replace with your Comparator cohort ID
    2222222  # Outcome:   <-- Replace with your Outcome cohort ID
  ),
  generateStats = TRUE
)

# Re-number cohorts to 1, 2, 3 for internal references throughout the analysis
cohortDefinitionSet[cohortDefinitionSet$cohortId == 0000000, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1111111, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 2222222, ]$cohortId <- 3

# ------------------------------------------------------------------------------
# Negative control outcomes
# - The Analysis Specifications provide a "negativeControlConceptSet" with id null.
# - If you have a WebAPI concept set ID for your negative control outcomes, set it
#   below to enable automatic resolution to concepts for NC outcomes.
# - If not provided (NULL), the script will skip negative control setup.
# ------------------------------------------------------------------------------
negativeControlConceptSetId <- NULL  # Replace NULL with a valid conceptSetId if available

if (!is.null(negativeControlConceptSetId)) {
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
    # Start NC IDs at 100+ to avoid collisions with target/comparator/outcome (1,2,3)
    mutate(cohortId = dplyr::row_number() + 100L) %>%
    select(cohortId, cohortName, outcomeConceptId)
} else {
  negativeControlOutcomeCohortSet <- NULL
}

# Validate no duplicate cohort IDs across primary cohorts and NC outcomes
allCohortIds <- cohortDefinitionSet$cohortId
if (!is.null(negativeControlOutcomeCohortSet)) {
  allCohortIds <- c(allCohortIds, negativeControlOutcomeCohortSet$cohortId)
}
if (any(duplicated(allCohortIds))) {
  stop("*** Error: duplicate cohort IDs found across cohortDefinitionSet and negativeControlOutcomeCohortSet ***")
}

# ------------------------------------------------------------------------------
# Create convenience data frames for use in module settings
# ------------------------------------------------------------------------------
# Outcomes list: Here we assume 1 primary outcome (cohortId == 3), with a cleanWindow
# aligned to the study design. If you use multiple outcomes, extend as needed.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target-Comparator pairing used in CohortMethod analyses
cmTcList <- tibble::tibble(
  targetCohortId = 1L,
  targetCohortName = cohortDefinitionSet$cohortName[cohortDefinitionSet$cohortId == 1],
  comparatorCohortId = 2L,
  comparatorCohortName = cohortDefinitionSet$cohortName[cohortDefinitionSet$cohortId == 2]
)

# Covariate selection from Analysis Specifications
# conceptsToInclude and conceptsToExclude are provided but empty/null in the specs.
# Keep these structures in case you need to specify custom covariate concept filters.
includedCovariateConcepts <- tibble::tibble(
  conceptId = c(),  # add numeric concept IDs to include
  conceptName = c() # optional names for documentation
)
excludedCovariateConcepts <- tibble::tibble(
  conceptId = c(),  # add numeric concept IDs to exclude
  conceptName = c() # optional names for documentation
)

# CohortGeneratorModule ---------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Negative control shared resource is optional; only create if we have definitions
if (!is.null(negativeControlOutcomeCohortSet)) {
  negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
    negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
    occurrenceType = "first",
    detectOnDescendants = TRUE
  )
} else {
  negativeControlsShared <- NULL
}

cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings ---------------------------------------------
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

# CohortMethodModule ------------------------------------------------------------
# Analysis Specifications mappings:
# - getDbCohortMethodDataArgs.studyPeriods: [{ studyStartDate = 20210101, studyEndDate = null }]
# - getDbCohortMethodDataArgs.maxCohortSize = 0
# - createStudyPopArgs, timeAtRisks list (multiple TARs, each yields an analysis)
# - propensityScoreAdjustment: psSettings (matchOnPsArgs with maxRatio=0, caliper=0.2, caliperScale="propensity score")
# - createPsArgs: prior.laplace (useCrossValidation = TRUE) and control params (tolerance = 2e-7, cvType = "auto", fold=10, cvRepetitions=10, noiseLevel="silent", resetCoefficients=TRUE, startingVariance=0.01)
# - fitOutcomeModelArgs: modelType="cox", stratified=FALSE, useCovariates=FALSE, inversePtWeighting=FALSE, prior.laplace(useCrossValidation=TRUE), control params (same but noiseLevel="quiet")

# Study period(s) from Analysis Specifications (YYYYMMDD); NA end date means open-ended
studyPeriods <- tibble::tibble(
  studyStartDate = c(20210101L),
  studyEndDate   = c(NA_integer_)
)

# Time-at-risk windows from Analysis Specifications
timeAtRisks <- tibble::tibble(
  label = c(
    "TAR_1_to_14_days_from_start",
    "TAR_1_to_28_days_from_start",
    "TAR_1_to_42_days_from_start",
    "TAR_1_to_90_days_from_start",
    "TAR_0_to_2_days_from_start"
  ),
  riskWindowStart = c(1L, 1L, 1L, 1L, 0L),
  startAnchor = c("cohort start", "cohort start", "cohort start", "cohort start", "cohort start"),
  riskWindowEnd = c(14L, 28L, 42L, 90L, 2L),
  endAnchor = c("cohort start", "cohort start", "cohort start", "cohort start", "cohort start"),
  minDaysAtRisk = c(1L, 1L, 1L, 1L, 1L)
)

# PS adjustment configurations from Analysis Specifications
# Only matchOnPsArgs is provided; stratifyByPsArgs is null in specs
matchOnPsArgsList <- tibble::tibble(
  label = c("Match_on_PS_caliper_0.2"),
  maxRatio = c(0),                 # As provided (note: 0 is unusual; consider >0 in real studies)
  caliper = c(0.2),
  caliperScale = c("propensity score")
)

stratifyByPsArgsList <- tibble::tibble(
  label = character(0),
  numberOfStrata = integer(0),
  baseSelection = character(0)
)

# Build PS configuration list (each entry has method, label, params)
psConfigList <- list()

# Convert matchOnPs data frame to configuration entries
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

# Convert stratifyByPs data frame to configuration entries (none provided in specs)
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

# Iterate through combinations of studyPeriods, TARs, and PS configs -------------
cmAnalysisList <- list()
targetComparatorOutcomesList <- list()
analysisId <- 1L

# Covariate Settings: start from defaults; addDescendantsToExclude = TRUE is aligned with Template
covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
  addDescendantsToExclude = TRUE
)

# Build outcome list: primary outcomes + optional negative controls
outcomeList <- list()

# Primary outcome(s) - outcomeOfInterest = TRUE
if (nrow(oList) > 0) {
  outcomeList <- append(
    outcomeList,
    lapply(seq_len(nrow(oList)), function(i) {
      CohortMethod::createOutcome(
        outcomeId = oList$outcomeCohortId[i],
        outcomeOfInterest = TRUE,
        trueEffectSize = NA,
        priorOutcomeLookback = 99999
      )
    })
  )
}

# Negative controls - outcomeOfInterest = FALSE (only if provided)
if (!is.null(negativeControlOutcomeCohortSet)) {
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

# Target-Comparator pairs
for (i in seq_len(nrow(cmTcList))) {
  # Merge in included/excluded covariate concept filters if provided (empty by default)
  targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
    targetId = cmTcList$targetCohortId[i],
    comparatorId = cmTcList$comparatorCohortId[i],
    outcomes = outcomeList,
    includedCovariateConceptIds = if (nrow(includedCovariateConcepts) > 0) includedCovariateConcepts$conceptId else c(),
    excludedCovariateConceptIds = if (nrow(excludedCovariateConcepts) > 0) excludedCovariateConcepts$conceptId else c()
  )
}

# Loops over study periods and TARs to create analyses
for (s in seq_len(nrow(studyPeriods))) {

  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s] # may be NA

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

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

      # getDbCohortMethodDataArgs mapped from Analysis Specifications:
      # - restrictToCommonPeriod: not specified in getDb; set to FALSE to align with createStudyPopArgs
      # - studyStartDate: 20210101; studyEndDate: NA
      # - maxCohortSize: 0
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # createPsArgs matches Analysis Specifications exactly for regularization and CV control
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE,  # Keep FALSE so execution continues even if a PS model fails
        prior = Cyclops::createPrior(
          priorType = "laplace",
          useCrossValidation = TRUE
        ),
        control = Cyclops::createControl(
          tolerance = 2e-07,         # 2e-7
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "silent",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # Covariate balance computation arguments (not explicitly in specs; included per Template)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs from Analysis Specifications
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
          tolerance = 2e-07,         # 2e-7
          cvType = "auto",
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "quiet",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # createStudyPopArgs from Analysis Specifications (per TAR row)
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = TRUE,
        washoutPeriod = 365,
        removeDuplicateSubjects = "remove all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 99999,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t]
      )

      # Build analysis entry with clear description of period, TAR, and PS method
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          ifelse(is.na(studyStartDate), "NA", as.character(studyStartDate)),
          ifelse(is.na(studyEndDate), "NA", as.character(studyEndDate)),
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
    }
  }
}

cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the analysis specifications -------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecificiations() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  { if (!is.null(negativeControlsShared)) Strategus::addSharedResources(., negativeControlsShared) else . } |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save to JSON -----------------------------------------------------------------
# Saved at: inst/<studyName>/<studyName>AnalysisSpecification.json
# Per Analysis Specifications: name = "rapidcyclejanssen"
dir.create(file.path("inst", studyName), recursive = TRUE, showWarnings = FALSE)
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", studyName, paste0(studyName, "AnalysisSpecification.json"))
)