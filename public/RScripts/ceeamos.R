################################################################################
# See the Create analysis specifications section
# of the UsingThisTemplate.md for more details.
#
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# This help page also contains links to the corresponding HADES package that
# further details.
################################################################################

# Core dependencies used across the script
library(dplyr)
library(Strategus)

# Helper to coerce vectors with possible NA to a clean integer vector (drops NA)
.safe_ids <- function(x) {
  x <- unique(na.omit(as.integer(x)))
  if (length(x) == 0) integer(0) else x
}

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from an ATLAS/WebAPI instance.
# NOTE:
# - Replace baseUrl and the placeholder cohortIds with the actual values for your study.
# - This script preserves the template placeholders intentionally; fill them in before running.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
# Per Template: provide the three cohort definition IDs (Target, Comparator, Outcome)
# Per Analysis Specifications (cohortDefinitions target/comparator/outcome have null IDs and empty names):
# - We keep placeholders here and renumber locally as 1 (T), 2 (C), and 3 (O) for downstream usage.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    0000000, # Target: 
    1111111, # Comparator: 
    2222222  # Outcome: 
  ),
  generateStats = TRUE
)

# Re-number cohorts to local IDs consistently used throughout the analysis:
# 1 = Target, 2 = Comparator, 3 = Outcome
cohortDefinitionSet[cohortDefinitionSet$cohortId == 0000000, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1111111, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 2222222, ]$cohortId <- 3

# Negative control outcomes
# Analysis Specifications negativeControlConceptSet.id = null, name = "".
# We preserve this by allowing an empty set if no conceptSetId is provided.
negativeControlConceptSetId <- as.integer(NA) # placeholder for null

if (is.na(negativeControlConceptSetId)) {
  # No negative control concept set specified; create an empty data frame
  negativeControlOutcomeCohortSet <- tibble::tibble(
    cohortId = integer(0),
    cohortName = character(0),
    outcomeConceptId = integer(0)
  )
} else {
  # If you provide a valid concept set ID, this block will resolve it to concepts
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
    rename(
      outcomeConceptId = "conceptId",
      cohortName = "conceptName"
    ) %>%
    mutate(cohortId = dplyr::row_number() + 100) %>% # negativeControl -> 101, 102, 103...
    select(cohortId, cohortName, outcomeConceptId)
}

# Safety check to ensure no cohortId collisions between designed cohorts and negative controls
if (nrow(negativeControlOutcomeCohortSet) > 0) {
  localCohortIds <- cohortDefinitionSet$cohortId
  ncCohortIds <- negativeControlOutcomeCohortSet$cohortId
  if (length(intersect(localCohortIds, ncCohortIds)) > 0) {
    stop("*** Error: duplicate cohort IDs found between main cohorts and negative controls ***")
  }
}

# Create convenient data frames for downstream modules -------------------------

# Outcomes list:
# Per Template, we assume the "Outcome" is the cohort with local ID = 3.
# Per Analysis Specifications: outcomeCohort contains a single entry with null id and empty name,
# but here we retain the template structure and expect users to fill in the placeholders.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator mapping for CohortMethod
# Per Analysis Specifications, target and comparator names are "", and IDs are provided here as local 1 and 2.
# We also add placeholder concept IDs used for covariate exclusion (kept as NA to match the nulls).
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "",  # exact empty name as in Analysis Specifications
  comparatorCohortId = 2,
  comparatorCohortName = "",  # exact empty name as in Analysis Specifications
  targetConceptId = NA_integer_,
  comparatorConceptId = NA_integer_,
  stringsAsFactors = FALSE
)

# Covariate selection
# Per Analysis Specifications:
# covariateSelection.conceptsToInclude: one entry with id = null, name = ""
# covariateSelection.conceptsToExclude: one entry with id = null, name = ""
includedCovariateConcepts <- data.frame(
  conceptId = NA_integer_,
  conceptName = "",
  stringsAsFactors = FALSE
)
excludedCovariateConcepts <- data.frame(
  conceptId = NA_integer_,
  conceptName = "",
  stringsAsFactors = FALSE
)

# CohortGeneratorModule --------------------------------------------------------
# Create Shared Resource specifications for cohorts and (optionally) negative controls
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

# Only create the negative control shared resource if we actually have any
if (nrow(negativeControlOutcomeCohortSet) > 0) {
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

# CohortDiagnosticsModule Settings --------------------------------------------
# Retain template defaults for diagnostics. These are independent of the Analysis Specifications.
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
# The following settings are driven by the Analysis Specifications, with annotations below.

# Study periods:
# Analysis Specifications -> getDbCohortMethodDataArgs.studyPeriods = [{ studyStartDate: null, studyEndDate: null }]
# We represent null dates as NA_character_ and a single row.
studyPeriods <- tibble::tibble(
  studyStartDate = as.character(NA),  # YYYYMMDD or NA
  studyEndDate = as.character(NA)     # YYYYMMDD or NA
)

# Time-at-risks (TARs):
# Analysis Specifications -> createStudyPopArgs.timeAtRisks:
#   [{ riskWindowStart: 0, startAnchor: "cohort start",
#      riskWindowEnd: 0, endAnchor: "cohort end", minDaysAtRisk: 1 }]
# We also carry a "label" column as per Template; the Analysis Specifications do not provide a label,
# so we keep it as an empty string "".
timeAtRisks <- tibble::tibble(
  label = "",  # intentionally empty per "No name auto-correct"
  riskWindowStart = 0,
  startAnchor = "cohort start",
  riskWindowEnd = 0,
  endAnchor = "cohort end"
)

# Propensity Score settings
# Analysis Specifications:
#   propensityScoreAdjustment.psSettings: one config with matchOnPsArgs:
#     { maxRatio: 10, caliper: 0.2, caliperScale: "standardized logit" }, stratifyByPsArgs: null
#   propensityScoreAdjustment.createPsArgs:
#     maxCohortSizeForFitting = 250000
#     errorOnHighCorrelation = TRUE
#     prior: { priorType = "laplace", useCrossValidation = TRUE }
#     control:
#       tolerance = 2e-7, cvType = "auto", fold = 10, cvRepetitions = 10,
#       noiseLevel = "silent", resetCoefficients = TRUE, startingVariance = 0.01
matchOnPsArgsList <- tibble::tibble(
  label = "",  # intentionally empty
  maxRatio = 10,
  caliper = 0.2,
  caliperScale = "standardized logit"  # exact name per Analysis Specifications
)

# No stratifyByPs configs (null in Analysis Specifications)
stratifyByPsArgsList <- tibble::tibble(
  label = character(0),
  numberOfStrata = integer(0),
  baseSelection = character(0)
)

# Build a single PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# Convert "match on PS" settings to psConfigList
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

# Convert "stratify by PS" settings to psConfigList (none in our specifications)
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

# Iterate through all analysis setting combinations
cmAnalysisList <- list()
analysisId <- 1
targetComparatorOutcomesList <- list()

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create PS adjustment arguments per configuration
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

      # Covariate settings using FeatureExtraction defaults
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcomes: include the main outcome(s) and attach negative controls (if provided)
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

      # Target-Comparator-Outcomes (TCO) definitions
      # Excluded/Included covariate concept IDs reflect the Analysis Specifications covariateSelection object,
      # which contains null/empty placeholders. We sanitize by dropping NAs through .safe_ids().
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = .safe_ids(c(
            cmTcList$targetConceptId[i],
            cmTcList$comparatorConceptId[i],
            excludedCovariateConcepts$conceptId
          )),
          includedCovariateConceptIds = .safe_ids(includedCovariateConcepts$conceptId)
        )
      }

      # Get DB CohortMethod data args:
      # Analysis Specifications -> getDbCohortMethodDataArgs:
      #   studyPeriods[{ start: null, end: null }] and maxCohortSize = 0
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Create PS args as per Analysis Specifications
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        # Not specified in Analysis Specifications: estimator; we leave it to default behavior
        prior = Cyclops::createPrior(
          priorType = "laplace",
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

      # Covariate balance settings (retain template defaults)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Fit outcome model args based on Analysis Specifications fitOutcomeModelArgs
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
          fold = 10,
          cvRepetitions = 10,
          noiseLevel = "quiet",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # Study population args based on Analysis Specifications createStudyPopArgs
      # Note: time-at-risk arguments are sourced from timeAtRisks row 't'
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
        minDaysAtRisk = 1
      )

      # Append the analysis configuration per combination of Study Period, TAR, and PS config
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
    }
  }
}

# Wrap CohortMethod module specifications
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
# Study name per Analysis Specifications: "ceeamos"
analysisSpecifications <- Strategus::createEmptyAnalysisSpecificiations() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  { if (!is.null(negativeControlsShared)) Strategus::addSharedResources(., negativeControlsShared) else . } |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save to JSON under inst/ceeamos
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "ceeamos", "ceeamosAnalysisSpecification.json")
)