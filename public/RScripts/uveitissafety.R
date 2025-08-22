################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script builds a Strategus analysis specification using the OHDSI HADES
# modules. It follows the Template provided and applies settings from the
# Analysis Specifications block.
#
# How to use this file:
# - Replace the placeholder IDs (0000000, 1111111, 2222222, 1234567) with your
#   actual ATLAS/WebAPI IDs for target, comparator, outcome, and negative
#   control concept set.
# - Update the baseUrl if you are not using the OHDSI demo WebAPI.
# - The settings in the CohortMethod section have been aligned to the provided
#   Analysis Specifications. Inline comments show how each parameter is mapped.
################################################################################

# Libraries --------------------------------------------------------------------
library(dplyr)
library(tibble)
library(ROhdsiWebApi)
library(Strategus)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)
library(ParallelLogger)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from your WebAPI. Update baseUrl and cohort IDs.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# NOTE:
#  - Replace 0000000, 1111111, 2222222 with your actual cohort IDs.
#  - The Analysis Specifications indicate target/comparator/outcome cohort IDs
#    are null/empty. These are left as placeholders to be provided by the user.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    0000000, # Target: (fill in your ATLAS cohort ID)
    1111111, # Comparator: (fill in your ATLAS cohort ID)
    2222222  # Outcome: (fill in your ATLAS cohort ID)
  ),
  generateStats = TRUE
)

# Re-number cohorts locally to 1 (target), 2 (comparator), 3 (outcome)
# This standardized numbering is used by cohortMethodModuleSpecifications later.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 0000000, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1111111, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 2222222, ]$cohortId <- 3

# Negative control outcomes ----------------------------------------------------
# NOTE:
#  - Replace 1234567 with your Concept Set ID for negative control outcomes.
#  - The Analysis Specifications specify an empty negativeControlConceptSet; we
#    keep a placeholder and assign cohort IDs starting at 101 to avoid overlap.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1234567, # (fill in your ATLAS Concept Set ID)
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
  mutate(cohortId = dplyr::row_number() + 100L) %>% # negative control ids: 101, 102, ...
  select(cohortId, cohortName, outcomeConceptId)

# Basic safety check to avoid ID collisions between main cohorts and NCs
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found across primary cohorts and negative control cohorts ***")
}

# Prepare data frames used in CohortMethod analyses ----------------------------
# Outcomes: Using the re-numbered outcome cohort with ID 3
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # As in Template (not specified in Analysis Specifications)

# Target and Comparator for CohortMethod analysis
# NOTE:
# - Names here are placeholders; they do not affect execution but improve logs.
cmTcList <- tibble::tibble(
  targetCohortId = 1L,
  targetCohortName = "target cohort name",
  comparatorCohortId = 2L,
  comparatorCohortName = "comparator cohort name",
  # These optional columns allow excluding drugs of interest from covariates.
  # Fill with concept IDs if desired, or leave as NA.
  targetConceptId = as.numeric(NA),
  comparatorConceptId = as.numeric(NA)
)

# Covariate selection (include/exclude) ----------------------------------------
# The Analysis Specifications define concept sets to include/exclude but they
# are empty. Below are placeholders to be filled with Concept IDs.
# - conceptsToInclude: use if you intend to include only specific covariates.
# - conceptsToExclude: use to exclude specific covariate concepts.
includedCovariateConcepts <- tibble::tibble(
  conceptId = as.integer(c()), # e.g., c(12345, 67890)
  conceptName = as.character(c())
)
excludedCovariateConcepts <- tibble::tibble(
  conceptId = as.integer(c()), # e.g., c(2345678, 3456789)
  conceptName = as.character(c())
)

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()

# Share cohort definitions (retrieved above)
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)

# Share negative control outcomes
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)

# Configure CohortGenerator to generate stats (as in Template)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnosticsModule Settings --------------------------------------------
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
# Study Periods:
# Analysis Specifications: "studyPeriods": [{ "studyStartDate": null, "studyEndDate": null }]
# Strategy:
# - Use a single row with NA (or empty string) when no restriction is desired.
studyPeriods <- tibble::tibble(
  studyStartDate = NA_character_, # leave NA/empty if not restricting study window
  studyEndDate   = NA_character_
)

# Time-at-risk (TAR):
# Analysis Specifications:
#   riskWindowStart = 1, startAnchor = "cohort start",
#   riskWindowEnd = 0, endAnchor = "cohort end", minDaysAtRisk = 1
timeAtRisks <- tibble::tibble(
  label = c("Primary TAR"),
  riskWindowStart = c(1),
  startAnchor = c("cohort start"), # "cohort start" | "cohort end"
  riskWindowEnd = c(0),
  endAnchor = c("cohort end")      # "cohort start" | "cohort end"
)

# Propensity Score settings:
# Analysis Specifications:
#   psSettings: matchOnPsArgs -> maxRatio = 10, caliper = 0.2, caliperScale = "standardized logit"
#   stratifyByPsArgs = null
matchOnPsArgsList <- tibble::tibble(
  label = c("Match caliper 0.2 standardized logit"),
  maxRatio = c(10),
  caliper = c(0.2),
  caliperScale = c("standardized logit") # "propensity score" | "standardized" | "standardized logit"
)

# No PS stratification specified
stratifyByPsArgsList <- tibble::tibble(
  label = character(0),
  numberOfStrata = integer(0),
  baseSelection = character(0) # "all" | "target" | "comparator"
)

# Build a single PS configuration list (each element: method, label, params) ---
psConfigList <- list()

# Convert "match on PS" rows to config entries
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

# Convert "stratify by PS" rows to config entries (none in this spec)
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

# Iterate through all analysis setting combinations -----------------------------
cmAnalysisList <- list()
analysisId <- 1L

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Match vs Stratify PS args
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

      # Covariate settings used when constructing CohortMethodData
      # - We use defaults and rely on excludedCovariateConceptIds in createTargetComparatorOutcomes
      # - You can optionally use includedCovariateConceptIds/excludedCovariateConceptIds here as well.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Outcomes of interest (real + negative controls)
      outcomeList <- append(
        # Primary outcome(s) of interest
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999
          )
        }),
        # Negative controls
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )

      # Target-comparator pair with exclusions of concepts if provided
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        # Merge all excluded concept ids from T/C concepts and user-defined exclusions
        excludedCovariateConceptIds <- unique(na.omit(c(
          cmTcList$targetConceptId[i],
          cmTcList$comparatorConceptId[i],
          excludedCovariateConcepts$conceptId
        )))

        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConceptIds
        )
      }

      # Get CohortMethod data args ---------------------------------------------
      # Analysis Specifications: getDbCohortMethodDataArgs
      # - studyStartDate / studyEndDate from studyPeriods (NA => no restriction)
      # - maxCohortSize = 0 (no sampling)
      # - restrictToCommonPeriod appears in Template under getDb, but this
      #   restriction is applied in createStudyPopulation in the specification.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        # NOTE: 'restrictToCommonPeriod' is not a parameter of this function in CohortMethod;
        # it's kept commented here to reflect the Template vs actual API.
        # restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Create PS args ---------------------------------------------------------
      # Analysis Specifications: propensityScoreAdjustment.createPsArgs
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # Following Template choice to allow pipeline completion
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

      # Balance args (shared and full) ----------------------------------------
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Fit outcome model args -------------------------------------------------
      # Analysis Specifications: fitOutcomeModelArgs
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

      # Create study population args ------------------------------------------
      # Analysis Specifications: createStudyPopArgs
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = TRUE,
        firstExposureOnly = TRUE,
        washoutPeriod = 365,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = TRUE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 99999,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1
      )

      # Append this analysis configuration ------------------------------------
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
      analysisId <- analysisId + 1L
    }
  }
}

# Build the CohortMethod module specification ----------------------------------
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Create the final analysis specifications -------------------------------------
# Study name from Analysis Specifications: "name": "uveitissafety"
analysisSpecifications <- Strategus::createEmptyAnalysisSpecificiations() |>
  Strategus::addSharedResources(cohortDefinitionShared) |>
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save to JSON (uses the study name as directory and file prefix)
ParallelLogger::saveSettingsToJson(
  analysisSpecifications,
  file.path("inst", "uveitissafety", "uveitissafetyAnalysisSpecification.json")
)