################################################################################
# CreateStrategusAnalysisSpecification.R
#
# Purpose:
# - Create an analysis specification JSON file for use with the Strategus package
#   following the "uveitissafety" study settings provided in the Analysis
#   Specifications.  This script follows the Strategus template and contains
#   detailed comments to explain how each setting is applied.
#
# IMPORTANT:
# - Several identifiers (cohort definition ids, negative control concept set id,
#   and any concept ids for included/excluded covariates) are left as placeholders
#   (NA or 0000000) below because the Analysis Specifications file provided
#   contained nulls for these fields. Replace those placeholders with the
#   appropriate numeric ids from your Atlas/WebAPI instance before running.
#
# Requirements:
# - R packages: Strategus, CohortMethod, FeatureExtraction, Cyclops, ROhdsiWebApi,
#   dplyr, ParallelLogger
#
################################################################################

library(dplyr)
library(Strategus)
library(ROhdsiWebApi)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)
library(ParallelLogger)

# STUDY NAME AND WEBAPI -------------------------------------------------------
studyName <- "uveitissafety"

# WebAPI base URL - change to your Atlas/WebAPI endpoint
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# COHORT DEFINITIONS ---------------------------------------------------------
# NOTE: The Analysis Specifications had null ids for the cohorts. Replace the
# placeholders below with the cohortIds exported from your Atlas/WebAPI.
#
# Example:
#   Replace TARGET_ATLAS_COHORT_ID, COMPARATOR_ATLAS_COHORT_ID, OUTCOME_ATLAS_COHORT_ID
#   with integer ids from Atlas -> Cohort Definitions page (the numeric id shown
#   in the URL or the cohort list).
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    0000000, # Target:   <-- REPLACE with Atlas cohort id for target
    1111111, # Comparator:   <-- REPLACE with Atlas cohort id for comparator
    2222222  # Outcome:   <-- REPLACE with Atlas cohort id for outcome
  ),
  generateStats = TRUE
)

# Re-number cohorts so they start from 1, 2, 3 for internal use in this study.
# This is convenient when constructing target/comparator/outcome mappings.
# If you used real atlas ids above, change the numbers in the comparisons below
# to the corresponding atlas ids you provided.
cohortDefinitionSet[cohortDefinitionSet$cohortId == 0000000, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1111111, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 2222222, ]$cohortId <- 3

# NEGATIVE CONTROLS ----------------------------------------------------------
# The Analysis Specifications provided a negativeControlConceptSet with null id.
# Replace NEG_CONTROL_CONCEPTSET_ID with your Atlas concept set id for negative
# controls (if you have one). If you do not have negative controls, you can
# omit this block or leave the variable as NULL.
negControlConceptSetId <- 1234567  # <-- REPLACE with your negative control concept set id (or set to NULL)

negativeControlOutcomeCohortSet <- NULL
if (!is.null(negControlConceptSetId)) {
  negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
    conceptSetId = negControlConceptSetId,
    baseUrl = baseUrl
  ) %>%
    ROhdsiWebApi::resolveConceptSet(baseUrl = baseUrl) %>%
    ROhdsiWebApi::getConcepts(baseUrl = baseUrl) %>%
    rename(outcomeConceptId = "conceptId",
           cohortName = "conceptName") %>%
    mutate(cohortId = row_number() + 100) %>% # put negative controls starting at id 101, 102, ...
    select(cohortId, cohortName, outcomeConceptId)
}

# Basic checks to ensure no cohort id duplication
if (!is.null(negativeControlOutcomeCohortSet)) {
  if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
    stop("*** Error: duplicate cohort IDs found between main cohorts and negative controls ***")
  }
}

# OUTCOME LIST (primary outcomes defined in cohortDefinitionSet) --------------
# We expect that the third cohort (cohortId == 3) corresponds to the outcome.
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365) # Clean window as in template (365 days)

# TARGET / COMPARATOR LIST ----------------------------------------------------
# The cohortDefinitionSet was re-numbered such that target = 1 and comparator = 2
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = cohortDefinitionSet %>% filter(.data$cohortId == 1) %>% pull(.data$cohortName),
  comparatorCohortId = 2,
  comparatorCohortName = cohortDefinitionSet %>% filter(.data$cohortId == 2) %>% pull(.data$cohortName),
  # If you have specific conceptIds to exclude for the target/comparator (e.g. drug concept ids),
  # add them here as targetConceptId and comparatorConceptId. The Analysis Specifications
  # did not include specific conceptIds, so we leave them as NA placeholders.
  targetConceptId = NA_integer_,
  comparatorConceptId = NA_integer_,
  stringsAsFactors = FALSE
)

# EXCLUDED/INCLUDED COVARIATES -----------------------------------------------
# The Analysis Specifications included empty lists for conceptsToInclude / exclude.
# If you want to exclude drug concepts (e.g., target or comparator ingredients),
# fill excludedCovariateConcepts with their conceptIds & names. For now, leave empty.
excludedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character(),
  stringsAsFactors = FALSE
)

# If you instead want to explicitly include a set of covariates, create
# includedCovariateConcepts data.frame. Left empty here by default:
includedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character(),
  stringsAsFactors = FALSE
)

# COHORT GENERATOR MODULE SHARED RESOURCES -----------------------------------
cg <- CohortGeneratorModule$new()
cohortDefinitionShared <- cg$createCohortSharedResourceSpecifications(cohortDefinitionSet)

negativeControlsShared <- NULL
if (!is.null(negativeControlOutcomeCohortSet)) {
  negativeControlsShared <- cg$createNegativeControlOutcomeCohortSharedResourceSpecifications(
    negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
    occurrenceType = "first",    # use first occurrence of the negative control concept
    detectOnDescendants = TRUE
  )
}

cohortGeneratorModuleSpecifications <- cg$createModuleSpecifications(generateStats = TRUE)

# COHORT DIAGNOSTICS MODULE --------------------------------------------------
cd <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cd$createModuleSpecifications(
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

# COHORT METHOD MODULE SETTINGS ----------------------------------------------
# STUDY PERIODS:
# The Analysis Specifications provided null study period dates. If you do not
# intend to restrict to a calendar period, leave studyPeriods empty. If you do,
# provide 'YYYYMMDD' strings.
studyPeriods <- tibble::tibble(
  studyStartDate = character(),  # e.g. "20100101"
  studyEndDate   = character()   # e.g. "20211231"
)
# NOTE: Because the createStudyPopulationArgs in the Analysis Specifications
# sets restrictToCommonPeriod = TRUE, you may want to supply studyPeriods if
# you truly need to restrict to a particular calendar window. Otherwise leave empty.

# TIME-AT-RISK (TAR)
# The Analysis Specifications defines a single TAR:
#   riskWindowStart = 1 (day after cohort start),
#   startAnchor = "cohort start",
#   riskWindowEnd = 0 (cohort end),
#   endAnchor = "cohort end",
#   minDaysAtRisk = 1
timeAtRisks <- tibble::tibble(
  label = "1_to_end",
  riskWindowStart = 1,
  startAnchor = "cohort start",
  riskWindowEnd = 0,
  endAnchor = "cohort end"
)

# PROPENSITY SCORE CONFIGURATION ----------------------------------------------
# The Analysis Specifications provide a single PS setting (match on PS):
#   maxRatio = 10, caliper = 0.2, caliperScale = "standardized logit"
matchOnPsArgsList <- tibble::tibble(
  label = "match_1to10_caliper0.2_stdlogit",
  maxRatio = 10,
  caliper = 0.2,
  caliperScale = "standardized logit"
)

# Build psConfigList from the matchOnPsArgsList
psConfigList <- list()
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

# BUILD CohortMethod analyses -------------------------------------------------
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(max(1, nrow(studyPeriods)))) { # if studyPeriods empty, loop once
  studyStartDate <- if (nrow(studyPeriods) > 0) studyPeriods$studyStartDate[s] else NULL
  studyEndDate   <- if (nrow(studyPeriods) > 0) studyPeriods$studyEndDate[s] else NULL

  for (t in seq_len(nrow(timeAtRisks))) {
    # For each PS configuration
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Create matchOnPsArgs or stratifyByPsArgs depending on method
      if (psCfg$method == "match") {
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

      # COVARIATE SETTINGS: use default covariates and add descendants to exclude if needed.
      # If you have includedCovariateConcepts (explicit list to include), you may
      # want to construct custom covariateSettings via FeatureExtraction::createCovariateSettings.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # OUTCOME DEFINITIONS: combine outcomes from main outcome list and negative controls.
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA_real_,
            priorOutcomeLookback = 99999
          )
        }),
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
      )

      # TARGET/COMPARATOR/OUTCOMES mapping for CohortMethod:
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          # Exclude target/comparator concept ids (if available) and any additional excluded concepts
          excludedCovariateConceptIds = unique(na.omit(c(
            cmTcList$targetConceptId[i],
            cmTcList$comparatorConceptId[i],
            excludedCovariateConcepts$conceptId
          )))
        )
      }

      # getDbCohortMethodDataArgs: get cohort-method-ready data from the CDM
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,          # from Analysis Specifications
        studyStartDate = studyStartDate,        # NULL if not provided
        studyEndDate = studyEndDate,            # NULL if not provided
        maxCohortSize = 0,                      # no size cap per specs
        covariateSettings = covariateSettings
      )

      # createPsArgs: create propensity score model-fitting options
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE, # allow Strategus to continue and collect diagnostics if PS model fails
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
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # Covariate balance arguments
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # fitOutcomeModelArgs: use Cox model, stratified, no covariates (PS-matched or stratified)
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

      # createStudyPopArgs: follow the Analysis Specifications values
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = TRUE,        # restrict to common observation period as specified
        firstExposureOnly = TRUE,             # only first exposure per person included
        washoutPeriod = 365,                  # 365 day washout (prior observation)
        removeDuplicateSubjects = "keep all", # keep all if a person is in multiple cohorts (per specs)
        censorAtNewRiskWindow = TRUE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 99999,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1,
        maxDaysAtRisk = 99999
      )

      # Append the configured CohortMethod analysis to the list
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s; TAR: %s; PS: %s",
          studyName,
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
    } # end ps configs loop
  } # end timeAtRisks loop
} # end studyPeriods loop

# Create CohortMethod module specifications
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# COMBINE INTO ANALYSIS SPECIFICATIONS ---------------------------------------
# Create an empty analysis specifications object, add shared resources and modules
analysisSpecifications <- Strategus::createEmptyAnalysisSpecifications() |>
  Strategus::addSharedResources(cohortDefinitionShared)

if (!is.null(negativeControlsShared)) {
  analysisSpecifications <- analysisSpecifications |> Strategus::addSharedResources(negativeControlsShared)
}

analysisSpecifications <- analysisSpecifications |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# SAVE TO JSON ---------------------------------------------------------------
# Save the specifications to inst/<studyName>/<studyName>AnalysisSpecification.json
# so that the package (or study folder) can be used by Strategus workers.
outputFile <- file.path("inst", studyName, paste0(studyName, "AnalysisSpecification.json"))
ParallelLogger::saveSettingsToJson(analysisSpecifications, outputFile)

# Final informational message (printed when script executes)
message("Strategus analysis specification JSON created at: ", outputFile)
message("NOTE: Replace placeholder cohort ids and concept set ids with real ids before executing the study.")