################################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script builds a Strategus analysis specification JSON for the
# "tramadolcodein" study based on the provided analysis settings.
#
# IMPORTANT:
# - Many cohort / concept IDs in this template are placeholders (0, 1234567,
#   0000000, etc.). Replace them with the real IDs from your Atlas / concept
#   sets before running Strategus.
# - This script is intentionally verbose in comments so users can understand how
#   the JSON settings are mapped from the Analysis Specifications.
################################################################################

# Required libraries -----------------------------------------------------------
library(dplyr)
library(Strategus)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)
library(ROhdsiWebApi)
library(ParallelLogger)

# Study name (used for file naming)
studyName <- "tramadolcodein"

# Shared Resources -------------------------------------------------------------
# Base WebAPI URL: replace with your Atlas/WebAPI endpoint if needed.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# ------------------------------------------------------------------------------
# Cohort Definitions
# ------------------------------------------------------------------------------
# NOTE:
# The analysis specification provided did not include concrete cohort IDs.
# We include placeholder cohort IDs here (0000000, 1111111, 2222222).
# Replace these with real cohort IDs (as integers) exported from Atlas.
#
# Order and re-numbering:
# - After exporting, we re-number the cohorts so that:
#   target cohort => cohortId = 1
#   comparator cohort => cohortId = 2
#   outcome cohort => cohortId = 3
#
# This numbering convention simplifies references later on.
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    0000000, # Target: REPLACE_WITH_TARGET_COHORT_ID
    1111111, # Comparator: REPLACE_WITH_COMPARATOR_COHORT_ID
    2222222  # Outcome: REPLACE_WITH_OUTCOME_COHORT_ID
  ),
  generateStats = TRUE
)

# Re-number cohorts so that target=1, comparator=2, outcome=3
cohortDefinitionSet[cohortDefinitionSet$cohortId == 0000000, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1111111, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 2222222, ]$cohortId <- 3

# ------------------------------------------------------------------------------
# Negative control outcomes
# ------------------------------------------------------------------------------
# The analysis spec did not include a concrete negative control concept set ID.
# Include a placeholder (1234567) below and replace with the real concept set ID.
#
# The pipeline will:
# 1. retrieve the concept set definition,
# 2. resolve it (add descendants if specified in the concept set),
# 3. convert the resolved concepts into the format expected by Strategus,
#    and offset their cohortIds to start at 101 (so they don't collide with
#    target/comparator/outcome cohort ids 1,2,3).
negativeControlConceptSetId <- 1234567 # REPLACE_WITH_NEGATIVE_CONTROL_CONCEPT_SET_ID

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

# Sanity check: ensure no duplicate cohort IDs
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found between cohortDefinitionSet and negativeControlOutcomeCohortSet ***")
}

# ------------------------------------------------------------------------------
# Build lists of cohorts used in analyses
# ------------------------------------------------------------------------------
# Outcomes (the primary outcome cohort exported above was re-numbered to 3)
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId,
         outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  # create the "clean window" (look-back to define incident events). The
  # Analysis Specifications requested a priorOutcomeLookBack of 365 days,
  # so we mirror that here as the cleanWindow for the cohort outcome config.
  mutate(cleanWindow = 365)

# Target / Comparator mapping
# Replaced with placeholder names; update targetCohortName and comparatorCohortName
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target cohort",    # REPLACE with descriptive name
  comparatorCohortId = 2,
  comparatorCohortName = "comparator cohort", # REPLACE with descriptive name
  # The following two columns are placeholders for potential excluded covariate
  # concept ids representing the drugs of interest themselves. Replace with
  # actual concept ids used to define target/comparator concept sets if you
  # want them excluded explicitly from the PS model.
  targetConceptId = c(2345678),
  comparatorConceptId = c(3456789)
)

# If you want to exclude specific covariate concept ids from LPS (e.g. the
# target/comparator drug concept sets), define them here. This template
# includes example placeholders; replace or remove as needed.
excludedCovariateConcepts <- data.frame(
  conceptId = c(2345678, 3456789), # placeholders: replace with real concept ids
  conceptName = c("target concept name", "comparator concept name")
)

# Optional: includedCovariateConcepts (not used here, but left as a reference)
# includedCovariateConcepts <- data.frame(
#   conceptId = c(),
#   conceptName = c()
# )

# CohortGeneratorModule --------------------------------------------------------
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

# CohortDiagnosticsModule -----------------------------------------------------
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
# The provided Analysis Specifications had:
# - studyPeriods with null start/end (meaning "no restriction"). Following the
#   template guidance, we set empty strings to indicate "no restriction".
studyPeriods <- tibble(
  studyStartDate = c(""), # if you want to restrict by date, use "YYYYMMDD"
  studyEndDate   = c("")
)

# The Analysis Specifications defined two time-at-risk windows:
# 1) riskWindowStart = 0, startAnchor = "cohort start", riskWindowEnd = 0,
#    endAnchor = "cohort end" (this typically represents an "on-treatment"
#    definition when combined with censoring at treatment end — here we map
#    it as a window anchored to start/end accordingly)
# 2) riskWindowStart = 0, startAnchor = "cohort start", riskWindowEnd = 9999,
#    endAnchor = "cohort start" (this represents an ITT-style window)
#
# We'll create two TARs and include minDaysAtRisk = 1 as specified.
timeAtRisks <- tibble(
  label = c("on_treatment", "intent_to_treat"),
  riskWindowStart = c(0, 0),
  startAnchor = c("cohort start", "cohort start"),
  riskWindowEnd = c(0, 9999),
  endAnchor = c("cohort end", "cohort start"),
  minDaysAtRisk = c(1, 1)
)

# Propensity Score settings ----------------------------------------------------
# The Analysis Specifications defined a single PS setting using matching:
# - maxRatio = 1 (1:1)
# - caliper = 0.2
# - caliperScale = "standardized logit"
#
# We create a tibble with that single row so the general template logic can
# convert it into a psConfigList entry.
matchOnPsArgsList <- tibble(
  label = c("match_1_to_1_caliper_0.2"),
  maxRatio = c(1),
  caliper = c(0.2),
  caliperScale = c("standardized logit") # options: "propensity score" | "standardized" | "standardized logit"
)

# No stratify-based PS settings provided in the analysis specification:
stratifyByPsArgsList <- tibble(
  label = character(),
  numberOfStrata = integer(),
  baseSelection = character()
)

# Build the generic psConfigList used in the nested analysis build loop.
psConfigList <- list()

# Convert matchOnPsArgsList rows to entries in psConfigList
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

# Convert stratifyByPsArgsList rows (none for this study)
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

# Build CohortMethod Analyses --------------------------------------------------
cmAnalysisList <- list()
analysisId <- 1

# Outer loop: study periods (here only one entry with empty strings)
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  # Time-at-risk loop
  for (t in seq_len(nrow(timeAtRisks))) {
    # For each PS configuration
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]

      # Build PS adjustment arguments depending on method
      if (psCfg$method == "match") {
        # CohortMethod::createMatchOnPsArgs supports caliperScale values:
        # "propensity score", "standardized", "standardized logit"
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

      # Covariate settings used to create covariates for PS model and balance
      # Default covariates are used; addDescendantsToExclude=TRUE ensures that
      # when we pass covariates-to-exclude (e.g. drug concept IDs), their
      # descendants are also excluded.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )

      # Prepare the outcome list: combine outcome(s) of interest and negative
      # control outcomes.
      outcomeList <- append(
        # Primary outcomes exported from cohortDefinitionSet (oList)
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            # The template used a very large lookback for priorOutcomeLookback in
            # the createOutcome (99999). This is a configuration related to PS
            # diagnostics; we keep it very large to avoid excluding outcomes
            # based on short lookbacks during PS diagnostics.
            priorOutcomeLookback = 99999
          )
        }),
        # Negative controls created from resolved concepts (cohortIds 101,102,...)
        lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
          CohortMethod::createOutcome(
            outcomeId = i,
            outcomeOfInterest = FALSE,
            trueEffectSize = 1
          )
        })
      )

      # Build the targetComparatorOutcomes structure for each TC pair.
      targetComparatorOutcomesList <- list()
      for (i in seq_len(nrow(cmTcList))) {
        # excludedCovariateConceptIds will combine:
        # - explicit target/comparator concept ids (if provided), and
        # - other excluded covariate concept ids (e.g., drug concept ids)
        excludedIds <- c(
          cmTcList$targetConceptId[i],
          cmTcList$comparatorConceptId[i],
          excludedCovariateConcepts$conceptId
        )
        # Remove NAs in case placeholders were not set
        excludedIds <- excludedIds[!is.na(excludedIds)]

        targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedIds
        )
      }

      # getDbCohortMethodDataArgs: map study restriction and covariate settings
      # The Analysis Specifications used:
      # - restrictToCommonPeriod = TRUE (in the template example; the provided
      #   Analysis Specification for getDbCohortMethodDataArgs had studyPeriods
      #   unspecified and maxCohortSize = 0).
      # We will set restrictToCommonPeriod = TRUE so that cohorts are restricted
      # to the database overlap period unless you change it.
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = ifelse(studyStartDate == "", NULL, studyStartDate),
        studyEndDate = ifelse(studyEndDate == "", NULL, studyEndDate),
        maxCohortSize = 0, # Analysis Specifications: maxCohortSize = 0 (no truncation)
        covariateSettings = covariateSettings
      )

      # Create PS arguments (regularized logistic regression via Cyclops)
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,            # from Analysis Specifications
        errorOnHighCorrelation = TRUE,               # from Analysis Specifications
        stopOnError = FALSE,                         # allow pipeline to continue if PS fit fails
        estimator = "att",                           # map to ATT estimator for weighting if needed (kept as template)
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
          noiseLevel = "silent",     # silent, as per the Analysis Specifications for createPsArgs
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )

      # Covariate balance computation arguments
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Fit outcome model arguments (Cox model specs from Analysis Specifications)
      fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
        modelType = "cox",
        stratified = FALSE,         # Analysis Specifications: stratified = false
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

      # CreateStudyPopulationArgs: map the TAR-specific settings from the
      # Analysis Specifications and the general createStudyPopArgs block.
      # Analysis Specifications:
      # - restrictToCommonPeriod: false
      # - firstExposureOnly: false
      # - washoutPeriod: 0
      # - removeDuplicateSubjects: "keep all"
      # - censorAtNewRiskWindow: false
      # - removeSubjectsWithPriorOutcome: true
      # - priorOutcomeLookBack: 365
      # - minDaysAtRisk: 1 (applied per TAR)
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
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        maxDaysAtRisk = 99999
      )

      # Append the CohortMethod analysis configuration to the list. Each
      # configuration is uniquely identified by analysisId and contains all
      # arguments required by the CohortMethod module in Strategus.
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "Study: %s-%s; TAR: %s; PS: %s",
          ifelse(studyStartDate == "", "noStudyStart", studyStartDate),
          ifelse(studyEndDate == "", "noStudyEnd", studyEndDate),
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
    } # end PS configs loop
  } # end TAR loop
} # end study periods loop

# Create the CohortMethodModule specifications
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# Assemble the full analysis specifications -----------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecificiations() %>%
  Strategus::addSharedResources(cohortDefinitionShared) %>%
  Strategus::addSharedResources(negativeControlsShared) %>%
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) %>%
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save the specifications to JSON to the inst/<studyName>/ folder ---------------
outputFolder <- file.path("inst", studyName)
if (!dir.exists(outputFolder)) dir.create(outputFolder, recursive = TRUE)

outputFile <- file.path(outputFolder, sprintf("%sAnalysisSpecification.json", studyName))
ParallelLogger::saveSettingsToJson(analysisSpecifications, outputFile)

message("Analysis specification saved to: ", outputFile)
message("NOTE: Review this file and replace any placeholder cohort/concept IDs before use.")
################################################################################