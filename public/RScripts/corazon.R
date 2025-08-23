################################################################################
# See the Create analysis specifications section
# of the UsingThisTemplate.md for more details.
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# This help page also contains links to the corresponding HADES package that
# further details.
# ##############################################################################
library(dplyr)
library(Strategus)

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts from your ATLAS (replace placeholders below).
# Note: Keep the names and structure exactly as in this template, and update the
# IDs to your own ATLAS cohort IDs. We re-number them locally to 1,2,3 for use
# downstream in the analysis (Target=1, Comparator=2, Outcome=3).
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1794126, # Target: replace with your ATLAS ID
    1794132, # Comparator: replace with your ATLAS ID
    1794131  # Outcome: replace with your ATLAS ID
  ),
  generateStats = TRUE
)

# Re-number cohorts to internal IDs (1=target, 2=comparator, 3=outcome)
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794126, ]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794132, ]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1794131, ]$cohortId <- 3

# Negative control outcomes:
# Replace conceptSetId with your negative control concept set ID from ATLAS.
# Note: The Analysis Specifications leave this unspecified; this block shows how
# to resolve a concept set into outcomeConceptIds that will be used as negative
# control outcomes in CohortMethod.
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1888110, # TODO: replace with your concept set ID
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
  mutate(cohortId = dplyr::row_number() + 100) %>% # negative control IDs start at 101+
  select(cohortId, cohortName, outcomeConceptId)

# Safety check: ensure no overlap between analysis cohorts and negative controls
if (length(intersect(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)) > 0) {
  stop("*** Error: duplicate cohort IDs found between main cohorts and negative controls ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis -------
# Outcomes: select the (renumbered) outcome cohort (ID=3) and apply a clean window
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis 
# Note: Names can be updated, but IDs must remain 1 and 2 as per re-numbering above.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target cohort name",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator cohort name",
  # Optional: If you know the specific ingredient/therapy concept IDs for the target and comparator,
  # specify them here. They can be used to exclude exposures from covariates explicitly.
  targetConceptId = NA_integer_,
  comparatorConceptId = NA_integer_
)

# Covariate selection settings -------------------------------------------------
# The Analysis Specifications provided slots for conceptsToInclude and conceptsToExclude.
# Populate the data frames below with your concept IDs to control covariate inclusion/exclusion.
# "addDescendantsToInclude/Exclude = TRUE" will include/exclude descendants automatically.
includedCovariateConcepts <- data.frame(
  conceptId = integer(),   # e.g., c(123, 456)
  conceptName = character()# e.g., c("include A", "include B")
)

excludedCovariateConcepts <- data.frame(
  conceptId = c(           # e.g., c(2345678, 3456789) to exclude exposures
    # 2345678, 3456789
  ),
  conceptName = c(
    # "target concept name", "comparator concept name"
  )
)

# CohortGeneratorModule --------------------------------------------------------
# Creates and generates cohorts and negative control outcome concept-based cohorts
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

# CohortDiagnoticsModule Settings ---------------------------------------------
# Runs diagnostic summaries for the cohorts defined above
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
# This section implements the Analysis Specifications for:
# - study periods (getDbCohortMethodDataArgs)
# - time-at-risk windows (createStudyPopArgs)
# - PS adjustment strategies (propensityScoreAdjustment)
# - PS fitting args (createPsArgs)
# - outcome model args (fitOutcomeModelArgs)

# Study periods: each row corresponds to one data extraction period
# From Analysis Specifications: 
#   1) 20100101 - 20191231
#   2) 20120101 - 20191231
studyPeriods <- tibble::tibble(
  studyStartDate = c("20100101", "20120101"),
  studyEndDate   = c("20191231", "20191231")
)

# Time-at-risks (TARs) for the outcomes of interest in your study
# From Analysis Specifications:
# TAR 1: start=cohort start, end=cohort end
# TAR 2: start=cohort start, end=9999 days from start
timeAtRisks <- tibble::tibble(
  label = c(
    "TAR1: cohort start -> cohort end",
    "TAR2: 0-9999 days from cohort start"
  ),
  riskWindowStart = c(0, 0),
  startAnchor = c("cohort start", "cohort start"), # "cohort start" | "cohort end"
  riskWindowEnd = c(0, 9999),
  endAnchor = c("cohort end", "cohort start")      # "cohort start" | "cohort end"
)

# Propensity Score settings - stratify by PS
# From Analysis Specifications: numberOfStrata = 5, baseSelection = "all"
stratifyByPsArgsList <- tibble::tibble(
  label = c("Stratify PS: 5 strata (all)"),
  numberOfStrata = c(5),
  baseSelection = c("all") # "all" | "target" | "comparator"
)

# Propensity Score settings - match on PS
# From Analysis Specifications: maxRatio = 0, caliper = 0.2, caliperScale = "standardized logit"
# Note: maxRatio=0 is an edge case; adjust if needed for your design.
matchOnPsArgsList <- tibble::tibble(
  label = c("Match PS: caliper 0.2 (standardized logit)"),
  maxRatio = c(0),
  caliper = c(0.2),
  caliperScale = c("standardized logit") # "propensity score" | "standardized" | "standardized logit"
)

# Build a single PS configuration list (each entry has: method, label, params)
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

# Convert "stratify by PS" rows to config entries
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

# Iterate through all analysis setting combinations
cmAnalysisList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]

  for (t in seq_len(nrow(timeAtRisks))) {

    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # Configure the PS adjustment approach for this analysis cell
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
      }

      # Covariate settings: respect include/exclude lists and add descendants
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        excludedCovariateConceptIds = excludedCovariateConcepts$conceptId,
        includedCovariateConceptIds = includedCovariateConcepts$conceptId,
        addDescendantsToExclude = TRUE,
        addDescendantsToInclude = TRUE
      )

      # Build outcomes: includes the main outcome(s) and negative control outcomes
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999 # from Analysis Specifications
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

      # Build target-comparator-outcome sets, excluding exposure concepts if provided
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

      # Data extraction settings (getDbCohortMethodDataArgs)
      # From Analysis Specifications: maxCohortSize = 0 (no cap)
      # We pass studyStartDate/studyEndDate for the current study period row
      # Setting restrictToCommonPeriod = FALSE (aligns with createStudyPopArgs)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = FALSE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )

      # Propensity score model settings (createPsArgs)
      # From Analysis Specifications:
      # maxCohortSizeForFitting = 250000
      # errorOnHighCorrelation = TRUE
      # prior: laplace, useCrossValidation = TRUE
      # control: tolerance=2e-7, cvType="auto", fold=10, cvRepetitions=10,
      #          noiseLevel="silent", resetCoefficients=TRUE, startingVariance=0.01
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
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

      # Covariate balance computation settings
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )

      # Outcome model settings (fitOutcomeModelArgs)
      # From Analysis Specifications:
      # modelType="cox", stratified=TRUE, useCovariates=FALSE, inversePtWeighting=FALSE
      # prior: laplace useCrossValidation=TRUE
      # control: tolerance=2e-7, cvType="auto", fold=10, cvRepetitions=10, 
      #          noiseLevel="quiet", resetCoefficients=TRUE, startingVariance=0.01
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

      # Study population settings (createStudyPopArgs)
      # From Analysis Specifications:
      # restrictToCommonPeriod = FALSE
      # firstExposureOnly = FALSE
      # washoutPeriod = 0
      # removeDuplicateSubjects = "keep all"
      # censorAtNewRiskWindow = FALSE
      # removeSubjectsWithPriorOutcome = TRUE
      # priorOutcomeLookback = 99999
      # TARs (riskWindowStart/End, startAnchor, endAnchor) taken from timeAtRisks[t,]
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
        minDaysAtRisk = 1
      )

      # Append the settings to the analysis list: each combination of period, TAR, and PS method
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

# Bundle CohortMethod specifications for Strategus
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
# Assemble all shared resources and module specs into a single Strategus JSON
analysisSpecifications <- Strategus::createEmptyAnalysisSpecificiations() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

# Save to inst/<studyName>/<studyName>AnalysisSpecification.json
studyName <- "corazon"
outputDir <- file.path("inst", studyName)
if (!dir.exists(outputDir)) {
  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
}
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path(outputDir, paste0(studyName, "AnalysisSpecification.json"))
)