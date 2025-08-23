################################################################################
# See the Create analysis specifications section
# of the UsingThisTemplate.md for more details.
# 
# More information about Strategus HADES modules can be found at:
# https://ohdsi.github.io/Strategus/reference/index.html#omop-cdm-hades-modules.
# This help page also contains links to the corresponding HADES package that
# further details.
################################################################################

# Packages ---------------------------------------------------------------------
# We load the packages used across the script. These include:
# - Strategus: to define the analysis specification and modules
# - ROhdsiWebApi: to fetch cohorts and concept sets from an OHDSI WebAPI
# - CohortMethod, FeatureExtraction, Cyclops: to build CohortMethod settings
# - dplyr, tibble, magrittr: convenience for data manipulation
library(Strategus)
library(ROhdsiWebApi)
library(CohortMethod)
library(FeatureExtraction)
library(Cyclops)
library(dplyr)
library(tibble)
library(magrittr)

# Study Name from <Analysis Specifications> ------------------------------------
# Using the exact study name provided in the Analysis Specifications:
studyName <- "antivegfkidney"

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts
# Note: Replace baseUrl with your own ATLAS/WebAPI endpoint as needed.
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions -----------------------------------------------------------
# The <Analysis Specifications> do not provide concrete target/comparator/outcome IDs.
# For reproducibility, keep an empty default here and annotate the mapping logic:
# - If you have cohort IDs, populate the vector below and optionally rename to 1, 2, 3
#   for target, comparator, and outcome respectively (mirroring the Template).
#
# Example (uncomment and edit when you have IDs):
# cohortIds <- c(
#   1234567, # Target
#   2345678, # Comparator
#   3456789  # Outcome
# )
cohortIds <- c(1794126, 1794132, 1794131)

if (length(cohortIds) > 0) {
  cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
    baseUrl = baseUrl,
    cohortIds = cohortIds,
    generateStats = TRUE
  )
  
  # Optional: Renumber cohorts to 1,2,3 if desired to align with Template patterns:
  # Here we assume the input vector order is [Target, Comparator, Outcome]
  if (length(cohortIds) >= 3) {
    idMap <- tibble::tibble(
      oldId = cohortIds[1:3],
      newId = 1:3
    )
    for (i in seq_len(nrow(idMap))) {
      cohortDefinitionSet[cohortDefinitionSet$cohortId == idMap$oldId[i], ]$cohortId <- idMap$newId[i]
    }
  }
} else {
  # Create a minimal empty set if no cohorts have been specified yet.
  # Strategus modules that depend on cohorts will not run until cohorts are defined.
  cohortDefinitionSet <- tibble::tibble(
    cohortId = integer(),
    cohortName = character(),
    json = character(),
    sql = character()
  )
}

# Negative control outcomes ----------------------------------------------------
# The <Analysis Specifications> provide:
# "negativeControlConceptSet": { "id": null, "name": "" }
# Hence, we will not resolve any negative control concept set.
# If you have a conceptSetId for negative controls, put it below; otherwise we create an empty set.
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
      # Cohort IDs for negative controls: 101, 102, ...
      cohortId = dplyr::row_number() + 100L
    ) %>%
    dplyr::select(cohortId, cohortName, outcomeConceptId)
} else {
  negativeControlOutcomeCohortSet <- tibble::tibble(
    cohortId = integer(),
    cohortName = character(),
    outcomeConceptId = integer()
  )
}

# Check for duplicate cohort IDs across designed cohorts and negative controls.
# (This is a corrected check versus the Template’s duplicated call)
if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found across cohorts and negative controls ***")
}

# Create some data frames to hold the cohorts for each analysis ----------------
# Outcomes:
# The Template filters the outcome cohort to id == 3. Here, if you renumbered to 1/2/3, keep it.
# If not, adapt the filter below accordingly. If no outcome cohort exists yet, oList remains empty.
oList <- cohortDefinitionSet %>%
  dplyr::filter(.data$cohortId == 3) %>%
  dplyr::mutate(
    outcomeCohortId = cohortId,
    outcomeCohortName = cohortName
  ) %>%
  dplyr::select(outcomeCohortId, outcomeCohortName) %>%
  dplyr::mutate(cleanWindow = 365)

# Target and Comparator for the CohortMethod analysis 
# Per Template: set targetId = 1 and comparatorId = 2 if you renumbered.
# If your cohorts have different ids, update here accordingly.
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "target cohort name",
  comparatorCohortId = 2,
  comparatorCohortName = "comparator cohort name",
  # Add placeholders for concept IDs to match Template fields later (NA-safe handling below)
  targetConceptId = NA_integer_,
  comparatorConceptId = NA_integer_
)

# Covariate selection mapping from <Analysis Specifications> -------------------
# "covariateSelection": conceptsToInclude / conceptsToExclude both provided but null entries.
# We keep them empty here. If you have specific concept IDs to include/exclude, add them below.
includedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character()
)

excludedCovariateConcepts <- data.frame(
  conceptId = integer(),
  conceptName = character()
)

# CohortGeneratorModule --------------------------------------------------------
# Shared resources for cohorts and negative controls (if any)
cgModuleSettingsCreator <- CohortGeneratorModule$new()

cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)

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

# CohortDiagnoticsModule Settings ---------------------------------------------
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
# Map <Analysis Specifications> to CM settings with detailed annotations.

# 1) Study periods from <Analysis Specifications>.getDbCohortMethodDataArgs.studyPeriods
# The specification shows one period with null start/end, which means "no restriction".
# The Template suggests using empty strings when not restricting to a time window.
studyPeriods <- tibble::tibble(
  studyStartDate = c(""),  # "" == not restricting
  studyEndDate   = c("")
)

# 2) Time-at-risks (TARs) from <Analysis Specifications>.createStudyPopArgs.timeAtRisks
# The specification provides one TAR:
# - riskWindowStart = 0, startAnchor = "cohort start"
# - riskWindowEnd = 0, endAnchor = "cohort end"
# - minDaysAtRisk = 1
timeAtRisks <- tibble::tibble(
  label = c("TAR: 0d from start to end"),     # annotation label for description
  riskWindowStart  = c(0),
  startAnchor      = c("cohort start"),       # "cohort start" | "cohort end"
  riskWindowEnd    = c(0),
  endAnchor        = c("cohort end"),         # "cohort start" | "cohort end"
  minDaysAtRisk    = c(1)
)

# 3) Propensity Score settings from <Analysis Specifications>.propensityScoreAdjustment
# Only one PS configuration (matchOnPsArgs) is provided:
# - maxRatio = 1, caliper = 0.2, caliperScale = "standardized logit"
matchOnPsArgsList <- tibble::tibble(
  label = c("PS match: 1:1 caliper 0.2 (standardized logit)"),
  maxRatio  = c(1),
  caliper = c(0.2),
  caliperScale  = c("standardized logit")     # "propensity score" | "standardized" | "standardized logit"
)

# stratifyByPsArgs is null per the specification:
stratifyByPsArgsList <- tibble::tibble(
  label = character(),
  numberOfStrata  = integer(),
  baseSelection = character()  # "all" | "target" | "comparator"
)

# 4) Construct a PS configuration list (each entry has: method, label, params)
psConfigList <- list()

# For "match on PS"
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

# For "stratify by PS" (none in this specification)
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

# 5) Iterate through all analysis setting combinations -------------------------
cmAnalysisList <- list()
targetComparatorOutcomesList <- list()
analysisId <- 1

for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]
  
  for (t in seq_len(nrow(timeAtRisks))) {
    for (p in seq_along(psConfigList)) {
      psCfg <- psConfigList[[p]]
      
      # PS adjustment method selection
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
        stop("Unknown PS method specified.")
      }
      
      # Covariate settings
      # The Template uses default settings and adds descendants to exclude list.
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )
      
      # Outcome list:
      # - Include the main outcome(s) of interest (from oList, if defined)
      # - Append negative controls (if any were created above)
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
      
      # Target-Comparator-Outcomes:
      # - Exclude covariate concept IDs according to covariateSelection and any drug-of-interest concepts
      #   The Template included cmTcList$targetConceptId/comparatorConceptId; we NA-protect those.
      for (i in seq_len(nrow(cmTcList))) {
        excludedIds <- c(
          cmTcList$targetConceptId[i],
          cmTcList$comparatorConceptId[i],
          excludedCovariateConcepts$conceptId
        )
        excludedIds <- unique(as.integer(stats::na.omit(excludedIds)))
        
        targetComparatorOutcomesList[[length(targetComparatorOutcomesList) + 1]] <- CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId[i],
          comparatorId = cmTcList$comparatorCohortId[i],
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedIds
        )
      }
      
      # Get DB CohortMethodData args from <Analysis Specifications>.getDbCohortMethodDataArgs
      # - studyStartDate/studyEndDate: empty strings => not restricting (as the spec provides null)
      # - maxCohortSize: 0 (per spec)
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )
      
      # Create PS args from <Analysis Specifications>.propensityScoreAdjustment.createPsArgs
      createPsArgs <- CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE,                  # Allow completion even if PS fails for some strata
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
          noiseLevel = "silent",
          resetCoefficients = TRUE,
          startingVariance = 0.01
        )
      )
      
      # Compute Covariate Balance args (shared and stratified)
      computeSharedCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs <- CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )
      
      # Fit Outcome Model args from <Analysis Specifications>.fitOutcomeModelArgs
      # - modelType = "cox"
      # - stratified = FALSE
      # - useCovariates = FALSE
      # - inversePtWeighting = FALSE
      # - prior = laplace, useCrossValidation = TRUE
      # - control = tolerance 2e-7, cvType auto, fold 10, cvRepetitions 10, noiseLevel quiet,
      #            resetCoefficients TRUE, startingVariance 0.01
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
      
      # Create Study Population args from <Analysis Specifications>.createStudyPopArgs
      # - restrictToCommonPeriod = FALSE
      # - firstExposureOnly = TRUE
      # - washoutPeriod = 365
      # - removeDuplicateSubjects = "keep all"
      # - censorAtNewRiskWindow = FALSE
      # - removeSubjectsWithPriorOutcome = TRUE
      # - priorOutcomeLookBack = 99999
      # - TAR settings from timeAtRisks above, and minDaysAtRisk = 1
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        restrictToCommonPeriod = FALSE,
        firstExposureOnly = TRUE,
        washoutPeriod = 365,
        removeDuplicateSubjects = "keep all",
        censorAtNewRiskWindow = FALSE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 99999,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = timeAtRisks$minDaysAtRisk[t],
        maxDaysAtRisk = 99999
      )
      
      # Append the analysis configuration to the list --------------------------
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "StudyPeriod: %s-%s; %s; PS: %s",
          ifelse(nchar(studyStartDate) == 0, "NoStart", studyStartDate),
          ifelse(nchar(studyEndDate) == 0, "NoEnd", studyEndDate),
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

# CM Module Specifications -----------------------------------------------------
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
# We construct the unified analysis specifications, adding:
# - Cohort definitions (shared resources)
# - Negative control outcomes (shared resources, only if present)
# - CohortGenerator, CohortDiagnostics, and CohortMethod modules
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

# Persist to JSON --------------------------------------------------------------
# Use the exact study name in the output location and file name.
ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", studyName, paste0(studyName, "AnalysisSpecification.json"))
)