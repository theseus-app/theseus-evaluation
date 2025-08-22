#!/usr/bin/env Rscript

# CreateStrategusAnalysisSpecification.R
#
# This script builds an OHDSI Strategus analysis specification (JSON) using the
# settings provided in the <Analysis Specifications> block. It uses CohortMethod
# constructors for the method-specific parts and writes a JSON file that can be
# consumed by Strategus. The script contains detailed annotations to explain how
# each setting is applied and where users should edit to fill study-specific
# values (cohort ids, names, dates, etc.).
#
# Requirements:
#  - R >= 3.6 (recommended)
#  - Packages: jsonlite, CohortMethod, Strategus (Strategus is optional for writing;
#    the script will use jsonlite if Strategus write utilities are not available)
#
# Usage:
#  - Edit the placeholders (NULL or empty strings) below with your ATLAS cohort IDs,
#    concept set IDs and study dates.
#  - Run: Rscript CreateStrategusAnalysisSpecification.R
#  - The output will be written to "analysisSpecification.json" in the working directory.

# Load packages ---------------------------------------------------------------
# We load packages with informative messages. If a package is missing, the script
# will stop and instruct the user to install it.
requiredPackages <- c("jsonlite", "CohortMethod", "Strategus")
for (pkg in requiredPackages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Package '%s' is required but not installed. Install with install.packages('%s') or BiocManager::install() if needed.", pkg, pkg))
  }
}

library(jsonlite)     # For writing JSON
library(CohortMethod) # For building CohortMethod analysis objects (helpers)
library(Strategus)    # For Strategus awareness (not all Strategus versions have write helpers)

# NOTE:
# The supplied <Analysis Specifications> contain many NULL/empty placeholders.
# You must fill the following fields for a complete specification:
#  - analysisName (human readable)
#  - cohortDefinitions: ids and names for target, comparator, outcome(s)
#  - negativeControlConceptSet id/name if using negative controls
#  - covariateSelection concept set ids/names to include/exclude
#  - study period dates in getDbCohortMethodDataArgs (yyyyMMdd or yyyy-mm-dd)
# If you leave fields as NULL/empty the generated JSON will contain those NULLs
# and Strategus or downstream components may fail.

# Top-level metadata ---------------------------------------------------------
analysisName <- "Study Name"     # Edit: a short human-readable study name

# Cohort definitions (ATLAS cohort IDs and names)
# Fill cohort ids (integers) and names (strings).
# If you do not want to supply an id, leave as NULL, but including ATLAS IDs is recommended.
cohortDefinitions <- list(
  targetCohort = list(
    id = NULL,             # Edit: ATLAS cohort ID for target (integer)
    name = ""              # Edit: human readable target cohort name
  ),
  comparatorCohort = list(
    id = NULL,             # Edit: ATLAS cohort ID for comparator (integer)
    name = ""              # Edit: human readable comparator cohort name
  ),
  outcomeCohort = list(
    list(
      id = NULL,           # Edit: ATLAS cohort ID for outcome (integer)
      name = ""            # Edit: human readable outcome cohort name
    )
    # Add more outcome cohorts as additional list(...) elements if needed
  )
)

# Negative control concept set (ATLAS concept set ID)
negativeControlConceptSet <- list(
  id = NULL,    # Edit: ATLAS concept set ID for negative controls (integer)
  name = ""     # Edit: human readable name for the negative control concept set
)

# Covariate selection: lists of ATLAS concept set IDs to include/exclude
# Default placeholders; edit to specify concept set ids to include/exclude in covariates.
covariateSelection <- list(
  conceptsToInclude = list(
    list(id = NULL, name = "") # Example: list(id=12345, name="Include concept set name")
    # Add more list(id=..., name="...") items as needed
  ),
  conceptsToExclude = list(
    list(id = NULL, name = "") # Example: list(id=67890, name="Exclude concept set name")
    # Add more list(id=..., name="...") items as needed
  )
)

# getDbCohortMethodDataArgs --------------------------------------------------
# This section defines the study periods and maximum cohort size when extracting
# cohort method data from the CDM. Dates can be provided as "yyyyMMdd" or "yyyy-mm-dd".
# maxCohortSize = 0 means no maximum (use full cohort).
getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
  studyStartDate = NULL,  # Edit: string "yyyyMMdd" or "yyyy-mm-dd", or NULL for no restriction
  studyEndDate = NULL,    # Edit: string "yyyyMMdd" or "yyyy-mm-dd", or NULL for no restriction
  maxCohortSize = 0       # 0 indicates no maximum size (use all subjects)
)

# Notes:
# If you need multiple study periods, Strategus/CohortMethod typically supports
# single studyStartDate/studyEndDate per analysis. If running multiple disjoint
# periods you may need to produce multiple analyses.

# createStudyPopArgs ---------------------------------------------------------
# Controls how the study population is constructed from cohorts:
# - restrictToCommonPeriod: whether to require observation overlap for target/comparator
# - firstExposureOnly: whether to include only the first exposure per person
# - washoutPeriod: required prior observation length in days
# - removeDuplicateSubjects: "keep all" | "keep first" | "remove all"
# - censorAtNewRiskWindow: whether to censor when a new exposure/risk window begins
# - removeSubjectsWithPriorOutcome: remove subjects with outcome during lookback
# - priorOutcomeLookBack: how far back (days) to inspect for prior outcome
# - timeAtRisks: list of time-at-risk windows (we supply one window below)
createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
  restrictToCommonPeriod = FALSE,  # FALSE per provided specification
  firstExposureOnly = FALSE,       # FALSE: include repeat exposures unless removed separately
  washoutPeriod = 365,             # As specified in the settings
  removeDuplicateSubjects = "remove all", # Remove subjects who are in both cohorts? per spec
  censorAtNewRiskWindow = FALSE,
  removeSubjectsWithPriorOutcome = TRUE,
  priorOutcomeLookback = 365,      # Look back 365 days for prior outcomes
  riskWindowStart = 0,
  startAnchor = "cohort start",    # "cohort start" | "cohort end"
  riskWindowEnd = 0,
  endAnchor = "cohort end",        # "cohort start" | "cohort end"
  minDaysAtRisk = 1                # Minimum days at risk for subject to be included
)

# Note on removeDuplicateSubjects:
# The template allowed "keep all", "keep first", or "remove all". We set to
# "remove all" per your specification. If you prefer to keep target/comparator
# overlap handling different, change accordingly.

# Propensity score settings --------------------------------------------------
# We configure creation of the propensity score (regularized logistic regression with Cyclops)
# and the matching parameters.
# createPsArgs: settings for fitting the PS model
createPsArgs <- CohortMethod::createCreatePsArgs(
  maxCohortSizeForFitting = 250000,
  errorOnHighCorrelation = TRUE,
  prior = Cyclops::createPrior("laplace", useCrossValidation = TRUE),
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

# Matching on propensity score: max ratio, caliper and caliper scale
# The <Analysis Specifications> shows a single PS setting with match (maxRatio=10)
matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
  maxRatio = 10,
  caliper = 0.2,
  caliperScale = "standardized logit" # Options: "propensity score" | "standardized" | "standardized logit"
)

# If you wanted stratification by PS instead, set matchOnPsArgs = NULL and use
# stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(numberOfStrata = 5, baseSelection = "all")

# fitOutcomeModelArgs --------------------------------------------------------
# Configure outcome model fitting. Per specification: Cox model, no covariates,
# not stratified, no IPTW, regularization settings provided.
fitOutcomeModelArgs <- CohortMethod::createFitOutcomeModelArgs(
  modelType = "cox",        # "logistic" | "poisson" | "cox"
  stratified = FALSE,
  useCovariates = FALSE,    # Do not include covariates: effect estimate based on PS-matched sample
  inversePtWeighting = FALSE,
  prior = Cyclops::createPrior("laplace", useCrossValidation = TRUE),
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

# Build the CohortMethod analysis object -------------------------------------
# CohortMethod organizes the method-specific pieces into an analysis object.
# The analysisId and description are arbitrary identifiers for the method variant
# you are specifying. If you plan multiple analysis variants, give each unique IDs.
cohortMethodAnalysis <- CohortMethod::createCohortMethodAnalysis(
  analysisId = 1L,
  description = "Primary analysis: PS matching (max ratio 10, caliper 0.2), Cox model",
  getDbCohortMethodDataArgs = getDbCohortMethodDataArgs,
  createStudyPopArgs = createStudyPopArgs,
  createPs = createPsArgs,
  matchOnPs = matchOnPsArgs,
  stratifyByPs = NULL,
  computeCovariateBalance = TRUE,
  fitOutcomeModel = fitOutcomeModelArgs
)

# Compose final analysis specification structure ------------------------------
# The structure follows the Template JSON, with sections for cohort definitions,
# covariate selection, method-specific settings, and model fitting settings.
analysisSpecification <- list(
  name = analysisName,
  cohortDefinitions = cohortDefinitions,
  negativeControlConceptSet = negativeControlConceptSet,
  covariateSelection = covariateSelection,
  # We include the getDbCohortMethodDataArgs and createStudyPopArgs as raw JSON-like lists
  # to match the template. The CohortMethod wrappers above also capture these for running.
  getDbCohortMethodDataArgs = list(
    studyPeriods = list(
      list(
        studyStartDate = getDbCohortMethodDataArgs$studyStartDate,
        studyEndDate = getDbCohortMethodDataArgs$studyEndDate
      )
    ),
    maxCohortSize = getDbCohortMethodDataArgs$maxCohortSize
  ),
  createStudyPopArgs = list(
    restrictToCommonPeriod = createStudyPopArgs$restrictToCommonPeriod,
    firstExposureOnly = createStudyPopArgs$firstExposureOnly,
    washoutPeriod = createStudyPopArgs$washoutPeriod,
    removeDuplicateSubjects = createStudyPopArgs$removeDuplicateSubjects,
    censorAtNewRiskWindow = createStudyPopArgs$censorAtNewRiskWindow,
    removeSubjectsWithPriorOutcome = createStudyPopArgs$removeSubjectsWithPriorOutcome,
    priorOutcomeLookBack = createStudyPopArgs$priorOutcomeLookback,
    timeAtRisks = list(
      list(
        description = "TAR 1",
        riskWindowStart = createStudyPopArgs$riskWindowStart,
        startAnchor = createStudyPopArgs$startAnchor,
        riskWindowEnd = createStudyPopArgs$riskWindowEnd,
        endAnchor = createStudyPopArgs$endAnchor,
        minDaysAtRisk = createStudyPopArgs$minDaysAtRisk
      )
    )
  ),
  propensityScoreAdjustment = list(
    psSettings = list(
      list(
        description = "PS 1",
        matchOnPsArgs = list(
          maxRatio = matchOnPsArgs$maxRatio,
          caliper = matchOnPsArgs$caliper,
          caliperScale = matchOnPsArgs$caliperScale
        ),
        stratifyByPsArgs = NULL
      )
    ),
    createPsArgs = list(
      maxCohortSizeForFitting = createPsArgs$maxCohortSizeForFitting,
      errorOnHighCorrelation = createPsArgs$errorOnHighCorrelation,
      prior = list(
        priorType = "laplace",
        useCrossValidation = TRUE
      ),
      control = list(
        tolerance = createPsArgs$control$tolerance,
        cvType = createPsArgs$control$cvType,
        fold = createPsArgs$control$fold,
        cvRepetitions = createPsArgs$control$cvRepetitions,
        noiseLevel = createPsArgs$control$noiseLevel,
        resetCoefficients = createPsArgs$control$resetCoefficients,
        startingVariance = createPsArgs$control$startingVariance
      )
    )
  ),
  fitOutcomeModelArgs = list(
    modelType = fitOutcomeModelArgs$modelType,
    stratified = fitOutcomeModelArgs$stratified,
    useCovariates = fitOutcomeModelArgs$useCovariates,
    inversePtWeighting = fitOutcomeModelArgs$inversePtWeighting,
    prior = list(
      priorType = "laplace",
      useCrossValidation = TRUE
    ),
    control = list(
      tolerance = fitOutcomeModelArgs$control$tolerance,
      cvType = fitOutcomeModelArgs$control$cvType,
      fold = fitOutcomeModelArgs$control$fold,
      cvRepetitions = fitOutcomeModelArgs$control$cvRepetitions,
      noiseLevel = fitOutcomeModelArgs$control$noiseLevel,
      resetCoefficients = fitOutcomeModelArgs$control$resetCoefficients,
      startingVariance = fitOutcomeModelArgs$control$startingVariance
    )
  ),
  # Top-level maximum cohort size (mirrors getDbCohortMethodDataArgs$maxCohortSize)
  maxCohortSize = getDbCohortMethodDataArgs$maxCohortSize,
  # Include the CohortMethod analysis object (as R structure) so users who run this R
  # script locally can load the object and pass to CohortMethod and Strategus execution.
  cohortMethodAnalysis = cohortMethodAnalysis
)

# Write JSON to file ----------------------------------------------------------
# Strategus typically expects a JSON analysis specification. We write the specification
# to "analysisSpecification.json". The JSON uses auto_unbox to avoid arrays for singletons.
outputFile <- "analysisSpecification.json"

# jsonlite::write_json will convert R NULL to JSON null.
jsonlite::write_json(analysisSpecification, path = outputFile, pretty = TRUE, auto_unbox = TRUE, na = "null")

message(sprintf("Analysis specification written to %s", outputFile))

# Optionally: if Strategus provides a write helper (varies by version), use it.
# We attempt to call Strategus::writeAnalysisSpecification if available; wrap in tryCatch.
if ("writeAnalysisSpecification" %in% ls(asNamespace("Strategus"), all.names = TRUE)) {
  tryCatch({
    Strategus::writeAnalysisSpecification(analysisSpecification, outputFile)
    message("Strategus::writeAnalysisSpecification succeeded (overwrote file).")
  }, error = function(e) {
    message("Strategus::writeAnalysisSpecification not used or failed; kept JSON written by jsonlite.")
  })
} else {
  message("Strategus::writeAnalysisSpecification not found in this Strategus version; JSON file remains the canonical output.")
}

# Final notes (printed to console)
message("IMPORTANT: Review and edit the generated JSON file to fill in cohort and concept set IDs, dates and names before executing Strategus.")
message("If you plan to run the analysis with Strategus, ensure the cohort definitions and concept sets exist in the Atlas instance and that your database connection details are configured in a Strategus project file as required.")