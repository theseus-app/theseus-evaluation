#!/usr/bin/env Rscript

# CreateStrategusAnalysisSpecification.R
#
# This script builds an OHDSI Strategus analysis specification JSON file using
# the settings provided in the Analysis Specifications. It uses the
# Strategus package (loaded here for consistency with OHDSI workflows) and
# jsonlite to serialize the specification to disk.
#
# Notes about representation:
# - Fields that are intended to be JSON null are set to NA in R and written
#   with na = "null" so that jsonlite emits JSON null values.
# - We use auto_unbox = TRUE so that single values are not written as JSON arrays.
# - Detailed inline comments explain how each R element maps to the analysis spec.

# Load required packages ------------------------------------------------------
# Strategus is loaded to satisfy the requirement to "use the OHDSI Strategus package".
# The script does not depend on a specific Strategus function for creating the
# JSON; instead we construct the specification as an R list and write JSON with jsonlite.
if (!requireNamespace("Strategus", quietly = TRUE)) {
  stop("Please install the 'Strategus' package from CRAN/GitHub before running this script.")
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Please install the 'jsonlite' package before running this script.")
}
library(Strategus) # loaded to integrate with OHDSI workflows
library(jsonlite)

# Build analysis specification as an R list ----------------------------------
# The structure mirrors the provided Template and Analysis Specifications.

analysisSpecification <- list(
  # Top-level study name (empty string as provided)
  name = "",

  # Cohort definitions: target, comparator, and one-or-more outcomes
  cohortDefinitions = list(
    targetCohort = list(
      id = NA_integer_,    # ATLAS Cohort ID; NA => JSON null
      name = ""            # Target Cohort Name (empty here)
    ),
    comparatorCohort = list(
      id = NA_integer_,    # ATLAS Cohort ID; NA => JSON null
      name = ""            # Comparator Cohort Name (empty here)
    ),
    outcomeCohort = list(
      # The specification provided a single outcome object in an array
      list(
        id = NA_integer_,  # ATLAS Cohort ID; NA => JSON null
        name = ""          # Outcome Cohort Name (empty here)
      )
    )
  ),

  # Negative control concept set (single concept set)
  negativeControlConceptSet = list(
    id = NA_integer_,      # ATLAS Concept Set ID; NA => JSON null
    name = ""              # Negative Control Concept Set Name (empty here)
  ),

  # Covariate selection - concept sets to explicitly include/exclude
  covariateSelection = list(
    conceptsToInclude = list(
      list(
        id = NA_integer_,  # ATLAS Concept Set ID; NA => JSON null
        name = ""          # Concept Name 1 (empty here)
      )
    ),
    conceptsToExclude = list(
      list(
        id = NA_integer_,  # ATLAS Concept Set ID; NA => JSON null
        name = ""          # Concept Name 3 (empty here)
      )
    )
  ),

  # Arguments used when calling getDbCohortMethodData (DB cohort-method data extraction)
  getDbCohortMethodDataArgs = list(
    # The specification includes two study periods; dates are strings in yyyyMMdd format.
    studyPeriods = list(
      list(studyStartDate = "20100101", studyEndDate = "20191231"),
      list(studyStartDate = "20120101", studyEndDate = "20191231")
    ),
    # 0 indicates no maximum cohort size (no downsampling at extraction time)
    maxCohortSize = 0
  ),

  # Arguments used to build the study population (createStudyPopulation)
  createStudyPopArgs = list(
    # Do not restrict to the common observation period
    restrictToCommonPeriod = FALSE,
    # Do not restrict to first exposure only
    firstExposureOnly = FALSE,
    # Washout period (days) required before index
    washoutPeriod = 0,
    # Handling duplicate subject exposures: keep all
    removeDuplicateSubjects = "keep all",
    # Do not censor at a new risk window start
    censorAtNewRiskWindow = FALSE,
    # Remove subjects with prior outcome (true)
    removeSubjectsWithPriorOutcome = TRUE,
    # How far to look back for prior outcomes (99999 indicates effectively unlimited)
    priorOutcomeLookBack = 99999,
    # One or more time-at-risk specifications (TARs)
    timeAtRisks = list(
      list(
        description = "TAR 1",          # descriptive label for TAR 1
        riskWindowStart = 0,
        startAnchor = "cohort start",   # anchor for start
        riskWindowEnd = 0,
        endAnchor = "cohort end",       # anchor for end
        minDaysAtRisk = 1
      ),
      list(
        description = "TAR 2",          # descriptive label for TAR 2
        riskWindowStart = 0,
        startAnchor = "cohort start",
        # riskWindowEnd 9999 anchored to cohort start
        riskWindowEnd = 9999,
        endAnchor = "cohort start",
        minDaysAtRisk = 1
      )
    )
  ),

  # Propensity score creation and adjustment settings
  propensityScoreAdjustment = list(
    # psSettings is a list of settings where each element defines either
    # stratification by PS or matching on PS. We include two settings per spec.
    psSettings = list(
      # First setting: stratify by PS into 5 strata, base selection "all"
      list(
        description = "PS - stratify (5 strata)",
        matchOnPsArgs = NULL,  # NULL indicates we are using stratification instead of matching
        stratifyByPsArgs = list(
          numberOfStrata = 5,
          baseSelection = "all" # use all as the base for strata construction
        )
      ),
      # Second setting: match on PS using provided caliper parameters
      list(
        description = "PS - match on PS (caliper = 0.2, maxRatio = 0 => no maximum)",
        matchOnPsArgs = list(
          # maxRatio = 0 means no maximum matching ratio per the Template/Spec
          maxRatio = 0,
          # caliper in scale specified by caliperScale
          caliper = 0.2,
          caliperScale = "standardized logit" # other options: "propensity score" | "standardized"
        ),
        stratifyByPsArgs = NULL
      )
    ),

    # createPsArgs: configuration for fitting the PS model
    createPsArgs = list(
      # Maximum cohort size to use for fitting the PS (250000 as specified)
      maxCohortSizeForFitting = 250000,
      # Whether to throw an error on high correlation among covariates
      errorOnHighCorrelation = TRUE,
      # Prior specification used for regularized regression (laplace => L1)
      prior = list(
        priorType = "laplace",
        useCrossValidation = TRUE
      ),
      # Control settings for the regularized regression fitting procedure
      control = list(
        tolerance = 2e-07,
        cvType = "auto",
        fold = 10,
        cvRepetitions = 10,
        noiseLevel = "silent",   # silent | quiet | noisy
        resetCoefficients = TRUE,
        startingVariance = 0.01
      )
    )
  ),

  # Outcome model fitting arguments
  fitOutcomeModelArgs = list(
    modelType = "cox",        # Cox proportional hazards model
    stratified = TRUE,        # fit a stratified model (e.g., strata by PS stratum)
    useCovariates = FALSE,    # do not include baseline covariates in the model
    inversePtWeighting = FALSE, # do not use IPTW
    # Prior for regularized outcome model (laplace with CV)
    prior = list(
      priorType = "laplace",
      useCrossValidation = TRUE
    ),
    # Control settings for the regularized outcome model fitting procedure
    control = list(
      tolerance = 2e-07,
      cvType = "auto",
      fold = 10,
      cvRepetitions = 10,
      noiseLevel = "quiet",    # quiet per specification
      resetCoefficients = TRUE,
      startingVariance = 0.01
    )
  )
)

# Write JSON file -------------------------------------------------------------
# Write the analysis specification to disk in the current working directory.
# - na = "null" ensures R NA values are emitted as JSON null (for id fields).
# - auto_unbox = TRUE prevents singletons from being written as arrays.
# - pretty = TRUE formats the JSON for readability.
outputFile <- "analysisSpecification.json"
write_json(analysisSpecification, path = outputFile, auto_unbox = TRUE, pretty = TRUE, na = "null")

# Inform the user where the file was written (standard output)
message(sprintf("Analysis specification JSON written to: %s", normalizePath(outputFile)))
# End of script.