#!/usr/bin/env Rscript

###############################################################################
# CreateStrategusAnalysisSpecification.R
#
# Purpose:
#   Create an analysis specification (JSON) that can be used by OHDSI Strategus
#   (and related tooling). This script builds an R list that mirrors the
#   "Template" structure and the supplied "Analysis Specifications" values,
#   and writes the specification to a JSON file.
#
# Notes:
# - We use jsonlite::write_json() with na = "null" so that R NA values are
#   serialized as JSON null (this keeps explicit nulls in the JSON).
# - We load Strategus to show the intended package context; the actual writing
#   is handled by jsonlite for broad compatibility. If you have additional
#   Strategus utilities to validate or register the specification, call them
#   after the file is created.
#
# Usage:
#   Rscript CreateStrategusAnalysisSpecification.R
#
# Output:
#   analysisSpecification.json in the current working directory
###############################################################################

# Load required packages
# - Strategus is loaded to show the integration context (some workflows call
#   functions from Strategus after creating the JSON). It is not strictly
#   required for this script, but keeping it makes explicit that the output
#   is intended for Strategus.
suppressPackageStartupMessages({
  library(Strategus) # for context / potential downstream validation / execution
  library(jsonlite)
})

# -----------------------------------------------------------------------------
# Build the analysis specification
# -----------------------------------------------------------------------------
# NOTE: We use NA_* types for fields that must be serialized as JSON null.
#       We use empty strings ("") for fields that were given as empty strings
#       in the Analysis Specifications (e.g., the "name" fields).
#
# Mapping of key sections:
# - name: top-level study name (empty string per the provided specification).
# - cohortDefinitions: target, comparator, and outcome(s) with ATLAS cohort IDs
#   set to NULL (serialized as JSON null) and names set to empty strings.
# - negativeControlConceptSet: concept set ID is NULL (serialized as JSON null).
# - covariateSelection: lists of concept set IDs to include/exclude; IDs are
#   NULL (NA), names are empty strings.
# - getDbCohortMethodDataArgs: studyPeriods with start/end dates set to NULL,
#   and maxCohortSize = 0 (0 means no maximum size).
# - createStudyPopArgs: mapping of inclusion/exclusion, washout, and time-at-risk.
# - propensityScoreAdjustment: PS settings (matching on PS), and createPsArgs
#   with regularization prior and control settings.
# - fitOutcomeModelArgs: Cox model settings with regularization prior and control.
# - maxCohortSize: root-level maximum cohort size (0 indicates no limit).
analysisSpecification <- list(
  # Top-level study name (empty string per the Analysis Specifications)
  name = "",

  # Cohort definitions: target, comparator, and one or more outcomes
  cohortDefinitions = list(
    targetCohort = list(
      id = NA_integer_, # ATLAS Cohort ID -> null
      name = ""         # Cohort name (empty string)
    ),
    comparatorCohort = list(
      id = NA_integer_, # ATLAS Cohort ID -> null
      name = ""         # Cohort name (empty string)
    ),
    outcomeCohort = list(
      # single-element list; can be expanded for multiple outcomes
      list(
        id = NA_integer_, # ATLAS Cohort ID -> null
        name = ""         # Outcome name (empty string)
      )
    )
  ),

  # Negative control concept set (concept set ID null, empty name)
  negativeControlConceptSet = list(
    id = NA_integer_,  # ATLAS Concept Set ID -> null
    name = ""          # Concept set name (empty string)
  ),

  # Covariate selection: concept sets to include and exclude
  covariateSelection = list(
    conceptsToInclude = list(
      list(
        id = NA_integer_, # ATLAS Concept Set ID -> null
        name = ""         # Concept name (empty string)
      )
    ),
    conceptsToExclude = list(
      list(
        id = NA_integer_, # ATLAS Concept Set ID -> null
        name = ""         # Concept name (empty string)
      )
    )
  ),

  # Arguments for getDbCohortMethodData()
  getDbCohortMethodDataArgs = list(
    studyPeriods = list(
      # A single study period is provided; start and end dates are null per spec
      list(
        studyStartDate = NA_character_, # yyyyMMdd or null
        studyEndDate = NA_character_    # yyyyMMdd or null
      )
    ),
    maxCohortSize = 0 # 0 indicates no maximum cohort size
  ),

  # Arguments for createStudyPopulation()
  createStudyPopArgs = list(
    restrictToCommonPeriod = TRUE,      # restrict exposures to the data common observation period
    firstExposureOnly = TRUE,           # keep only first exposure per person
    washoutPeriod = 365,                # required days of prior observation
    removeDuplicateSubjects = "keep all", # options: "keep all" | "keep first" | "remove all"
    censorAtNewRiskWindow = TRUE,       # censor subjects at new risk window start if applicable
    removeSubjectsWithPriorOutcome = TRUE, # exclude subjects with the outcome prior to cohort entry
    priorOutcomeLookBack = 99999,       # days to look back for prior outcomes (99999 implies effectively unlimited)
    timeAtRisks = list(
      # single time-at-risk definition; can be expanded to multiple TARs
      list(
        # description is optional in the provided specification; omitted here.
        riskWindowStart = 1,           # start offset in days
        startAnchor = "cohort start",  # "cohort start" | "cohort end"
        riskWindowEnd = 0,             # end offset in days
        endAnchor = "cohort end",      # "cohort start" | "cohort end"
        minDaysAtRisk = 1              # minimum days at risk
      )
    )
  ),

  # Propensity score specification: matching on PS (not stratification)
  propensityScoreAdjustment = list(
    psSettings = list(
      list(
        # description field omitted in Analysis Specifications; can be added if desired.
        matchOnPsArgs = list(
          maxRatio = 10,                    # maximum matched comparator per target (0 = no maximum)
          caliper = 0.2,                    # caliper used for matching (0 = no caliper)
          caliperScale = "standardized logit" # scale of the caliper
        ),
        stratifyByPsArgs = NA              # null because we are matching, not stratifying
      )
    ),
    createPsArgs = list(
      maxCohortSizeForFitting = 250000,    # downsample threshold for PS fitting (0 = no downsample)
      errorOnHighCorrelation = TRUE,       # throw error on high correlation among covariates
      prior = list(                        # regularization prior specification
        priorType = "laplace",             # use L1 (lasso-like) regularization
        useCrossValidation = TRUE
      ),
      control = list(                      # control parameters for model fitting / CV
        tolerance = 2e-07,
        cvType = "auto",                   # "auto" or "grid"
        fold = 10,
        cvRepetitions = 10,
        noiseLevel = "silent",             # "silent" | "quiet" | "noisy"
        resetCoefficients = TRUE,
        startingVariance = 0.01
      )
    )
  ),

  # Outcome model fitting arguments
  fitOutcomeModelArgs = list(
    modelType = "cox",          # "logistic" | "poisson" | "cox"
    stratified = TRUE,          # use stratified model (e.g., stratified Cox)
    useCovariates = FALSE,      # do not include covariates (use PS matched/stratified design)
    inversePtWeighting = FALSE, # no IPTW
    prior = list(               # regularization prior for outcome model (if used)
      priorType = "laplace",
      useCrossValidation = TRUE
    ),
    control = list(             # control parameters for outcome model regularization & CV
      tolerance = 2e-07,
      cvType = "auto",
      fold = 10,
      cvRepetitions = 10,
      noiseLevel = "quiet",
      resetCoefficients = TRUE,
      startingVariance = 0.01
    )
  ),

  # Additional top-level maxCohortSize (0 indicates no limit)
  maxCohortSize = 0
)

# -----------------------------------------------------------------------------
# Write the specification to JSON
# -----------------------------------------------------------------------------
# - pretty = TRUE for human-readable formatting.
# - auto_unbox = TRUE so that length-1 vectors are not written as arrays.
# - na = "null" so that R NA values are written as JSON null (preserving explicit
#   null markers used in the Analysis Specifications).
outputFile <- "analysisSpecification.json"
write_json(analysisSpecification,
           path = outputFile,
           pretty = TRUE,
           auto_unbox = TRUE,
           na = "null")

# Informational message
message("Analysis specification written to: ", normalizePath(outputFile))

# -----------------------------------------------------------------------------
# Optional: further integration with Strategus
# -----------------------------------------------------------------------------
# If you want to register, validate, or run this specification with Strategus,
# uncomment and adapt the following example calls (make sure your Strategus
# version supports these functions and that you have configured the execution
# environment).
#
# Example (pseudo-code / template):
#
# # Validate the specification (if Strategus provides validation utilities)
# # Strategus::validateAnalysisSpecification(outputFile)
#
# # To execute analysis you would normally pass the JSON spec to the
# # Strategus driver (e.g., planner/executor) or to a study-specific runner.
# -----------------------------------------------------------------------------

# End of script
###############################################################################