#!/usr/bin/env Rscript

###############################################################################
# CreateStrategusAnalysisSpecification.R
#
# Purpose:
#   Build an OHDSI/Strategus analysis specification (JSON) for a comparative
#   cohort study. This script creates a list that matches the expected
#   Strategus analysis specification schema (see Template) and writes it to
#   disk as a pretty-printed JSON file (analysisSpecification.json).
#
# Notes:
#   - This script uses the Strategus package namespace to ensure the package
#     is available, but the specification is written using jsonlite to avoid
#     depending on a specific Strategus write function name (which can vary
#     across versions). If you have Strategus-specific helpers available in
#     your environment, you can replace the JSON write step with the
#     Strategus helper call.
#   - Many fields are left intentionally NULL or as placeholder text where
#     ATLAS cohort IDs / concept set IDs are required. Replace these with the
#     appropriate numeric IDs or strings from your ATLAS export before using.
#
# Output:
#   - analysisSpecification.json : analysis specification in JSON format
#
###############################################################################

# Ensure required packages are available
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Please install the 'jsonlite' package (install.packages('jsonlite')) before running this script.")
}
if (!requireNamespace("Strategus", quietly = TRUE)) {
  warning("The 'Strategus' package is not installed or not available. This script will still produce the analysisSpecification.json file, but you may want Strategus installed for downstream packaging/execution.")
} else {
  # load Strategus namespace so users know it is intended to be used with Strategus
  # (we avoid attaching it to keep the script non-intrusive)
  invisible(requireNamespace("Strategus"))
}

# -----------------------------------------------------------------------------
# Build the analysis specification as an R list that mirrors the JSON template.
# Replace the NULL values and placeholders (IDs, names, dates) with the proper
# values from your ATLAS export / study definitions prior to use.
# -----------------------------------------------------------------------------

analysisSpecification <- list(
  # Top level name for the study/analysis. Change to a descriptive study name.
  name = "Study Name",

  # Cohort definitions:
  # - targetCohort / comparatorCohort: the ATLAS cohort IDs for exposed groups.
  # - outcomeCohort: a list (vector) of outcome cohorts; multiple outcomes allowed.
  cohortDefinitions = list(
    targetCohort = list(
      id = NULL,        # ATLAS cohort ID for the target (replace NULL with integer)
      name = "Target Cohort Name"  # Descriptive name (replace as needed)
    ),
    comparatorCohort = list(
      id = NULL,        # ATLAS cohort ID for the comparator (replace NULL)
      name = "Comparator Cohort Name"
    ),
    outcomeCohort = list(
      # List of outcome cohort objects (each with id and name). This example
      # shows one outcome; add more list elements for multiple outcomes.
      list(
        id = NULL,      # ATLAS cohort ID for the outcome (replace NULL)
        name = "Outcome Cohort Name"
      )
    )
  ),

  # Negative control concept set:
  # - This is a concept set ID from ATLAS used to identify negative control
  #   outcomes. If you don't have negative controls, leave id NULL and name blank.
  negativeControlConceptSet = list(
    id = NULL,         # ATLAS Concept Set ID for negative controls (replace NULL)
    name = "Negative Control Concept Set Name"
  ),

  # Covariate selection:
  # - conceptsToInclude: list concept set IDs to explicitly include as covariates.
  # - conceptsToExclude: list concept set IDs to explicitly exclude from covariates.
  covariateSelection = list(
    conceptsToInclude = list(
      # Example: one concept set to include. Add more elements as needed.
      list(
        id = NULL,     # ATLAS Concept Set ID to include (replace NULL)
        name = "Concept Name 1"
      )
    ),
    conceptsToExclude = list(
      # Example: one concept set to exclude. Add more elements as needed.
      list(
        id = NULL,     # ATLAS Concept Set ID to exclude (replace NULL)
        name = "Concept Name 3"
      )
    )
  ),

  # getDbCohortMethodDataArgs:
  # - studyPeriods: limit cohorts to particular study date ranges (format yyyyMMdd).
  # - maxCohortSize: maximum number of subjects to keep per cohort (0 = no limit).
  getDbCohortMethodDataArgs = list(
    studyPeriods = list(
      list(
        studyStartDate = "",  # yyyyMMdd or empty string to indicate no start bound
        studyEndDate = ""     # yyyyMMdd or empty string to indicate no end bound
      )
    ),
    maxCohortSize = 0        # 0 indicates no maximum size (no downsampling here)
  ),

  # createStudyPopArgs:
  # - Controls how the study population is created from cohort eras.
  createStudyPopArgs = list(
    restrictToCommonPeriod = FALSE,  # If TRUE, only keep subjects in common observation period
    firstExposureOnly = TRUE,        # TRUE = only first exposure per subject will be kept
    washoutPeriod = 365,             # Required prior observation (days) before index
    removeDuplicateSubjects = "keep all", # Options: "keep all" | "keep first" | "remove all"
    censorAtNewRiskWindow = FALSE,   # If TRUE, censor when a new risk window starts
    removeSubjectsWithPriorOutcome = TRUE, # Remove subjects with the outcome in priorOutcomeLookBack
    priorOutcomeLookBack = 99999,    # Look-back window (days) to evaluate prior outcomes
    timeAtRisks = list(
      # One or more time-at-risk (TAR) definitions. Add more list elements for
      # additional TARs. The anchors must be either "cohort start" or "cohort end".
      list(
        description = "TAR 1",        # Informal label for this TAR (optional)
        riskWindowStart = 0,          # Days from start anchor when risk starts
        startAnchor = "cohort start", # "cohort start" | "cohort end"
        riskWindowEnd = 0,            # Days from end anchor when risk ends
        endAnchor = "cohort end",     # "cohort start" | "cohort end"
        minDaysAtRisk = 1             # Minimum days at risk to include subject
      )
    )
  ),

  # Propensity score adjustment:
  # - psSettings: a list of PS adjustment strategies (matching or stratification).
  #   Each element can contain matchOnPsArgs (for matching) or stratifyByPsArgs.
  # - createPsArgs: settings for how to build the PS (e.g., regularization).
  propensityScoreAdjustment = list(
    psSettings = list(
      list(
        description = "PS 1",
        # matchOnPsArgs configures matching on the PS. If set to NULL,
        # stratifyByPsArgs is expected instead.
        matchOnPsArgs = list(
          maxRatio = 1,                 # maximum number of comparators per target (1 = 1:1)
          caliper = 0.2,                # caliper width (0 means no caliper)
          caliperScale = "standardized logit" # "propensity score" | "standardized" | "standardized logit"
        ),
        # stratifyByPsArgs = NULL (we're matching here), but shown for reference:
        stratifyByPsArgs = NULL
      )
    ),
    createPsArgs = list(
      maxCohortSizeForFitting = 250000,   # Downsample to this size for model fitting (0 = no downsampling)
      errorOnHighCorrelation = TRUE,      # Throw an error if covariates are highly correlated
      prior = list(
        priorType = "laplace",            # Regularization prior; "laplace" = L1 (lasso-like)
        useCrossValidation = TRUE         # Use CV to tune regularization strength
      ),
      control = list(
        tolerance = 2e-07,
        cvType = "auto",                  # "auto" or "grid" for CV search
        fold = 10,                        # number of CV folds
        cvRepetitions = 10,               # number of CV repetitions
        noiseLevel = "silent",            # "silent" | "quiet" | "noisy"
        resetCoefficients = TRUE,
        startingVariance = 0.01           # starting variance for regularization search (-1 to derive from data)
      )
    )
  ),

  # fitOutcomeModelArgs:
  # - Settings used to fit the outcome model (e.g., cox regression).
  fitOutcomeModelArgs = list(
    modelType = "cox",                     # "logistic" | "poisson" | "cox"
    stratified = FALSE,                    # fit a stratified model if TRUE
    useCovariates = FALSE,                 # include covariates in the model?
    inversePtWeighting = FALSE,            # use IPTW instead of covariate adjustment?
    prior = list(
      priorType = "laplace",               # regularization prior for outcome model
      useCrossValidation = TRUE            # tune regularization with CV
    ),
    control = list(
      tolerance = 2e-07,
      cvType = "auto",
      fold = 10,
      cvRepetitions = 10,
      noiseLevel = "quiet",               # quieter logging for model fitting
      resetCoefficients = TRUE,
      startingVariance = 0.01
    )
  ),

  # Global maximum cohort size applied at some stages (0 = no maximum)
  maxCohortSize = 0
)

# -----------------------------------------------------------------------------
# Write the JSON file
# - auto_unbox = TRUE makes single-element vectors be written as scalars (convenient
#   for many numeric and logical fields).
# - pretty = TRUE writes an easy-to-read file.
# -----------------------------------------------------------------------------
outputFile <- "analysisSpecification.json"

jsonlite::write_json(analysisSpecification, path = outputFile, auto_unbox = TRUE, pretty = TRUE)

message(sprintf("Analysis specification written to '%s'.", outputFile))
message("Review and replace NULL placeholders (cohort IDs, concept set IDs, dates, names) with real values before using with Strategus.")
# End of script.