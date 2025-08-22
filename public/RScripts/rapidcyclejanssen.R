#!/usr/bin/env Rscript

########################################################################
# CreateStrategusAnalysisSpecification.R
#
# Purpose:
#   Build an OHDSI/Strategus analysis specification JSON using the
#   settings provided in the Analysis Specifications. This script
#   produces a JSON file (analysisSpecification.json) that can be used
#   by Strategus-based study tooling to run cohort comparisons,
#   propensity-score adjusted analyses, and outcome models.
#
# Usage:
#   Rscript CreateStrategusAnalysisSpecification.R
#
# Notes:
#   - This script intentionally mirrors the structure expected by
#     Strategus and the OHDSI analysis specification template.
#   - Fields that were "null" in the specification are set to NULL here
#     so they will appear as null in the produced JSON.
#   - Replace NULL or empty-string placeholders with actual ATLAS cohort
#     IDs, concept set IDs, and names before running the study.
#
########################################################################

# Load required packages. Strategus is referenced to make clear this
# is a Strategus-style specification. jsonlite is used to write the
# final JSON file. If Strategus-specific helper functions are required
# later, they can be used after loading the package.
if (!requireNamespace("Strategus", quietly = TRUE)) {
  warning("Strategus package not installed. The script will still write the JSON specification, but Strategus-specific helper functions will not be available.")
} else {
  # load Strategus to make the environment consistent with OHDSI workflows
  library(Strategus)
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("The 'jsonlite' package is required but not installed. Install it with install.packages('jsonlite').")
}
library(jsonlite)

# -----------------------------------------------------------------------------
# Build the analysis specification list
# -----------------------------------------------------------------------------
# The structure below matches the template described in the Analysis
# Specifications. For clarity each block includes comments that explain
# how the corresponding settings are applied.
# -----------------------------------------------------------------------------

analysisSpecification <- list(
  # Top-level name for the study. Blank in the provided spec.
  name = "",

  # Cohort definitions: target, comparator, and one or more outcomes.
  # The 'id' fields correspond to ATLAS Cohort IDs (set to NULL when
  # not provided). Replace NULLs with integer IDs before use.
  cohortDefinitions = list(
    targetCohort = list(
      id = NULL,    # ATLAS cohort ID for the target cohort (NULL as placeholder)
      name = ""     # Human readable name for the target cohort
    ),
    comparatorCohort = list(
      id = NULL,    # ATLAS cohort ID for the comparator cohort (NULL as placeholder)
      name = ""     # Human readable name for the comparator cohort
    ),
    # outcomeCohort is an array: here we include one outcome object as in the spec
    outcomeCohort = list(
      list(
        id = NULL,  # ATLAS cohort ID for the outcome (NULL as placeholder)
        name = ""   # Human readable name for the outcome
      )
    )
  ),

  # Negative control concept set (used to fetch negative control outcomes).
  negativeControlConceptSet = list(
    id = NULL,     # ATLAS concept set ID for negative controls (NULL placeholder)
    name = ""      # Human readable name for the concept set
  ),

  # Covariate selection: concept sets to include and to exclude.
  covariateSelection = list(
    conceptsToInclude = list(
      list(
        id = NULL,  # ATLAS concept set ID to include (NULL placeholder)
        name = ""   # Human readable name for included concept set
      )
    ),
    conceptsToExclude = list(
      list(
        id = NULL,  # ATLAS concept set ID to exclude (NULL placeholder)
        name = ""   # Human readable name for excluded concept set
      )
    )
  ),

  # Arguments used when extracting cohort method data from the database.
  getDbCohortMethodDataArgs = list(
    # studyPeriods is a list of periods. Dates are expressed as "yyyyMMdd"
    # per the template. studyEndDate is NULL here meaning "no end date".
    studyPeriods = list(
      list(
        studyStartDate = "20210101", # 2021-01-01
        studyEndDate = NULL          # no end date specified
      )
    ),
    # maxCohortSize = 0 means no maximum cohort size is applied.
    maxCohortSize = 0
  ),

  # Arguments that define how the study population is constructed
  # (washout, first exposure, duplicate handling, prior outcome removal,
  # and the time-at-risk definitions).
  createStudyPopArgs = list(
    restrictToCommonPeriod = FALSE,   # Do not require target/comparator to share common observation periods
    firstExposureOnly = TRUE,         # Keep only the first exposure per person (as specified)
    washoutPeriod = 365,              # 365 days required prior observation (washout)
    # removeDuplicateSubjects: "remove all" will remove any person who appears in both cohorts
    removeDuplicateSubjects = "remove all",
    censorAtNewRiskWindow = FALSE,    # Do not censor when new risk window starts
    removeSubjectsWithPriorOutcome = TRUE,  # Remove persons with prior outcome in lookback
    priorOutcomeLookBack = 99999,     # Long lookback period effectively means "ever prior"
    # timeAtRisks: list of exposure risk windows to evaluate
    timeAtRisks = list(
      list(
        description = "TAR 1",            # optional description to identify TAR in reports
        riskWindowStart = 1,
        startAnchor = "cohort start",     # anchor meaning the cohort start is day 0
        riskWindowEnd = 14,
        endAnchor = "cohort start",       # end anchored to cohort start as well
        minDaysAtRisk = 1
      ),
      list(
        description = "TAR 2",
        riskWindowStart = 1,
        startAnchor = "cohort start",
        riskWindowEnd = 28,
        endAnchor = "cohort start",
        minDaysAtRisk = 1
      ),
      list(
        description = "TAR 3",
        riskWindowStart = 1,
        startAnchor = "cohort start",
        riskWindowEnd = 42,
        endAnchor = "cohort start",
        minDaysAtRisk = 1
      ),
      list(
        description = "TAR 4",
        riskWindowStart = 1,
        startAnchor = "cohort start",
        riskWindowEnd = 90,
        endAnchor = "cohort start",
        minDaysAtRisk = 1
      ),
      list(
        description = "TAR 5",
        riskWindowStart = 0,               # includes day 0 (cohort start) through day 2
        startAnchor = "cohort start",
        riskWindowEnd = 2,
        endAnchor = "cohort start",
        minDaysAtRisk = 1
      )
    )
  ),

  # Propensity score creation and adjustment settings. The specification
  # indicates a matching approach (matchOnPsArgs) with maxRatio = 0 to
  # indicate no maximum (0 means no maximum), a caliper, and propensity
  # score scale set to "propensity score".
  propensityScoreAdjustment = list(
    psSettings = list(
      list(
        description = "PS 1",
        # matchOnPsArgs used (stratifyByPsArgs is NULL per the spec)
        matchOnPsArgs = list(
          maxRatio = 0,            # 0 means no maximum ratio enforced
          caliper = 0.2,           # caliper width (0 means no caliper)
          caliperScale = "propensity score"  # scale for caliper
        ),
        stratifyByPsArgs = NULL
      )
    ),
    # createPsArgs controls the fitting of the PS model. These settings
    # reflect penalized logistic regression with cross-validated prior
    # (laplace) and control parameters for the optimizer and CV.
    createPsArgs = list(
      maxCohortSizeForFitting = 250000,  # downsample if larger than this; 0 disables downsampling
      errorOnHighCorrelation = TRUE,     # error out if highly correlated covariates detected
      prior = list(
        priorType = "laplace",           # regularization prior type (L1 / Lasso)
        useCrossValidation = TRUE
      ),
      control = list(
        tolerance = 2e-7,
        cvType = "auto",                 # automatic CV selection (grid/auto)
        fold = 10,
        cvRepetitions = 10,
        noiseLevel = "silent",           # silence optimizer messages during PS fitting
        resetCoefficients = TRUE,
        startingVariance = 0.01
      )
    )
  ),

  # Outcome model fitting arguments. The specification requests a Cox
  # proportional hazards model without stratification and without
  # additional covariates (i.e., effect estimate from exposure after
  # PS adjustment). Regularization (laplace) with CV is specified.
  fitOutcomeModelArgs = list(
    modelType = "cox",            # model type: "cox"
    stratified = FALSE,           # not using stratified model here
    useCovariates = FALSE,        # do not include covariates in the model (rely on PS)
    inversePtWeighting = FALSE,   # not using IPTW
    prior = list(
      priorType = "laplace",
      useCrossValidation = TRUE
    ),
    control = list(
      tolerance = 2e-7,
      cvType = "auto",
      fold = 10,
      cvRepetitions = 10,
      noiseLevel = "quiet",       # quieter optimizer output for outcome model
      resetCoefficients = TRUE,
      startingVariance = 0.01
    )
  )
)

# -----------------------------------------------------------------------------
# Write the JSON to disk
# -----------------------------------------------------------------------------
# The file name below is a conventional name used for Strategus analysis
# specifications. Users can rename as desired. We set auto_unbox=TRUE so
# that length-1 vectors are written as scalars, pretty=TRUE for readability,
# and null values are written as JSON null.
# -----------------------------------------------------------------------------

outputFile <- "analysisSpecification.json"

# Convert to JSON and write. NULL fields will become JSON null.
write_json(analysisSpecification,
           path = outputFile,
           pretty = TRUE,
           auto_unbox = TRUE,
           na = "null")

# Informational message for interactive use. When running non-interactively,
# users can check the file at the path above.
message(sprintf("Analysis specification written to: %s", normalizePath(outputFile, mustWork = FALSE)))

# End of script.
# -----------------------------------------------------------------------------
# Next steps (manual):
#  - Edit the JSON produced to fill in the ATLAS cohort IDs and concept set
#    IDs (replace NULLs). Alternatively, update this R script to set those
#    values programmatically prior to re-running.
#  - Use Strategus study execution tools (or other OHDSI tooling) to run the
#    study using the produced analysisSpecification.json.
# -----------------------------------------------------------------------------