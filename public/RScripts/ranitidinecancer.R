#!/usr/bin/env Rscript

###############################################################################
# CreateStrategusAnalysisSpecification.R
#
# This script builds an OHDSI Strategus analysis specification JSON file
# based on the provided Analysis Specifications and the Template.
#
# Instructions:
# - Update the NULL id fields and empty name fields to the actual ATLAS Cohort
#   IDs and Concept Set IDs / names before running downstream Strategus study
#   execution steps.
# - The script writes a file named "analysisSpecification.json" in the current
#   working directory. You can change the output file path below if desired.
#
# The script uses the jsonlite package to write the JSON and imports the
# Strategus package for context and to signal compatibility with Strategus.
#
# NOTE: This script intentionally mirrors the structure expected by Strategus
# (see Template). Many fields are left NULL or empty as placeholders; replace
# them with real values for a runnable analysis.
###############################################################################

# Load required packages
# Strategus is loaded to show intent and compatibility with Strategus-based
# workflows. jsonlite is used to serialize the R list to JSON.
suppressPackageStartupMessages({
  library(Strategus)  # ensures compatibility; used for context/comments only
  library(jsonlite)
})

# 1) Top-level study name
# Replace "Study Name" with a descriptive study name.
analysisSpecification <- list(
  name = "Study Name",

  # 2) Cohort definitions
  # Provide ATLAS cohort IDs (integers) and human-readable names for target,
  # comparator, and outcome cohorts. NULL values are placeholders.
  cohortDefinitions = list(
    targetCohort = list(
      id = NULL,    # ATLAS cohort ID for target (replace NULL with integer)
      name = ""     # Human readable name for target cohort
    ),
    comparatorCohort = list(
      id = NULL,    # ATLAS cohort ID for comparator
      name = ""     # Human readable name for comparator cohort
    ),
    outcomeCohort = list(
      # Outcome cohorts is a list/array; we include one element here per the
      # provided Analysis Specifications. Add additional outcome objects as needed.
      list(
        id = NULL,  # ATLAS cohort ID for outcome
        name = ""    # Human readable name for outcome
      )
    )
  ),

  # 3) Negative control concept set
  # Provide the ATLAS Concept Set ID and name for negative controls, or leave
  # as NULL/"" to indicate no negative control set provided here.
  negativeControlConceptSet = list(
    id = NULL,     # ATLAS Concept Set ID (replace NULL with integer if available)
    name = ""      # Descriptive name for the concept set
  ),

  # 4) Covariate selection
  # You can specify lists of Concept Set IDs to include or exclude from the
  # covariate set. Each entry is an object with id (Concept Set ID) and name.
  covariateSelection = list(
    conceptsToInclude = list(
      # Example single placeholder entry; replace with real concept set ids/names
      list(
        id = NULL,  # ATLAS Concept Set ID to include
        name = ""   # Human readable name for the concept set
      )
    ),
    conceptsToExclude = list(
      list(
        id = NULL,  # ATLAS Concept Set ID to exclude
        name = ""   # Human readable name for the concept set
      )
    )
  ),

  # 5) getDbCohortMethodDataArgs
  # These settings are passed to the CohortMethod function that constructs the
  # analysis-ready cohort-method data. studyPeriods can be used to constrain
  # data extraction by study date windows. Dates are expected as "yyyyMMdd"
  # strings; NULL indicates no date constraint for that bound.
  getDbCohortMethodDataArgs = list(
    studyPeriods = list(
      list(
        studyStartDate = NULL, # e.g., "20120101"; NULL means no lower bound
        studyEndDate = NULL    # e.g., "20201231"; NULL means no upper bound
      )
    ),
    maxCohortSize = 0 # 0 indicates no maximum size (no down-sampling at this step)
  ),

  # 6) createStudyPopArgs
  # These options control how the study population is constructed from the
  # cohortMethodData. They map to createStudyPopulation arguments.
  createStudyPopArgs = list(
    restrictToCommonPeriod = FALSE,   # Restrict to periods common to both exposure cohorts?
    firstExposureOnly = FALSE,        # Only include first exposure per subject?
    washoutPeriod = 365,              # Pre-index continuous observation required (days)
    removeDuplicateSubjects = "keep all", # "keep all" | "keep first" | "remove all"
    censorAtNewRiskWindow = FALSE,    # Censor when a new risk window starts (TRUE/FALSE)
    removeSubjectsWithPriorOutcome = TRUE, # Remove subjects with outcome in lookback?
    priorOutcomeLookBack = 365,       # Days of lookback to identify prior outcome

    # timeAtRisks: specify one or more risk windows. Each element defines:
    # - description: optional human-readable label
    # - riskWindowStart: integer (days relative to anchor)
    # - startAnchor: "cohort start" or "cohort end"
    # - riskWindowEnd: integer (days relative to anchor)
    # - endAnchor: "cohort start" or "cohort end"
    # - minDaysAtRisk: minimum days at risk
    timeAtRisks = list(
      list(
        description = "TAR 1",
        riskWindowStart = 365,
        startAnchor = "cohort start",   # anchor for start
        riskWindowEnd = 9999,
        endAnchor = "cohort start",     # anchor for end (note: both anchored to start)
        minDaysAtRisk = 1
      )
    )
  ),

  # 7) propensityScoreAdjustment
  # Settings for PS creation and application (matching / stratification).
  propensityScoreAdjustment = list(
    psSettings = list(
      list(
        description = "PS 1",
        # matchOnPsArgs used here (stratifyByPsArgs should be NULL when matching)
        matchOnPsArgs = list(
          maxRatio = 1,                    # Maximum comparator:target ratio (1 => 1:1)
          caliper = 0.2,                   # Caliper size; 0 => no caliper
          caliperScale = "standardized logit" # "propensity score" | "standardized" | "standardized logit"
        ),
        stratifyByPsArgs = NULL           # NULL because we are matching
      )
    ),

    # createPsArgs: controls PS model fitting and regularization behavior.
    # maxCohortSizeForFitting: down-sample if cohort > this value (0 means no down-sampling)
    createPsArgs = list(
      maxCohortSizeForFitting = 250000,
      errorOnHighCorrelation = TRUE,     # Whether to error if covariates highly collinear

      # prior: when regularization is used in PS model, specify prior type and CV usage
      prior = list(
        priorType = "laplace",           # e.g., "laplace" for L1 (lasso)
        useCrossValidation = TRUE
      ),

      # control: parameters for glmnet-like optimization and cross-validation
      control = list(
        tolerance = 2e-7,
        cvType = "auto",                 # "auto" or "grid"
        fold = 10,
        cvRepetitions = 10,
        noiseLevel = "silent",           # "silent" | "quiet" | "noisy"
        resetCoefficients = TRUE,
        startingVariance = 0.01
      )
    )
  ),

  # 8) fitOutcomeModelArgs
  # Settings to fit outcome model (e.g., Cox proportional hazards).
  fitOutcomeModelArgs = list(
    modelType = "cox",            # "logistic" | "poisson" | "cox"
    stratified = FALSE,           # Fit a stratified model (TRUE/FALSE)
    useCovariates = FALSE,        # Whether to include covariates other than PS
    inversePtWeighting = FALSE,   # Use IPTW instead of regression adjustment?

    # prior and control for regularized outcome models (if used)
    prior = list(
      priorType = "laplace",
      useCrossValidation = TRUE
    ),
    control = list(
      tolerance = 2e-7,
      cvType = "auto",
      fold = 10,
      cvRepetitions = 10,
      noiseLevel = "quiet",
      resetCoefficients = TRUE,
      startingVariance = 0.01
    )
  ),

  # 9) Top-level maxCohortSize
  # This is a convenience top-level cap; 0 indicates no cap.
  maxCohortSize = 0
)

###############################################################################
# Write the analysis specification to JSON
#
# - auto_unbox = TRUE ensures length-1 vectors are written as scalars.
# - pretty = TRUE produces human-readable JSON.
# - null = "null" makes sure R NULL values are serialized to JSON null.
#
# Output file is "analysisSpecification.json" in the current working directory.
###############################################################################

outputFile <- "analysisSpecification.json"

# Write JSON to disk
write_json(analysisSpecification,
           path = outputFile,
           pretty = TRUE,
           auto_unbox = TRUE,
           null = "null")

# Informational message to user (printed to stdout when script runs)
message("Analysis specification written to: ", normalizePath(outputFile))

# End of script
###############################################################################