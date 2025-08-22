#!/usr/bin/env Rscript

# CreateStrategusAnalysisSpecification.R
# This script builds an analysis specification (as an R list) using the settings provided
# in the Analysis Specifications and writes it to a JSON file suitable for use with
# OHDSI Strategus (or other pipeline components that consume the same JSON structure).
#
# NOTE:
# - The script uses the Strategus package (loaded for context) and jsonlite for JSON output.
# - Many fields intentionally contain NULL or empty strings where the Analysis Specifications
#   provided null/empty values. Users should replace those NULL/empty values with actual
#   ATLAS cohort/concept set IDs and human-readable names before running the analysis.
#
# Usage:
#  - Edit the filed values below (IDs and names) where NULL or "" currently appear.
#  - Run this script to produce "analysisSpecification.json" in the working directory.
#
# Output:
#  - analysisSpecification.json : JSON file containing the analysis specification.

# Load required packages -------------------------------------------------------
# Strategus is loaded to indicate the intended execution environment. The actual
# JSON creation is performed with jsonlite for robust JSON formatting.
suppressPackageStartupMessages({
  library(Strategus)  # for context; Strategus is typically used to run the specification after creation
  library(jsonlite)   # for writing the JSON file
})

# Build the analysis specification ------------------------------------------------
# The structure follows the provided Template and the concrete values from
# the Analysis Specifications JSON in the request.
analysisSpecification <- list(
  # Friendly name of the study/analysis
  name = "", # Study Name -- replace with descriptive study name

  # Cohort definitions:
  cohortDefinitions = list(
    targetCohort = list(
      id = NULL, # ATLAS Cohort ID for the target cohort (replace NULL with integer ID)
      name = ""  # Human-readable name for target cohort
    ),
    comparatorCohort = list(
      id = NULL, # ATLAS Cohort ID for the comparator cohort (replace NULL with integer ID)
      name = ""  # Human-readable name for comparator cohort
    ),
    # List of outcome cohorts (can contain multiple outcome definitions)
    outcomeCohort = list(
      list(
        id = NULL, # ATLAS Cohort ID for the outcome (replace NULL with integer ID)
        name = ""  # Human-readable outcome name
      )
    )
  ),

  # Negative control concept set (ATLAS concept set id and name)
  negativeControlConceptSet = list(
    id = NULL,  # ATLAS Concept Set ID for negative controls
    name = ""    # Human-readable name for negative control concept set
  ),

  # Covariate selection: concept sets to include or exclude when defining custom covariates
  covariateSelection = list(
    conceptsToInclude = list(
      list(
        id = NULL, # ATLAS Concept Set ID to include as covariate(s)
        name = ""  # Name for the included concept set
      )
      # Add additional concept sets here if needed
    ),
    conceptsToExclude = list(
      list(
        id = NULL, # ATLAS Concept Set ID to exclude from covariate selection
        name = ""  # Name for the excluded concept set
      )
      # Add additional excluded concept sets as needed
    )
  ),

  # Arguments used to query the database for cohort method data (cohort counts, covariates, etc.)
  getDbCohortMethodDataArgs = list(
    studyPeriods = list(
      # Each object defines a study period with start and end dates (yyyyMMdd)
      list(
        studyStartDate = "20111101",
        studyEndDate = "20190331"
      ),
      list(
        studyStartDate = "20130301",
        studyEndDate = "20161231"
      )
    ),
    # Maximum cohort size when fetching data; 0 indicates no maximum (use all subjects)
    maxCohortSize = 0
  ),

  # Arguments used to create the study population and time-at-risk definitions
  createStudyPopArgs = list(
    # If TRUE, restrict target and comparator exposures to their common observation period
    restrictToCommonPeriod = FALSE,
    # If TRUE, restrict to the first exposure per person
    firstExposureOnly = FALSE,
    # Required washout period (days) before index to ensure prior observation
    washoutPeriod = 365,
    # How to handle duplicate subjects within a cohort:
    # "keep all" | "keep first" | "remove all"
    removeDuplicateSubjects = "keep all",
    # If TRUE, censor at the start of a new risk window (e.g., at subsequent exposures)
    censorAtNewRiskWindow = FALSE,
    # If TRUE, remove subjects with prior outcome within the lookback window
    removeSubjectsWithPriorOutcome = TRUE,
    # Lookback window (days) to determine prior outcome; 99999 typically means "ever before"
    priorOutcomeLookBack = 99999,
    # One or more time-at-risk (TAR) definitions. Each TAR defines how long follow-up is.
    timeAtRisks = list(
      # TAR 1: 0 to 365 days after cohort start (inclusive behavior defined by analysis engine)
      list(
        description = "Risk window: cohort start + 0 to 365 days",
        riskWindowStart = 0,
        startAnchor = "cohort start",  # anchor can be "cohort start" or "cohort end"
        riskWindowEnd = 365,
        endAnchor = "cohort start",    # anchor for end ("cohort start" or "cohort end")
        minDaysAtRisk = 1
      ),
      # TAR 2: 0 to 1825 days (5 years) after cohort start
      list(
        description = "Risk window: cohort start + 0 to 1825 days",
        riskWindowStart = 0,
        startAnchor = "cohort start",
        riskWindowEnd = 1825,
        endAnchor = "cohort start",
        minDaysAtRisk = 1
      ),
      # TAR 3: From day 1 after cohort start until cohort end
      # Note: riskWindowEnd = 0 with endAnchor = "cohort end" means "until cohort end"
      list(
        description = "Risk window: day 1 after cohort start until cohort end",
        riskWindowStart = 1,
        startAnchor = "cohort start",
        riskWindowEnd = 0,
        endAnchor = "cohort end",
        minDaysAtRisk = 1
      )
    )
  ),

  # Propensity score creation and adjustment settings
  propensityScoreAdjustment = list(
    # A list of PS-based adjustment strategies (matching or stratification)
    psSettings = list(
      # Setting 1: 1:1 PS matching with caliper = 0.2 (standardized logit scale)
      list(
        description = "1:1 PS matching, caliper 0.2 (standardized logit)",
        matchOnPsArgs = list(
          maxRatio = 1,                # maximum matching ratio (1:1)
          caliper = 0.2,               # caliper width; 0 means no caliper
          caliperScale = "standardized logit" # scale for caliper: "propensity score" | "standardized" | "standardized logit"
        ),
        stratifyByPsArgs = NULL
      ),
      # Setting 2: variable ratio matching up to 1:10 with caliper 0.2
      list(
        description = "Up to 1:10 PS matching, caliper 0.2 (standardized logit)",
        matchOnPsArgs = list(
          maxRatio = 10,               # up to 10 comparators per target
          caliper = 0.2,
          caliperScale = "standardized logit"
        ),
        stratifyByPsArgs = NULL
      ),
      # Setting 3: Stratification on PS into 10 strata (deciles), using all subjects as base
      list(
        description = "Stratify by PS into 10 strata (all as base)",
        matchOnPsArgs = NULL,
        stratifyByPsArgs = list(
          numberOfStrata = 10,
          baseSelection = "all"  # "all" | "target" | "comparator"
        )
      )
    ),

    # Arguments controlling how the propensity score is created (regularized regression settings)
    createPsArgs = list(
      # Maximum cohort size for fitting the PS model; 250000 means larger cohorts will be downsampled
      maxCohortSizeForFitting = 250000,
      # Whether to error out if extremely high correlations among covariates are detected
      errorOnHighCorrelation = TRUE,
      # Prior specification for regularization; NULL would indicate no regularization
      prior = list(
        priorType = "laplace",         # "laplace" indicates L1 regularization (lasso-like)
        useCrossValidation = TRUE     # use CV to select penalty
      ),
      # Control settings for the regularized regression solver and cross-validation
      control = list(
        tolerance = 2e-7,
        cvType = "auto",              # "auto" or "grid"
        fold = 10,
        cvRepetitions = 10,
        noiseLevel = "silent",        # verbosity: "silent" | "quiet" | "noisy"
        resetCoefficients = TRUE,
        startingVariance = 0.01       # starting variance for the prior (or -1 to auto-estimate)
      )
    )
  ),

  # Outcome model fitting arguments
  fitOutcomeModelArgs = list(
    modelType = "cox",           # "logistic" | "poisson" | "cox"
    stratified = FALSE,          # whether to fit a stratified model
    useCovariates = FALSE,       # whether to include covariates (beyond PS adjustment) in outcome model
    inversePtWeighting = FALSE,  # whether to use IPTW in outcome model
    # Prior and control are used when regularized outcome models are desired;
    # here we include a Laplace prior with CV as in the PS settings.
    prior = list(
      priorType = "laplace",
      useCrossValidation = TRUE
    ),
    control = list(
      tolerance = 2e-7,
      cvType = "auto",
      fold = 10,
      cvRepetitions = 10,
      noiseLevel = "quiet",        # quieter verbosity for outcome model fitting
      resetCoefficients = TRUE,
      startingVariance = 0.01
    )
  )
)

# Write JSON file ---------------------------------------------------------------
# Default file name; change if desired.
outputFile <- "analysisSpecification.json"

# Use jsonlite::write_json to produce a nicely formatted JSON file.
# auto_unbox = TRUE ensures single-element vectors are written as scalars,
# pretty = TRUE makes the file human-readable.
#
# Note: jsonlite will omit list elements that are R NULL. If you require explicit
# JSON null values for particular keys (instead of omitting the key), replace
# NULL with I(list(NULL)) or explicitly set a value of NA and use na = "null".
# Here we preserve natural R semantics and allow keys with NULL to be omitted.
write_json(analysisSpecification, path = outputFile, pretty = TRUE, auto_unbox = TRUE)

# Informative message (printed to console) about the created file
message(sprintf("Analysis specification written to: %s", normalizePath(outputFile, mustWork = FALSE)))

# End of script ----------------------------------------------------------------
# After reviewing or editing the produced JSON file, you can hand it to Strategus
# to execute the analysis pipeline (for example, via Strategus::execute or by using
# the OHDSI study runner that accepts this specification).
#
# Example (pseudo):
# Strategus::execute(analysisSpecificationFile = outputFile, ...additional arguments...)
#
# Replace NULL and empty string placeholders above with real ATLAS IDs and names
# before running the analysis in a production environment.