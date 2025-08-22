#!/usr/bin/env Rscript

# CreateStrategusAnalysisSpecification.R
# This script constructs an OHDSI/Strategus analysis specification based on the
# provided analysis settings and writes it to a JSON file that can be consumed
# by Strategus.
#
# Notes:
# - The script uses the Strategus package (loaded below) to indicate intended
#   usage; the actual writing of the specification is handled with jsonlite so
#   the JSON formatting is explicit and predictable.
# - NULL values for cohort/concept IDs are preserved as JSON null values so
#   downstream processes can detect that an ID was intentionally left unspecified.
#
# Usage:
#   Rscript CreateStrategusAnalysisSpecification.R
#
# Output:
#   analysisSpecification.json in the working directory

# Load required packages
# Strategus is loaded to indicate the intended integration target and for users
# who will run Strategus functions after creating this JSON. jsonlite is used
# to write the JSON file.
suppressPackageStartupMessages({
  library(Strategus)   # used downstream by study execution; loaded here for context
  library(jsonlite)    # used to write the JSON spec
})

# Construct the analysis specification as an R list.
# Comments are included inline to explain each field and how it maps to the
# settings provided in the Analysis Specifications and the Template.
analysisSpecification <- list(
  # Top-level name for the study/specification. According to the provided JSON
  # this was empty; you may change it to a descriptive name if desired.
  name = "",

  # Definitions of the cohorts used in the study:
  cohortDefinitions = list(
    targetCohort = list(
      id = NULL,              # ATLAS Cohort ID for the target cohort (NULL = unspecified)
      name = ""               # Human-readable name for the target cohort
    ),
    comparatorCohort = list(
      id = NULL,              # ATLAS Cohort ID for the comparator cohort (NULL = unspecified)
      name = ""               # Human-readable name for the comparator cohort
    ),
    # Outcomes: list of outcome cohort definitions; multiple outcomes are supported
    outcomeCohort = list(
      list(
        id = NULL,            # ATLAS Cohort ID for the first outcome (NULL = unspecified)
        name = ""             # Human-readable name for the outcome
      )
    )
  ),

  # Negative control concept set: typically a concept set ID defined in ATLAS
  negativeControlConceptSet = list(
    id = NULL,                # ATLAS Concept Set ID (NULL = unspecified)
    name = ""                 # Human-readable name for the negative control concept set
  ),

  # Covariate selection: explicit concept sets to include/exclude when creating covariates
  covariateSelection = list(
    conceptsToInclude = list(
      list(
        id = NULL,            # ATLAS Concept Set ID to include (NULL = unspecified)
        name = ""             # Name of the concept set to include
      )
      # Add more include concept sets as needed
    ),
    conceptsToExclude = list(
      list(
        id = NULL,            # ATLAS Concept Set ID to exclude (NULL = unspecified)
        name = ""             # Name of the concept set to exclude
      )
      # Add more exclude concept sets as needed
    )
  ),

  # Arguments passed to CohortMethod::getDbCohortMethodData (or Strategus wrapper)
  getDbCohortMethodDataArgs = list(
    # Study periods: multiple date ranges can be defined. Dates are strings in yyyyMMdd.
    studyPeriods = list(
      list(studyStartDate = "20010101", studyEndDate = "20171231"),
      list(studyStartDate = "20010101", studyEndDate = "20151130")
    ),
    # Maximum cohort size: 0 indicates no maximum (do not downsample at this stage).
    maxCohortSize = 0
  ),

  # Arguments to create the study population from cohorts
  createStudyPopArgs = list(
    # Whether to restrict target and comparator to their common enrollment period.
    restrictToCommonPeriod = FALSE,
    # Whether to restrict to first exposure only (first drug era / first cohort entry)
    firstExposureOnly = FALSE,
    # Washout (observation) period required before cohort entry
    washoutPeriod = 0,
    # How to handle duplicate subject records within a cohort:
    # "keep all" | "keep first" | "remove all"
    removeDuplicateSubjects = "keep all",
    # When a new risk window starts for the same person, should we censor at that time?
    censorAtNewRiskWindow = FALSE,
    # Remove subjects who had the outcome prior to the risk window
    removeSubjectsWithPriorOutcome = TRUE,
    # Number of days to look back for prior outcomes (99999 implies a long look-back)
    priorOutcomeLookBack = 99999,

    # Time-at-risk definitions (one or more). Multiple TARs are supported.
    timeAtRisks = list(
      list(
        # Optional description for the TAR. Template example had "TAR 1" but spec omitted.
        # description = "TAR 1",

        # Risk window start relative to the start anchor (see startAnchor)
        riskWindowStart = 1,
        # Anchor for riskWindowStart: "cohort start" or "cohort end"
        startAnchor = "cohort start",
        # Risk window end relative to endAnchor. A value of 0 typically indicates
        # the end anchor itself (e.g., end of cohort).
        riskWindowEnd = 0,
        # Anchor for riskWindowEnd: "cohort start" or "cohort end"
        endAnchor = "cohort end",
        # Minimum days at risk required to include subject in analysis
        minDaysAtRisk = 1
      )
    )
  ),

  # Propensity score creation and adjustment settings
  propensityScoreAdjustment = list(
    # List of PS-based strategies to apply. Each entry can specify matching or stratification.
    psSettings = list(
      list(
        # Description optional; omitted in provided spec
        # description = "PS 1",

        # Matching arguments (NULL means stratify by PS instead)
        matchOnPsArgs = list(
          maxRatio = 1,                       # maximum number of comparator per target
          caliper = 0.05,                     # caliper width
          caliperScale = "propensity score"   # scale used for caliper ("propensity score")
        ),
        stratifyByPsArgs = NULL              # Not used when matching
      ),
      list(
        matchOnPsArgs = list(
          maxRatio = 10,
          caliper = 0.2,
          caliperScale = "standardized logit" # use standardized logit for caliper scaling
        ),
        stratifyByPsArgs = NULL
      )
    ),

    # Settings for creating propensity scores (regularized regression settings)
    createPsArgs = list(
      # If > 0, downsample when fitting PS to limit memory/time. Here 250000 per spec.
      maxCohortSizeForFitting = 250000,
      # Whether to error if covariates show high correlation
      errorOnHighCorrelation = TRUE,
      # Prior specification for regularized regression (laplace = L1)
      prior = list(
        priorType = "laplace",
        useCrossValidation = TRUE
      ),
      # Control arguments for fitting and cross-validation
      control = list(
        tolerance = 2e-7,
        cvType = "auto",
        fold = 10,
        cvRepetitions = 10,
        noiseLevel = "quiet",     # "silent" | "quiet" | "noisy" - prefer quiet as per spec
        resetCoefficients = TRUE,
        startingVariance = 0.01
      )
    )
  ),

  # Outcome model fitting arguments
  fitOutcomeModelArgs = list(
    # Type of model to fit: "cox", "logistic", or "poisson"
    modelType = "cox",
    # If TRUE and matching/stratification applied, fit a stratified model
    stratified = TRUE,
    # Whether to include additional covariates in the outcome model (beyond PS)
    useCovariates = FALSE,
    # Whether to use inverse probability of treatment weighting instead of PS-based design
    inversePtWeighting = FALSE,
    # Prior specification (regularization) for the outcome model (laplace with CV)
    prior = list(
      priorType = "laplace",
      useCrossValidation = TRUE
    ),
    # Control parameters for fitting the outcome model (CV, tolerance, etc.)
    control = list(
      tolerance = 2e-7,
      cvType = "auto",
      fold = 10,
      cvRepetitions = 10,
      noiseLevel = "quiet",
      resetCoefficients = TRUE,
      startingVariance = 0.01
    )
  )
)

# Write the specification to JSON.
# We keep NULL values in the R list so they will be written as JSON null.
# jsonlite::write_json accepts 'null' to control how R NULL is represented; we
# set null = "null" to ensure NULL -> JSON null (instead of dropping the key).
outputFile <- "analysisSpecification.json"
write_json(analysisSpecification, path = outputFile, pretty = TRUE, auto_unbox = TRUE, null = "null")

# Informational message (prints when script is run interactively or via Rscript)
message(sprintf("Analysis specification written to '%s'.", outputFile))

# Example next steps for users (not executed here):
# - Use Strategus functions (Strategus::createMultipleAnalyses / Strategus::execute)
#   or custom runner code to read analysisSpecification.json and run the study.
#
# Example (conceptual):
#   spec <- jsonlite::read_json("analysisSpecification.json", simplifyVector = TRUE)
#   Strategus::createAndRunAnalyses(analysisSpecification = spec, ...)
#
# End of script.