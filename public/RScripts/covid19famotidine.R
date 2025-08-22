#!/usr/bin/env Rscript

# CreateStrategusAnalysisSpecification.R
#
# This script builds an OHDSI/Strategus analysis specification (as an R list)
# based on the supplied Analysis Specifications and writes it to a JSON file.
# The produced JSON can be used by Strategus-driven study execution frameworks.
#
# Notes:
# - We load the Strategus package to allow optional validation of the specification
#   if the validation function is available in your Strategus installation.
# - jsonlite is used to serialize the R list to JSON. The JSON layout follows the
#   Template provided in the Analysis Specifications section.
#
# Usage:
# Rscript CreateStrategusAnalysisSpecification.R
#
# Output:
# - "analysisSpecification.json" in the working directory

# Load required packages ------------------------------------------------------
# Strategus is used for (optional) validation. jsonlite does the JSON export.
suppressPackageStartupMessages({
  library(jsonlite)   # for write_json
  # Strategus may not be installed in every environment; the script will keep going
  # even if Strategus is not available, but we attempt to load it for validation.
  tryCatch({
    library(Strategus)
  }, error = function(e) {
    message("Strategus package is not available. The specification will still be written to JSON,")
    message("but optional Strategus validation will be skipped. Install Strategus to enable validation.")
  })
})

# Build analysis specification ------------------------------------------------
# The structure below mirrors the JSON Template and the provided Analysis Specifications.
# Fields are populated exactly as provided in the Analysis Specifications. If a field
# was null in the specifications it is represented as NULL here. Empty strings ("")
# are preserved where present.

analysisSpecification <- list(
  # Top-level name of the analysis. Empty here because Analysis Specifications gave "".
  name = "",

  # Cohort definitions: target, comparator and list of outcome cohorts.
  # Use NULL for atlas cohort IDs that are not specified.
  cohortDefinitions = list(
    targetCohort = list(
      id = NULL,   # ATLAS Cohort ID for the target cohort (NULL as per spec)
      name = ""    # Human readable name of the target cohort (empty string as per spec)
    ),
    comparatorCohort = list(
      id = NULL,   # ATLAS Cohort ID for the comparator cohort (NULL as per spec)
      name = ""    # Human readable name of the comparator cohort (empty string)
    ),
    outcomeCohort = list(
      # The specifications included a single outcome object with id = null and name = ""
      list(
        id = NULL, # ATLAS Cohort ID for the outcome (NULL)
        name = ""  # Outcome name (empty)
      )
    )
  ),

  # Negative control concept set (concept set id and name). NULL as per spec.
  negativeControlConceptSet = list(
    id = NULL,    # ATLAS Concept Set ID for negative controls
    name = ""     # Name of the negative control concept set
  ),

  # Covariate selection: lists of concept sets to include/exclude in covariate creation.
  covariateSelection = list(
    conceptsToInclude = list(
      # The Analysis Specifications included one concept to include with id = null and name = ""
      list(
        id = NULL,
        name = ""
      )
    ),
    conceptsToExclude = list(
      # The Analysis Specifications included one concept to exclude with id = null and name = ""
      list(
        id = NULL,
        name = ""
      )
    )
  ),

  # Arguments for getDbCohortMethodData (time periods and maximum cohort size)
  getDbCohortMethodDataArgs = list(
    studyPeriods = list(
      # A single study period is provided with explicit start and end dates (yyyyMMdd)
      list(
        studyStartDate = "20200201",  # Start date of study period (YYYYMMDD)
        studyEndDate = "20200530"     # End date of study period (YYYYMMDD)
      )
    ),
    maxCohortSize = 0  # 0 indicates no maximum size (as per spec)
  ),

  # Arguments used to create the study population from cohortMethod data
  createStudyPopArgs = list(
    restrictToCommonPeriod = FALSE,   # Do not restrict to the common observation period
    firstExposureOnly = FALSE,        # Do not restrict to the first exposure only
    washoutPeriod = 0,                # No washout period required
    removeDuplicateSubjects = "keep all", # Keep all duplicates as specified
    censorAtNewRiskWindow = FALSE,    # Do not censor at new risk window start
    removeSubjectsWithPriorOutcome = TRUE, # Remove subjects with prior outcome
    priorOutcomeLookBack = 30,        # Look back 30 days for prior outcomes
    timeAtRisks = list(
      # List of time-at-risk (TAR) definitions. One TAR as per specifications.
      list(
        # description omitted in Analysis Specifications; not required
        riskWindowStart = 1,        # Start of risk window relative to anchor
        startAnchor = "cohort start", # Anchor for start of risk window
        riskWindowEnd = 30,         # End of risk window relative to anchor
        endAnchor = "cohort start",   # Anchor for end of risk window
        minDaysAtRisk = 1           # Minimum required days at risk
      )
    )
  ),

  # Propensity score adjustment settings
  propensityScoreAdjustment = list(
    # Multiple PS strategies are provided in the specifications: stratification and matching.
    psSettings = list(
      # First PS setting: stratify by PS into 5 strata, baseSelection "all"
      list(
        matchOnPsArgs = NULL,  # NULL because this entry uses stratification
        stratifyByPsArgs = list(
          numberOfStrata = 5,
          baseSelection = "all" # Use both cohorts ("all") for strata cutpoints
        )
      ),

      # Second PS setting: match on PS with caliper and max ratio
      list(
        matchOnPsArgs = list(
          maxRatio = 1,                # Maximum matching ratio (1:1 matching)
          caliper = 0.2,               # Caliper width
          caliperScale = "standardized logit" # Caliper scale (standardized logit)
        ),
        stratifyByPsArgs = NULL # NULL because this entry uses matching
      )
    ),

    # Arguments used to create propensity scores (regularized logistic regression settings)
    createPsArgs = list(
      maxCohortSizeForFitting = 250000, # Maximum cohort size for fitting; 0 would disable downsampling
      errorOnHighCorrelation = TRUE,    # Throw an error if covariates are highly correlated
      prior = list(                     # Regularization prior settings
        priorType = "laplace",          # L1 (lasso / laplace) regularization
        useCrossValidation = TRUE       # Use CV to select regularization parameter
      ),
      control = list(                   # Control parameters for model fitting and CV
        tolerance = 2e-07,
        cvType = "auto",                # Let the algorithm choose CV approach
        fold = 10,
        cvRepetitions = 10,
        noiseLevel = "silent",          # Verbosity level (silent)
        resetCoefficients = TRUE,
        startingVariance = 0.01         # Initial variance for regularization solver
      )
    )
  ),

  # Outcome model fitting arguments (Cox model, stratified, with regularization settings)
  fitOutcomeModelArgs = list(
    modelType = "cox",          # Cox proportional hazards model
    stratified = TRUE,          # Fit stratified model (e.g., strata from PS)
    useCovariates = FALSE,      # Do not use covariates directly (rely on stratification/IPTW)
    inversePtWeighting = FALSE, # Do not use IPTW in this configuration
    prior = list(               # Regularization for outcome model (if applicable)
      priorType = "laplace",
      useCrossValidation = TRUE
    ),
    control = list(             # Control parameters for outcome model fitting
      tolerance = 2e-07,
      cvType = "auto",
      fold = 10,
      cvRepetitions = 10,
      noiseLevel = "quiet",
      resetCoefficients = TRUE,
      startingVariance = 0.01
    )
  )
)

# Optionally validate the specification using Strategus (if available) -----------
# Not all versions of Strategus provide a validator; check before calling.
if ("Strategus" %in% rownames(utils::installed.packages())) {
  # Use tryCatch to avoid stopping the script if the validation function is not present.
  tryCatch({
    # Some Strategus versions expose a validateAnalysisSpecification function.
    # We attempt to call it if available. If not available, we skip validation.
    if (exists("validateAnalysisSpecification", where = asNamespace("Strategus"), inherits = FALSE)) {
      # validateAnalysisSpecification typically returns TRUE if valid, otherwise throws or returns errors
      validationResult <- Strategus::validateAnalysisSpecification(analysisSpecification)
      if (isTRUE(validationResult)) {
        message("Analysis specification validated by Strategus.")
      } else {
        # If the function returns details rather than TRUE, print them.
        message("Strategus validation result:")
        print(validationResult)
      }
    } else {
      message("Strategus is installed but does not provide 'validateAnalysisSpecification' in this version; skipping validation.")
    }
  }, error = function(e) {
    message("An error occurred while attempting Strategus validation: ", e$message)
    message("Proceeding to write JSON without Strategus validation.")
  })
} else {
  # Strategus is not installed; user was notified earlier.
  message("Skipping Strategus validation because the package is not installed.")
}

# Write JSON file -------------------------------------------------------------
# We use auto_unbox = TRUE to avoid unnecessary arrays for single values and pretty = TRUE
# for readable formatting. Null values (R NULL) will be converted to JSON null.
outputFile <- "analysisSpecification.json"
write_json(analysisSpecification, path = outputFile, auto_unbox = TRUE, pretty = TRUE, na = "null")

message(sprintf("Analysis specification written to '%s'.", outputFile))

# End of script.