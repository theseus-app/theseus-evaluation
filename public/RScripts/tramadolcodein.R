#!/usr/bin/env Rscript

# CreateStrategusAnalysisSpecification.R
# This script builds an OHDSI/Strategus analysis specification (JSON) from the settings
# given in the provided "Analysis Specifications" and the "Template".
# The script uses the Strategus package (loaded below) for provenance/compatibility reasons,
# and uses jsonlite to write a JSON file that can be consumed by Strategus workflows.
#
# IMPORTANT:
# - Fields that were null in the Analysis Specifications are set to NA_* (NA_integer_, NA_character_)
#   in R. When writing the JSON, those NA values are emitted as JSON null.
# - Edit the cohort IDs, concept set IDs and names, study period dates, and any other
#   placeholder values before running in a production workflow.
#
# Output: analysisSpecification.json in the working directory

# Load required packages. Strategus is loaded to indicate the target execution environment
# (and could be used for validation if newer Strategus helper functions are available).
suppressPackageStartupMessages({
  library(Strategus)   # used for compatibility and potential validation calls; intentionally loaded
  library(jsonlite)    # used to create the JSON specification
})

# ------------------------------------------------------------------------------
# Build the analysis specification as an R list following the Template structure.
# Comments here explain how each piece maps back to the Analysis Specifications input.
# ------------------------------------------------------------------------------

analysisSpecification <- list(
  # Top-level analysis name: keep as "Study Name" placeholder from Template.
  # In the provided Analysis Specifications the "name" was empty, so a placeholder is used.
  name = "Study Name",

  # Cohort definitions: target, comparator and one-or-more outcome cohorts.
  # ID fields were null in the provided Analysis Specifications, therefore set to NA_integer_.
  cohortDefinitions = list(
    targetCohort = list(
      id = NA_integer_,   # ATLAS Cohort ID for target (NULL in input -> NA here -> null in JSON)
      name = ""           # Cohort name was empty in the input
    ),
    comparatorCohort = list(
      id = NA_integer_,   # ATLAS Cohort ID for comparator
      name = ""           # Cohort name empty in the input
    ),
    outcomeCohort = list(
      # The input allowed multiple outcome cohorts; the provided spec had one entry with null id.
      list(
        id = NA_integer_, # ATLAS Cohort ID for outcome (NULL in input)
        name = ""         # Outcome name empty in input
      )
    )
  ),

  # Negative control concept set (was null in input -> NA_integer_ here)
  negativeControlConceptSet = list(
    id = NA_integer_,    # ATLAS Concept Set ID for negative controls (NULL -> NA)
    name = ""            # Name empty in input
  ),

  # Covariate selection: concept sets to include and exclude.
  # Input had null IDs for included/excluded concept sets -> use NA_integer_.
  covariateSelection = list(
    conceptsToInclude = list(
      list(
        id = NA_integer_, # ATLAS Concept Set ID to include (NULL in input)
        name = ""         # Name empty in input
      )
    ),
    conceptsToExclude = list(
      list(
        id = NA_integer_, # ATLAS Concept Set ID to exclude (NULL in input)
        name = ""         # Name empty in input
      )
    )
  ),

  # Arguments used when fetching cohort method data from the database.
  # studyPeriods had null start/end dates in the input -> NA_character_ here.
  getDbCohortMethodDataArgs = list(
    studyPeriods = list(
      list(
        studyStartDate = NA_character_, # yyyyMMdd; null in input -> NA_character_ here
        studyEndDate = NA_character_    # yyyyMMdd; null in input -> NA_character_ here
      )
    ),
    # maxCohortSize in the Analysis Specifications was 0 (no maximum)
    maxCohortSize = 0
  ),

  # createStudyPopArgs: controls how the study population is derived from cohort entries.
  # These values are set directly from the Analysis Specifications.
  createStudyPopArgs = list(
    restrictToCommonPeriod = FALSE,     # false in input
    firstExposureOnly = FALSE,          # false in input
    washoutPeriod = 0,                  # 0 in input
    removeDuplicateSubjects = "keep all",# "keep all" in input
    censorAtNewRiskWindow = FALSE,      # false in input
    removeSubjectsWithPriorOutcome = TRUE, # true in input
    priorOutcomeLookBack = 365,         # 365 in input
    timeAtRisks = list(
      # The Analysis Specifications included two time-at-risk definitions. We supply both.
      list(
        description = "TAR 1",          # descriptive label derived from template example
        riskWindowStart = 0,
        startAnchor = "cohort start",   # "cohort start" per spec
        riskWindowEnd = 0,
        endAnchor = "cohort end",       # "cohort end"
        minDaysAtRisk = 1
      ),
      list(
        description = "TAR 2",
        riskWindowStart = 0,
        startAnchor = "cohort start",
        riskWindowEnd = 9999,
        endAnchor = "cohort start",     # second TAR uses cohort start as end anchor per input
        minDaysAtRisk = 1
      )
    )
  ),

  # Propensity score adjustment settings:
  # - psSettings: array with one setting that uses matching (matchOnPsArgs). stratifyByPsArgs is null.
  # - createPsArgs: regularization/prior and optimizer control settings come from the input.
  propensityScoreAdjustment = list(
    psSettings = list(
      list(
        description = "PS 1",  # simple descriptive label
        # The input specified matchOnPsArgs (not stratifyByPsArgs), so we fill these values:
        matchOnPsArgs = list(
          maxRatio = 1,                     # maximum matching ratio from input
          caliper = 0.2,                    # caliper width from input
          caliperScale = "standardized logit" # scale for caliper
        ),
        stratifyByPsArgs = NULL            # null in input -> will become JSON null
      )
    ),
    createPsArgs = list(
      # Maximum cohort size used when fitting the PS model. 250000 in the input.
      maxCohortSizeForFitting = 250000,
      # Errors should be thrown if there is high correlation among covariates
      errorOnHighCorrelation = TRUE,
      # Prior specification for regularized regression (laplace + CV per input)
      prior = list(
        priorType = "laplace",
        useCrossValidation = TRUE
      ),
      # Control list used by the PS-fitting routine (matching input values)
      control = list(
        tolerance = 2e-7,
        cvType = "auto",
        fold = 10,
        cvRepetitions = 10,
        noiseLevel = "silent",
        resetCoefficients = TRUE,
        startingVariance = 0.01
      )
    )
  ),

  # Outcome model fit arguments:
  # The input specified a Cox model, no covariates, no IPTW, and regularization priors/control.
  fitOutcomeModelArgs = list(
    modelType = "cox",           # "cox" per Analysis Specifications
    stratified = FALSE,          # false in input
    useCovariates = FALSE,       # false in input
    inversePtWeighting = FALSE,  # false in input
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

  # Top-level maximum cohort size (0 indicates no maximum)
  maxCohortSize = 0
)

# ------------------------------------------------------------------------------
# Convert the R list to JSON and write to file.
# - We use jsonlite::toJSON with auto_unbox = TRUE so that single values are not
#   wrapped into arrays unnecessarily.
# - We set na = "null" so that NA_* values are emitted as JSON null.
# - We set null = "null" so any explicit NULLs become JSON null.
# ------------------------------------------------------------------------------

outputFile <- "analysisSpecification.json"

# Convert to JSON string first (allows full control over formatting)
jsonText <- jsonlite::toJSON(analysisSpecification,
                             auto_unbox = TRUE,
                             pretty = TRUE,
                             na = "null",
                             null = "null")

# Write the JSON to disk
writeLines(jsonText, con = outputFile)

# Informative message (printed to stdout when script is run)
message("Analysis specification written to: ", normalizePath(outputFile))

# End of script.
# Note: Before executing Strategus workflows using the produced specification,
# make sure to replace the placeholder NA ids and empty names with the real
# ATLAS cohort IDs, concept set IDs and names, and to set study period dates
# (yyyyMMdd) where appropriate.