#!/usr/bin/env Rscript

# CreateStrategusAnalysisSpecification.R
#
# This script builds an analysis specification (as a JSON file) suitable for use
# with OHDSI's Strategus-based study execution. It:
#  - demonstrates how the provided Analysis Specifications map to the JSON template,
#  - constructs an R list with the required structure,
#  - writes the list to disk as a JSON file that Strategus (and other OHDSI tools)
#    can read.
#
# NOTE:
#  - The script loads the Strategus package to indicate the intended ecosystem.
#    The actual writing of the specification is done via jsonlite, which produces
#    the JSON file that Strategus will use at runtime.
#  - Many cohort and concept set IDs are left as NULL placeholders (as provided).
#    Replace the NULLs with the appropriate ATLAS cohort IDs / concept set IDs
#    before running your study.
#
# Usage:
#  Rscript CreateStrategusAnalysisSpecification.R
#
# Output:
#  analysisSpecification.json (in the current working directory)
#
# -----------------------
# Load libraries
# -----------------------
# Load Strategus to indicate the intended runtime environment. Strategus is not
# strictly required to construct the JSON spec, but it will be used during study
# execution. jsonlite is used to serialize the specification to disk.
if (!requireNamespace("Strategus", quietly = TRUE)) {
  stop("The Strategus package is required but not installed. Please install it from OHDSI GitHub or CRAN as appropriate.")
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("The jsonlite package is required but not installed. Please install it.")
}
library(Strategus) # for context; not directly used to write JSON below
library(jsonlite)

# -----------------------
# Build the analysis specification
# -----------------------
# We follow the JSON Template structure. Fields provided as NULL are left NULL
# (they will be serialized as JSON null). Numeric dates in the Analysis
# Specifications are converted to string "yyyyMMdd" per the Template.
#
# If you wish to add more cohorts, covariate concept sets, or outcomes, add
# additional elements to the corresponding lists.

analysisSpecification <- list(
  # Human-readable study name. The Analysis Specifications had an empty name;
  # replace with a meaningful name for the study if desired.
  name = "Study Name",

  cohortDefinitions = list(
    targetCohort = list(
      id = NULL,            # ATLAS cohort ID for the target cohort (replace NULL)
      name = "Target Cohort Name"
    ),
    comparatorCohort = list(
      id = NULL,            # ATLAS cohort ID for the comparator cohort (replace NULL)
      name = "Comparator Cohort Name"
    ),
    outcomeCohort = list(
      # This is a list of outcome cohort definitions. The Analysis Specifications
      # provided a single outcome object with id NULL.
      list(
        id = NULL,          # ATLAS cohort ID for the outcome cohort (replace NULL)
        name = "Outcome Cohort Name"
      )
    )
  ),

  # Negative control concept set (ATLAS concept set id). Replace NULL with the
  # proper concept set id for negative controls.
  negativeControlConceptSet = list(
    id = NULL,
    name = "Negative Control Concept Set Name"
  ),

  # Covariate selection: concept sets to include and exclude.
  # Replace the NULL ids with concept set IDs from ATLAS if you want to include/exclude specific covariates.
  covariateSelection = list(
    conceptsToInclude = list(
      list(
        id = NULL,         # ATLAS Concept Set ID to include in covariates (replace NULL)
        name = "Concept Name 1"
      )
    ),
    conceptsToExclude = list(
      list(
        id = NULL,         # ATLAS Concept Set ID to exclude from covariates (replace NULL)
        name = "Concept Name 3"
      )
    )
  ),

  # Arguments for CohortMethod::getDbCohortMethodData (or equivalent Strategus module).
  # studyPeriods is a list of start/end date ranges; dates should be strings "yyyyMMdd".
  # maxCohortSize = 0 indicates "no maximum size".
  getDbCohortMethodDataArgs = list(
    studyPeriods = list(
      list(
        studyStartDate = sprintf("%08d", 20171201), # "20171201"
        studyEndDate = sprintf("%08d", 20231231)    # "20231231"
      )
    ),
    maxCohortSize = 0
  ),

  # Study population creation arguments. These map to createStudyPopulation/censoring settings.
  createStudyPopArgs = list(
    restrictToCommonPeriod = FALSE,   # do not restrict exposure cohorts to their common time period
    firstExposureOnly = FALSE,        # do not restrict to first exposure only
    washoutPeriod = 365,              # require 365 days of observation prior to index date
    removeDuplicateSubjects = "keep all", # options: "keep all" | "keep first" | "remove all"
    censorAtNewRiskWindow = FALSE,    # do not censor at start of a new risk window
    removeSubjectsWithPriorOutcome = TRUE, # remove subjects with outcome prior to time-at-risk
    priorOutcomeLookBack = 99999,     # very large look-back period (effectively unlimited)
    timeAtRisks = list(
      list(
        description = "TAR 1",        # textual description for this time-at-risk
        riskWindowStart = 0,
        startAnchor = "cohort start", # "cohort start" or "cohort end"
        riskWindowEnd = 0,
        endAnchor = "cohort end",     # "cohort start" or "cohort end"
        minDaysAtRisk = 1
      )
    )
  ),

  # Propensity score creation and adjustment settings.
  propensityScoreAdjustment = list(
    # psSettings is a list containing different strategies: one for matching, one for stratification.
    psSettings = list(
      list(
        description = "PS matching (1:1 caliper 0.2 on PS scale)",
        matchOnPsArgs = list(
          maxRatio = 1,              # maximum matching ratio (1:1)
          caliper = 0.2,             # caliper width
          caliperScale = "propensity score" # caliper scale option
        ),
        stratifyByPsArgs = NULL     # null because this entry uses matching
      ),
      list(
        description = "PS stratification (5 strata, base = all)",
        matchOnPsArgs = NULL,       # null because this entry uses stratification
        stratifyByPsArgs = list(
          numberOfStrata = 5,
          baseSelection = "all"     # "all" => strata defined across combined population
        )
      )
    ),

    # Settings used when fitting the propensity score model.
    createPsArgs = list(
      maxCohortSizeForFitting = 250000, # 0 would disable downsampling; here we allow up to 250k
      errorOnHighCorrelation = FALSE,   # do not error out on high covariate correlation
      prior = list(                     # regularization prior (Laplace => L1, with CV)
        priorType = "laplace",
        useCrossValidation = TRUE
      ),
      control = list(                   # control settings for Cyclops fitting (or equivalent)
        tolerance = 2e-7,
        cvType = "auto",                # cross-validation type
        fold = 10,
        cvRepetitions = 10,
        noiseLevel = "quiet",           # control the verbosity of the fitting routine
        resetCoefficients = FALSE,
        startingVariance = 0.01
      )
    )
  ),

  # Outcome model fitting arguments. This defines the model type and regularization.
  fitOutcomeModelArgs = list(
    modelType = "cox",                 # "cox" proportional hazards model
    stratified = TRUE,                 # stratified model (e.g., by matched pair or PS stratum)
    useCovariates = TRUE,              # include covariates in the outcome model
    inversePtWeighting = FALSE,        # do not use IPTW in this configuration
    prior = list(                      # prior for regularized outcome model
      priorType = "laplace",
      useCrossValidation = TRUE
    ),
    control = list(                    # control settings for the outcome-model regularization
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

# -----------------------
# Serialize to JSON
# -----------------------
# Write the specification to analysisSpecification.json. The parameters ensure:
#  - pretty formatting for human readability,
#  - auto_unbox ensures atomic vectors (length-one) are unboxed,
#  - nulls from R's NULL become JSON null (handled naturally by jsonlite).
outputFile <- "analysisSpecification.json"

# Use write_json; ensure NULL stays as null and not omitted. jsonlite will include
# NULLs in lists; however when a list element itself is NULL, jsonlite will drop
# it by default. To ensure explicit null entries (where intended) are serialized,
# we kept the list entries as explicit NULLs (as done above). jsonlite will
# drop list elements which are absent (missing), but will emit "null" for values
# explicitly set to NULL inside lists.
#
# If you later observe missing keys in the JSON because of top-level NULL list
# elements being dropped, you can replace NULL with jsonlite::unbox(NA) and set
# na = "null" in write_json. In our construction above, keys exist and values are NULL,
# so they will appear as JSON nulls.
write_json(analysisSpecification,
           path = outputFile,
           pretty = TRUE,
           auto_unbox = TRUE,
           null = "null")

cat(sprintf("Analysis specification written to: %s\n", outputFile))

# -----------------------
# Final notes for users
# -----------------------
#  - Before running a Strategus study using this specification:
#      * Replace all NULL cohort and concept set IDs with the actual ATLAS IDs.
#      * Adjust the study name and any descriptions to be informative.
#      * If you want to add multiple outcomes or additional covariate sets,
#        append them to the corresponding lists above.
#  - Strategus expects a particular directory layout and additional config files
#    (connection details, output folder settings, etc.). This script only creates
#    the analysis specification JSON; you still need to configure execution
#    settings for your environment.
#
# End of script.