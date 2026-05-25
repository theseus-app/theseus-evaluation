import path from "node:path";
import fs from "node:fs/promises";
import { getProvider, studyDtoSchema, canonicalTemplate, type StudyDTO } from "theseus-core";
import dotenv from "dotenv";
dotenv.config();

const TEMPLATE_PATH = path.resolve(process.cwd(), "public", "templates", "customAtlasTemplate_v1.5.0_annotated.txt");

type Vendor = "OPENAI" | "GEMINI" | "DEEPSEEK" | "CLAUDE";
type Size = "FLAGSHIP" | "LIGHT";

async function readTextFile(abs: string) {
  return fs.readFile(abs, "utf8");
}

export async function text2jsonPDF(
  pdfPath: string,
  vendor: Vendor,
  size: Size,
): Promise<{ updatedSpec: StudyDTO | null; rawResponse: string }> {
  if (vendor === "DEEPSEEK") {
    throw new Error(
      "DeepSeek does not support PDF/image input. Extract text from the PDF and call text2json instead.",
    );
  }

  const analysisSpecificationsTemplate = await readTextFile(TEMPLATE_PATH);

  const json_fields_descriptions = `
    ["name"] : We can give the analysis a unique name.
["cohortDefinitions"] : We assume the cohorts have already been created in ATLAS.
["negativeControlConceptSet"] : Negative control outcomes are outcomes that are not believed to be caused by either the target or the comparator. Here we assume the concept set has already been created.
["covariateSelection"]["conceptsToInclude"] : When specifying covariates here, all other covariates (aside from those you specified) are left out. We usually want to include all baseline covariates, letting the regularized regression build a model that balances all covariates. The only reason we might want to specify particular covariates is to replicate an existing study that manually picked covariates.
["covariateSelection"]["conceptsToExclude"] : Rather than specifying which concepts to include, we can instead specify concepts to exclude. When we submit a concept set in this field, we use every covariate except for those that we submitted.
["getDbCohortMethodDataArgs"][studyPeriods"] : Study start and end dates can be used to limit the analyses to a specific period. The study end date also truncates risk windows, meaning no outcomes beyond the study end date will be considered. Leave blank to use all time.
["getDbCohortMethodDataArgs"]["restrictToCommonPeriod"] : Should the study be restricted to the period when both exposures are present? (E.g. when both drugs are on the market)
["getDbCohortMethodDataArgs"]["firstExposureOnly"] : Can be used to restrict to the first exposure per patient.
["getDbCohortMethodDataArgs"]["washoutPeriod"] : The minimum required continuous observation time prior to index date for a person to be included in the cohort.
["getDbCohortMethodDataArgs"]["removeDuplicateSubjects"] :  What happens when a subject is in both target and comparator cohorts. "Keep All" indicating to keep the subjects in both cohorts. With this option it might be possible to double-count subjects and outcomes. "Keep First" indicating to keep the subject in the first cohort that occurred. "Remove All" indicating to remove the subject from both cohorts.
["createStudyPopArgs"]["censorAtNewRiskWindow"] : If the options "keep all" or "keep first" are selected, we may wish to censor the time when a person is in both cohorts.
["createStudyPopArgs"]["removeSubjectsWithPriorOutcome"] : We can choose to remove subjects that have the outcome prior to the risk window start.
["createStudyPopArgs"]["priorOutcomeLookBack"] : If we choose to remove people that had the outcome before, we can select how many days we should look back when identifying prior outcomes.
["timeAtRisks"] : We set the start of time-at-risk to one day after cohort start, so one day after treatment initiation. A reason to set the time-at-risk start to be later than the cohort start is because we may want to exclude outcome events that occur on the day of treatment initiation if we do not believe it biologically plausible they can be caused by the drug.We can set the end of the time-at-risk to the cohort end, so when exposure stops, which can be referred to as an "on-treatment" design. We can choose to set the end of the time-at-risk to a fixed duration after cohort entry regardless of whether exposure continues, which can be referred to as an "intent-to-treat" design. In the extreme we could set the time-at-risk end to a large number of days (e.g. 99999) after cohort entry, meaning we will effectively follow up subjects until observation end. 
["propensityScoreAdjustment"]["psSettings"] : We can choose to stratify or match on the propensity score. When stratifying we need to specify the number of strata and whether to select the strata based on the target, comparator, or entire study population. When matching we need to specify the maximum number of people from the comparator group to match to each person in the target group. Typical values are 1 for one-on-one matching, or a large number (e.g. 100) for variable-ratio matching. We also need to specify the caliper: the maximum allowed difference between propensity scores to allow a match. The caliper can be defined on difference caliper scales: the propensity score scale (the PS itself), the standardized scale (in standard deviations of the PS distributions), the standardized logit scale (in standard deviations of the PS distributions after the logit transformation to make the PS more normally distributed).
["propensityScoreAdjustment"]["createPsArgs"]["maxCohortSizeForFitting"] : What is the maximum number of people to include in the propensity score model when fitting?
["propensityScoreAdjustment"]["createPsArgs"]["errorOnHighCorrelation"] : If any covariate has an unusually high correlation (either positive or negative), this will throw an error.
["fitOutcomeModelArgs"]["modelType"] : The statistical model we will use to estimate the relative risk of the outcome between target and comparator cohorts.
["fitOutcomeModelArgs"]["stratified"] : Whether the regression should be conditioned on the strata. For one-to-one matching this is likely unnecessary and would just lose power. For stratification or variable-ratio matching it is required.
["fitOutcomeModelArgs"]["useCovariates"] : We can also choose to add the covariates to the outcome model to adjust the analysis. We recommend keeping the outcome model as simple as possible and not include additional covariates.
["fitOutcomeModelArgs"]["inversePtWeighting"] : Instead of stratifying or matching on the propensity score we can also choose to use inverse probability of treatment weighting (IPTW).
["prior"]["priorType"] : Specify the prior distribution. 
["prior"]["useCrossValidation"] : Perform cross-validation to determine prior-variance.
["control"]["tolerance"] : Maximum relative change in convergence criterion from successive iterations.
["control"]["cvType] : Cross validation search type.
["control"]["fold"] : Number of random folds to employ in cross validation.
["control"]["cvRepetitions"] : Number of repetitions of cross validation.
["control"]["noiseLevel"] : Noise level for Cyclops screen output.
["control"]["resetCoefficients"] : Reset all coefficients to 0 between model fits under cross-validation.
["control"]["startingVariance"] : Starting variance for auto-search cross-validation (-1 mean use estimate based on data).`;

  const currentAnalysisSpecifications = JSON.stringify(canonicalTemplate, null, 2);

  const prompt = `<Instruction>
From the provided PDF file, extract the key information to configure a population-level estimation study using the OMOP-CDM.
Leave any settings at their default values if they are not specified in the file.
Refer to the fields and value types provided in the <Analysis Specifications Template> and do not add any additional fields.
For each field, refer to <JSON Fields Descriptions> to ensure accurate mapping of the relevant information to the corresponding JSON structure.
If the document describes multiple settings for a field (e.g., more than one timeAtRisk window), produce a separate entry for each setting within its corresponding array.
</Instruction>

<Current Analysis Specifications>
${currentAnalysisSpecifications}
</Current Analysis Specifications>

<JSON Fields Descriptions>
${json_fields_descriptions}
</JSON Fields Descriptions>

<Analysis Specifications Template>
${analysisSpecificationsTemplate}
</Analysis Specifications Template>`;

  const pdfBytes = await fs.readFile(pdfPath);

  try {
    const updatedSpec = await getProvider(vendor, size).generateStructured(studyDtoSchema, {
      messages: [
        {
          role: "user",
          content: [
            { type: "text", text: prompt },
            { type: "file", mediaType: "application/pdf", data: pdfBytes },
          ],
        },
      ],
    });
    return { updatedSpec, rawResponse: JSON.stringify(updatedSpec) };
  } catch (err) {
    return { updatedSpec: null, rawResponse: String(err instanceof Error ? err.message : err) };
  }
}
