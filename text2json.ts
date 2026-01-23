import path from "node:path";
import fs from "node:fs/promises";
import { OpenAI } from "openai/client.js";
import { StudyDTO } from "./flatten";
import dotenv from "dotenv";
import { MODEL_MAP } from "./text2jsonPRIMARY";
dotenv.config();
const TEMPLATE_PATH = path.resolve(process.cwd(), "public", "templates", "customAtlasTemplate_v1.4.0_annotated.txt");
const JSON_PATH = path.resolve(process.cwd(), "public", "templates", "customAtlasTemplate_v1.4.json")
// --- 모델 맵 ---

async function readTextFile(abs: string) {
    return fs.readFile(abs, "utf8");
}

async function readJsonFile<T = unknown>(abs: string): Promise<T> {
    const txt = await fs.readFile(abs, "utf8");
    return JSON.parse(txt) as T;
}

export async function text2json(
    text: string,
    vendor: "OPENAI" | "GEMINI" | "DEEPSEEK" | "CLAUDE",
    size: "FLAGSHIP" | "LIGHT"
): Promise<{ updatedSpec: StudyDTO | null; rawResponse: string }> {
    const analysisSpecificationsTemplate = await readTextFile(TEMPLATE_PATH);
    const selected = MODEL_MAP[vendor][size];

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
["getDbCohortMethodDataArgs"]["removeDuplicateSubjects"] :  What happens when a subject is in both target and comparator cohorts. “Keep All” indicating to keep the subjects in both cohorts. With this option it might be possible to double-count subjects and outcomes. “Keep First” indicating to keep the subject in the first cohort that occurred. “Remove All” indicating to remove the subject from both cohorts.
["createStudyPopArgs"]["censorAtNewRiskWindow"] : If the options “keep all” or “keep first” are selected, we may wish to censor the time when a person is in both cohorts.
["createStudyPopArgs"]["removeSubjectsWithPriorOutcome"] : We can choose to remove subjects that have the outcome prior to the risk window start.
["createStudyPopArgs"]["priorOutcomeLookBack"] : If we choose to remove people that had the outcome before, we can select how many days we should look back when identifying prior outcomes.
["timeAtRisks"] : We set the start of time-at-risk to one day after cohort start, so one day after treatment initiation. A reason to set the time-at-risk start to be later than the cohort start is because we may want to exclude outcome events that occur on the day of treatment initiation if we do not believe it biologically plausible they can be caused by the drug.We can set the end of the time-at-risk to the cohort end, so when exposure stops, which can be referred to as an “on-treatment” design. We can choose to set the end of the time-at-risk to a fixed duration after cohort entry regardless of whether exposure continues, which can be referred to as an “intent-to-treat” design. In the extreme we could set the time-at-risk end to a large number of days (e.g. 99999) after cohort entry, meaning we will effectively follow up subjects until observation end. 
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
["control"]["startingVariance"] : Starting variance for auto-search cross-validation (-1 mean use estimate based on data).`


    const currentAnalysisSpecObj = await readJsonFile<StudyDTO>(JSON_PATH);
    const currentAnalysisSpecifications = JSON.stringify(currentAnalysisSpecObj, null, 4);


    const prompt = `<Instruction>
From the provided <Text>, extract the key information and update the <Current Analysis Specifications> JSON to configure a population-level estimation study using the OMOP-CDM.
Leave any settings at their default values if they are not specified in the <Text>.
Refer to the fields and value types provided in the <Analysis Specifications Template> and do not add any additional fields.
For each fields, refer to <JSON Fields Descriptions> to ensure accurate mapping of the relevant information from <Text> to the corresponding JSON structure.
Additional sensitivity analyses beyond the primary analysis may have also been conducted. If the text describes multiple settings for each field (e.g., more than one timeAtRisk window), generate a separate JSON object for each setting within its corresponding array. Fields that can contain multiple objects are annotated in the <Analysis Specifications Template>.
Following the <Output Style> format, output the updated analysis specifications JSON and provide a description of how the new settings are applied to the specification. 
</Instruction>

<Text>
${text}
</Text>

<Current Analysis Specifications>
${currentAnalysisSpecifications}
</Current Analysis Specifications>

<JSON Fields Descriptions>
${json_fields_descriptions}
</JSON Fields Descriptions>

<Analysis Specifications Template>
${analysisSpecificationsTemplate}
</Analysis Specifications Template>

<Output Style>
\`\`\`json
analysis specifications 
\`\`\`
---
Description
</Output Style>`;

    // --- vendor별 API 초기화 ---
    let completionText = "";
    if (vendor === "OPENAI") {
        const openai = new OpenAI({ apiKey: selected.key });
        const res = await openai.chat.completions.create({
            model: selected.name,
            temperature: 0,
            messages: [
                { role: "user", content: prompt }],
        });
        completionText = res.choices[0]?.message?.content ?? "";
    } else if (vendor === "GEMINI") {
        const maxRetries = 10;
        const baseDelayMs = 60000; // 60초

        let lastErrText = "";
        for (let attempt = 0; attempt < maxRetries; attempt++) {
            const resp = await fetch(
                `https://generativelanguage.googleapis.com/v1beta/models/${selected.name}:generateContent?key=${selected.key}`,
                {
                    method: "POST",
                    headers: { "Content-Type": "application/json" },
                    body: JSON.stringify({
                        contents: [
                            {
                                parts: [
                                    { text: `${prompt}` }
                                ]
                            }
                        ],
                        generationConfig: {
                            temperature: 0,
                            maxOutputTokens: 65536
                        }
                    }),
                }
            );

            if (resp.ok) {
                const data = await resp.json();

                // finishReason 체크 (MAX_TOKENS, SAFETY 등)
                const finishReason = data.candidates?.[0]?.finishReason;
                if (finishReason && finishReason !== "STOP") {
                    console.warn(`[WARN] Gemini finishReason: ${finishReason}`);
                }

                // Gemini가 여러 part로 응답할 수 있으므로 모든 part를 합침
                const parts = data.candidates?.[0]?.content?.parts ?? [];
                completionText = parts.map((p: any) => p.text ?? "").join("");
                break;
            }

            lastErrText = await resp.text();
            const shouldRetry = resp.status === 503 || resp.status === 429;
            const isLast = attempt === maxRetries - 1;
            if (!shouldRetry || isLast) {
                console.error("Gemini API Error:", resp.status, lastErrText);
                throw new Error(`Gemini API failed: ${resp.status} ${lastErrText}`);
            }

            const delay = baseDelayMs * (attempt + 1);
            console.warn(`Gemini overloaded (${resp.status}). Retrying in ${delay / 1000}s... (attempt ${attempt + 1}/${maxRetries})`);
            await sleep(delay);
        }
    }
    else if (vendor === "CLAUDE") {
        const resp = await fetch("https://api.anthropic.com/v1/messages", {
            method: "POST",
            headers: {
                "Content-Type": "application/json",
                "x-api-key": selected.key,
                "anthropic-version": "2023-06-01",
            },
            body: JSON.stringify({
                model: selected.name,
                temperature: 0,
                max_tokens: 4000,
                messages: [
                    { role: "user", content: prompt }  // system 제거
                ],
            }),
        });

        // 에러 체크 추가
        if (!resp.ok) {
            const errorText = await resp.text();
            console.error("Claude API Error:", resp.status, errorText);
            throw new Error(`Claude API failed: ${resp.status} ${errorText}`);
        }

        const data = await resp.json();
        // console.log("Claude Response:", JSON.stringify(data, null, 2));  // 디버깅용
        completionText = data.content?.[0]?.text ?? "";

    } else if (vendor === "DEEPSEEK") {
        const maxRetries = 10;
        const baseDelayMs = 60000; // 60초

        let lastErrText = "";
        for (let attempt = 0; attempt < maxRetries; attempt++) {
            const resp = await fetch("https://api.deepseek.com/chat/completions", {
                method: "POST",
                headers: {
                    "Content-Type": "application/json",
                    Authorization: `Bearer ${selected.key}`,
                },
                body: JSON.stringify({
                    model: selected.name,
                    temperature: 0,
                    messages: [
                        { role: "user", content: prompt },
                    ],
                    response_format: { type: "json_object" },
                    stream: false,
                }),
            });

            if (resp.ok) {
                const data = await resp.json();

                // 응답 구조 확인
                if (!data.choices || !data.choices[0]) {
                    console.error("Unexpected DeepSeek response structure:", data);
                    throw new Error("Invalid DeepSeek response structure");
                }

                completionText = data.choices[0]?.message?.content ?? "";
                break;
            }

            lastErrText = await resp.text();
            const shouldRetry = resp.status === 503 || resp.status === 429 || resp.status === 500 || resp.status === 502;
            const isLast = attempt === maxRetries - 1;
            if (!shouldRetry || isLast) {
                console.error("DeepSeek API Error:", resp.status, lastErrText);
                throw new Error(`DeepSeek API failed: ${resp.status} ${lastErrText}`);
            }

            const delay = baseDelayMs * (attempt + 1);
            console.warn(`DeepSeek error (${resp.status}). Retrying in ${delay / 1000}s... (attempt ${attempt + 1}/${maxRetries})`);
            await sleep(delay);
        }
    }


    const content = completionText.trim();
    const m = content.match(/([\s\S]*?)\n?---\n?([\s\S]*)/);
    const updatedSpecRaw = m ? m[1] : content;
    const description = m ? (m[2]?.trim() ?? "") : "";

    return { updatedSpec: safeParseJsonFromText(updatedSpecRaw), rawResponse: completionText };
}

/** ---------- helpers: parse / pretty ---------- */
/** 코드펜스 제거 (```json ... ```), 앞뒤 공백 트림 */
function stripCodeFences(text: string): string {
    return text
        .replace(/^\s*```[\w-]*\s*/i, "")
        .replace(/\s*```+\s*$/i, "")
        .trim();
}

/** 텍스트에서 첫 번째 JSON(객체/배열) 블록만 추출 */
function extractFirstJson(text: string): string | null {
    const s = stripCodeFences(text);

    // 1) 첫 여는 괄호 위치 찾기
    let start = -1;
    for (let i = 0; i < s.length; i++) {
        const ch = s[i];
        if (ch === "{" || ch === "[") {
            start = i;
            break;
        }
    }
    if (start === -1) return null;

    // 2) 문자열/이스케이프를 고려한 괄호 매칭
    let depth = 0;
    let inStr = false;
    let esc = false;
    const open = s[start];

    for (let i = start; i < s.length; i++) {
        const ch = s[i];

        if (inStr) {
            if (esc) {
                esc = false;
            } else if (ch === "\\") {
                esc = true;
            } else if (ch === '"') {
                inStr = false;
            }
            continue;
        }

        if (ch === '"') {
            inStr = true;
        } else if (ch === "{" || ch === "[") {
            depth++;
        } else if (ch === "}" || ch === "]") {
            depth--;
            if (depth === 0) {
                // 열었던 종류와 닫힘 종류가 맞는지(느슨히 체크)
                const isObjectJson = open === "{" && ch === "}";
                const isArrayJson = open === "[" && ch === "]";
                if (isObjectJson || isArrayJson) {
                    return s.slice(start, i + 1);
                }
            }
        }
    }
    return null;
}

/** 느슨한 파서: 텍스트에서 JSON만 뽑아 안전 파싱 */
function safeParseJsonFromText(text: string): any {
    if (!text) return null;

    // 1차: 괄호 매칭으로 추출
    const candidate = extractFirstJson(text);
    if (candidate) {
        try {
            return JSON.parse(candidate);
        } catch {
            // 계속 진행
        }
    }

    // 2차: 첫 '{'부터 잘라서 시도 (백업)
    const braceIdx = text.indexOf("{");
    if (braceIdx !== -1) {
        try {
            return JSON.parse(text.slice(braceIdx));
        } catch {
            // noop
        }
    }

    // 3차: 첫 '['부터 잘라서 시도 (배열 루트인 경우)
    const bracketIdx = text.indexOf("[");
    if (bracketIdx !== -1) {
        try {
            return JSON.parse(text.slice(bracketIdx));
        } catch {
            // noop
        }
    }

    return null;
}

function sleep(ms: number) {
    return new Promise((resolve) => setTimeout(resolve, ms));
}