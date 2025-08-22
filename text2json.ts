import path from "node:path";
import fs from "node:fs/promises";
import { OpenAI } from "openai";
import { StudyDTO } from "./flatten";

const TEMPLATE_PATH = path.resolve(process.cwd(), "public", "templates", "customAtlasTemplate_v1.3.0_annotated.txt");
const MODEL_NAME = "gpt-5-mini-2025-08-07";
const OPENAI_API_KEY = 'your api key'
const openai = new OpenAI({ apiKey: OPENAI_API_KEY });

async function readTextFile(abs: string) {
    return fs.readFile(abs, "utf8");
}

export async function text2json(
    text: string
): Promise<{ updatedSpec: StudyDTO | null; description: string }> {
    const analysisSpecificationsTemplate = await readTextFile(TEMPLATE_PATH);


    const currentAnalysisSpecifications = `
{
    name: "",
    cohortDefinitions: {
        targetCohort: { id: null, name: "" },
        comparatorCohort: { id: null, name: "" },
        outcomeCohort: [{ id: null, name: "" }],
    },
    negativeControlConceptSet: { id: null, name: "" },
    covariateSelection: {
        conceptsToInclude: [{ id: null, name: "" }],
        conceptsToExclude: [{ id: null, name: "" }],
    },
    getDbCohortMethodDataArgs: {
        studyPeriods: [
            {
                studyStartDate: null,
                studyEndDate: null,
            },
        ],
        maxCohortSize: 0,
    },
    createStudyPopArgs: {
        restrictToCommonPeriod: false,
        firstExposureOnly: false,
        washoutPeriod: 0,
        removeDuplicateSubjects: "keep all",
        censorAtNewRiskWindow: false,
        removeSubjectsWithPriorOutcome: true,
        priorOutcomeLookBack: 99999,
        timeAtRisks: [
            {
                description: "",
                riskWindowStart: 0,
                startAnchor: "cohort start",
                riskWindowEnd: 0,
                endAnchor: "cohort end",
                minDaysAtRisk: 1,
            },
        ],
    },
    propensityScoreAdjustment: {
        psSettings: [
            {
                description: "PS 1",
                matchOnPsArgs: { maxRatio: 1, caliper: 0.2, caliperScale: "standardized logit" },
                stratifyByPsArgs: null,
            },
        ],
        createPsArgs: {
            maxCohortSizeForFitting: 250000,
            errorOnHighCorrelation: true,
            prior: { priorType: "laplace", useCrossValidation: true },
            control: {
                tolerance: 2e-7,
                cvType: "auto",
                fold: 10,
                cvRepetitions: 10,
                noiseLevel: "silent",
                resetCoefficients: true,
                startingVariance: 0.01,
            },
        },
    },
    fitOutcomeModelArgs: {
        modelType: "cox",
        stratified: false,
        useCovariates: false,
        inversePtWeighting: false,
        prior: { priorType: "laplace", useCrossValidation: true },
        control: {
            tolerance: 2e-7,
            cvType: "auto",
            fold: 10,
            cvRepetitions: 10,
            noiseLevel: "quiet",
            resetCoefficients: true,
            startingVariance: 0.01,
        },
    },
};
`

    const prompt = `<Instruction>
From the provided <Text>, extract the key information and update the <Current Analysis Specifications> JSON to configure a population-level estimation study using the OMOP-CDM.
Leave any settings at their default values if they are not specified in the <Text>.
Refer to the fields and value types provided in the <Analysis Specifications Template> and do not add any additional fields.
Following the <Output Style> format, output the updated analysis specifications JSON and provide a description of how the new settings are applied to the specification. 
</Instruction>

<Text>
${text}
</Text>

<Current Analysis Specifications>
${currentAnalysisSpecifications}
</Current Analysis Specifications>

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

    const completion = await openai.chat.completions.create({
        model: MODEL_NAME,
        messages: [{ role: "user", content: prompt }],
    });

    const content = completion.choices[0]?.message?.content?.trim() ?? "";
    const m = content.match(/([\s\S]*?)\n?---\n?([\s\S]*)/);
    const updatedSpecRaw = m ? m[1] : content;
    const description = m ? (m[2]?.trim() ?? "") : "";

    return { updatedSpec: safeParseJsonFromText(updatedSpecRaw), description };
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