import fs from "node:fs/promises";
import path from "node:path";
import OpenAI from "openai";

const TEMPLATE_PATH = path.resolve(
    process.cwd(),
    "public",
    "templates",
    "CreateStrategusAnalysisSpecification_template.R"
);
const OPENAI_API_KEY = 'openai-api-key'
const MODEL_NAME = "gpt-4.1-2025-04-14";
const openai = new OpenAI({ apiKey: OPENAI_API_KEY });
/** Remove code fences (```lang ... ```) from LLM outputs. */
export function stripCodeFences(text: string): string {
    let cleaned = text.trim();
    cleaned = cleaned.replace(/^```[\w-]*\n?/, "");
    cleaned = cleaned.replace(/\n?```$/, "");
    return cleaned.trim();
}
async function readTextFile(abs: string) {
    return fs.readFile(abs, "utf8");
}

/**
 * ATLAS JSON -> Strategus R script
 */

export async function json2strategus(
    analysisSpecifications: string,
    opts?: { origin?: string; cache?: RequestCache }
): Promise<string> {
    const template = await readTextFile(TEMPLATE_PATH);

    const prompt = `<Instruction>
Refer to settings in <Analysis Specifications> and use the OHDSI Strategus package to write CreateStrategusAnalysisSpecification.R script. 
Refer to <Template> to write the script.
No name auto-correct: use EXACT names from <Template>/<Analysis Specifications>
Output only the R script without any additional text.
Include detailed annotations within the script to help users understand how the settings are applied.
</Instruction>

<Analysis Specifications>
${analysisSpecifications}
</Analysis Specifications>

<Template>
${template}
</Template>`;

    const completion = await openai.chat.completions.create({
        // TODO: replace with an actually available model in your account
        model: MODEL_NAME,
        messages: [{ role: "user", content: prompt }],
        temperature: 0
    });

    const content = completion.choices[0]?.message?.content?.trim() ?? "";
    return stripCodeFences(content);
}
