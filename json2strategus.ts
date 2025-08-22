import fs from "node:fs/promises";
import path from "node:path";
import OpenAI from "openai";

const TEMPLATE_PATH = path.resolve(process.cwd(), "public", "templates", "customAtlasTemplate_v1.3.0_annotated.txt");
const OPENAI_API_KEY = 'your api key'
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
        model: "gpt-5-mini-2025-08-07",
        messages: [{ role: "user", content: prompt }],
    });

    const content = completion.choices[0]?.message?.content?.trim() ?? "";
    return stripCodeFences(content);
}
