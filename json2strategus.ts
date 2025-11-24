import fs from "node:fs/promises";
import path from "node:path";
import OpenAI from "openai";
import dotenv from "dotenv";
dotenv.config();
const TEMPLATE_PATH = path.resolve(
    process.cwd(),
    "public",
    "templates",
    "CreateStrategusAnalysisSpecification_template_v1.1.R"
);
const OPENAI_API_KEY = 'openai-api-key'
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


// --- 모델 맵 ---
const MODEL_MAP = {
    OPENAI: {
        FLAGSHIP: { name: "gpt-5", key: process.env.OPENAI_API_KEY ?? "openai-api-key" },
        LIGHT: { name: "gpt-5-mini", key: process.env.OPENAI_API_KEY ?? "openai-api-key" },
    },
    CLAUDE: {
        FLAGSHIP: { name: "claude-sonnet-4-5", key: process.env.CLAUDE_API_KEY ?? "claude-api-key" },
        LIGHT: { name: "claude-haiku-4-5", key: process.env.CLAUDE_API_KEY ?? "claude-api-key" },
    },
    GEMINI: {
        FLAGSHIP: { name: "gemini-2.5-pro", key: process.env.GOOGLE_API_KEY ?? "google_api_key" },
        LIGHT: { name: "gemini-2.5-flash", key: process.env.GOOGLE_API_KEY ?? "google_api_key" },
    },
    DEEPSEEK: {
        FLAGSHIP: { name: "deepseek-reasoner", key: process.env.DEEPSEEK_API_KEY ?? "deepseek-api-key" },
        LIGHT: { name: "deepseek-chat", key: process.env.DEEPSEEK_API_KEY ?? "deepseek-api-key" },
    },
} as const;

/**
 * ATLAS JSON -> Strategus R script
 */

export async function json2strategus(
    analysisSpecifications: string,
    opts: { vendor: keyof typeof MODEL_MAP; size: "FLAGSHIP" | "LIGHT" }
): Promise<string> {
    const template = await readTextFile(TEMPLATE_PATH);
    const { vendor, size } = opts;
    const selected = MODEL_MAP[vendor][size];

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

    let completionText = "";

    if (vendor === "OPENAI") {
        const openai = new OpenAI({ apiKey: selected.key });
        const res = await openai.chat.completions.create({
            model: selected.name,
            messages: [{ role: "user", content: prompt }],
        });
        completionText = res.choices[0]?.message?.content ?? "";
    } else if (vendor === "GEMINI") {
        const resp = await fetch(
            `https://generativelanguage.googleapis.com/v1beta/models/${selected.name}:generateContent?key=${selected.key}`,
            {
                method: "POST",
                headers: { "Content-Type": "application/json" },
                body: JSON.stringify({
                    contents: [{ parts: [{ text: prompt }] }],
                    generationConfig: { maxOutputTokens: 120000 },
                }),
            }
        );
        const data = await resp.json();
        completionText = data.candidates?.[0]?.content?.parts?.[0]?.text ?? "";
    } else if (vendor === "CLAUDE") {
        const resp = await fetch("https://api.anthropic.com/v1/messages", {
            method: "POST",
            headers: {
                "Content-Type": "application/json",
                "x-api-key": selected.key,
                "anthropic-version": "2023-06-01",
            },
            body: JSON.stringify({
                model: selected.name,
                messages: [{ role: "user", content: prompt }],
                max_tokens: 120000,
            }),
        });
        const data = await resp.json();
        completionText = data.content?.[0]?.text ?? "";
    } else if (vendor === "DEEPSEEK") {
        const resp = await fetch("https://api.deepseek.com/chat/completions", {
            method: "POST",
            headers: {
                "Content-Type": "application/json",
                Authorization: `Bearer ${selected.key}`,
            },
            body: JSON.stringify({
                model: selected.name,
                messages: [{ role: "user", content: prompt }],
                stream: false,
            }),
        });
        const data = await resp.json();
        completionText = data.choices?.[0]?.message?.content ?? "";
    }

    return stripCodeFences(completionText);
}
