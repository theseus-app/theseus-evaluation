import {
  getProvider,
  studyDtoSchema,
  buildText2SpecPrompt,
  type StudyDTO,
} from "theseus-core";
import dotenv from "dotenv";
dotenv.config();

export async function text2json(
  text: string,
  vendor: "OPENAI" | "GEMINI" | "DEEPSEEK" | "CLAUDE",
  size: "FLAGSHIP" | "LIGHT",
): Promise<{ updatedSpec: StudyDTO | null; rawResponse: string }> {
  // Prompt is owned by theseus-core (single source of truth). The instruction,
  // <Analysis Specifications Template>, and <JSON Fields Descriptions> all come
  // from core's TEXT2SPEC_* assets so this evaluator never drifts from the
  // canonical prompt.
  const prompt = buildText2SpecPrompt(text);

  try {
    const updatedSpec = await getProvider(vendor, size).generateStructured(studyDtoSchema, { prompt });
    return { updatedSpec, rawResponse: JSON.stringify(updatedSpec) };
  } catch (err) {
    return { updatedSpec: null, rawResponse: String(err instanceof Error ? err.message : err) };
  }
}
