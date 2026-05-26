import { generateStrategusScript, studyDtoSchema } from "theseus-core";

/**
 * StudyDTO JSON -> Strategus R script (deterministic, no LLM).
 * vendor/size are no longer relevant; the output is rule-based.
 */
export async function json2strategus(analysisSpecifications: string): Promise<string> {
  const dto = studyDtoSchema.parse(JSON.parse(analysisSpecifications));
  return generateStrategusScript(dto);
}
