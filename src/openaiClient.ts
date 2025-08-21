import { config as loadEnv } from "dotenv";
loadEnv({ path: ".env.local" });
import OpenAI from "openai";

let _client: OpenAI | null = null;
export function getOpenAI() {
    if (_client) return _client;
    const key = process.env.OPENAI_API_KEY;
    if (!key) throw new Error("Missing OPENAI_API_KEY in .env.local");
    _client = new OpenAI({ apiKey: key });
    return _client;
}
