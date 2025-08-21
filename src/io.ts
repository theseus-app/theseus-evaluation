import fs from "node:fs/promises";
import path from "node:path";

export async function readPublicText(relPath: string): Promise<string> {
    const abs = path.resolve(process.cwd(), "public", relPath.replace(/^\/+/, ""));
    return fs.readFile(abs, "utf8");
}

export function stripCodeFences(text: string): string {
    return text.replace(/^```[\w-]*\n?/, "").replace(/\n?```$/, "").trim();
}

export function normStr(s: string): string {
    return s.trim().toLowerCase().replace(/\s+/g, " ");
}

export function normNum(x: unknown): unknown {
    if (typeof x === "number") return x;
    if (typeof x === "string" && x.trim() !== "" && !Number.isNaN(Number(x))) {
        return Number(x);
    }
    return x;
}

export function getByPath(obj: any, dotted: string): any {
    let cur = obj;
    for (const k of dotted.split(".")) {
        if (cur && typeof cur === "object" && k in cur) cur = cur[k];
        else return undefined;
    }
    return cur;
}
