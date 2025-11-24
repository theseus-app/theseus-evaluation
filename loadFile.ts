import fs from "node:fs/promises";
import path from "node:path";
import { pathToFileURL } from "node:url";


export type ModulePair = { name: string; goldJson: any; studyText: string };

export async function loadFile(type: string): Promise<ModulePair[]> {

    // 타입별 폴더 매핑
    const folderMap: Record<string, string> = {
        DEFAULT: "default",
        PRIMARY: "primary",
        METHOD: "method",
        PDF: "pdf",
    };

    const folderName = folderMap[type.toUpperCase()] ?? folderMap.DEFAULT;

    const GOLD_DIR = path.resolve(process.cwd(), "public", "goldStandard", folderName);

    const entries = await fs.readdir(GOLD_DIR, { withFileTypes: true });

    // 파일 확장자: ts / js 지원 (필요시 추가)
    const candidates = entries
        .filter((e) => e.isFile())
        .map((e) => e.name)
        .filter((n) => /\.(ts|js|mjs|cjs)$/i.test(n));

    const pairs: ModulePair[] = [];

    for (const file of candidates) {
        const abs = path.join(GOLD_DIR, file);
        const url = pathToFileURL(abs).href;

        // tsx 실행기(ESM) 하에서 dynamic import
        const mod = await import(url);

        // export 키에서 TEXT*, JSON* 찾기
        const textKeys = Object.keys(mod).filter((k) => /^TEXT/i.test(k));
        const jsonKeys = Object.keys(mod).filter((k) => /^JSON/i.test(k));

        if (textKeys.length !== 1 || jsonKeys.length !== 1) {
            // 케이스당 정확히 하나씩 있어야 매칭이 명확
            console.warn(`[WARN] Skip ${file}: expected exactly 1 TEXT* and 1 JSON* export (got TEXT=${textKeys.length}, JSON=${jsonKeys.length}).`);
            continue;
        }

        const textKey = textKeys[0];
        const jsonKey = jsonKeys[0];
        const studyText = String(mod[textKey]);
        const goldJson = mod[jsonKey];

        // 케이스 이름은 export 접미사를 따르거나(예: TEXTCORAZON → CORAZON), 파일명 기반
        const nameFromExport = textKey.replace(/^TEXT/i, "") || jsonKey.replace(/^JSON/i, "");
        const baseName = path.basename(file, path.extname(file));
        const caseName = nameFromExport || baseName;

        pairs.push({ name: caseName, goldJson, studyText });
    }

    if (!pairs.length) {
        console.warn(`[WARN] No module pairs found in ${GOLD_DIR}. Make sure each file exports exactly one TEXT* and one JSON* const.`);
    }
    return pairs;
}