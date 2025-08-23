import path from "node:path";
import fs from "node:fs/promises";

import { loadFile, type ModulePair } from "./loadFile";                  // 경로 확인
import { StudyDTO } from "./flatten";
import { defaultDTO } from "./defaultDTO";
import { json2strategus } from "./json2strategus";

// 저장 폴더
const RS_DIR = path.resolve(process.cwd(), "public", "RScripts");

// 파일명용 slug
function slugify(s: string) {
    return s
        .trim()
        .toLowerCase()
        .replace(/[\s/\\]+/g, "-")
        .replace(/[^a-z0-9-_]/g, "")
        .replace(/-+/g, "-")
        .replace(/^-|-$/g, "") || "case";
}

async function ensureDir(abs: string) {
    await fs.mkdir(abs, { recursive: true });
}

/**
 * 기본값으로 채우는 딥 머지
 * - 객체: 키 단위 재귀
 * - 배열: gold가 제공하면 gold 배열 사용, 없으면 기본 배열 사용 (아이템 단위 머지는 하지 않음)
 * - 원시값: gold가 제공하면 gold, 아니면 기본
 * - "없는 항목"만 기본값으로 채움 (gold가 null을 명시했다면 null 유지)
 */
function fillWithDefaults<T>(base: T, gold: any): T {
    if (gold === undefined) return base;
    if (gold === null) return gold as T;

    // 배열: 교체 전략
    if (Array.isArray(base)) {
        if (Array.isArray(gold)) return gold as T;
        return base;
    }

    // 객체: 재귀 병합
    if (typeof base === "object" && base !== null) {
        const out: any = Array.isArray(base) ? [] : {};
        const keys = new Set<string>([
            ...Object.keys(base as any),
            ...Object.keys((gold as any) ?? {}),
        ]);
        for (const k of keys) {
            const bVal = (base as any)[k];
            const gHas = Object.prototype.hasOwnProperty.call(gold, k);
            const gVal = gHas ? (gold as any)[k] : undefined;

            if (gHas) {
                // gold가 해당 키를 "제공"했을 때:
                if (
                    bVal &&
                    typeof bVal === "object" &&
                    !Array.isArray(bVal) &&
                    gVal &&
                    typeof gVal === "object" &&
                    !Array.isArray(gVal)
                ) {
                    out[k] = fillWithDefaults(bVal, gVal);
                } else if (Array.isArray(bVal)) {
                    out[k] = Array.isArray(gVal) ? gVal : bVal; // 배열은 교체
                } else {
                    out[k] = gVal; // 원시값/기타는 교체
                }
            } else {
                // gold에 "없는 항목" → 기본값 사용
                out[k] = bVal;
            }
        }
        return out as T;
    }

    // 원시: gold가 있으면 gold
    return gold as T;
}

type PerCase = {
    name: string;
    fileName: string;
    createdAt: string;
    savedPath: string;
};

export async function getRScripts(): Promise<{
    totalCases: number;
    outDir: string;
    cases: PerCase[];
}> {
    // 1) goldJson 불러오기
    const pairs: ModulePair[] = await loadFile();
    if (!pairs.length) {
        console.warn("[WARN] No module pairs found (TEXT*/JSON*).");
    }

    await ensureDir(RS_DIR);

    const results: PerCase[] = [];
    for (const p of pairs) {
        const caseSlug = slugify(p.name);

        const outName = `${caseSlug}.R`;
        const outPath = path.join(RS_DIR, outName);

        try {
            // 2) defaultDTO로 채워서 StudyDTO 만들기
            const dto: StudyDTO = fillWithDefaults<StudyDTO>(defaultDTO, p.goldJson);

            //이름 지정
            dto.name = caseSlug
            //예시 지정
            dto.cohortDefinitions = {
                targetCohort: { id: 1794126, name: "target1" },
                comparatorCohort: { id: 1794132, name: "comparator1" },
                outcomeCohort: [{ id: 1794131, name: "outcome1" }]
            }
            //예시 지정
            dto.negativeControlConceptSet = { id: 1888110, name: "negative" }

            // 3) json2strategus로 R 스크립트 생성
            const script = await json2strategus(JSON.stringify(dto, null, 2));

            // 저장
            await fs.writeFile(outPath, script, "utf8");

            results.push({
                name: p.name,
                fileName: outName,
                createdAt: new Date().toISOString(),
                savedPath: path.relative(process.cwd(), outPath),
            });

            console.log(`[OK] Saved R script: ${path.relative(process.cwd(), outPath)}`);
        } catch (err) {
            console.error(`[ERROR] ${p.name}:`, err);
            // 실패 케이스도 빈 파일이라도 남기고 싶으면 아래 주석 해제
            // await fs.writeFile(outPath, `# Failed to generate script: ${String(err)}`, "utf8");
        }
    }

    // 인덱스 파일(선택)
    const indexPath = path.join(RS_DIR, "_summary.index.json");
    await fs.writeFile(
        indexPath,
        JSON.stringify(
            {
                createdAt: new Date().toISOString(),
                totalCases: results.length,
                results,
            },
            null,
            2
        ),
        "utf8"
    );

    console.log(`[DONE] ${results.length} script(s). Summary: ${path.relative(process.cwd(), indexPath)}`);

    return {
        totalCases: results.length,
        outDir: RS_DIR,
        cases: results,
    };
}

// 단독 실행
if (require.main === module) {
    getRScripts().catch((e) => {
        console.error(e);
        process.exit(1);
    });
}
