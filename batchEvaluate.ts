// scripts/batchEvaluate.tsx
// 실행: npx ts-node scripts/batchEvaluate.tsx
// 또는 package.json에 "batch": "ts-node scripts/batchEvaluate.tsx" 등록 후 `pnpm run batch`

import path from "node:path";
import fs from "node:fs/promises";
import { loadFile, ModulePair } from "./loadFile"
import { StudyDTO, FlattenStudyDTO, flattenStudy } from "./flatten";
import { evaluateFlat } from "./evaluate";
import { text2json } from "./text2json";
// 결과 저장 폴더
const RESULTS_DIR = path.resolve(process.cwd(), "public", "results");

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

type PerCaseResult = {
    name: string;
    fileName: string;
    createdAt: string;
    metrics: {
        jaccard: number;
        recall: number;
        precision: number;
    };
    counts: {
        gold: number;
        pred: number;
        intersection: number;
        union: number;
        both: number;
        predOnly: number;
        goldOnly: number;
    };
    details: {
        bothJson: string[];       // 공통
        predJsonOnly: string[];   // pred에만
        goldJsonOnly: string[];   // gold에만
    };
    sectionAccuracy: {
        studyPeriods: boolean | null,
        timeAtRisks: boolean | null,
        propensityScoreAdjustment: boolean | null,
        fitOutcomeModelArgs: boolean | null,
    },
    goldJson: StudyDTO | null;
    predJson: StudyDTO | null;
};

export async function runBatchEvaluate(): Promise<{
    totalCases: number;
    resultsDir: string;
    cases: PerCaseResult[];
}> {
    // 1) goldJson + studyText 로드 (여러 케이스)
    const pairs: ModulePair[] = await loadFile();

    if (!pairs.length) {
        console.warn("[WARN] No pairs found. Check GOLD_DIR and exports (TEXT*, JSON*).");
    }

    await ensureDir(RESULTS_DIR);

    const summary: PerCaseResult[] = [];

    // 2) 각 케이스 순회
    for (const p of pairs) {
        const caseSlug = slugify(p.name);
        const outName = `${caseSlug}.json`;
        const outPath = path.join(RESULTS_DIR, outName);

        try {
            // 2-1) text2Json: predJson 생성
            const { updatedSpec } = await text2json(p.studyText);
            const predJson = updatedSpec as StudyDTO | null;

            // 방어: 모델 응답이 파싱 실패한 경우
            if (!predJson || typeof predJson !== "object") {
                const bad: PerCaseResult = {
                    name: p.name,
                    fileName: outName,
                    createdAt: new Date().toISOString(),
                    metrics: { jaccard: 0, recall: 0, precision: 0 },
                    counts: { gold: 0, pred: 0, intersection: 0, union: 0, both: 0, predOnly: 0, goldOnly: 0 },
                    details: { bothJson: [], predJsonOnly: [], goldJsonOnly: [] },
                    sectionAccuracy: { studyPeriods: null, timeAtRisks: null, propensityScoreAdjustment: null, fitOutcomeModelArgs: null },
                    goldJson: p.goldJson,
                    predJson: predJson
                };
                await fs.writeFile(outPath, JSON.stringify(bad, null, 2), "utf8");
                summary.push(bad);
                console.warn(`[WARN] ${p.name}: predJson parse failed. Saved zero-metrics result.`);
                continue;
            }

            // 2-2) flatten: gold / pred 평탄화
            const flatGold: FlattenStudyDTO = flattenStudy(p.goldJson);
            const flatPred: FlattenStudyDTO = flattenStudy(predJson);

            // 2-3) evaluate: Jaccard / Recall / Precision
            const evalRes = evaluateFlat(flatGold, flatPred);

            // 2-4) 결과 저장 (results/<파일이름>.json)
            const one: PerCaseResult = {
                name: p.name,
                fileName: outName,
                createdAt: new Date().toISOString(),
                metrics: {
                    jaccard: evalRes.jaccard,
                    recall: evalRes.recall,
                    precision: evalRes.precision,
                },
                counts: evalRes.counts,
                details: evalRes.details,
                sectionAccuracy: evalRes.sectionAccuracy,
                goldJson: p.goldJson,   // 원본 gold
                predJson: predJson      // 모델이 생성한 예측
            };

            await fs.writeFile(outPath, JSON.stringify(one, null, 2), "utf8");
            summary.push(one);
            console.log(`[OK] Saved: ${path.relative(process.cwd(), outPath)}  (J=${one.metrics.jaccard.toFixed(3)} / R=${one.metrics.recall.toFixed(3)} / P=${one.metrics.precision.toFixed(3)})`);
        } catch (err) {
            console.error(`[ERROR] ${p.name}:`, err);
            // 에러도 파일로 남겨두면 디버깅 편함
            const fail = {
                name: p.name,
                error: String(err instanceof Error ? err.message : err),
                createdAt: new Date().toISOString(),
            };
            await fs.writeFile(outPath, JSON.stringify(fail, null, 2), "utf8");
        }
    }

    // ... for 루프 종료 직후, 요약 파일 쓰기 전에 추가
    const totalCases = summary.length;
    const EPS = 1e-9;

    // jaccard === 1 집계 (부동소수 안전하게)
    const jaccardPerfectCount = summary.reduce((acc, c) => {
        const j = c.metrics?.jaccard ?? 0;
        return acc + (Math.abs(j - 1) < EPS ? 1 : 0);
    }, 0);

    // 섹션별 집계 헬퍼
    function sectionCounts(
        key: keyof PerCaseResult["sectionAccuracy"]
    ) {
        let evaluated = 0;   // null이 아닌 값
        let trueCount = 0;   // true 개수
        for (const c of summary) {
            const v = c.sectionAccuracy?.[key] ?? null;
            if (v !== null) {
                evaluated++;
                if (v === true) trueCount++;
            }
        }
        const falseCount = evaluated - trueCount;
        const accuracy = evaluated ? trueCount / evaluated : null;
        return { evaluated, trueCount, falseCount, accuracy };
    }

    const sectionAccuracySummary = {
        studyPeriods: sectionCounts("studyPeriods"),
        timeAtRisks: sectionCounts("timeAtRisks"),
        propensityScoreAdjustment: sectionCounts("propensityScoreAdjustment"),
        fitOutcomeModelArgs: sectionCounts("fitOutcomeModelArgs"),
    };


    // 전체 요약 파일도 하나 남겨두기
    const indexPath = path.join(RESULTS_DIR, "_summary.index.json");
    await fs.writeFile(
        indexPath,
        JSON.stringify(
            {
                createdAt: new Date().toISOString(),
                totalCases,
                jaccardPerfect: {
                    count: jaccardPerfectCount,
                    percent: totalCases ? jaccardPerfectCount / totalCases : null
                },
                sectionAccuracySummary,
                results: summary,
            },
            null,
            2
        ),
        "utf8"
    );

    console.log(`[DONE] ${summary.length} case(s). Summary: ${path.relative(process.cwd(), indexPath)}`);

    return {
        totalCases: summary.length,
        resultsDir: RESULTS_DIR,
        cases: summary,
    };
}

// 단독 실행 지원
if (require.main === module) {
    runBatchEvaluate().catch((e) => {
        console.error(e);
        process.exit(1);
    });
}


