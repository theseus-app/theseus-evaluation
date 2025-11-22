// scripts/batchEvaluate.tsx
// 실행: npx ts-node scripts/batchEvaluate.tsx
// 또는 package.json에 "batch": "ts-node scripts/batchEvaluate.tsx" 등록 후 `pnpm run batch`

import path from "node:path";
import fs from "node:fs/promises";
import { loadFile, ModulePair } from "./loadFile"
import { StudyDTO, FlattenStudyDTO, flattenStudy } from "./flatten";
import { evaluateFlat } from "./evaluate";
import { text2json } from "./text2json";

// --- parse CLI args ---
function parseArgs() {
    const args = process.argv.slice(2);
    const argMap: Record<string, string> = {};
    for (const a of args) {
        const [k, v] = a.split("=");
        if (k && v) argMap[k.replace(/^--/, "").toLowerCase()] = v.toUpperCase();
    }

    const vendor = argMap["vendor"];
    const size = argMap["size"];
    if (!vendor || !size) {
        console.error("❌ Usage: ts-node scripts/batchEvaluate.tsx --vendor=OPENAI|GEMINI|DEEPSEEK|CLAUDE --size=FLAGSHIP|LIGHT");
        process.exit(1);
    }

    const supportedVendors = ["OPENAI", "GEMINI", "DEEPSEEK", "CLAUDE"];
    const supportedSizes = ["FLAGSHIP", "LIGHT"];
    if (!supportedVendors.includes(vendor) || !supportedSizes.includes(size)) {
        console.error(`❌ Invalid vendor/size. Supported vendors: ${supportedVendors.join(", ")} / sizes: ${supportedSizes.join(", ")}`);
        process.exit(1);
    }

    return { vendor, size };
}

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
        bothJson: string[];
        predJsonOnly: string[];
        goldJsonOnly: string[];
    };
    sectionAccuracy: {
        studyPeriods: boolean | null;
        timeAtRisks: boolean | null;
        propensityScoreAdjustment: boolean | null;
        // fitOutcomeModelArgs: boolean | null;
    };
    sectionCounts?: {
        studyPeriods?: any;
        timeAtRisks?: any;
        propensityScoreAdjustment?: any;
        // fitOutcomeModelArgs?: any;
    };
    goldJson: StudyDTO | null;
    predJson: StudyDTO | null;
};

export async function runBatchEvaluate(): Promise<{
    totalCases: number;
    resultsDir: string;
    cases: PerCaseResult[];
}> {
    const { vendor, size } = parseArgs();

    // 1) goldJson + studyText 로드 (여러 케이스)
    const pairs: ModulePair[] = await loadFile();

    if (!pairs.length) {
        console.warn("[WARN] No pairs found. Check GOLD_DIR and exports (TEXT*, JSON*).");
    }
    const RESULTS_DIR = path.resolve(process.cwd(), "public", `results_${vendor.toLowerCase()}_${size.toLowerCase()}`);

    await ensureDir(RESULTS_DIR);

    const summary: PerCaseResult[] = [];

    // 2) 각 케이스 순회
    for (const p of pairs) {
        const caseSlug = slugify(p.name);
        const outName = `${caseSlug}.json`;
        const outPath = path.join(RESULTS_DIR, outName);

        try {
            // 2-1) text2Json: predJson 생성
            const { updatedSpec, rawResponse } = await text2json(
                p.studyText,
                vendor as "OPENAI" | "GEMINI" | "DEEPSEEK" | "CLAUDE",
                size as "FLAGSHIP" | "LIGHT"
            );

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
                    sectionAccuracy: { studyPeriods: null, timeAtRisks: null, propensityScoreAdjustment: null, 
                        // fitOutcomeModelArgs: null 
                    },
                    goldJson: p.goldJson,
                    predJson: predJson
                };

                await fs.writeFile(outPath, JSON.stringify(bad, null, 2), "utf8");
                summary.push(bad);
                console.warn(`⚠️ [WARN] ${p.name}: JSON parse failed.`);
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
                sectionCounts: evalRes.sectionCounts,
                goldJson: p.goldJson,
                predJson: predJson,
            };

            await fs.writeFile(outPath, JSON.stringify(one, null, 2), "utf8");
            summary.push(one);
            console.log(`[OK] Saved: ${path.relative(process.cwd(), outPath)}  (J=${one.metrics.jaccard.toFixed(3)} / R=${one.metrics.recall.toFixed(3)} / P=${one.metrics.precision.toFixed(3)})`);

        } catch (err: any) {
            console.error(`❌ [ERROR] ${p.name}:`, err);

            // 실패 시 원문 응답을 따로 저장
            const failOutPath = path.join(RESULTS_DIR, `${slugify(p.name)}_error.json`);
            const errorData = {
                name: p.name,
                error: String(err instanceof Error ? err.message : err),
                stack: err?.stack ?? null,
                createdAt: new Date().toISOString(),
            };
            await fs.writeFile(failOutPath, JSON.stringify(errorData, null, 2), "utf8");
            console.warn(`⚠️ Raw error info saved to ${failOutPath}`);
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
        // fitOutcomeModelArgs: sectionCounts("fitOutcomeModelArgs"),
    };

    // 섹션별 macro/micro metrics 요약
    function sectionMetricsSummaryAll(key: keyof NonNullable<PerCaseResult["sectionCounts"]>) {
        // --- Macro (평균)
        let macroCount = 0;
        let precisionSum = 0;
        let recallSum = 0;
        let f1Sum = 0;

        // --- Micro (전체 TP/FP/FN 합)
        let TP = 0;
        let FP = 0;
        let FN = 0;

        for (const c of summary) {
            const s = c.sectionCounts?.[key];
            if (!s) continue;

            // Macro part
            if (typeof s.precision === "number") {
                precisionSum += s.precision;
                recallSum += s.recall ?? 0;
                f1Sum += s.f1 ?? 0;
                macroCount++;
            }

            // Micro part
            TP += s.bothCount ?? 0;
            FP += s.predOnlyCount ?? 0;
            FN += s.goldOnlyCount ?? 0;
        }

        // Macro average
        const precisionMacro = macroCount ? precisionSum / macroCount : null;
        const recallMacro = macroCount ? recallSum / macroCount : null;
        const f1Macro = macroCount ? f1Sum / macroCount : null;

        // Micro average (전체 집계)
        const precisionMicro = TP + FP > 0 ? TP / (TP + FP) : null;
        const recallMicro = TP + FN > 0 ? TP / (TP + FN) : null;
        const f1Micro =
            precisionMicro && recallMicro && precisionMicro + recallMicro > 0
                ? (2 * precisionMicro * recallMicro) / (precisionMicro + recallMicro)
                : null;

        return {
            macro: {
                count: macroCount,
                precision: precisionMacro,
                recall: recallMacro,
                f1: f1Macro,
            },
            micro: {
                TP,
                FP,
                FN,
                precision: precisionMicro,
                recall: recallMicro,
                f1: f1Micro,
            },
        };
    }

    const sectionMetricsSummaryCombined = {
        studyPeriods: sectionMetricsSummaryAll("studyPeriods"),
        timeAtRisks: sectionMetricsSummaryAll("timeAtRisks"),
        propensityScoreAdjustment: sectionMetricsSummaryAll("propensityScoreAdjustment"),
        // fitOutcomeModelArgs: sectionMetricsSummaryAll("fitOutcomeModelArgs"),
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
                sectionMetricsSummary: sectionMetricsSummaryCombined,
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


