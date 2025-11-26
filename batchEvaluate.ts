// scripts/batchEvaluate.tsx
// 실행: npx ts-node scripts/batchEvaluate.tsx
// 또는 package.json에 "batch": "ts-node scripts/batchEvaluate.tsx" 등록 후 `pnpm run batch`

import path from "node:path";
import fs from "node:fs/promises";
import { loadFile, ModulePair } from "./loadFile";

// 🔹 DEFAULT/METHOD/PDF용
import { StudyDTO, FlattenStudyDTO, flattenStudy } from "./flatten";
// 🔹 PRIMARY용 (flatten.ts에 같이 export 해둔다고 가정)
import {
  StudyDTOPRIMARY,
  FlattenStudyDTOPRIMARY,
  flattenStudyPRIMARY,
} from "./flattenPrimary";

import { evaluateFlat } from "./evaluate";
import { evaluateFlatPRIMARY } from "./evaluatePrimary";

import { text2json } from "./text2json";
import { text2jsonPDF } from "./text2jsonPDF";
import { text2jsonPRIMARY } from "./text2jsonPRIMARY";

const MAX_ATTEMPTS_PER_CASE = 3;

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
  const type = argMap["type"];
  if (!vendor || !size) {
    console.error(
      "❌ Usage: ts-node scripts/batchEvaluate.tsx --vendor=OPENAI|GEMINI|DEEPSEEK|CLAUDE --size=FLAGSHIP|LIGHT",
    );
    process.exit(1);
  }

  const supportedVendors = ["OPENAI", "GEMINI", "DEEPSEEK", "CLAUDE"];
  const supportedSizes = ["FLAGSHIP", "LIGHT"];
  const supportedTypes = ["DEFAULT", "PRIMARY", "METHOD", "PDF"];
  if (
    !supportedVendors.includes(vendor) ||
    !supportedSizes.includes(size) ||
    !supportedTypes.includes(type)
  ) {
    console.error(
      `❌ Invalid vendor/size/type. Supported vendors: ${supportedVendors.join(
        ", ",
      )} / sizes: ${supportedSizes.join(", ")} / type: ${supportedTypes.join(", ")}`,
    );
    process.exit(1);
  }

  return { vendor, size, type };
}

// 파일명용 slug
function slugify(s: string) {
  return (
    s
      .trim()
      .toLowerCase()
      .replace(/[\s/\\]+/g, "-")
      .replace(/[^a-z0-9-_]/g, "")
      .replace(/-+/g, "-")
      .replace(/^-|-$/g, "") || "case"
  );
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
  // 🔹 PRIMARY/DEFAULT 둘 다 받을 수 있게 any(or union)로 풀어줌
  goldJson: any;
  predJson: any;
};

export async function runBatchEvaluate(): Promise<{
  totalCases: number;
  resultsDir: string;
  cases: PerCaseResult[];
}> {
  const { vendor, size, type } = parseArgs();
  const isPdfType = type === "PDF";
  const isPrimaryType = type === "PRIMARY";

  // 1) goldJson + studyText 로드 (여러 케이스)
  const pairs: ModulePair[] = await loadFile(type);

  if (!pairs.length) {
    console.warn(
      "[WARN] No pairs found. Check GOLD_DIR and exports (TEXT*, JSON*) or PDF files.",
    );
  }
  const RESULTS_DIR = path.resolve(
    process.cwd(),
    "public",
    "results",
    type.toLowerCase(),
    `${vendor.toLowerCase()}_${size.toLowerCase()}`,
  );

  await ensureDir(RESULTS_DIR);

  const summary: PerCaseResult[] = [];

  // 2) 각 케이스 순회
  for (const p of pairs) {
    const caseSlug = slugify(p.name);
    const outName = `${caseSlug}.json`;
    const outPath = path.join(RESULTS_DIR, outName);

    try {
      let predJson: any = null;
      let lastError: any = null;

      // -------------------------
      // 2-1) LLM 호출 (텍스트→JSON)
      // -------------------------
      for (let attempt = 0; attempt < MAX_ATTEMPTS_PER_CASE; attempt++) {
        try {
          let updatedSpec: any = null;

          if (isPdfType) {
            if (!p.pdfPath) {
              throw new Error(`Missing PDF path for case ${p.name}`);
            }
            const res = await text2jsonPDF(
              p.pdfPath,
              vendor as "OPENAI" | "GEMINI" | "DEEPSEEK" | "CLAUDE",
              size as "FLAGSHIP" | "LIGHT",
            );
            updatedSpec = res.updatedSpec ?? null;
          } else if (isPrimaryType) {
            // 🔹 PRIMARY 타입 → text2jsonPRIMARY
            if (!p.studyText) {
              throw new Error(`Missing study text for case ${p.name}`);
            }
            const { updatedSpec: spec } = await text2jsonPRIMARY(
              p.studyText,
              vendor as "OPENAI" | "GEMINI" | "DEEPSEEK" | "CLAUDE",
              size as "FLAGSHIP" | "LIGHT",
            );
            updatedSpec = spec ?? null;
          } else {
            // 🔹 DEFAULT / METHOD
            if (!p.studyText) {
              throw new Error(`Missing study text for case ${p.name}`);
            }
            const { updatedSpec: spec } = await text2json(
              p.studyText,
              vendor as "OPENAI" | "GEMINI" | "DEEPSEEK" | "CLAUDE",
              size as "FLAGSHIP" | "LIGHT",
            );
            updatedSpec = spec ?? null;
          }

          if (updatedSpec && typeof updatedSpec === "object") {
            predJson = updatedSpec;
            break;
          }

          lastError = new Error("JSON parse failed (null or non-object)");
          const isLast = attempt === MAX_ATTEMPTS_PER_CASE - 1;
          if (!isLast) {
            console.warn(
              `[WARN] ${p.name}: JSON parse failed. Retrying (${attempt + 1}/${MAX_ATTEMPTS_PER_CASE})...`,
            );
          }
        } catch (err) {
          lastError = err;
          const isLast = attempt === MAX_ATTEMPTS_PER_CASE - 1;
          if (!isLast) {
            console.warn(
              `[WARN] ${p.name}: ${String(
                err,
              )}. Retrying (${attempt + 1}/${MAX_ATTEMPTS_PER_CASE})...`,
            );
          }
        }
      }

      if (!predJson || typeof predJson !== "object") {
        const bad: PerCaseResult = {
          name: p.name,
          fileName: outName,
          createdAt: new Date().toISOString(),
          metrics: { jaccard: 0, recall: 0, precision: 0 },
          counts: {
            gold: 0,
            pred: 0,
            intersection: 0,
            union: 0,
            both: 0,
            predOnly: 0,
            goldOnly: 0,
          },
          details: { bothJson: [], predJsonOnly: [], goldJsonOnly: [] },
          sectionAccuracy: {
            studyPeriods: null,
            timeAtRisks: null,
            propensityScoreAdjustment: null,
            // fitOutcomeModelArgs: null
          },
          goldJson: p.goldJson,
          predJson: predJson,
        };

        await fs.writeFile(outPath, JSON.stringify(bad, null, 2), "utf8");
        summary.push(bad);
        const errMsg = lastError
          ? String(lastError instanceof Error ? lastError.message : lastError)
          : "Unknown parse error";
        console.warn(
          `[WARN] ${p.name}: JSON parse failed after ${MAX_ATTEMPTS_PER_CASE} attempt(s). Last error: ${errMsg}`,
        );
        continue;
      }

      // --------------------------------
      // 2-2) flatten: gold / pred 평탄화
      // --------------------------------
      let evalRes: any;

      if (isPrimaryType) {
        // 🔹 PRIMARY 플로우
        const flatGoldPRIMARY: FlattenStudyDTOPRIMARY = flattenStudyPRIMARY(
          p.goldJson as StudyDTOPRIMARY,
        );
        const flatPredPRIMARY: FlattenStudyDTOPRIMARY = flattenStudyPRIMARY(
          predJson as StudyDTOPRIMARY,
        );
        evalRes = evaluateFlatPRIMARY(flatGoldPRIMARY, flatPredPRIMARY);
      } else {
        // 🔹 기존 플로우 (DEFAULT / METHOD / PDF)
        const flatGold: FlattenStudyDTO = flattenStudy(p.goldJson as StudyDTO);
        const flatPred: FlattenStudyDTO = flattenStudy(predJson as StudyDTO);
        evalRes = evaluateFlat(flatGold, flatPred);
      }

      // --------------------------------
      // 2-3) 결과 저장 (results/<파일이름>.json)
      // --------------------------------
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
      console.log(
        `[OK] Saved: ${path.relative(
          process.cwd(),
          outPath,
        )}  (J=${one.metrics.jaccard.toFixed(3)} / R=${one.metrics.recall.toFixed(
          3,
        )} / P=${one.metrics.precision.toFixed(3)})`,
      );
    } catch (err: any) {
      console.error(`❌ [ERROR] ${p.name}:`, err);

      // 실패 시 원문 응답을 따로 저장
      const failOutPath = path.join(
        RESULTS_DIR,
        `${slugify(p.name)}_error.json`,
      );
      const errorData = {
        name: p.name,
        error: String(err instanceof Error ? err.message : err),
        stack: err?.stack ?? null,
        createdAt: new Date().toISOString(),
      };
      await fs.writeFile(failOutPath, JSON.stringify(errorData, null, 2), "utf8");
      console.warn(`Raw error info saved to ${failOutPath}`);
    }
  }

  // ... for 루프 종료 직후, 요약 파일 쓰기 전에 추가
  const totalCases = summary.length;

  // 섹션별 집계 헬퍼
  function sectionCounts(key: keyof PerCaseResult["sectionAccuracy"]) {
    let evaluated = 0; // null이 아닌 값
    let trueCount = 0; // true 개수
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

  // 전체 field 단위 TP/FP/FN 집계
  let wholeTP = 0;
  let wholeFP = 0;
  let wholeFN = 0;

  for (const c of summary) {
    const cnt = c.counts;
    // both ≒ TP, predOnly ≒ FP, goldOnly ≒ FN 으로 사용
    const tp = cnt.both ?? cnt.intersection ?? 0;
    const fp = cnt.predOnly ?? 0;
    const fn = cnt.goldOnly ?? 0;

    wholeTP += tp;
    wholeFP += fp;
    wholeFN += fn;
  }

  const fieldPrecision =
    wholeTP + wholeFP > 0 ? wholeTP / (wholeTP + wholeFP) : null;

  const fieldSensitivity =
    wholeTP + wholeFN > 0 ? wholeTP / (wholeTP + wholeFN) : null;

  const fieldFPperCase = totalCases > 0 ? wholeFP / totalCases : null;

  // 전체 요약 파일도 하나 남겨두기
  const indexPath = path.join(RESULTS_DIR, "_summary.index.json");
  await fs.writeFile(
    indexPath,
    JSON.stringify(
      {
        createdAt: new Date().toISOString(),
        totalCases,
        // 전체 field 기준 요약 메트릭
        fieldLevelMetrics: {
          wholeTP,
          wholeFP,
          wholeFN,
          fieldPrecision,
          fieldSensitivity,
          fieldFPperCase,
        },
        // 섹션별 정답 여부 요약
        sectionAccuracySummary,
        // 각 케이스별 상세 결과
        results: summary,
      },
      null,
      2),
    "utf8",
  );

  console.log(
    `[DONE] ${summary.length} case(s). Summary: ${path.relative(
      process.cwd(),
      indexPath,
    )}`,
  );

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
