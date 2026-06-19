import fs from "node:fs/promises";
import path from "node:path";
import {
  Anchor,
  BaseSelection,
  CaliperScale,
  FlattenStudyDTO,
  ModelType,
  RemoveDuplicate,
  StudyDTO,
  TrimByPsArgs,
  flattenStudy,
} from "./flatten";
import {
  FlattenStudyDTOPRIMARY,
  StudyDTOPRIMARY,
  flattenStudyPRIMARY,
} from "./flattenPrimary";
import { loadFile, ModulePair } from "./loadFile";
import { applyAcceptToPredFlat, canonEquivalence } from "./acceptableAnswers";

// ===== 네가 원하는 최종 Flatten 타입 =====
export type FlattenStudyDoneDTO = {
  /////// getDbCohortMethodDataArgs ///////
  studyPeriods: { description?: string; studyStartDate: string | null; studyEndDate: string | null }[]; // yyyyMMdd

  timeAtRisks: {
    riskWindowStart: number;
    startAnchor: Anchor;
    riskWindowEnd: number;
    endAnchor: Anchor;
    minDaysAtRisk: number;
  }[];

  /////// psSettings (top-level, C3) ///////
  psSettings: {
    description: string;
    trimByPsArgs: TrimByPsArgs | null;
    maxRatio: number | null;
    caliper: number | null;
    caliperScale: CaliperScale | null;
    numberOfStrata: number | null;
    baseSelection: BaseSelection | null;
    inversePtWeighting: boolean;
  }[];

  /////// fitOutcomeModelArgs — outcomeModels per item (C5) ///////
  outcomeModels: {
    description: string;
    modelType: ModelType;
    useCovariates: boolean;
  }[];
};

type FlatDone = FlattenStudyDoneDTO;

// --- 안정 직렬화(키 정렬) ---
const stableStringify = (v: any): string => {
  const recur = (x: any): any => {
    if (x === null || x === undefined) return null;
    if (Array.isArray(x)) return x.map(recur);
    if (typeof x === "object") {
      const entries = Object.entries(x)
        .map(([k, v]) => [k, recur(v)] as const)
        .sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0));
      const o: Record<string, any> = {};
      for (const [k, v] of entries) o[k] = v;
      return o;
    }
    return x;
  };
  return JSON.stringify(recur(v));
};

// 값 정규화
const normVal = (v: unknown): string => {
  if (v === null || v === undefined) return "null";
  if (typeof v === "string") return v.trim();
  if (typeof v === "number" || typeof v === "boolean") return String(v);
  return stableStringify(v);
};

// 객체 항목 캐논화
const canonicalizeObject = (field: string, obj: Record<string, unknown>): string => {
  const entries = Object.entries(obj)
    .map(([k, v]) => [k, normVal(v)] as const)
    .sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0));
  const body = entries.map(([k, v]) => `${k}=${v}`).join("|");
  return `${field}:{${body}}`;
};

// ======== flatten 헬퍼 ========

const pickDoneFields = (flat: FlattenStudyDTO): FlattenStudyDoneDTO => ({
  studyPeriods: flat.studyPeriods ?? [],
  timeAtRisks: flat.timeAtRisks ?? [],
  psSettings: flat.psSettings ?? [],
  outcomeModels: flat.outcomeModels ?? [],
});

const normalizeToArray = <T>(v: T[] | T | undefined | null): T[] => {
  if (Array.isArray(v)) return v;
  return v === undefined || v === null ? [] : [v];
};

const defaultCreatePsArgs = {
  maxCohortSizeForFitting: 0,
  errorOnHighCorrelation: false,
  prior: null,
  control: null,
};

const defaultGetDbCohortMethodDataArgs = {
  studyPeriods: [] as { description: string; studyStartDate: string | null; studyEndDate: string | null }[],
  maxCohortSize: 0,
  firstExposureOnly: false,
  removeDuplicateSubjects: "keep all" as RemoveDuplicate,
  restrictToCommonPeriod: false,
  washoutPeriod: 0,
};

const defaultCreateStudyPopArgs = {
  censorAtNewRiskWindow: false,
  removeSubjectsWithPriorOutcome: false,
  priorOutcomeLookback: 0,
  timeAtRisks: [] as {
    riskWindowStart: number;
    startAnchor: Anchor;
    riskWindowEnd: number;
    endAnchor: Anchor;
    minDaysAtRisk: number;
  }[],
};

export const flattenStudyDone = (dto: Partial<StudyDTO>): FlattenStudyDoneDTO => {
  // 기본 flattenStudy는 array를 기대하므로 object로 들어온 경우 array로 감싸준다.
  const normalized: Partial<StudyDTO> = {
    ...dto,
    getDbCohortMethodDataArgs: {
      ...defaultGetDbCohortMethodDataArgs,
      ...(dto.getDbCohortMethodDataArgs ?? {}),
      studyPeriods: normalizeToArray((dto.getDbCohortMethodDataArgs as any)?.studyPeriods),
    },
    createStudyPopArgs: {
      ...defaultCreateStudyPopArgs,
      ...(dto.createStudyPopArgs ?? {}),
      timeAtRisks: normalizeToArray((dto.createStudyPopArgs as any)?.timeAtRisks),
    },
    // C3: psSettings/createPsArgs are now top-level (no propensityScoreAdjustment wrapper)
    psSettings: normalizeToArray((dto as any)?.psSettings),
    createPsArgs: (dto as any)?.createPsArgs ?? defaultCreatePsArgs,
  };

  return pickDoneFields(flattenStudy(normalized));
};

// PRIMARY: flattenStudyPRIMARY 결과를 done 형태(배열 기반)로 변환
const pickDoneFieldsPrimary = (
  flat: FlattenStudyDTOPRIMARY,
): FlattenStudyDoneDTO => ({
  studyPeriods: flat.studyPeriodsPRIMARY
    ? [
        {
          studyStartDate: flat.studyPeriodsPRIMARY.studyStartDate,
          studyEndDate: flat.studyPeriodsPRIMARY.studyEndDate,
        },
      ]
    : [],
  timeAtRisks: flat.timeAtRisksPRIMARY
    ? [
        {
          riskWindowStart: flat.timeAtRisksPRIMARY.riskWindowStart,
          startAnchor: flat.timeAtRisksPRIMARY.startAnchor,
          riskWindowEnd: flat.timeAtRisksPRIMARY.riskWindowEnd,
          endAnchor: flat.timeAtRisksPRIMARY.endAnchor,
          minDaysAtRisk: flat.timeAtRisksPRIMARY.minDaysAtRisk,
        },
      ]
    : [],
  psSettings: flat.psSettingsPRIMARY
    ? [
        {
          description: "",
          trimByPsArgs: null,
          maxRatio: flat.psSettingsPRIMARY.maxRatio,
          caliper: flat.psSettingsPRIMARY.caliper,
          caliperScale: flat.psSettingsPRIMARY.caliperScale,
          numberOfStrata: flat.psSettingsPRIMARY.numberOfStrata,
          baseSelection: flat.psSettingsPRIMARY.baseSelection,
          inversePtWeighting: false,
        },
      ]
    : [],
  outcomeModels: [],
});

export const flattenStudyDonePrimary = (
  dto: Partial<StudyDTOPRIMARY>,
): FlattenStudyDoneDTO => pickDoneFieldsPrimary(flattenStudyPRIMARY(dto));

// ======== facts 변환 ========

export const toFactsDone = (flat: FlatDone): Set<string> => {
  const facts = new Set<string>();

  // 빈 배열([])도 채점에 포함: 비어있으면 sentinel fact를 emit한다.
  //  gold 빈 + pred 빈     → sentinel끼리 매칭 → TP (정답)
  //  gold 빈 + pred 값있음 → gold sentinel은 FN, pred 값은 FP (오답)
  const EMPTY = (field: string) => canonicalizeObject(field, { __empty__: true });

  if ((flat.studyPeriods ?? []).length === 0) {
    facts.add(EMPTY("studyPeriods"));
  } else {
    for (const sp of flat.studyPeriods ?? []) {
      facts.add(
        canonicalizeObject("studyPeriods", {
          studyStartDate: sp.studyStartDate,
          studyEndDate: sp.studyEndDate,
        }),
      );
    }
  }

  if ((flat.timeAtRisks ?? []).length === 0) {
    facts.add(EMPTY("timeAtRisks"));
  } else {
    for (const t of flat.timeAtRisks ?? []) {
      facts.add(
        canonicalizeObject("timeAtRisks", {
          riskWindowStart: normalizeRiskWindowStart(t.riskWindowStart),
          startAnchor: t.startAnchor,
          riskWindowEnd: normalizeRiskWindowEnd(t.riskWindowEnd),
          endAnchor: t.endAnchor,
          // minDaysAtRisk: 채점 제외 (evaluate.ts와 동일 — test "minDaysAtRisk ignored")
        }),
      );
    }
  }

  if ((flat.psSettings ?? []).length === 0) {
    facts.add(EMPTY("psSettings"));
  } else {
    for (const p of flat.psSettings ?? []) {
      facts.add(
        canonicalizeObject("psSettings", {
          maxRatio: normalizeMaxRatio(p.maxRatio),
          caliper: p.caliper,
          caliperScale: p.caliperScale,
          numberOfStrata: p.numberOfStrata,
          baseSelection: p.baseSelection,
          trimByPsArgs: p.trimByPsArgs ?? null,
          inversePtWeighting: p.inversePtWeighting,
        }),
      );
    }
  }

  // outcomeModels[] — per-item modelType/useCovariates (C5)
  if ((flat.outcomeModels ?? []).length === 0) {
    facts.add(EMPTY("outcomeModels"));
  } else {
    for (const om of flat.outcomeModels ?? []) {
      facts.add(
        canonicalizeObject("outcomeModels", {
          modelType: om.modelType,
          useCovariates: om.useCovariates,
        }),
      );
    }
  }

  return facts;
};

// ======== 공통 유틸 ========

const safeDiv = (num: number, den: number, whenZero: number): number =>
  den === 0 ? whenZero : num / den;

// fact 키 추출: "a=b" -> "a", "foo:{...}" -> "foo"
const factKey = (f: string): string => {
  const objIdx = f.indexOf(":{");
  if (objIdx !== -1) return f.slice(0, objIdx);
  const eqIdx = f.indexOf("=");
  return eqIdx === -1 ? f : f.slice(0, eqIdx);
};

// acceptableAnswers의 canonEquivalence 단일 소스 사용 (evaluate.ts와 동일).
//  riskWindowEnd: 99999 ≡ 9999 ≡ 999999  /  riskWindowStart: 0 ≡ 1  /  maxRatio: 0 ≡ 100
const normalizeRiskWindowEnd = (n: number | null | undefined): number | null =>
  canonEquivalence("riskWindowEnd", n ?? null) as number | null;

const normalizeRiskWindowStart = (n: number | null | undefined): number | null =>
  canonEquivalence("riskWindowStart", n ?? null) as number | null;

const normalizeMaxRatio = (n: number | null | undefined): number | null =>
  canonEquivalence("maxRatio", n ?? null) as number | null;

const setEq = (A: Set<string>, B: Set<string>) =>
  A.size === B.size && [...A].every((x) => B.has(x));

const canonStudyPeriods = (arr: FlatDone["studyPeriods"]) =>
  new Set(
    (arr ?? []).map((sp) =>
      canonicalizeObject("studyPeriods", {
        studyStartDate: sp.studyStartDate,
        studyEndDate: sp.studyEndDate,
      }),
    ),
  );

const canonTimeAtRisks = (arr: FlatDone["timeAtRisks"]) =>
  new Set(
    (arr ?? []).map((t) =>
      canonicalizeObject("timeAtRisks", {
        riskWindowStart: normalizeRiskWindowStart(t.riskWindowStart),
        startAnchor: t.startAnchor,
        riskWindowEnd: normalizeRiskWindowEnd(t.riskWindowEnd),
        endAnchor: t.endAnchor,
        // minDaysAtRisk: 채점 제외 (evaluate.ts와 동일)
      }),
    ),
  );

const canonPsSettings = (arr: FlatDone["psSettings"]) =>
  new Set(
    (arr ?? []).map((p) =>
      canonicalizeObject("psSettings", {
        maxRatio: normalizeMaxRatio(p.maxRatio),
        caliper: p.caliper,
        caliperScale: p.caliperScale,
        numberOfStrata: p.numberOfStrata,
        baseSelection: p.baseSelection,
        trimByPsArgs: p.trimByPsArgs ?? null,
        inversePtWeighting: p.inversePtWeighting,
      }),
    ),
  );

const canonOutcomeModels = (arr: FlatDone["outcomeModels"]) =>
  new Set(
    (arr ?? []).map((om) =>
      canonicalizeObject("outcomeModels", {
        modelType: om.modelType,
        useCovariates: om.useCovariates,
      }),
    ),
  );

// ======== 메인 비교 함수 ========

export const evaluateFlatDone = (gold: FlatDone, predRaw: FlatDone, caseName?: string) => {
  // 복수정답 accept: pred를 gold 기준으로 snap (gold는 불변). caseName 없으면 미적용.
  // accept 레이어는 timeAtRisks(riskWindowStart/End)·studyPeriods(studyEndDate)만 건드리며
  // 두 flat 타입이 해당 필드를 공유하므로 done flat에 그대로 적용 가능.
  const pred = caseName
    ? (applyAcceptToPredFlat(caseName, gold as any, predRaw as any) as unknown as FlatDone)
    : predRaw;
  const AF = toFactsDone(gold);
  const BF = toFactsDone(pred);

  // gold에 존재하는 key만 스코프
  const goldKeys = new Set<string>();
  for (const f of AF) goldKeys.add(factKey(f));

  const BF_scoped = new Set<string>();
  for (const f of BF) if (goldKeys.has(factKey(f))) BF_scoped.add(f);

  // 교집합/차집합
  const inter = new Set<string>();
  for (const f of AF) if (BF_scoped.has(f)) inter.add(f);

  const union = new Set<string>(AF);
  for (const f of BF_scoped) union.add(f);

  const TP = inter.size;
  const FP = [...BF_scoped].filter((f) => !AF.has(f)).length;
  const FN = [...AF].filter((f) => !BF_scoped.has(f)).length;

  const jaccard = safeDiv(TP, union.size, 1);
  const recall = safeDiv(TP, AF.size, 1);
  const precision = safeDiv(TP, BF_scoped.size, 1);

  // 섹션별 메트릭 (기존 방식 유지)
  const diffMetrics = (
    goldArr: any[] | undefined,
    predArr: any[] | undefined,
    canonizer: (x: any) => Set<string>,
  ) => {
    const setA = canonizer(goldArr);
    const setB = canonizer(predArr);
    const both = [...setA].filter((x) => setB.has(x));
    const predOnly = [...setB].filter((x) => !setA.has(x));
    const goldOnly = [...setA].filter((x) => !setB.has(x));

    const TP = both.length;
    const FP = predOnly.length;
    const FN = goldOnly.length;

    const precision = TP + FP > 0 ? TP / (TP + FP) : null;
    const recall = TP + FN > 0 ? TP / (TP + FN) : null;
    const f1 =
      precision && recall && precision + recall > 0
        ? (2 * precision * recall) / (precision + recall)
        : null;

    const accuracy = TP + FP + FN > 0 ? TP / (TP + FP + FN) : null;

    return {
      bothCount: TP,
      predOnlyCount: FP,
      goldOnlyCount: FN,
      precision,
      recall,
      f1,
      accuracy,
    };
  };

  // studyPeriods — gold가 비어있어도(null/[]) 항상 채점.
  // gold 빈 + pred 빈 → 정답(true), gold 빈 + pred가 기간 생성 → 오답(false)
  const spSetGold = canonStudyPeriods(gold.studyPeriods);
  const spSetPred = canonStudyPeriods(pred.studyPeriods);
  const spCounts = diffMetrics(gold.studyPeriods, pred.studyPeriods, (arr) => canonStudyPeriods(arr));
  const spAcc: boolean | null = setEq(spSetGold, spSetPred);

  // timeAtRisks — 빈 항목([])도 항상 채점
  const tarSetGold = canonTimeAtRisks(gold.timeAtRisks);
  const tarSetPred = canonTimeAtRisks(pred.timeAtRisks);
  const tarCounts = diffMetrics(gold.timeAtRisks, pred.timeAtRisks, (arr) => canonTimeAtRisks(arr));
  const tarAcc: boolean | null = setEq(tarSetGold, tarSetPred);

  // propensityScoreAdjustment (psSettings만 비교) — 빈 항목도 항상 채점
  const psSetGold = canonPsSettings(gold.psSettings);
  const psSetPred = canonPsSettings(pred.psSettings);
  const psCounts = diffMetrics(gold.psSettings, pred.psSettings, (arr) => canonPsSettings(arr));
  const psAcc: boolean | null = setEq(psSetGold, psSetPred);

  // outcomeModels (C5) — 빈 항목도 항상 채점
  const omSetGold = canonOutcomeModels(gold.outcomeModels);
  const omSetPred = canonOutcomeModels(pred.outcomeModels);
  const omCounts = diffMetrics(gold.outcomeModels, pred.outcomeModels, (arr) => canonOutcomeModels(arr));
  const omAcc: boolean | null = setEq(omSetGold, omSetPred);

  return {
    jaccard,
    recall,
    precision,
    counts: {
      gold: AF.size,
      pred: BF_scoped.size,
      intersection: TP,
      union: union.size,
      both: TP,
      predOnly: FP,
      goldOnly: FN,
    },
    details: {
      bothJson: [...inter],
      predJsonOnly: [...BF_scoped].filter((f) => !AF.has(f)),
      goldJsonOnly: [...AF].filter((f) => !BF_scoped.has(f)),
    },
    sectionAccuracy: {
      studyPeriods: spAcc,
      timeAtRisks: tarAcc,
      propensityScoreAdjustment: psAcc,
      outcomeModels: omAcc,
    },
    sectionCounts: {
      studyPeriods: spCounts,
      timeAtRisks: tarCounts,
      propensityScoreAdjustment: psCounts,
      outcomeModels: omCounts,
    },
  };
};

// ==============================
// 기존 결과 JSON 재평가 러너
// ==============================

type ParsedArgs = { vendor: string; size: string; type: string };

function parseArgs(): ParsedArgs {
  const args = process.argv.slice(2);
  const argMap: Record<string, string> = {};
  for (const a of args) {
    const [k, v] = a.split("=");
    if (k && v) argMap[k.replace(/^--/, "").toLowerCase()] = v.toUpperCase();
  }

  const vendor = argMap["vendor"];
  const size = argMap["size"];
  const type = argMap["type"];
  if (!vendor || !size || !type) {
    console.error(
      "❌ Usage: tsx evaluateDoneJson.ts --vendor=OPENAI|GEMINI|DEEPSEEK|CLAUDE --size=FLAGSHIP|LIGHT --type=DEFAULT|METHOD|PDF",
    );
    process.exit(1);
  }

  const supportedVendors = ["OPENAI", "GEMINI", "DEEPSEEK", "CLAUDE"];
  const supportedSizes = ["FLAGSHIP", "LIGHT"];
  const supportedTypes = ["DEFAULT", "METHOD", "PDF", "PRIMARY"];

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

const ensureDir = async (abs: string) => {
  await fs.mkdir(abs, { recursive: true });
};

export type DoneCaseResult = {
  name: string;
  fileName: string;
  createdAt: string;
  metrics: { jaccard: number; recall: number; precision: number };
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
    outcomeModels: boolean | null;
  };
  sectionCounts?: {
    studyPeriods?: any;
    timeAtRisks?: any;
    propensityScoreAdjustment?: any;
    outcomeModels?: any;
  };
  goldJson?: any;
  predJson?: any;
  error?: string | null;
};

const folderMap: Record<string, string> = {
  DEFAULT: "default",
  METHOD: "method",
  PDF: "pdf",
  PRIMARY: "primary",
};

function isTestDataset(): boolean {
  const goldDir = process.env.GOLD_STANDARD_DIR;
  if (!goldDir) return false;
  const resolved = path.resolve(process.cwd(), goldDir);
  return path.basename(resolved).toLowerCase() === "goldstandardtest";
}

export async function runEvaluateDone(): Promise<{
  totalCases: number;
  resultsDir: string;
  sourceDir: string;
  cases: DoneCaseResult[];
}> {
  const { vendor, size, type } = parseArgs();
  const isPrimary = type === "PRIMARY";
  const folderName = folderMap[type];
  const lowerVendor = vendor.toLowerCase();
  const lowerSize = size.toLowerCase();
  const datasetSegment = isTestDataset() ? "non-ohdsi" : "ohdsi";

  const sourceDir = path.resolve(
    process.cwd(),
    "public",
    "results",
    datasetSegment,
    folderName,
    `${lowerVendor}_${lowerSize}`,
  );
  const outputDir = path.resolve(
    process.cwd(),
    "public",
    "results",
    "done",
    datasetSegment,
    folderName,
    `${lowerVendor}_${lowerSize}`,
  );

  await ensureDir(outputDir);

  // Gold standard를 새로 읽어옴
  const goldPairs = await loadFile(type);
  const goldMap = new Map<string, any>();
  for (const pair of goldPairs) {
    // name을 소문자로 정규화해서 매핑
    goldMap.set(pair.name.toLowerCase(), pair.goldJson);
  }
  console.log(`[INFO] Loaded ${goldPairs.length} gold standard(s) from ${type} folder`);

  const entries = await fs.readdir(sourceDir, { withFileTypes: true });
  // merino.json 제외
  const EXCLUDED_FILES = ["merino.json"];

  const jsonFiles = entries
    .filter((e) => e.isFile())
    .map((e) => e.name)
    .filter((n) => n.endsWith(".json") && !n.startsWith("_") && !EXCLUDED_FILES.includes(n.toLowerCase()));

  if (!jsonFiles.length) {
    console.warn(`[WARN] No case files found under ${sourceDir}`);
  }

  const summary: DoneCaseResult[] = [];

  for (const file of jsonFiles) {
    const abs = path.join(sourceDir, file);
    const outPath = path.join(outputDir, file);
    const baseName = path.basename(file, ".json");

    try {
      const raw = JSON.parse(await fs.readFile(abs, "utf8"));
      // Gold standard는 새로 읽어온 것 사용, predJson은 기존 결과 파일에서 사용
      const goldJson = goldMap.get(baseName.toLowerCase()) ?? raw.goldJson;
      const predJson = raw.predJson;

      if (!goldMap.has(baseName.toLowerCase())) {
        console.warn(`[WARN] ${baseName}: gold standard not found in current gold dir, using stored goldJson`);
      }

      if (!goldJson || !predJson) {
        const bad: DoneCaseResult = {
          name: raw.name ?? baseName,
          fileName: file,
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
            outcomeModels: null,
          },
          goldJson,
          predJson,
          error: "Missing goldJson or predJson",
        };
        await fs.writeFile(outPath, JSON.stringify(bad, null, 2), "utf8");
        summary.push(bad);
        continue;
      }

      const flatGold = isPrimary
        ? flattenStudyDonePrimary(goldJson as StudyDTOPRIMARY)
        : flattenStudyDone(goldJson as StudyDTO);
      const flatPred = isPrimary
        ? flattenStudyDonePrimary(predJson as StudyDTOPRIMARY)
        : flattenStudyDone(predJson as StudyDTO);
      const evalRes = evaluateFlatDone(flatGold, flatPred, raw.name ?? baseName);

      const one: DoneCaseResult = {
        name: raw.name ?? baseName,
        fileName: file,
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
        goldJson,
        predJson,
        error: null,
      };

      await fs.writeFile(outPath, JSON.stringify(one, null, 2), "utf8");
      summary.push(one);
      console.log(
        `[DONE-EVAL] ${file}: J=${one.metrics.jaccard.toFixed(
          3,
        )} / R=${one.metrics.recall.toFixed(3)} / P=${one.metrics.precision.toFixed(
          3,
        )}`,
      );
    } catch (err: any) {
      const fail: DoneCaseResult = {
        name: baseName,
        fileName: file,
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
          outcomeModels: null,
        },
        error: String(err?.message ?? err),
      };
      await fs.writeFile(outPath, JSON.stringify(fail, null, 2), "utf8");
      summary.push(fail);
      console.warn(`[WARN] Failed to evaluate ${file}: ${fail.error}`);
    }
  }

  const totalCases = summary.length;

  // 섹션별 집계 헬퍼
  function sectionCounts(key: keyof DoneCaseResult["sectionAccuracy"]) {
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
    outcomeModels: sectionCounts("outcomeModels"),
  };

  // ===== 항목(섹션)별 field 단위 집계 (빈배열 sentinel 포함) =====
  // details(fact)를 섹션 key로 그룹화 → 항목별 gold 개수(goldItems) + TP/FP/FN + precision/sensitivity.
  // goldItems = TP+FN = 해당 항목의 gold fact 수(빈배열은 sentinel 1개로 계상).
  const SECTION_FACT_KEYS = ["studyPeriods", "timeAtRisks", "psSettings", "outcomeModels"] as const;
  const SECTION_LABEL: Record<string, string> = {
    studyPeriods: "studyPeriods",
    timeAtRisks: "timeAtRisks",
    psSettings: "propensityScoreAdjustment",
    outcomeModels: "outcomeModels",
  };
  const sectionFieldAgg = (sectionKey: string) => {
    let tp = 0;
    let fp = 0;
    let fn = 0;
    for (const c of summary) {
      for (const f of c.details?.bothJson ?? []) if (factKey(f) === sectionKey) tp++;
      for (const f of c.details?.predJsonOnly ?? []) if (factKey(f) === sectionKey) fp++;
      for (const f of c.details?.goldJsonOnly ?? []) if (factKey(f) === sectionKey) fn++;
    }
    const goldItems = tp + fn; // 채점 대상 gold 항목 수 (빈배열 sentinel 포함)
    const predItems = tp + fp;
    const precision = tp + fp > 0 ? tp / (tp + fp) : null;
    const sensitivity = tp + fn > 0 ? tp / (tp + fn) : null;
    const f1 =
      precision && sensitivity && precision + sensitivity > 0
        ? (2 * precision * sensitivity) / (precision + sensitivity)
        : null;
    return { goldItems, predItems, tp, fp, fn, precision, sensitivity, f1 };
  };
  const sectionFieldMetrics: Record<string, ReturnType<typeof sectionFieldAgg>> = {};
  for (const k of SECTION_FACT_KEYS) sectionFieldMetrics[SECTION_LABEL[k]] = sectionFieldAgg(k);

  console.log(
    `\n[SECTION FIELD METRICS] ${datasetSegment}/${folderName}/${lowerVendor}_${lowerSize}  (cases=${totalCases})`,
  );
  console.log(
    "  " +
      "section".padEnd(26) +
      "goldItems".padStart(10) +
      "TP".padStart(6) +
      "FP".padStart(6) +
      "FN".padStart(6) +
      "P%".padStart(8) +
      "S%".padStart(8),
  );
  for (const k of SECTION_FACT_KEYS) {
    const m = sectionFieldMetrics[SECTION_LABEL[k]];
    console.log(
      "  " +
        SECTION_LABEL[k].padEnd(26) +
        String(m.goldItems).padStart(10) +
        String(m.tp).padStart(6) +
        String(m.fp).padStart(6) +
        String(m.fn).padStart(6) +
        (m.precision == null ? "n/a" : (m.precision * 100).toFixed(1)).padStart(8) +
        (m.sensitivity == null ? "n/a" : (m.sensitivity * 100).toFixed(1)).padStart(8),
    );
  }

  // 전체 field 단위 TP/FP/FN 집계
  let wholeTP = 0;
  let wholeFP = 0;
  let wholeFN = 0;

  for (const c of summary) {
    const cnt = c.counts;
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

  const indexPath = path.join(outputDir, "_summary.index.json");
  await fs.writeFile(
    indexPath,
    JSON.stringify(
      {
        createdAt: new Date().toISOString(),
        totalCases,
        fieldLevelMetrics: {
          wholeTP,
          wholeFP,
          wholeFN,
          fieldPrecision,
          fieldSensitivity,
          fieldFPperCase,
        },
        sectionAccuracySummary,
        sectionFieldMetrics,
        results: summary,
      },
      null,
      2,
    ),
    "utf8",
  );

  console.log(
    `[DONE] ${summary.length} case(s). Summary: ${path.relative(
      process.cwd(),
      indexPath,
    )}`,
  );

  return {
    totalCases,
    resultsDir: outputDir,
    sourceDir,
    cases: summary,
  };
}

// 단독 실행 지원
if (require.main === module) {
  runEvaluateDone().catch((e) => {
    console.error(e);
    process.exit(1);
  });
}
