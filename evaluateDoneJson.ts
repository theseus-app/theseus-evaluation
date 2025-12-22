import fs from "node:fs/promises";
import path from "node:path";
import {
  Anchor,
  BaseSelection,
  CaliperScale,
  FlattenStudyDTO,
  RemoveDuplicate,
  StudyDTO,
  flattenStudy,
} from "./flatten";
import {
  FlattenStudyDTOPRIMARY,
  StudyDTOPRIMARY,
  flattenStudyPRIMARY,
} from "./flattenPrimary";

// ===== 네가 원하는 최종 Flatten 타입 =====
export type FlattenStudyDoneDTO = {
  /////// getDbCohortMethodDataArgs ///////
  studyPeriods: { studyStartDate: string | null; studyEndDate: string | null }[]; // yyyyMMdd

  timeAtRisks: {
    riskWindowStart: number;
    startAnchor: Anchor;
    riskWindowEnd: number;
    endAnchor: Anchor;
    minDaysAtRisk: number;
  }[];

  /////// propensityScoreAdjustment ///////
  psSettings: {
    maxRatio: number | null;
    caliper: number | null;
    caliperScale: CaliperScale | null;
    numberOfStrata: number | null;
    baseSelection: BaseSelection | null;
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
});

const normalizeToArray = <T>(v: T[] | T | undefined | null): T[] => {
  if (Array.isArray(v)) return v;
  return v === undefined || v === null ? [] : [v];
};

const isBlankString = (v: unknown) =>
  typeof v === "string" && v.trim() === "";

const filterEmptyStudyPeriods = (
  arr: { studyStartDate: string | number | null; studyEndDate: string | number | null }[],
) =>
  (arr ?? []).filter((sp) => {
    const s = sp?.studyStartDate;
    const e = sp?.studyEndDate;
    return !(isBlankString(s) && isBlankString(e));
  });

const defaultCreatePsArgs = {
  maxCohortSizeForFitting: 0,
  errorOnHighCorrelation: false,
  prior: null,
  control: null,
};

const defaultGetDbCohortMethodDataArgs = {
  studyPeriods: [] as { studyStartDate: string | number | null; studyEndDate: string | number | null }[],
  maxCohortSize: 0,
};

const defaultCreateStudyPopArgs = {
  restrictToCommonPeriod: false,
  firstExposureOnly: false,
  washoutPeriod: 0,
  removeDuplicateSubjects: "keep all" as RemoveDuplicate,
  censorAtNewRiskWindow: false,
  removeSubjectsWithPriorOutcome: false,
  priorOutcomeLookBack: 0,
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
      studyPeriods: filterEmptyStudyPeriods(
        normalizeToArray((dto.getDbCohortMethodDataArgs as any)?.studyPeriods),
      ),
    },
    createStudyPopArgs: {
      ...defaultCreateStudyPopArgs,
      ...(dto.createStudyPopArgs ?? {}),
      timeAtRisks: normalizeToArray((dto.createStudyPopArgs as any)?.timeAtRisks),
    },
    propensityScoreAdjustment: {
      ...(dto.propensityScoreAdjustment ?? {}),
      psSettings: normalizeToArray(
        (dto.propensityScoreAdjustment as any)?.psSettings,
      ),
      createPsArgs:
        (dto.propensityScoreAdjustment as any)?.createPsArgs ?? defaultCreatePsArgs,
    },
  };

  return pickDoneFields(flattenStudy(normalized));
};

// PRIMARY: flattenStudyPRIMARY 결과를 done 형태(배열 기반)로 변환
const pickDoneFieldsPrimary = (
  flat: FlattenStudyDTOPRIMARY,
): FlattenStudyDoneDTO => ({
  studyPeriods: flat.studyPeriodsPRIMARY
    ? filterEmptyStudyPeriods([
        {
          studyStartDate: flat.studyPeriodsPRIMARY.studyStartDate,
          studyEndDate: flat.studyPeriodsPRIMARY.studyEndDate,
        },
      ])
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
          maxRatio: flat.psSettingsPRIMARY.maxRatio,
          caliper: flat.psSettingsPRIMARY.caliper,
          caliperScale: flat.psSettingsPRIMARY.caliperScale,
          numberOfStrata: flat.psSettingsPRIMARY.numberOfStrata,
          baseSelection: flat.psSettingsPRIMARY.baseSelection,
        },
      ]
    : [],
});

export const flattenStudyDonePrimary = (
  dto: Partial<StudyDTOPRIMARY>,
): FlattenStudyDoneDTO => pickDoneFieldsPrimary(flattenStudyPRIMARY(dto));

// ======== facts 변환 ========

const isEmptyStudyPeriod = (sp: {
  studyStartDate: string | null;
  studyEndDate: string | null;
}) => {
  // null 값만 있는 경우는 포함해야 하므로, 빈 문자열만 비움으로 간주
  const blank = (v: string | null) =>
    typeof v === "string" && v.trim() === "";
  return blank(sp.studyStartDate) && blank(sp.studyEndDate);
};

const isEmptyTimeAtRisk = (tar: {
  riskWindowStart: number | null;
  startAnchor: Anchor | null;
  riskWindowEnd: number | null;
  endAnchor: Anchor | null;
  minDaysAtRisk: number | null;
}) => {
  // null만 있는 경우는 포함시키고, 완전 공백/미기재(빈 문자열/undefined)일 때만 제외
  const numEmpty = (n: number | null | undefined) => n === undefined;
  const strEmpty = (s: string | null | undefined) =>
    s === undefined || (typeof s === "string" && s.trim() === "");
  return (
    numEmpty(tar.riskWindowStart) &&
    numEmpty(tar.riskWindowEnd) &&
    numEmpty(tar.minDaysAtRisk) &&
    strEmpty(tar.startAnchor) &&
    strEmpty(tar.endAnchor)
  );
};

const isEmptyPsSetting = (ps: {
  maxRatio: number | null;
  caliper: number | null;
  caliperScale: CaliperScale | null;
  numberOfStrata: number | null;
  baseSelection: BaseSelection | null;
}) => {
  // null만 있는 경우는 포함, undefined/빈 문자열만 있는 경우에만 제외
  const numEmpty = (n: number | null | undefined) => n === undefined;
  const strEmpty = (s: string | null | undefined) =>
    s === undefined || (typeof s === "string" && s.trim() === "");
  return (
    numEmpty(ps.maxRatio) &&
    numEmpty(ps.caliper) &&
    strEmpty(ps.caliperScale) &&
    numEmpty(ps.numberOfStrata) &&
    strEmpty(ps.baseSelection)
  );
};

export const toFactsDone = (flat: FlatDone): Set<string> => {
  const facts = new Set<string>();

  for (const sp of flat.studyPeriods ?? []) {
    if (isEmptyStudyPeriod(sp)) continue; // 빈 기간은 비교에서 제외
    facts.add(
      canonicalizeObject("studyPeriods", {
        studyStartDate: sp.studyStartDate,
        studyEndDate: sp.studyEndDate,
      }),
    );
  }

  for (const t of flat.timeAtRisks ?? []) {
    if (isEmptyTimeAtRisk(t)) continue; // 빈 TAR는 제외
    facts.add(
      canonicalizeObject("timeAtRisks", {
        riskWindowStart: t.riskWindowStart,
        startAnchor: t.startAnchor,
        riskWindowEnd: normalizeRiskWindowEnd(t.riskWindowEnd),
        endAnchor: t.endAnchor,
        minDaysAtRisk: t.minDaysAtRisk,
      }),
    );
  }

  for (const p of flat.psSettings ?? []) {
    if (isEmptyPsSetting(p)) continue; // 빈 psSettings는 제외
    facts.add(
      canonicalizeObject("psSettings", {
        maxRatio: normalizeMaxRatio(p.maxRatio),
        caliper: p.caliper,
        caliperScale: p.caliperScale,
        numberOfStrata: p.numberOfStrata,
        baseSelection: p.baseSelection,
      }),
    );
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

const normalizeRiskWindowEnd = (n: number | null | undefined): number | null => {
  if (n === null || n === undefined) return null;
  return n === 9999 ? 99999 : n;
};

const normalizeMaxRatio = (n: number | null | undefined): number | null => {
  if (n === null || n === undefined) return null;
  return n === 100 ? 0 : n;
};

const setEq = (A: Set<string>, B: Set<string>) =>
  A.size === B.size && [...A].every((x) => B.has(x));

const canonStudyPeriods = (arr: FlatDone["studyPeriods"]) =>
  new Set(
    (arr ?? [])
      .filter((sp) => !isEmptyStudyPeriod(sp))
      .map((sp) =>
        canonicalizeObject("studyPeriods", {
          studyStartDate: sp.studyStartDate,
          studyEndDate: sp.studyEndDate,
        }),
      ),
  );

const canonTimeAtRisks = (arr: FlatDone["timeAtRisks"]) =>
  new Set(
    (arr ?? [])
      .filter((t) => !isEmptyTimeAtRisk(t))
      .map((t) =>
        canonicalizeObject("timeAtRisks", {
          riskWindowStart: t.riskWindowStart,
          startAnchor: t.startAnchor,
          riskWindowEnd: normalizeRiskWindowEnd(t.riskWindowEnd),
          endAnchor: t.endAnchor,
          minDaysAtRisk: t.minDaysAtRisk,
        }),
      ),
  );

const canonPsSettings = (arr: FlatDone["psSettings"]) =>
  new Set(
    (arr ?? [])
      .filter((p) => !isEmptyPsSetting(p))
      .map((p) =>
        canonicalizeObject("psSettings", {
          maxRatio: normalizeMaxRatio(p.maxRatio),
          caliper: p.caliper,
          caliperScale: p.caliperScale,
          numberOfStrata: p.numberOfStrata,
          baseSelection: p.baseSelection,
        }),
      ),
  );

// ======== 메인 비교 함수 ========

export const evaluateFlatDone = (gold: FlatDone, pred: FlatDone) => {
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

  // studyPeriods
  const spSetGold = canonStudyPeriods(gold.studyPeriods);
  const spSetPred = canonStudyPeriods(pred.studyPeriods);
  const spSpecified = spSetGold.size > 0;
  const spCounts = spSpecified
    ? diffMetrics(gold.studyPeriods, pred.studyPeriods, (arr) => canonStudyPeriods(arr))
    : null;
  const spAcc: boolean | null = spSpecified ? setEq(spSetGold, spSetPred) : null;

  // timeAtRisks
  const tarSetGold = canonTimeAtRisks(gold.timeAtRisks);
  const tarSetPred = canonTimeAtRisks(pred.timeAtRisks);
  const tarSpecified = tarSetGold.size > 0;
  const tarCounts = tarSpecified
    ? diffMetrics(gold.timeAtRisks, pred.timeAtRisks, (arr) => canonTimeAtRisks(arr))
    : null;
  const tarAcc: boolean | null = tarSpecified ? setEq(tarSetGold, tarSetPred) : null;

  // propensityScoreAdjustment (psSettings만 비교)
  const psSetGold = canonPsSettings(gold.psSettings);
  const psSetPred = canonPsSettings(pred.psSettings);
  const psSpecified = psSetGold.size > 0;
  const psCounts = psSpecified
    ? diffMetrics(gold.psSettings, pred.psSettings, (arr) => canonPsSettings(arr))
    : null;
  const psAcc: boolean | null = psSpecified ? setEq(psSetGold, psSetPred) : null;

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
    },
    sectionCounts: {
      studyPeriods: spCounts,
      timeAtRisks: tarCounts,
      propensityScoreAdjustment: psCounts,
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
  };
  sectionCounts?: {
    studyPeriods?: any;
    timeAtRisks?: any;
    propensityScoreAdjustment?: any;
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

  const sourceDir = path.resolve(
    process.cwd(),
    "public",
    "results",
    folderName,
    `${lowerVendor}_${lowerSize}`,
  );
  const outputDir = path.resolve(
    process.cwd(),
    "public",
    "results",
    "done",
    folderName,
    `${lowerVendor}_${lowerSize}`,
  );

  await ensureDir(outputDir);

  const entries = await fs.readdir(sourceDir, { withFileTypes: true });
  const jsonFiles = entries
    .filter((e) => e.isFile())
    .map((e) => e.name)
    .filter((n) => n.endsWith(".json") && !n.startsWith("_"));

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
      const goldJson = raw.goldJson;
      const predJson = raw.predJson;

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
      const evalRes = evaluateFlatDone(flatGold, flatPred);

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
  };

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
