# 복수정답 인정 Layer + Offline 재채점 — 설계

작성일: 2026-06-03
대상 repo: `theseus-evaluation`

## 1. 배경 / 목표

LLM의 text→JSON 추출 결과를 gold standard와 비교 채점하는데, 현재는 **완전일치(exact-match) fact 비교**다. 의미상 동등하거나 임상적으로 허용 가능한 값까지 오답 처리되는 문제가 있어, 허용 가능한 복수정답을 **한 곳에서 관리하는 layer**를 만든다. 동시에:

1. **gold standard 수정사항 반영** (이미 `dr-you/main`에 push됨 — `7f9dde0 gold standard 수정`, merge 완료)
2. **`minDaysAtRisk`를 채점에서 제외** (현재 포함되어 있음 — `evaluate.ts:73`, `:160`)
3. 위 변경들을 **LLM 재실행 없이** 기존 채점결과(`predJson` 보존됨)에 offline 재채점으로 반영

비목표(YAGNI): LLM 재호출, 새 gold case 추가, `evaluatePrimary.ts`/`flattenPrimary.ts`(레거시) 경로 변경, `done/` 파생 산출물 재생성 로직.

## 2. 현재 채점 구조 (확인된 사실)

- 채점 대상 4섹션만: `studyPeriods` / `timeAtRisks` / `psSettings` / `outcomeModels` (`evaluate.ts:53`).
- 각 항목을 canonical fact 문자열로 만들어 Set 교집합/차집합으로 Jaccard·precision·recall + 섹션별 정확도 계산.
- **이미 하드코딩된 동등 규칙 3개** (`evaluate.ts:120-135`):
  - `riskWindowStart`: 0 ≡ 1 → 1로 정규화
  - `riskWindowEnd`: 9999 ≡ 99999 → 99999로 정규화
  - `maxRatio`: 0 ≡ 100 → 0으로 정규화 (대칭)
- `minDaysAtRisk`는 TAR fact의 일부로 **현재 채점에 포함됨** → 완전일치라 이 값만 달라도 해당 TAR 전체가 오답.
- 결과 저장 구조 (`public/results/<ohdsi|non-ohdsi>/<type>/<vendor>_<size>/`):
  - per-case `<casename>.json` + `_summary.index.json`
  - 각 case에 `name`, `fileName`, `goldJson`, `predJson` 보존 → **offline 재채점 가능**
  - 단, `reScore.ts`는 저장된(=과거) `goldJson`을 그대로 써서 **gold 수정이 반영되지 않음**.
- gold는 `.ts` 파일에서 `loadFile(type)`로 로드, case 이름 = `JSON*`/`TEXT*` export 접미사 (예: `JSONACEiARBMortality` → `ACEiARBMortality`). 결과의 `c.name`과 동일.

## 3. 허용 규칙 표 → 두 종류로 분류

### (A) equivalence (공통, 대칭) — 모든 study 적용
| component | 동등 집합 | canonical | 현재 상태 |
|---|---|---|---|
| `riskWindowStart` | {0, 1} | 1 | 구현됨 |
| `riskWindowEnd` | {9999, 99999, 999999} | 99999 | **999999 신규 추가** |
| `maxRatio` | {0, 100} | 0 | 구현됨 |

gold·pred 양쪽 값을 canonical로 변환 → 대칭이라 gold가 어느 값이든 안전.

### (B) accept (study별, gold 기준 비대칭) — pred를 gold 값으로 snap
키 = `(caseName, section, component, goldValue)` → 허용 pred 집합/범위. **gold가 그 값을 가질 때만 발동**하므로 type(default/primary/...)·TAR 위치 구분이 불필요하다. 값은 숫자뿐 아니라 **문자열/null/빈값**도 지원(studyEndDate 용).

> 출처: 사용자 v2 리스트(2026-06-03) + 이전 표의 Dabigatran 유지(추가/수정 누적). gold 값은 실제 `.ts` 파일에서 교차 확인 완료.

| caseName | section | component | goldValue | 허용 pred | gold 확인 |
|---|---|---|---|---|---|
| DabigatranRivaroxabanAF | timeAtRisks | riskWindowEnd | 630 | 630–651 (양끝 포함) | non-OHDSI default |
| DabigatranRivaroxabanAF | timeAtRisks | riskWindowEnd | 420 | 420–434 | 〃 |
| DabigatranRivaroxabanAF | timeAtRisks | riskWindowEnd | 210 | 210–217 | 〃 |
| ACEiARBMortality | timeAtRisks | riskWindowEnd | 1825 | {1826} | non-OHDSI |
| TicagrelorClopidogrel | timeAtRisks | riskWindowStart | 29 | {28} | OHDSI default·method TAR 4–6 = 29 ✓ |
| AntiVEGFKidneyAug2 | timeAtRisks | riskWindowStart | 31 | {30} | OHDSI primary_augmented ✓ |
| CORAZONAug2 | timeAtRisks | riskWindowStart | 31 | {30} | ✓ |
| COVID19PPIandH2RAAug2 | timeAtRisks | riskWindowStart | 31 | {30} | ✓ |
| SemaglutideandNAIONAug2 | timeAtRisks | riskWindowStart | 31 | {30} | ✓ |
| TramadolCodeinAug2 | timeAtRisks | riskWindowStart | 31 | {30} | ✓ |
| TicagrelorClopidogrelAug2 | studyPeriods | studyEndDate | "" (빈값) | {null, "", "20190331"} | OHDSI: start=20111101, end="" ✓ |

> 설계상 안전장치: 규칙은 (caseName, goldValue) 동시 게이트라, gold가 해당 값을 안 가지면 발동하지 않음. caseName은 `c.name`(canonical) **lowercase 정규화** 후 비교. Aug2 case 이름은 export 접미사 그대로(`AntiVEGFKidneyAug2` 등).

## 4. 컴포넌트 설계

### 4.1 `acceptableAnswers.ts` (신규, single source of truth)
```ts
// 공통 동등 클래스
export const EQUIVALENCE: Record<string, number[][]> = {
  riskWindowStart: [[0, 1]],
  riskWindowEnd:   [[9999, 99999, 999999]],
  maxRatio:        [[0, 100]],
};
// 각 클래스의 canonical = 배열 첫 원소(현행 동작 보존: start→1, end→99999, maxRatio→0)

// study별 허용 (gold 기준 snap). 값은 number | string | null 지원.
type Scalar = number | string | null;
export type AcceptRule =
  | { kind: "set"; values: Scalar[] }
  | { kind: "range"; min: number; max: number };   // range는 숫자 전용
export const ACCEPT: Array<{
  caseName: string;          // lowercase 정규화 후 비교 (c.name 기준)
  section: "timeAtRisks" | "studyPeriods";
  component: "riskWindowStart" | "riskWindowEnd" | "studyEndDate";
  goldValue: Scalar;         // 빈값 ""은 normVal과 동일 규칙으로 비교
  rule: AcceptRule;
}> = [ /* 표 (B) */ ];

export const canonEquivalence = (component: string, v: number|null) => number|null;
// EQUIVALENCE에 속하면 canonical 반환, 아니면 원값. (riskWindowStart/End/maxRatio)

export const snapPredScalar = (
  caseName: string, section: string, component: string,
  goldValues: Scalar[], predV: Scalar
) => Scalar;
// gold가 가진 값 중 (caseName,section,component,goldValue) accept 규칙에 predV가
// 매칭되면 그 goldValue로 snap. (range는 숫자 predV만, set은 ===/빈값 동등 비교)
```

### 4.2 `evaluate.ts` 수정 (최소)
- `normalizeRiskWindowStart` / `normalizeRiskWindowEnd` / `normalizeMaxRatio` → `canonEquivalence(...)` 호출로 교체. (riskWindowEnd에 999999 자동 포함)
- TAR canonicalizer **2곳**(`:73`, `:160`)에서 `minDaysAtRisk` 줄 **삭제**.
- `evaluateFlat(A, B, caseName?)` 시그니처에 `caseName` 추가(옵셔널, 미전달 시 accept 규칙 미적용 → 하위호환).
- accept 적용: fact화 직전에 pred 값을 snap (gold는 snap하지 않음 — 비대칭).
  - `timeAtRisks`: gold TAR들의 riskWindowStart/End 값 목록으로 `snapPredScalar`를 적용해 pred riskWindowStart/End를 snap (`toFacts`·`canonTimeAtRisks` 양쪽).
  - `studyPeriods`: gold period들의 studyEndDate 값 목록으로 pred studyEndDate를 snap (`toFacts`·`canonStudyPeriods` 양쪽). 단 `isEmptyStudyPeriod`(start·end 모두 빈값) 판정은 snap **이후** 값으로 하지 않고 **원본 기준 유지** — TicagrelorClopidogrelAug2는 start가 present라 빈 기간이 아니므로 영향 없음.

### 4.3 `reScoreFresh.ts` (신규, LLM 없음) — `reScore.ts` 대체/확장
- 대상: `public/results/{ohdsi,non-ohdsi}/{default,primary,primary_augmented,method,pdf}/<vendor>_<size>/_summary.index.json` 중 존재하는 것 전부 (missing skip). vendors=openai/gemini/claude/deepseek, sizes=flagship/light.
- type→gold 폴더 매핑: ohdsi→`public/goldStandard/<type>`, non-ohdsi→`public/goldStandardTest/<type>`.
- **fresh gold 로드**: 각 결과 dir의 type에 맞춰 `loadFile`(또는 동등 로직)로 현재 `.ts` gold를 읽어 `name→goldJson` 맵 구성.
- 각 case: `freshGold = map[c.name]` (없으면 경고+기존 goldJson fallback), `flattenStudy(freshGold)` + `flattenStudy(c.predJson)` → `evaluateFlat(flatGold, flatPred, c.name)`.
- per-case `.json`의 `goldJson`도 fresh로 갱신, metrics/counts/details/sectionAccuracy/sectionCounts 재기록. `_summary.index.json` 재집계 (`reScore.ts`와 동일 집계식).
- `DRY_RUN=1` 지원: before→after 요약만 출력, 파일 미기록.
- 콘솔에 dir별 `SP x/y→x'/y'`, `P`, `S` 변화 출력 (기존 포맷 유지).

## 5. 데이터 흐름
```
git merge dr-you/main (완료) → .ts gold = 최신
        ↓ (reScoreFresh.ts)
loadFile(type) → fresh goldJson  ┐
저장된 predJson ─────────────────┤→ snap(accept) → flatten → evaluateFlat(caseName)
                                 ┘        (minDaysAtRisk 제외, equivalence 적용)
        ↓
per-case .json + _summary.index.json 갱신 (LLM 호출 0)
```

## 6. 테스트 전략
- `acceptableAnswers.test`: equivalence canonical(999999 포함), range/set snap, 비매칭 시 원값 유지, 미등록 study no-op.
- `evaluate` 회귀: 기존 동등규칙(0≡1, 9999≡99999, 100≡0) 동작 보존; `minDaysAtRisk`만 다른 TAR가 이제 정답 처리; accept 케이스(Dabigatran 645→630 등)가 정답 처리.
- `reScoreFresh` DRY_RUN: 수정된 gold 케이스(DPP4i studyEndDate, DOACs aug1 riskWindowEnd 30, IVIron studyStartDate)의 점수 변화가 기대대로인지 1개 dir로 확인 후 전체 적용.
- 안전: `DRY_RUN`으로 before/after diff 먼저 확인, 그 다음 실기록.

## 7. 영향 받는 파일
- 신규: `acceptableAnswers.ts`, `reScoreFresh.ts`, 테스트 파일.
- 수정: `evaluate.ts` (normalize 교체, minDaysAtRisk 제거, caseName 인자).
- 데이터: `public/results/**/_summary.index.json` 및 per-case `.json` 재기록.
- `reScore.ts`: 보존(레거시) 또는 `reScoreFresh.ts`로 대체 — 구현 시 결정.

## 8. 확정된 결정 / 구현 시 확인
- **확정**: Ticagrelor riskWindowStart=29 = OHDSI `TicagrelorClopidogrel` default·method TAR 4–6 (gold 확인 완료). Dabigatran 범위 규칙 **유지**. 공통 0≡1·0≡100 **유지**(같은 config에서 관리). range 경계 **양끝 포함**.
- **구현 시 확인**: `done/` dir 재채점 포함 여부(기본 제외). studyEndDate snap 시 period 매칭 방식(Aug2는 단일 period로 보이나 다중 대비 studyStartDate로 매칭 권장).
