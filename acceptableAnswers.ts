// acceptableAnswers.ts
// 단일 소스: 채점 허용 규칙(복수정답)을 한 곳에서 관리.
//  (A) equivalence: 대칭 동등 클래스 — gold·pred 양쪽에 적용 (canonical = 배열 첫 원소)
//  (B) accept: study별 gold-기준 비대칭 허용 — pred를 gold 값으로 snap (다음 태스크에서 추가)

export type Scalar = number | string | null;

// ---- (A) 공통 equivalence (대칭) ----
const EQUIVALENCE: Record<string, Scalar[][]> = {
  riskWindowStart: [[1, 0]],
  riskWindowEnd: [[99999, 9999, 999999]],
  maxRatio: [[0, 100]],
};

export const canonEquivalence = (component: string, v: Scalar): Scalar => {
  if (v === null || v === undefined) return null;
  const classes = EQUIVALENCE[component];
  if (!classes) return v;
  for (const cls of classes) {
    if (cls.includes(v)) return cls[0]; // canonical = 첫 원소
  }
  return v;
};
