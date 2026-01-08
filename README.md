# theseus-evaluation

Text2json evaluation runner and results repo.

## Setup
- Install deps: `pnpm install`
- Create `.env` with API keys as needed:
  - `OPENAI_API_KEY`
  - `CLAUDE_API_KEY`
  - `GOOGLE_API_KEY`
  - `DEEPSEEK_API_KEY`

## Key paths
- Gold standard + input: `public/goldStandard` (alt dataset: `public/goldStandardTest`)
- API wiring: `text2json.ts` (also `text2jsonPDF.ts`, `text2jsonPRIMARY.ts`)
- Results output: `public/results` (test dataset: `public/results/test`, post-processed in `public/results/done` or `public/results/done/test`)

## Gold standard selection
Default uses `public/goldStandard`. To run against `public/goldStandardTest`, prefix commands with `GOLD_STANDARD_DIR=public/goldStandardTest`.
When `GOLD_STANDARD_DIR=public/goldStandardTest`, results are written under `public/results/test/<type>` and post-processed under `public/results/done/test/<type>`.

Example:
```
GOLD_STANDARD_DIR=public/goldStandardTest pnpm dlx tsx batchEvaluate.ts --vendor=OPENAI --size=FLAGSHIP --type=DEFAULT
```

## Batch evaluation (text2json)
Command template:
```
pnpm dlx tsx batchEvaluate.ts --vendor=<OPENAI|GEMINI|CLAUDE|DEEPSEEK> --size=<FLAGSHIP|LIGHT> --type=<DEFAULT|PRIMARY|METHOD|PDF>
```

### METHOD
```
pnpm dlx tsx batchEvaluate.ts --vendor=OPENAI --size=FLAGSHIP --type=METHOD
pnpm dlx tsx batchEvaluate.ts --vendor=OPENAI --size=LIGHT --type=METHOD
pnpm dlx tsx batchEvaluate.ts --vendor=GEMINI --size=FLAGSHIP --type=METHOD
pnpm dlx tsx batchEvaluate.ts --vendor=GEMINI --size=LIGHT --type=METHOD
pnpm dlx tsx batchEvaluate.ts --vendor=CLAUDE --size=FLAGSHIP --type=METHOD
pnpm dlx tsx batchEvaluate.ts --vendor=CLAUDE --size=LIGHT --type=METHOD
pnpm dlx tsx batchEvaluate.ts --vendor=DEEPSEEK --size=FLAGSHIP --type=METHOD
pnpm dlx tsx batchEvaluate.ts --vendor=DEEPSEEK --size=LIGHT --type=METHOD
```

### PRIMARY
```
pnpm dlx tsx batchEvaluate.ts --vendor=OPENAI --size=FLAGSHIP --type=PRIMARY
pnpm dlx tsx batchEvaluate.ts --vendor=OPENAI --size=LIGHT --type=PRIMARY
pnpm dlx tsx batchEvaluate.ts --vendor=GEMINI --size=FLAGSHIP --type=PRIMARY
pnpm dlx tsx batchEvaluate.ts --vendor=GEMINI --size=LIGHT --type=PRIMARY
pnpm dlx tsx batchEvaluate.ts --vendor=CLAUDE --size=FLAGSHIP --type=PRIMARY
pnpm dlx tsx batchEvaluate.ts --vendor=CLAUDE --size=LIGHT --type=PRIMARY
pnpm dlx tsx batchEvaluate.ts --vendor=DEEPSEEK --size=FLAGSHIP --type=PRIMARY
pnpm dlx tsx batchEvaluate.ts --vendor=DEEPSEEK --size=LIGHT --type=PRIMARY
```

### PDF
```
pnpm dlx tsx batchEvaluate.ts --vendor=OPENAI --size=FLAGSHIP --type=PDF
pnpm dlx tsx batchEvaluate.ts --vendor=OPENAI --size=LIGHT --type=PDF
pnpm dlx tsx batchEvaluate.ts --vendor=GEMINI --size=FLAGSHIP --type=PDF
pnpm dlx tsx batchEvaluate.ts --vendor=GEMINI --size=LIGHT --type=PDF
pnpm dlx tsx batchEvaluate.ts --vendor=CLAUDE --size=FLAGSHIP --type=PDF
pnpm dlx tsx batchEvaluate.ts --vendor=CLAUDE --size=LIGHT --type=PDF
pnpm dlx tsx batchEvaluate.ts --vendor=DEEPSEEK --size=FLAGSHIP --type=PDF
pnpm dlx tsx batchEvaluate.ts --vendor=DEEPSEEK --size=LIGHT --type=PDF
```

### DEFAULT
```
pnpm dlx tsx batchEvaluate.ts --vendor=OPENAI --size=FLAGSHIP --type=DEFAULT
pnpm dlx tsx batchEvaluate.ts --vendor=OPENAI --size=LIGHT --type=DEFAULT
pnpm dlx tsx batchEvaluate.ts --vendor=GEMINI --size=FLAGSHIP --type=DEFAULT
pnpm dlx tsx batchEvaluate.ts --vendor=GEMINI --size=LIGHT --type=DEFAULT
pnpm dlx tsx batchEvaluate.ts --vendor=CLAUDE --size=FLAGSHIP --type=DEFAULT
pnpm dlx tsx batchEvaluate.ts --vendor=CLAUDE --size=LIGHT --type=DEFAULT
pnpm dlx tsx batchEvaluate.ts --vendor=DEEPSEEK --size=FLAGSHIP --type=DEFAULT
pnpm dlx tsx batchEvaluate.ts --vendor=DEEPSEEK --size=LIGHT --type=DEFAULT
```

## Re-generate results (field-level accuracy fix)
Command template:
```
npx tsx evaluateDoneJson.ts --vendor=<OPENAI|GEMINI|CLAUDE|DEEPSEEK> --size=<FLAGSHIP|LIGHT> --type=<DEFAULT|PRIMARY|METHOD>
```

### DEFAULT
```
npx tsx evaluateDoneJson.ts --vendor=OPENAI --size=FLAGSHIP --type=DEFAULT
npx tsx evaluateDoneJson.ts --vendor=OPENAI --size=LIGHT --type=DEFAULT
npx tsx evaluateDoneJson.ts --vendor=GEMINI --size=FLAGSHIP --type=DEFAULT
npx tsx evaluateDoneJson.ts --vendor=GEMINI --size=LIGHT --type=DEFAULT
npx tsx evaluateDoneJson.ts --vendor=CLAUDE --size=FLAGSHIP --type=DEFAULT
npx tsx evaluateDoneJson.ts --vendor=CLAUDE --size=LIGHT --type=DEFAULT
npx tsx evaluateDoneJson.ts --vendor=DEEPSEEK --size=FLAGSHIP --type=DEFAULT
npx tsx evaluateDoneJson.ts --vendor=DEEPSEEK --size=LIGHT --type=DEFAULT
```

### PRIMARY
```
npx tsx evaluateDoneJson.ts --vendor=OPENAI --size=FLAGSHIP --type=PRIMARY
npx tsx evaluateDoneJson.ts --vendor=OPENAI --size=LIGHT --type=PRIMARY
npx tsx evaluateDoneJson.ts --vendor=GEMINI --size=FLAGSHIP --type=PRIMARY
npx tsx evaluateDoneJson.ts --vendor=GEMINI --size=LIGHT --type=PRIMARY
npx tsx evaluateDoneJson.ts --vendor=CLAUDE --size=FLAGSHIP --type=PRIMARY
npx tsx evaluateDoneJson.ts --vendor=CLAUDE --size=LIGHT --type=PRIMARY
npx tsx evaluateDoneJson.ts --vendor=DEEPSEEK --size=FLAGSHIP --type=PRIMARY
npx tsx evaluateDoneJson.ts --vendor=DEEPSEEK --size=LIGHT --type=PRIMARY
```

### METHOD
```
npx tsx evaluateDoneJson.ts --vendor=OPENAI --size=FLAGSHIP --type=METHOD
npx tsx evaluateDoneJson.ts --vendor=OPENAI --size=LIGHT --type=METHOD
npx tsx evaluateDoneJson.ts --vendor=GEMINI --size=FLAGSHIP --type=METHOD
npx tsx evaluateDoneJson.ts --vendor=GEMINI --size=LIGHT --type=METHOD
npx tsx evaluateDoneJson.ts --vendor=CLAUDE --size=FLAGSHIP --type=METHOD
npx tsx evaluateDoneJson.ts --vendor=CLAUDE --size=LIGHT --type=METHOD
npx tsx evaluateDoneJson.ts --vendor=DEEPSEEK --size=FLAGSHIP --type=METHOD
npx tsx evaluateDoneJson.ts --vendor=DEEPSEEK --size=LIGHT --type=METHOD
```
