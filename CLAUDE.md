# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an evaluation framework for text-to-JSON extraction from clinical study protocols. It benchmarks multiple LLM vendors (OpenAI, Gemini, Claude, DeepSeek) on their ability to convert clinical study text into structured OMOP-CDM analysis specifications.

## Commands

### Setup
```bash
pnpm install
```

### Run Batch Evaluation
```bash
pnpm dlx tsx batchEvaluate.ts --vendor=<OPENAI|GEMINI|CLAUDE|DEEPSEEK> --size=<FLAGSHIP|LIGHT> --type=<DEFAULT|PRIMARY|METHOD|PDF>
```

### Use Test Dataset
```bash
GOLD_STANDARD_DIR=public/goldStandardTest pnpm dlx tsx batchEvaluate.ts --vendor=OPENAI --size=FLAGSHIP --type=DEFAULT
```

### Re-evaluate Results (Field-Level Accuracy)
```bash
npx tsx evaluateDoneJson.ts --vendor=<OPENAI|GEMINI|CLAUDE|DEEPSEEK> --size=<FLAGSHIP|LIGHT> --type=<DEFAULT|PRIMARY|METHOD>
```

## Architecture

### Core Pipeline
1. **loadFile.ts** - Loads gold standard files (TEXT*/JSON* exports) from `public/goldStandard/<type>/`
2. **text2json.ts / text2jsonPDF.ts / text2jsonPRIMARY.ts** - LLM API wrappers that convert study text to structured JSON
3. **flatten.ts / flattenPrimary.ts** - Flatten nested StudyDTO objects for comparison
4. **evaluate.ts / evaluatePrimary.ts** - Compare flattened gold vs predicted JSON, compute Jaccard/precision/recall
5. **batchEvaluate.ts** - Orchestrates the full pipeline across all test cases

### Data Flow
```
Gold Standard Files (TEXT* + JSON*)
        ↓
    loadFile.ts
        ↓
  text2json*.ts (LLM call)
        ↓
    flatten*.ts
        ↓
    evaluate*.ts
        ↓
  Results JSON (public/results/<type>/<vendor>_<size>/)
```

### Key Types
- **StudyDTO** (flatten.ts) - Full nested structure for DEFAULT/METHOD/PDF types
- **StudyDTOPRIMARY** (flattenPrimary.ts) - Simplified structure for PRIMARY type
- **FlattenStudyDTO** - Flattened version used for evaluation comparison

### Evaluation Metrics
- **Jaccard index** - Intersection over union of field-value facts
- **Precision** - Correct predictions / total predictions
- **Recall** - Correct predictions / total gold standard fields
- **Section accuracy** - Per-section exact match (studyPeriods, timeAtRisks, propensityScoreAdjustment)

## Directory Structure

- `public/goldStandard/<type>/` - Gold standard test cases (default, primary, method, pdf)
- `public/goldStandardTest/` - Alternative smaller test dataset
- `public/results/<type>/<vendor>_<size>/` - Raw evaluation outputs
- `public/results/done/<type>/` - Post-processed results
- `public/templates/` - JSON schema templates for LLM prompts

## Environment Variables

Create `.env` with API keys:
- `OPENAI_API_KEY`
- `CLAUDE_API_KEY` (for Anthropic API)
- `GOOGLE_API_KEY` (for Gemini)
- `DEEPSEEK_API_KEY`
