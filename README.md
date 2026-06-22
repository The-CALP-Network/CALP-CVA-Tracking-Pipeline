# CALP-CVA-Tracking-Pipeline
Data pipeline for Cash and Voucher Assistance identification from humanitarian flows.

For the full methodology behind each step — what each data source is, why each classification rule exists, and known limitations — see [GUIDE.md](GUIDE.md). This README covers installation and how to run the pipeline.

## Installation (R)

1. Download and install a version of R for your operating system: https://cran.r-project.org/
2. Download and install RStudio Open Source: https://www.rstudio.com/products/rstudio/download/
3. (Optional for Windows) Download and install the correct Rtools version: https://cran.r-project.org/bin/windows/Rtools/
4. Download and install either git-scm https://git-scm.com/downloads, or GitHub Desktop https://desktop.github.com/
5. Clone this repository to your hard drive.

## Installation (Python)

Used only for the machine-learning classification step (`classifier_code/`).

1. Download and install Python (3.12 or higher), ensuring to add Python and pip to your PATH environment variables: https://www.python.org/downloads/

```
pip install virtualenv
cd CALP-CVA-Tracking-Pipeline/classifier_code
python3 -m virtualenv venv
```

**macOS/Linux:**
```
source venv/bin/activate
pip install -r requirements.txt
```

**Windows (PowerShell):**
```
venv\Scripts\activate.ps1
pip install -r requirements.txt
```

A pre-trained classifier (`cva-flow-weighted-classifier2/best_model/`) is loaded from disk by `flow_inference.py` — no Hugging Face account or internet access is required to run inference. Retraining the model (`train_flow_classifier_weighted.py`) is only needed if you are expanding the training data; see [classifier_code/README.md](classifier_code/README.md).

## Running

The pipeline runs as a numbered sequence of R scripts, with one pause partway through step 8 to run the Python classifier. All R scripts are designed to be run with the project root as the working directory (`Rscript` from the repo root, or `source("code/util/utils.R"); enforce_project_root()` first if running interactively).

```
Rscript code/05_fts_curated_master.R
Rscript code/06_fetch_projects.R
Rscript code/07_process_project_data.R
Rscript code/08a_fts_prepare_for_inference.R

# Pause here to run the ML classifier
cd classifier_code
source venv/bin/activate          # or venv\Scripts\activate.ps1 on Windows
python3 flow_inference.py
deactivate
cd ..

Rscript code/08b_fts_combine_inference.R
Rscript code/09_calculate_cva.R
Rscript code/10_global_cva_analysis.R
```

Steps 09 and 10 also depend on manual review files (`output/cva_to_manually_classify.csv` and `reference_datasets/historical_cva_decisions.csv`) — see the "Manual review" section below before running them for the first time on a new year's data.

### Manual review

After step `08b`, some flows remain ambiguous even with keyword matching, project metadata, and the ML classifier. `09_calculate_cva.R` writes these to `output/cva_to_manually_classify.csv`, excluding any IDs already present in `reference_datasets/historical_cva_decisions.csv` (the accumulated record of past manual decisions).

To complete a round of manual review:

1. Open `output/cva_to_manually_classify.csv` and assess each flow's `all_text` field for whether it fully, partially, or does not support CVA.
2. Save your decisions as `output/cva_manually_classified.csv`, including at minimum the `id`, `CVAamount`, and `CVAamount_type` columns for every flow you've classified.
3. Run `Rscript code/10_global_cva_analysis.R`. This script validates the file (checks required columns are present and `CVAamount` values are positive and finite, and warns if any IDs don't match the current queue), incorporates the decisions into the CVA totals, and removes the now-resolved rows from `output/cva_to_manually_classify.csv` so the file only ever contains genuinely pending items.

Confirmed positive decisions are also automatically appended to `classifier_code/CVA_flow_descriptions.csv` as new training examples, so each year's manual review round improves the next year's classifier.

## File purposes and descriptions

### code/util/utils.R

Shared helper functions sourced by every script in the pipeline.

- `enforce_project_root()`: ensures the working directory is the repository root regardless of whether the script is run via `Rscript` from a different directory or interactively in RStudio.
- `load_packages(...)`: installs (if missing) and silently loads the given CRAN packages.

### code/util/util_fts_get_flows.R

Accesses the Financial Tracking Service (FTS) API to retrieve flow data, paginating through all result pages and optionally unnesting the nested `sourceObjects`/`destinationObjects` columns into flat fields.

**Inputs:** `year`, `planid`, `emergencyid`, `globalclusterid`, `destinationlocationid` filters (all optional); `unnest` (default `TRUE`).
**Outputs:** A data.table of flows, one row per flow (or per source/destination object combination if not unnested).

### code/util/util_fts_split_rows.R

Splits a row into multiple rows by a delimited column (e.g. usage years recorded as `"2023; 2024"`), dividing the specified value column(s) evenly across the resulting rows.

**Inputs:** `data`, `value.cols` (default `"amountUSD"`), `split.col`, `split.pattern` (default `"; "`), `remove.unsplit`.
**Outputs:** A data.table with one row per split value.

### code/util/util_deflators.R

Calculates GDP deflators by country and year from the IMF World Economic Outlook (WEO) database, supplemented with OECD DAC deflator data where available (which is preferred over WEO-derived figures for DAC donors and the DAC aggregate). Includes manual ISO-code patching for territories that share a parent country's currency/economy (e.g. UK overseas territories use the GBR deflator), and extrapolation of missing years using each country's average historical growth rate. Output is saved to `deflators_2024USD.csv` and read directly by `util_fts_curated_flows.R` rather than recomputed on every pipeline run.

**Note:** this script reads a local OECD DAC deflator file (`reference_datasets/Deflators-base-2024.xlsx`) and an external DAC Table 1/2a CSV path that is currently hardcoded to a personal Google Drive location — this needs to be updated to a shared/repository path before this script can be re-run by anyone other than the original author. See "Known issues" below.

### code/util/util_exchange_rates.R

Retrieves USD exchange rates by country and year from OECD, World Bank, and IMF IFS sources (in that priority order, each only filling gaps left by the previous), and writes the combined dataset to `reference_datasets/usd_exchange_rates.csv`. Used for converting non-USD survey submissions in step 10.

### code/util/util_fts_curated_flows.R

The core FTS curation function. Downloads (or reads cached) raw flow data per year, then applies a substantial set of cleaning and enrichment steps:

- Removes outgoing-boundary flows and de-duplicates flows that appear on both sides of an organisational boundary (preferring `incoming` over `internal`).
- Splits multi-year flows evenly across their usage years.
- Assigns a destination country ISO3 code, including a `Multi-destination_org_country` / `MULTI` code for multi-country flows.
- Determines each flow's source organisation country and applies the corresponding GDP deflator (falling back to the DAC aggregate deflator for non-government or unmatched source organisations), producing `amountUSD_defl` and `amountUSD_defl_millions` columns.
- Classifies source and destination organisations into standardised channel types (NGOs and CSOs, UN Multi, Public Sector, RCRC, Other, etc.) using a combination of a manually maintained Development Initiatives (DI) coding reference and FTS's own organisation type fields as a fallback.
- Maps source and destination clusters to standardised global clusters using a DI cluster-mapping reference, falling back to FTS's own global cluster field, and flags flows reported against multiple clusters as `"Multiple clusters specified"`.
- Flags domestic response (government funding within its own country), new-to-country, new-to-plan, new-to-sector, and COVID-related flows.
- Adds dummy reverse flows to cancel out the netting effect of intra-country transfers when aggregating by country.
- Excludes a manually maintained list of flow IDs identified by LLM-assisted review as erroneous or non-financial.
- Restricts the final output to `paid` and `commitment` status flows within the requested year range.

**Inputs:** `years`, `update_years`, `dataset_path`, `deflators_path`, `base_year`, `weo_ver`, `dummy_intra_country_flows`.
**Outputs:** A curated data.table covering all requested years.

### code/05_fts_curated_master.R

Orchestrates `util_fts_curated_flows.R` across all configured years and writes one CSV per year to `fts/fts_curated_<year>.csv`.

### code/06_fetch_projects.R

For every unique project ID referenced in the curated FTS flows, fetches the project's metadata and Q&A field responses from the HPC Projects API, and saves one RData file per year to `projects/project_data_<year>.RData`. Skips years that are already cached.

### code/07_process_project_data.R

Processes the raw project Q&A data into per-project CVA indicators:

- Searches all unique question strings for CVA-related keywords (in English, French, and Spanish) and flags any that aren't already present in `reference_datasets/cva_project_questions.csv` for manual review (written to `output/potential_new_cash_questions.csv`).
- Extracts a standardised `cva_percentage` from questions labelled `quantC`/`quantV` (cash/voucher budget share questions), summing cash and voucher percentages per project and capping the total at 100%.
- Extracts a boolean `cva` flag from questions labelled `flagCVA`.
- Reconciles the two: a 0% quantitative answer implies a `FALSE` boolean flag (and vice versa) where the other source is missing.

**Outputs:** `output/questions.csv`, `output/potential_new_cash_questions.csv` (if any), `projects/cash_projects.csv`, `projects/project_text.csv`.

### code/08a_fts_prepare_for_inference.R

Part 1 of the CVA relevance classification step — run before the ML inference step. Joins project CVA metadata onto the curated FTS flows, then classifies each flow's CVA relevance:

1. **Sector/method/cluster**: `Full` if reported as `Cash transfer programming (CTP)` or assigned to a single recognised cash cluster; `Partial` if assigned to a cash cluster alongside other clusters. CTP classification is applied last so a flow explicitly reported as CTP cannot be downgraded to `Partial` by the multi-cluster rule.
2. **Project CVA percentage**: applied only to flows not already classified in step 1, so the `relevance` column never diverges from how `CVAamount` will actually be calculated downstream. `Full` if ≥75%, `Partial` if between 0% and 75%.
3. **Keyword match**: flags flows containing CVA-related keywords (English/French/Spanish terms and acronyms) in their combined flow description + project text (`all_text`) for ML candidacy.

Flows still unclassified (`relevance == "None"`) that have either a keyword match or a project-level CVA flag are written to `classifier_code/fts_to_inference.csv` for the ML step. The full flagged-and-unflagged dataset is saved to `output/fts_output_pre_ml.csv`.

### classifier_code/flow_inference.py

Loads the locally saved fine-tuned classifier (`cva-flow-weighted-classifier2/best_model/`) and predicts, for each flow in `fts_to_inference.csv`, whether its text describes `Full` or `Partial` CVA relevance, along with a confidence score (the model's predicted probability for the `Full` class specifically — so a low score indicates the model favours `Partial`, not that it's uncertain about CVA relevance generally).

**Inputs:** `fts_to_inference.csv`. **Outputs:** `fts_to_inference_output.csv` (adds `predicted_class`, `predicted_confidence`).

### code/08b_fts_combine_inference.R

Part 2 of the CVA relevance classification step — run after the ML inference step. Merges the ML predictions back onto the pre-ML dataset and updates `relevance` for any flow that was a genuine ML candidate (keyword match or project CVA flag, still unclassified, with a valid prediction). Writes the full set of flagged (non-`"None"`) flows to `output/fts_output_CVA.csv`.

### classifier_code/train_flow_classifier_weighted.py

Fine-tunes a BERT-based classifier (`alex-miller/ODABert`) on `CVA_flow_descriptions.csv` to distinguish `Full` from `Partial` CVA relevance, using a class-weighted loss to handle label imbalance. Runs entirely locally (no Hugging Face Hub push); the best checkpoint (by evaluation loss, with early stopping) is saved to `cva-flow-weighted-classifier2/best_model/` for `flow_inference.py` to load. See [classifier_code/README.md](classifier_code/README.md) for hardware notes and hyperparameter rationale.

### code/09_calculate_cva.R

Calculates the estimated CVA USD amount for each flagged flow, applying methods in priority order until a flow receives a non-zero amount:

1. **Sector/method/cluster — Full**: the entire flow amount.
2. **Sector/method/cluster — Partial**: the flow amount divided by the number of destination clusters reported.
3. **Project CVA percentage**: the flow amount multiplied by the project's reported CVA budget share.
4. **High-confidence ML — Full**: the entire flow amount, if `predicted_confidence ≥ 0.8` and the text contains a common CVA keyword (cash/voucher/CVA/coupon).
5. **High-confidence ML — Partial**: the flow amount multiplied by the empirical average Partial-to-total ratio observed elsewhere in the dataset, if `predicted_confidence ≤ 0.2` and the text contains a common CVA keyword.
6. **Prior manual decisions**: the entire flow amount, for flows previously marked `"Decision: accept; judgement"` or `"Decision: include; judgement"` in `reference_datasets/historical_cva_decisions.csv`. Confirmed positives are also appended to the classifier's training data.
7. **New manual queue**: remaining flows with mid-range ML confidence (strictly between 0.2 and 0.8, without a common-keyword match) that haven't already been decided are written to `output/cva_to_manually_classify.csv` for the next manual review round.

**Outputs:** `output/cva_to_manually_classify.csv`, `output/fts_cva.csv` (flows with `CVAamount > 0`, before incorporation of any newly completed manual review — see step 10).

### code/10_global_cva_analysis.R

Performs the full global CVA analysis:

1. **Incorporates manual review.** If `output/cva_manually_classified.csv` exists, validates it (required columns present, `CVAamount` positive and finite, warns on IDs not found in the current queue), merges it into the FTS CVA dataset, and removes the now-resolved IDs from `output/cva_to_manually_classify.csv`.
2. **Combines FTS and survey data.** Aggregates FTS CVA amounts by destination organisation, organisation sub-type, and year; excludes organisation-years already covered by direct CALP Network survey submissions (via `reference_datasets/fts_survey_overlap.csv`) to avoid double-counting; combines the two sources.
3. **Imputes missing programming-cost (PC) or transfer-value (TV) figures** bidirectionally using the annual PC→TV ratio (third sheet of the survey workbook), for respondents who only reported one of the two.
4. **De-duplicates sub-grants.** Matches sub-grant recipient organisation names against the combined organisation list using a four-strategy cascade (exact match → fuzzy/Levenshtein match → recipient-name-is-substring-of-org-name → org-name-is-substring-of-recipient-name), plus a small set of manual overrides for known tricky cases (acronyms, multilingual variants, RCRC national societies). Subtracts matched sub-grant amounts from the recipient's total to avoid counting both the donor's and recipient's report of the same transfer.
5. **Aggregates by organisation type and locality** (`Org_type` × `Local_type`, where `Local_type` distinguishes national/local organisations from international ones based on FTS sub-type) and by year.

**Outputs:** `output/cva_agg.csv` (by organisation), `output/cva_agg_org_type.csv` (by organisation type, locality, and year).