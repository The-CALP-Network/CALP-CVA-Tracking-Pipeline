# CALP-CVA-Tracking-Pipeline
Data pipeline for Cash and Voucher Assistance identification from humanitarian flows

## Installation (R)

1. Download and install a version of R for your operating system: https://cran.r-project.org/
2. Download and install RStudio Open Source: https://www.rstudio.com/products/rstudio/download/
3. (Optional for windows) Download and install the correct Rtools version: https://cran.r-project.org/bin/windows/Rtools/
4. Download and install either git-scm https://git-scm.com/downloads, or Github Desktop https://desktop.github.com/
5. Clone this repository to your hard-drive.

## Installation (Python)

1. Download and install Python (3.12 or higher), ensuring to add python and pip to your PATH environmental variables: https://www.python.org/downloads/windows/

```
pip install virtualenv
cd CALP-CVA-Tracking-Pipeline/classifier_code
python3 -m virtualenv venv
source venv/bin/activate
pip install -r requirements.txt
```

## Running
```
Rscript code/05_fts_curated_master.R
Rscript code/06_fetch_projects.R
Rscript code/07_process_project_data.R
Rscript code/08_fts_keyword_searching_cash.R # Pausing halfway to run Python code
cd classifier_code
source venv/bin/activate
python3 flow_inference.py
cd ..
# Finish 08
Rscript code/09_calculate_cva.R
Rscript code/10_global_cva_analysis.R
```

## File purposes and descriptions

### code/01_fts_get_flows.R

This script accesses the Financial Tracking Service (FTS) API to retrieve flows data. The data can be filtered by year, plan ID, emergency ID, global cluster ID, and destination location ID. The script also includes an option to unnest the output, which is recommended for large downloads.

**Inputs:**
- `year`: A vector of years to filter the data.
- `planid`: A vector of plan IDs to filter the data.
- `emergencyid`: A vector of emergency IDs to filter the data.
- `globalclusterid`: A vector of global cluster IDs to filter the data.
- `destinationlocationid`: A vector of destination location IDs to filter the data.
- `unnest`: A boolean indicating whether to unnest the output (default is TRUE).

**Outputs:**
- A data table containing the flows data, optionally unnested.

**Purpose:**
- To retrieve and process flows data from the FTS API.

### code/02_fts_split_rows.R

This script splits rows in the FTS flows data by a chosen column. It is useful for handling cases where a single flow entry needs to be divided into multiple entries based on a specified pattern.

**Inputs:**
- `data`: The data table containing the flows data to be split.
- `value.cols`: The column containing values to be split evenly across the new rows (default is "amountUSD").
- `split.col`: The column to be used for splitting the rows (default is "destinationObjects_Location.name").
- `split.pattern`: The pattern used to split the values in the `split.col` (default is "; ").
- `remove.unsplit`: A boolean indicating whether to remove the original unsplit columns (default is TRUE).

**Outputs:**
- A data table with the rows split according to the specified column and pattern.

**Purpose:**
- To divide flow entries into multiple rows based on a specified column.

### code/03_deflators.R

This script retrieves and calculates GDP deflators for various countries using data from the IMF World Economic Outlook (WEO) database. The deflators are used to adjust financial data for inflation, allowing for comparisons in constant prices.

**Inputs:**
- `base_year`: The base year for the deflators (default is 2021).
- `currency`: The currency for the GDP data, options are "USD" (default), "LCU" (local currency units), or "PPP" (purchasing power parity).
- `weo_ver`: The version of the WEO database to use (default is the latest available).
- `approximate_missing`: A boolean indicating whether to approximate missing deflators (default is TRUE).

**Outputs:**
- A data table containing GDP deflators for various countries and years.

**Purpose:**
- To provide GDP deflators for adjusting financial data to constant prices.

### code/04_fts_curated_flows.R

This script downloads and curates FTS flows data from the FTS API. It processes the data to remove outgoing flows, duplicates, and pledges, and applies various transformations to ensure accurate analysis.

**Inputs:**
- `years`: A vector of years for which to download and process data (default is 2017:2024).
- `update_years`: A vector of years to update even if data already exists (default is NA).
- `dataset_path`: The path to save the downloaded data (default is "fts").
- `base_year`: The base year for deflation (default is 2022).
- `weo_ver`: The version of the WEO database to use for deflators (default is NULL).
- `dummy_intra_country_flows`: A boolean indicating whether to add dummy reverse flows to cancel out intra-country flows (default is TRUE).

**Outputs:**
- A curated data table containing the FTS flows data, with transformations applied for accurate analysis.

**Purpose:**
- To download, process, and curate FTS flows data.

### code/05_fts_curated_master.R

This script orchestrates the downloading, processing, and saving of curated FTS flows data. It uses the `fts_curated_flows` function to retrieve and process the data, and then saves the curated data for each specified year.

**Inputs:**
- `years`: A vector of years for which to download and process data (default is 2017:2024).
- `update_years`: A vector of years to update even if data already exists (default is NA).
- `path`: The path to save the curated data (default is "fts/").

**Outputs:**
- Curated CSV files for each specified year, saved in the specified path.

**Purpose:**
- To automate the process of downloading, processing, and saving curated FTS flows data for multiple years.

### code/06_fetch_projects.R

This script fetches project data from the FTS API for specified years. It retrieves detailed information about each project, including objectives, global clusters, and associated organizations, and saves the data for further analysis.

**Inputs:**
- `years`: A vector of years for which to fetch project data (default is 2017:2024).

**Outputs:**
- RData files containing detailed project data for each specified year, saved in the "projects" directory.

**Purpose:**
- To retrieve and save detailed project data from the FTS API for specified years.

### code/07_process_project_data.R

This script processes the project data fetched from the FTS API. It merges project data from multiple years, identifies projects related to Cash and Voucher Assistance (CVA) based on specific keywords, and standardizes the answers for further analysis.

**Inputs:**
- Project data files for each year, saved in the "projects" directory.

**Outputs:**
- A CSV file containing unique questions related to CVA, saved in the "output" directory.
- A CSV file containing processed project data with CVA-related information, saved in the "projects" directory.
- A CSV file containing project text information, saved in the "projects" directory.

**Purpose:**
- To process and standardize project data.

### code/08_fts_keyword_searching_cash.R

This script identifies Cash and Voucher Assistance (CVA) related flows in the FTS data using keyword searching and machine learning. It processes the data to flag relevant flows based on predefined keywords, project-level CVA information, and machine learning predictions.

**Inputs:**
- Curated FTS flows data files for each year, saved in the "fts" directory.
- Processed project data files, saved in the "projects" directory.
- Keywords and acronyms related to CVA.

**Outputs:**
- A CSV file containing FTS flows flagged as CVA-related, saved in the "output" directory.
- A CSV file containing FTS flows to be inferred by the machine learning model, saved in the "classifier_code" directory.
- A CSV file containing the machine learning model's inference results, saved in the "classifier_code" directory.

**Purpose:**
- To identify and flag CVA-related flows in the FTS data using a combination of keyword searching, project-level information, and machine learning predictions.

### classifier_code/flow_inference.py

This script uses a pre-trained machine learning model to infer the relevance of FTS flows to Cash and Voucher Assistance (CVA). It processes the text descriptions of flows and predicts their relevance based on the model's classification.

**Inputs:**
- A CSV file containing FTS flows to be inferred, saved as `fts_to_inference.csv` in the "classifier_code" directory.

**Outputs:**
- A CSV file containing the inference results, including predicted class and confidence, saved as `fts_to_inference_output.csv` in the "classifier_code" directory.

**Purpose:**
- To use a machine learning model to classify FTS flows as relevant or not relevant to CVA, based on text descriptions.

### classifier_code/train_flow_classifier_weighted.py

This script trains a machine learning model to classify FTS flows as relevant or not relevant to Cash and Voucher Assistance (CVA). It uses a weighted loss function to handle class imbalance and evaluates the model's performance using various metrics.

**Inputs:**
- A CSV file containing labeled FTS flow descriptions, saved as `CVA_flow_descriptions.csv` in the "classifier_code" directory.

**Outputs:**
- A trained machine learning model for classifying FTS flows, saved to the Huggingface Hub.

**Purpose:**
- To train a machine learning model that can classify FTS flows based on text descriptions, with a focus on handling class imbalance using a weighted loss function.

### code/09_calculate_cva.R

This script calculates the Cash and Voucher Assistance (CVA) amounts from the flagged FTS flows. It determines the CVA amount based on sector, method, cluster relevance, project CVA percentage, and machine learning predictions. It also prepares a file for manual classification and combines the results.

**Inputs:**
- A CSV file containing FTS flows flagged as CVA-related, saved as `fts_output_CVA.csv` in the "output" directory.
- A manually classified CSV file (if available)

**Outputs:**
- A CSV file containing flows to be manually classified, saved as `cva_to_manually_classify.csv` in the "output" directory.
- A CSV file containing the final CVA amounts, saved as `fts_cva.csv` in the "output" directory.

**Purpose:**
- To calculate CVA amounts from flagged FTS flows and prepare data for manual classification.

### code/10_global_cva_analysis.R

This script performs a global analysis of Cash and Voucher Assistance (CVA) amounts. It aggregates CVA data from various sources, matches subgrant recipients to organizations, and calculates unduplicated CVA amounts by organization type.

**Inputs:**
- `output/fts_cva.csv`: A CSV file containing flagged FTS flows with CVA amounts.
- `reference_datasets/cva_survey_data.xlsx`: An Excel file containing CVA survey data.
- `reference_datasets/fts_survey_overlap.csv`: A CSV file containing the overlap between FTS and survey data.
- `reference_datasets/cva_org_type.csv`: A CSV file containing organization types for CVA.
- `reference_datasets/usd_exchange_rates.csv`: A CSV file containing USD exchange rates for multiple countries and years.

**Outputs:**
- `output/cva_agg.csv`: A CSV file containing aggregated CVA data by organization.
- `output/cva_agg_org_type.csv`: A CSV file containing aggregated CVA data by organization type.

**Purpose:**
- To perform a comprehensive analysis of global CVA amounts, including aggregation, matching, and calculation of unduplicated amounts by organization type.

### code/util_oecd_sdmx.R

This script defines a function to retrieve and process exchange rate data from the OECD SDMX-JSON API. It parses the JSON response and converts it into a structured data frame.

**Inputs:**
- `url`: The URL of the OECD SDMX-JSON API endpoint to retrieve data from.

**Outputs:**
- A data frame containing the parsed exchange rate data from the OECD API.

**Purpose:**
- To provide a utility function for retrieving and processing exchange rate data from the OECD SDMX-JSON API.

### code/util_exchange_rates.R

This script retrieves and processes exchange rate data from various sources, including the OECD, World Bank, and IMF. It combines the data into a single dataset of USD exchange rates for multiple countries and years.

**Inputs:**
- `reference_datasets/isos.csv`: A CSV file containing ISO country codes and names.
- `reference_datasets/oecd_ex.csv`: A CSV file containing OECD exchange rate data (if it exists).
- `reference_datasets/wb_ex.csv`: A CSV file containing World Bank exchange rate data (if it exists).
- `reference_datasets/ifs.csv`: A CSV file containing IMF exchange rate data (if it exists).

**Outputs:**
- `reference_datasets/usd_exchange_rates.csv`: A CSV file containing combined USD exchange rates for multiple countries and years.

**Purpose:**
- To retrieve, process, and combine exchange rate data from multiple sources into a single dataset for analysis.
