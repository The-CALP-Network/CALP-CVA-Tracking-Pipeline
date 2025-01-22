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
```