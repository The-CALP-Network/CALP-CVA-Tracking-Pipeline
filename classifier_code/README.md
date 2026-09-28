# BERT-based Text Classification

Fine-tunes and runs a binary `Partial`/`Full` CVA-relevance classifier on FTS flow text, used by `code/08a_fts_prepare_for_inference.R` and `code/08b_fts_combine_inference.R` in the main pipeline.

## Installation

```
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

## Running inference

This is the step the main pipeline pauses for, between `08a_fts_prepare_for_inference.R` and `08b_fts_combine_inference.R`. It reads `fts_to_inference.csv` (written by `08a`) and writes `fts_to_inference_output.csv` (read by `08b`):

```
source venv/bin/activate          # or venv\Scripts\activate.ps1 on Windows
python3 flow_inference.py
```

`flow_inference.py` loads the locally saved fine-tuned model from `cva-flow-weighted-classifier2/best_model/` and runs on GPU automatically if one is available (`torch.cuda.is_available()`), falling back to CPU otherwise. No Hugging Face account or internet access is required — this is a change from earlier versions of this script, which pulled the model from the Hugging Face Hub on every run. On a small to medium batch of flows, CPU-only inference is generally fast enough; for larger batches a CUDA-capable GPU will help significantly. A Windows launcher is provided as `run_model.ps1`.

## Retraining the classifier

Only needed if you're expanding or correcting the training data (`CVA_flow_descriptions.csv`) — for example after a batch of manual review decisions has been folded in automatically by `09_calculate_cva.R`, or after manually adding non-CVA negative examples.

```
source venv/bin/activate
python3 train_flow_classifier_weighted.py
```

This fine-tunes `alex-miller/ODABert` (a BERT model pretrained on ODA-related text) on `CVA_flow_descriptions.csv`, using a class-weighted loss to handle the imbalance between `Partial` and `Full` examples, with early stopping based on evaluation loss. The script:

- Runs entirely locally — no Hugging Face Hub account or push step.
- Trains with `fp16` mixed precision and `gradient_accumulation_steps` set for use on consumer GPUs with limited VRAM (developed and tested against an 8GB card); adjust `per_device_train_batch_size` and `gradient_accumulation_steps` in `TrainingArguments` if you have more or less VRAM available.
- Saves the best checkpoint (by evaluation loss, via `load_best_model_at_end=True`) to `cva-flow-weighted-classifier2/best_model/`, which is what `flow_inference.py` loads.
- Splits training data into train/test sets stratified by label where possible; if any label has fewer than 2 examples, stratification will fail — make sure you have at least a handful of examples of each class (and if you add a new label, e.g. a non-CVA negative class, make sure there are enough examples for both the train and test splits before running).

### Notes on the training data

`CVA_flow_descriptions.csv` currently has two binary labels: `0` (`Partial`) and `1` (`Full`). The model has never seen a non-CVA example and cannot itself reject a flow as irrelevant to CVA — that filtering happens upstream via keyword and project-flag candidate selection in `08a_fts_prepare_for_inference.R`. Adding a third, non-CVA class to the training data (for example, from manually reviewed flows that were rejected as false positives) would let the model make that distinction directly, but this hasn't been implemented; doing so would require updating `unique_labels` and the downstream logic in `flow_inference.py` and `09_calculate_cva.R` that currently assumes a two-class `id2label` scheme.

The training data file has its quotation marks stripped on every write from `09_calculate_cva.R` (`gsub("\"", "", text)`) as a workaround for a CSV quoting/round-trip issue between R's `fwrite` and pandas.