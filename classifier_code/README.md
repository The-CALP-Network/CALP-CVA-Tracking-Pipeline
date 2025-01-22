# BERT-based Text Classification

## Installation

```
python3 -m virtualenv venv
source venv/bin/activate
pip install -r requirements.txt
```


## Running

`train_classifier_weighted.py` is designed to be run in something like Google Colab.
Simply copy the commented-out text into separate code blocks of a Colab ipynotebook, set the runtime to a T4 GPU, and run.

To run `example_inference.py`:

```
source venv/bin/activate
python3 example_inference.py
```