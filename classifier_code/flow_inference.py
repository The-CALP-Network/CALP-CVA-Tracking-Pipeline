from transformers import AutoModelForSequenceClassification, AutoTokenizer
import torch
import pandas as pd
from tqdm import tqdm

card = "../classifier_code/cva-flow-weighted-classifier2/best_model"
device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
tokenizer = AutoTokenizer.from_pretrained(card)
model = AutoModelForSequenceClassification.from_pretrained(card).to(device)

def inference(example):
    inputs = tokenizer(example['text'], return_tensors="pt", truncation=True, max_length=512)
    inputs = {k: v.to(device) for k, v in inputs.items()}
    with torch.no_grad():
        logits = model(**inputs).logits
    predicted_class_id = logits.argmax().item()
    example['predicted_class'] = model.config.id2label[predicted_class_id]
    class_confidence = float(torch.softmax(logits[0], dim=0)[1])
    probs = torch.softmax(logits[0], dim=0)
    example['predicted_confidence'] = float(probs[model.config.label2id["Full"]])
    return example

def main():
    df = pd.read_csv("fts_to_inference.csv")
    results = [inference({"text": text}) for text in tqdm(df["text"], desc="Running inference")]
    df["predicted_class"] = [r["predicted_class"] for r in results]
    df["predicted_confidence"] = [r["predicted_confidence"] for r in results]
    df.to_csv("fts_to_inference_output.csv", index=False)

if __name__ == '__main__':
    main()
