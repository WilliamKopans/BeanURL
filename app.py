import re
from flask import Flask, request, jsonify
from PyPDF2 import PdfReader
import numpy as np
from statsmodels.stats.outliers_influence import OLSInfluence

app = Flask(__name__)

# Compiled regular expression pattern
pattern = re.compile("Be an Outsider")

def extract_text_from_pdf(file_path):
    # Compiled regular expression pattern
    pattern = re.compile("Be an Outsider")

    page_numbers = []
    with open(file_path, 'rb') as file:
        pdf_reader = PdfReader(file)
        num_pages = len(pdf_reader.pages)

        for i, page in enumerate(pdf_reader.pages):
            page_content = page.extract_text()

            lines = [line for line in page_content.split("\n") if pattern.search(line)]
            page_content = pattern.sub('', "\n".join(lines))
            page_content = page_content[:page_content.rfind(" ")]
            if len(re.findall("\d{2,}", page_content)) > 1:
                page_content = page_content[:page_content.rfind(" ")]
            page_content = re.sub('[^0-9]', '', page_content)

            if page_content.isdigit() and int(page_content) < int(num_pages * 2 + 2):
                page_numbers.append((i + 1, int(page_content)))

    return page_numbers

def perform_linear_regression(page_numbers):
    pairs = np.array(page_numbers)
    x = pairs[:, 0]
    y = pairs[:, 1]

    model = np.polyfit(x, y, 1)
    slope = model[0]

    while len(x) >= 6:
        if abs(slope - 2) <= 0.5:
            break

        influence = OLSInfluence(np.vstack((x, y)).T)
        cooks_d = influence.cooks_distance[0]
        studentized_res = influence.resid_studentized_external

        max_index = np.argmax(cooks_d + studentized_res)

        x = np.delete(x, max_index)
        y = np.delete(y, max_index)

    return list(zip(x, y))

def predict_page_numbers(file_path, num_pages):
    pages_with_numbers = extract_text_from_pdf(file_path)
    filtered_page_numbers = perform_linear_regression(pages_with_numbers)

    slope, intercept = np.polyfit(np.array(filtered_page_numbers)[:, 0], np.array(filtered_page_numbers)[:, 1], 1)
    page_numbers = {}
    for i in range(1, num_pages + 2):
        if slope * i + intercept <= 0:
            continue

        page_number = round(abs(slope * i + intercept))
        page_numbers[page_number] = i
        page_numbers[page_number + 1] = i

    return page_numbers

@app.route("/predict", methods=["POST"])
def handle_predict():
    if "pdf" not in request.files or "key" not in request.form:
        return jsonify({"error": "Missing PDF file or desired key."}), 400

    pdf_file = request.files["pdf"]
    key = request.form["key"]

    file_path = "/tmp/uploaded_pdf.pdf"
    pdf_file.save(file_path)

    num_pages = len(PdfReader(file_path).pages)
    predicted_page_numbers = predict_page_numbers(file_path, num_pages)

    return jsonify(predicted_page_numbers)

if __name__ == "__main__":
    app.run(debug=True)
