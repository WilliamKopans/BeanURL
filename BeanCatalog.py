# pdf_file_path = "/Users/williamkopans/Downloads/LLB_HH23-BestofSummer-REL.pdf"
# pdf_file_path = "/Users/williamkopans/Downloads/LLB_AD23-SummerProspect-REL.pdf"
# pdf_file_path = "/Users/williamkopans/Downloads/LLB_KH23-FlyFishing-REL.pdf"
pdf_file_path = "/Users/williamkopans/Downloads/LLB_KE23-SpringOutdoor-REL.pdf"

import re
from PyPDF2 import PdfReader
import numpy as np
import matplotlib.pyplot as plt
from statsmodels.stats.outliers_influence import OLSInfluence


def extract_text_from_pdf(file_path):
    page_numbers = []
    with open(file_path, 'rb') as file:
        pdf_reader = PdfReader(file)
        num_pages = len(pdf_reader.pages)

        for i in range(num_pages):
            page = pdf_reader.pages[i]
            page_content = page.extract_text()

            page_content = "\n".join([line for line in page_content.split("\n") if re.search("Be an Outsider", line)])
            page_content = page_content.replace("Call 1·800·221·4221  7 am - 1 1 pm ET or Shop at llbean.com", "")
            page_content = page_content.replace("Be an Outsider", "")
            page_content = page_content[:page_content.rfind("·")]
            page_content = page_content.replace("See more colors at llbean.com", "")
            page_content = page_content.replace("·", "")
            page_content = page_content[:page_content.rfind(" ")]
            if len(re.findall("\d{2,}", page_content)) > 1:
                page_content = page_content[:page_content.rfind(" ")]
            page_content = re.sub('[^0-9]', '', page_content)

            if page_content.isdigit() and int(page_content) < int(len(pdf_reader.pages) * 2 + 2):
                page_numbers.append((i + 1, int(page_content)))

    return page_numbers


def plot_page_numbers(page_numbers):
    x = [pair[0] for pair in page_numbers]
    y = [pair[1] for pair in page_numbers]

    plt.scatter(x, y)
    plt.plot(np.unique(x), np.poly1d(np.polyfit(x, y, 1))(np.unique(x)))
    plt.show()


def perform_linear_regression(page_numbers):
    x = np.array([pair[0] for pair in page_numbers])
    y = np.array([pair[1] for pair in page_numbers])

    while len(x) >= 6:
        model = np.polyfit(x, y, 1)
        slope = model[0]

        if abs(slope - 2) <= 0.5:
            break

        # Calculate Cook's distance and studentized residuals
        influence = OLSInfluence(np.vstack((x, y)).T)
        cooks_d = influence.cooks_distance[0]
        studentized_res = influence.resid_studentized_external

        # Find the index of the point with the maximum Cook's distance or studentized residual
        max_index = np.argmax(cooks_d + studentized_res)

        # Remove the outlier point
        x = np.delete(x, max_index)
        y = np.delete(y, max_index)

    return list(zip(x, y))

# make a function that check that the slope between each filtered page number is eqactly 2
# if not, remove the point with the highest cook's distance or studentized residual
# if the slope is 2, return the page numbers
def check_slope(page_numbers):
    x = np.array([pair[0] for pair in page_numbers])
    y = np.array([pair[1] for pair in page_numbers])

    model = np.polyfit(x, y, 1)
    slope = model[0]

    if abs(slope - 2) <= 0.5:
        return page_numbers

    # Calculate Cook's distance and studentized residuals
    influence = OLSInfluence(np.vstack((x, y)).T)
    cooks_d = influence.cooks_distance[0]
    studentized_res = influence.resid_studentized_external

    # Find the index of the point with the maximum Cook's distance or studentized residual
    max_index = np.argmax(cooks_d + studentized_res)

    # Remove the outlier point
    x = np.delete(x, max_index)
    y = np.delete(y, max_index)

    return list(zip(x, y))






pages_with_numbers = extract_text_from_pdf(pdf_file_path)
plot_page_numbers(pages_with_numbers)

filtered_page_numbers = perform_linear_regression(pages_with_numbers)
print("filtered_page_numbers: ", filtered_page_numbers)
plot_page_numbers(filtered_page_numbers)
print("Slope: ", np.polyfit([pair[0] for pair in filtered_page_numbers], [pair[1] for pair in filtered_page_numbers], 1)[0])

print("Check slope: ", check_slope(filtered_page_numbers) == filtered_page_numbers)

slope, intercept = np.polyfit([pair[0] for pair in filtered_page_numbers], [pair[1] for pair in filtered_page_numbers], 1)
print("Slope: ", slope)
print("Intercept: ", intercept)

# Using the slope and intercept, predict the page number for each page from 1 to number of pdf pages times two plus 2 with incriments of 1 for the pdf pages and with two catlog pages per pdf page
# The first number should be the PDF number and the second number should be the catalog page number. There are two catalog page numbers per PDF page number so the slope shuold be devide by 2. All numbers should be ablsolute values and rounded
def predict_page_numbers(slope, intercept, num_pages):
    page_numbers = []
    for i in range(1, num_pages + 2):
        if slope * i + intercept <= 0:
            continue

        page_number = round(abs(slope * i + intercept))
        page_numbers.append((i, page_number))
        page_numbers.append((i, page_number + 1))

    return page_numbers

predicted_page_numbers = predict_page_numbers(slope, intercept, len(PdfReader(pdf_file_path).pages))
print("predicted_page_numbers: ", predicted_page_numbers)

print(pdf_file_path)
