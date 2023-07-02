# pdf_file_path = "/Users/williamkopans/Downloads/LLB_HH23-BestofSummer-REL.pdf"
# pdf_file_path = "/Users/williamkopans/Downloads/LLB_AD23-SummerProspect-REL.pdf"
# pdf_file_path = "/Users/williamkopans/Downloads/LLB_KH23-FlyFishing-REL.pdf"
pdf_file_path = "/Users/williamkopans/Downloads/LLB_KE23-SpringOutdoor-REL.pdf"


import re
import numpy as np
from PyPDF2 import PdfReader
from scipy import stats
import matplotlib.pyplot as plt


with open(pdf_file_path, 'rb') as file:
    pdf_reader = PdfReader(file)
    num_pages = len(pdf_reader.pages)
    page_numbers = []

    if num_pages >= 5:
        for i in range(len(pdf_reader.pages)):
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

            if page_content.isdigit():
                page_numbers.append((i + 1, int(page_content)))

        page_numbers_np = np.array(page_numbers)
        page_numbers_np = page_numbers_np[page_numbers_np[:,1] <= num_pages * 2]

        # Plot the page number pairings with a trend line
        plt.scatter(page_numbers_np[:,0], page_numbers_np[:,1])
        plt.plot(page_numbers_np[:,0], np.poly1d(np.polyfit(page_numbers_np[:,0], page_numbers_np[:,1], 1))(page_numbers_np[:,0]))
        plt.show()

        pdf_pages = page_numbers_np[:,0].astype(int)
        catalog_pages = page_numbers_np[:,1].astype(int)

        slope = None
        while True:
            coeff = np.polyfit(pdf_pages, catalog_pages, 1)
            slope, intercept = coeff[0], coeff[1]
            if 1.3 <= slope <= 2.7:
                break
            else:
                z_scores = np.abs(stats.zscore(page_numbers_np))
                page_numbers_np = page_numbers_np[z_scores < np.max(z_scores)]
                pdf_pages = page_numbers_np[:,0].astype(int)
                catalog_pages = page_numbers_np[:,1].astype(int)

        print(f"Slope: {slope}, Y-intercept: {intercept}")


        def estimate_catalog_page(pdf_page, coeff):
            estimate = (int(round(coeff[0] * pdf_page + coeff[1])), int(round(coeff[0] * pdf_page + coeff[1] + 1)))
            if estimate[0] < 0 or estimate[1] < 0:
                return ("Invalid Page Number", "Invalid Page Number")
            else:
                return estimate


        for i in range(1, num_pages + 1):
            catalog_page = estimate_catalog_page(i, coeff)
            if catalog_page[0] != "Invalid Page Number" and catalog_page[1] != "Invalid Page Number":
                print(f"PDF Page # {i} is Catalog Page # {catalog_page[0]}")
                print(f"PDF Page # {i} is Catalog Page # {catalog_page[1]}")


        if len(page_numbers_np) < 6 and not 1.75 <= slope <= 2.25:
            print("Low Confidence")
            print(f"Number of page pairs: {len(page_numbers_np)}")
            print(f"Slope: {slope}")
        if len(page_numbers_np) < 12 and not 1.5 <= slope <= 2.5:
            print("Low Confidence")
            print(f"Number of page pairs: {len(page_numbers_np)}")
            print(f"Slope: {slope}")

    else:
        print(f"The PDF has less than 5 pages. It has {num_pages} pages.")


print(pdf_file_path)

# Ask user to enter a string
input_string = input("Enter a string: ")
