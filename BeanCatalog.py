# pdf_file_path = "/Users/williamkopans/Downloads/LLB_HH23-BestofSummer-REL.pdf"
# pdf_file_path = "/Users/williamkopans/Downloads/LLB_AD23-SummerProspect-REL.pdf"
pdf_file_path = "/Users/williamkopans/Downloads/LLB_KH23-FlyFishing-REL.pdf"
# pdf_file_path = "/Users/williamkopans/Downloads/LLB_KE23-SpringOutdoor-REL.pdf"



import re
import numpy as np
from PyPDF2 import PdfReader
from scipy import stats


# Open your PDF file
with open(pdf_file_path, 'rb') as file:
    # Create a PDF file reader object
    pdf_reader = PdfReader(file)

    # Get the number of pages in the PDF
    num_pages = len(pdf_reader.pages)

    # Array of PDF page numbers to page numbers in the catalog
    page_numbers = []

    # Make sure your PDF has at least 5 pages
    if num_pages >= 5:
        # Loop through all pages
        for i in range(len(pdf_reader.pages)):
            # Get the page
            page = pdf_reader.pages[i]

            # Extract the text from the page
            page_content = page.extract_text()

            # Only keep lines that contain "Be an Outsider"
            page_content = "\n".join([line for line in page_content.split("\n") if re.search("Be an Outsider", line)])

            # Various cleaning steps
            page_content = page_content.replace("Call 1·800·221·4221  7 am - 1 1 pm ET or Shop at llbean.com", "")
            page_content = page_content.replace("Be an Outsider", "")
            page_content = page_content[:page_content.rfind("·")]
            page_content = page_content.replace("See more colors at llbean.com", "")
            page_content = page_content.replace("·", "")
            page_content = page_content[:page_content.rfind(" ")]
            if len(re.findall("\d{2,}", page_content)) > 1:
                page_content = page_content[:page_content.rfind(" ")]

            page_content = re.sub('[^0-9]', '', page_content)  # remove all non-digit characters

            # Print the page content
            if page_content.isdigit():  # ensure it can be converted to integer
                page_numbers.append((i + 1, int(page_content)))

        # Convert your list of tuples to a NumPy array
        page_numbers_np = np.array(page_numbers)

        # Remove pairs where the catalog page number is greater than the number of pdf pages times two
        page_numbers_np = page_numbers_np[page_numbers_np[:,1] <= num_pages * 2]

        # Separate the PDF page numbers and the catalog page numbers
        pdf_pages = page_numbers_np[:,0].astype(int)
        catalog_pages = page_numbers_np[:,1].astype(int)

        # Remove outliers until the slope is between 1.3 and 2.7
        slope = None
        while True:
            coeff = np.polyfit(pdf_pages, catalog_pages, 1)
            slope, intercept = coeff[0], coeff[1]
            if 1.3 <= slope <= 2.7:
                break
            else:
                # compute z scores
                z_scores = np.abs(stats.zscore(page_numbers_np))
                # remove most extreme outlier
                page_numbers_np = page_numbers_np[z_scores < np.max(z_scores)]
                pdf_pages = page_numbers_np[:,0].astype(int)
                catalog_pages = page_numbers_np[:,1].astype(int)

        # Print the slope and y-intercept of the line
        print(f"Slope: {slope}, Y-intercept: {intercept}")

        # Create a function to estimate catalog page from pdf page number
        def estimate_catalog_page(pdf_page, coeff):
            estimate = int(round(coeff[0] * pdf_page + coeff[1]))
            if estimate < 0:
                return "Invalid Page Number"
            else:
                return estimate

        # Generate the estimated catalog page numbers for all pdf pages
        for i in range(1, num_pages + 1):
            catalog_page = estimate_catalog_page(i, coeff)
            if catalog_page != "Invalid Page Number":
                print(f"PDF Page # {i} is Catalog Page # {catalog_page}")

        # Check the conditions for low confidence
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
