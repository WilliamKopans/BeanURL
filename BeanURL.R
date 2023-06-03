# Need to add price and add checks for columns such as WhyWeLoveIt, FabricAndCare, and Specs not to be mixed up
library(tidyverse)
library(rvest)
library(httr)

URL <- "https://www.llbean.com/llb/shop/816?page=school-backpacks-and-lunch-boxes"
URL <- "https://www.llbean.com/llb/shop/818?page=school-backpacks&csp=f&bc=50-816&start=1&viewCount=48&nav=ln-816"

# Retrieve webpage content from URL
response <- content(GET(URL), as = "text")

# Extract all text nodes from the HTML content
entities <- html_nodes(read_html(response), xpath = "//*/text()")

# Clean and transform the data into a structured data frame
df <- data.frame(entity = trimws(entities, "both"), stringsAsFactors = FALSE) %>%
  filter(entity != "") %>%
  filter(startsWith(entity, "window.__INITIAL_STATE_"))

# Split the data
df2 <- as.data.frame(strsplit(df[1,1], "\\},\\{")[[1]])

# Prepare and structure data for final output
suppressMessages({
  df3 <- df2 %>%
    pivot_longer(everything(), names_to = "Key", values_to = "Value") %>%
    select(-Key) %>%
    filter(grepl("^\"page_productName", Value)) %>%
    separate(Value, into = paste0("col", 1:15), sep = '":"') 
  
  # Separate each column at every instance of '","'
  for (i in 1:ncol(df3)) {
    col_name <- paste0("col", i)
    df3 <- separate(df3, col = col_name, into = col_name, sep = '","')
  }
})
# Choose necessary columns, rename them, and prepare the final URLs
df4 <- df3 %>%
  select(col3, col4, col7, col9) %>% 
  rename(NamesForURL = col3,
         SomeTag = col4,
         NamesReadable = col7,
         InternalIDs = col9) %>% 
  mutate(NamesForURL = paste0("https://www.llbean.com/llb/shop/",SomeTag,"?page=", NamesForURL))

URL_List <- list(df4[,1])

for (link in URL_List) {
  print(link)
}



ProductInformation <- function(URL_List_Products){
  
  DF_To_Export <- data.frame()
  
  URL_List_Products <- URL_List_Products[[1]]$NamesForURL
  
  for (link in URL_List_Products){
    
    URLProductPage <- link
    print(URLProductPage)
    
    # Retrieve webpage content from URL
    responseProductPage <- content(GET(URLProductPage), as = "text")
    
    # Extract all text nodes from the HTML content
    entitiesProductPage <- html_nodes(read_html(responseProductPage), xpath = "//*/text()")
    
    # Clean and transform the data into a structured data frame
    dfProductPage <- data.frame(entity = trimws(entitiesProductPage, "both"), stringsAsFactors = FALSE) %>%
      filter(entity != "") %>%
      filter(startsWith(entity, "window.__INITIAL_STATE_"))
    # Split first row of dfProductPage and convert to data frame
    df2ProductPage <- as.data.frame(strsplit(dfProductPage[1,1], "\\},\\{")[[1]])
    
    # Process and select specific columns from 942nd row of df2ProductPage
    testdf <- df2ProductPage
    names(testdf) <- "BFG"
    NeededRow <- as.data.frame(testdf[grepl("rel\":\"alternate\",\"href", testdf$BFG), ])
    NeededRow$BFGTwo <- as.data.frame(testdf[grepl("\"property\":\"og:image", testdf$BFG), ])
    
    names <- colnames(NeededRow)
    names[1] <- "BFG"
    names[2] <- "BFGTwo"
    colnames(NeededRow) <- names
    
    names(NeededRow)[1] <- "BFG"
    names(NeededRow)[2] <- "BFGTwo"
    
    text <- as.character(NeededRow$BFGTwo)
    regex_pattern <- "\"content\":\"(.*?)\""
    match <- str_match(text, regex_pattern)
    NeededRow$BFGTwo[1] <- match[2]
    
    
    
    df2ProductPageSelect <- NeededRow %>% 
      separate('BFG', into = paste0("col", 1:200), sep = ':', extra = "merge", remove = FALSE)  %>%
      select(where(~ any(. != "", na.rm = TRUE))) %>%
      select(BFGTwo, col27, col42, col48, col52, col58, col70, col61, col62, col63, col64, col65, col66) %>% 
      mutate(Specs = paste(col61, col62, col63, col64, col65, col66, collapse = "")) %>% 
      select(BFGTwo, col27, col42, col48, col52, col58, col70, Specs) %>% 
      rename(
        ProductName = col27,
        ProductDetails = col42,
        AdditionalFeatures = col48,
        Construction = col52,
        FabricAndCare = col58,
        WhyWeLoveIt = col70
      )
    
    # Transpose df2ProductPageSelect
    df2ProductPageSelect_transposed <- as.data.frame(t(df2ProductPageSelect)) 
    
    # Process transposed dataframe to clean up and reformat data
    df2ProductPageSelect_transposed <- df2ProductPageSelect_transposed %>%
      separate(col = V1, into = paste0("V", 1:100), sep = '","', remove = FALSE, convert = FALSE)
    # df2ProductPageSelect_transposed[] <- lapply(df2ProductPageSelect_transposed, function(x) gsub("[^A-Za-z0-9 .%-]", "", x))
    df2ProductPageSelect_transposed <- df2ProductPageSelect_transposed %>%
      select_if(function(col) any(col != "")) %>%
      mutate_all(~ ifelse(. == "productCopy", NA, .)) %>%
      mutate_all(~ str_replace(.x, "constructionHeadline", "")) %>%
      mutate_all(~ str_replace(.x, "specsHeadline", "")) %>%
      mutate_all(~ str_replace(.x, "additionalFeaturesHeadline", "")) %>%
      mutate_all(~ str_replace(.x, "specs", "")) %>%
      mutate_all(~ str_replace(.x, "Dimensions ", "")) %>%
      unite(combined_column, everything(), sep = " <br> ") %>%
      mutate_all(~ str_replace_all(.x, "<br> NA", "")) %>%
      mutate_all(~ str_replace(.x, "^ <br>  ", "")) %>%
      mutate_all(~ str_replace(.x, " NA$", ""))
    
    # Transpose again to get final product data frame
    FinalProductDF <- as.data.frame(t(df2ProductPageSelect_transposed))
    # print(head(FinalProductDF))
    DF_To_Export <- bind_rows(DF_To_Export, FinalProductDF)
    
    
  }
  
  DF_To_Export <- DF_To_Export %>%
    mutate_all(~ str_replace_all(.x, "\\[|\\]|\\{|\\}|\"", "")) %>% 
    mutate_all(~ str_replace(.x, "<br> isDormant", "")) %>%
    filter(ProductDetails != "truedsplRsvLinkFlg ") %>%
    mutate_all(~ str_replace(.x, "copy   ", "")) %>%
    mutate_all(~ str_replace(.x, "copy", "")) %>%
    mutate(across(everything(), ~ifelse(grepl("isODSProduct", .), "", .))) %>% 
    mutate(across(everything(), ~ifelse(grepl("rue,dsplRsvLinkFlg", .), "", .))) %>%
    mutate_all(~ str_replace(.x, "^\\s*<br>\\s*", "")) %>% 
    mutate_all(~ str_replace(.x, "premiseStatement", "")) %>% 
    rename(Images = BFGTwo)
  
  
    
  return(DF_To_Export)
}  
  

DF_To_Export <- ProductInformation(URL_List)

write.csv(DF_To_Export, "DemoCSV_Bean_June2.csv")

# Working: 
# URLProductPage <- URL_List[[1]]$NamesForURL[1]
# URLProductPage <- URL_List[[1]]$NamesForURL[2]
# URLProductPage <- URL_List[[1]]$NamesForURL[3]




