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
      ) %>% 
      mutate(PLink = as.character(URLProductPage))
    
    #
    TempDF <- df2ProductPageSelect %>% 
      filter(ProductDetails == "") %>%
      mutate(
        WhyWeLoveIt = "",
        Specs = ""
      )
    
    Gapsdf2ProductPageSelect <- NeededRow %>% 
      separate('BFG', into = paste0("col", 1:200), sep = ':', extra = "merge", remove = FALSE)  %>%
      select(where(~ any(. != "", na.rm = TRUE))) %>%
      select(col27, col42, col47, col52, col53, col57, col54, col58, col75, col61,col62,col63,col64,col65, col66, col70, col75, BFGTwo, col67,col68, col69, col70, col71) %>% 
      # select(col27, col47, col49, col53, col57, col63, col75, col66,col67,col68,col60,col70,col71, BFGTwo) %>% 
      mutate(Specs = paste(col67,col68, col69, col70, col71, collapse = "")) %>% 
      select(BFGTwo, col27, col47, col53, col57, col63,col70, col75, Specs) %>% 
      rename(
        ProductDetails = col47,
        ProductName = col27,
        AdditionalFeatures = col53,
        Construction = col57,
        FabricAndCare = col63,
        WhyWeLoveIt = col75
      ) %>% 
      mutate(PLink = as.character(URLProductPage)) %>% 
      select("BFGTwo", "ProductName", "ProductDetails", "AdditionalFeatures", "Construction", "FabricAndCare", "WhyWeLoveIt", "Specs", "PLink")
    
    # df2ProductPageSelect <- df2ProductPageSelect %>% 
    #   filter(ProductDetails != "")
    # 
    # Gapsdf2ProductPageSelect <-  Gapsdf2ProductPageSelect %>% 
    #   bind_cols(Gapsdf2ProductPageSelect, df2ProductPageSelect)
    
    
    #  print(data.frame(lapply(head(df2ProductPageSelect), function(x) substr(x, 1, 10))))
    # print(df2ProductPageSelect$Construction)
    if (nrow(TempDF) > 0 || any(grepl("premiseStatement", df2ProductPageSelect$Construction)) || any(grepl("\\{\"copy\"", df2ProductPageSelect$Construction))) {
      print("Fixed Row")
      df2ProductPageSelect <- Gapsdf2ProductPageSelect %>% 
        mutate(changed = TRUE) 
      # print(paste0("Revised: ", df2ProductPageSelect$Construction))
      # print(data.frame(lapply(head(Gapsdf2ProductPageSelect), function(x) substr(x, 1, 10))))
    }
    # print("New:")
    # print(substr(df2ProductPageSelect$Construction, 1, 5))
    # cat("\n")
    
    df2ProductPageSelect <- df2ProductPageSelect %>% 
      mutate_all(~ str_replace_all(.x, "componentDesc", "")) %>%
      mutate_all(~ str_replace_all(.x, "specCopy", "")) %>% 
      mutate_all(~ str_replace_all(.x, ".,", "")) %>%
      mutate(WhyWeLoveIt = ifelse(grepl("Why We Love It,secondaryHeaderTxt      ", WhyWeLoveIt), "", WhyWeLoveIt))
      # mutate(Specs = ifelse(startsWith(Specs, "Designed For"), Specs, "")) %>%
      # mutate(Specs = ifelse(str_detect(Specs, "^Designed For"), Specs, ""))
    
    
    
    #
    
    
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
    filter(ProductDetails != "truedsplRsvLinkFlg ") %>%
    mutate_all(~ str_replace_all(.x, "\\[|\\]|\\{|\\}|\"", "")) %>% 
    mutate_all(~ str_replace(.x, "<br> isDormant", "")) %>%
    mutate_all(~ str_replace(.x, "copy   ", "")) %>%
    mutate_all(~ str_replace(.x, "copy", "")) %>%
    mutate_all(~ str_replace(.x, "^\\s*<br>\\s*", "")) %>% 
    mutate_all(~ str_replace(.x, "premiseStatement", "")) %>%
    mutate(across(everything(), ~ifelse(grepl("isODSProduct", .), "", .))) %>% 
    mutate(across(everything(), ~ifelse(grepl("rue,dsplRsvLinkFlg", .), "", .))) %>%
    rename(Images = BFGTwo) %>%
    mutate(Specs = str_trim(Specs),
           WhyWeLoveIt = if_else(!grepl("ItsecondaryHeaderTxt", WhyWeLoveIt), WhyWeLoveIt, ""),
           FabricAndCare = if_else(!grepl("polyester.Cotton X-Pac", FabricAndCare), FabricAndCare, ""), # Will want to combine these and make it pretty later
           FabricAndCare = if_else(!grepl("denier nylon bottom", FabricAndCare), FabricAndCare, ""),
           FabricAndCare = if_else(!grepl("Bluesign", FabricAndCare), FabricAndCare, ""),
           FabricAndCare = str_replace(FabricAndCare, ".Spot", ". Spot"),
           Specs = str_replace(Specs, "up.Capacity", "up. Capacity"),
           ProductName = str_replace(ProductName, "isDormant", ""),
           Specs = str_replace_all(Specs, "\\\\", "\" "),
           Specs = if_else(!str_detect(Specs, fixed("Designed For  Ages")), paste("Designed For: all ages. ", Specs), Specs),
           Specs = if_else(!str_detect(Specs, fixed("  ")), paste("", Specs), Specs),
           Specs = str_replace(Specs, "Designed For  Ages\\. ", "Designed For: Ages"),
           Specs = str_replace_all(Specs, "  ", " "),
           FabricAndCare = str_replace(FabricAndCare, "laundry ba then ", "laundry bag then "),
           ProductDetails = str_replace(ProductDetails, "everyda everywhere", "everyday everywhere"),
           Specs = str_replace(Specs, "Designed For Ages", "Designed For: ages")
    )
  
  return(DF_To_Export)
}  
  

FixBrokenProducts <- function(PreviousDF){
  TempDF <- PreviousDF %>% 
    filter(ProductDetails == "") %>%
    mutate(
      WhyWeLoveIt = "",
      Specs = ""
    )
} 

  






DF_To_Export <- ProductInformation(URL_List)

write.csv(DF_To_Export, "DemoCSV_Bean_June2.csv")

# Working: 
# URLProductPage <- URL_List[[1]]$NamesForURL[1]
# URLProductPage <- URL_List[[1]]$NamesForURL[2]
# URLProductPage <- URL_List[[1]]$NamesForURL[3]



# Weird things like no why we love it: https://www.llbean.com/llb/shop/125522?page=mountain-classic-school-backpack
  
  








