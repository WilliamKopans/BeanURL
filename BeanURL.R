library(tidyverse)
library(rvest)
library(httr)

URL <- "https://www.llbean.com/llb/shop/816?page=school-backpacks-and-lunch-boxes"
URL <- "https://www.llbean.com/llb/shop/818?page=school-backpacks&csp=f&bc=50-816&start=1&viewCount=48&nav=ln-816"
# URL <- "https://www.llbean.com/llb/shop/516673?page=luggage-and-duffle-bags&bc=50&csp=f&nav=gnro-818" # Can't quite work on other pages but with modification it might be able to

ExtractBackpacks <- function(URL_Of_Category){
    
    response <- content(GET(URL), as = "text")
    
    entities <- html_nodes(read_html(response), xpath = "//*/text()")
    
    df <- data.frame(entity = trimws(entities, "both"), stringsAsFactors = FALSE) %>%
      filter(entity != "") %>%
      filter(startsWith(entity, "window.__INITIAL_STATE_"))
    
    df2 <- as.data.frame(strsplit(df[1,1], "\\},\\{")[[1]])
    
    suppressMessages({
      df3 <- df2 %>%
        pivot_longer(everything(), names_to = "Key", values_to = "Value") %>%
        select(-Key) %>%
        filter(grepl("^\"page_productName", Value)) %>%
        separate(Value, into = paste0("col", 1:15), sep = '":"') 
      
      for (i in 1:ncol(df3)) {
        col_name <- paste0("col", i)
        df3 <- separate(df3, col = col_name, into = col_name, sep = '","')
      }
    })
    
    df4 <- df3 %>%
      select(col3, col4, col7, col9) %>% 
      rename(NamesForURL = col3,
             SomeTag = col4,
             NamesReadable = col7,
             InternalIDs = col9) %>% 
      mutate(NamesForURL = paste0("https://www.llbean.com/llb/shop/",SomeTag,"?page=", NamesForURL))
    
    URL_List <- list(df4[,1])
    return(URL_List)
}

ProductInformation <- function(URL_List_Products){
  
  DF_To_Export <- data.frame()
  
  URL_List_Products <- URL_List_Products[[1]]$NamesForURL
  
  for (link in URL_List_Products){
    
    URLProductPage <- link
    print(URLProductPage)
    
    responseProductPage <- content(GET(URLProductPage), as = "text")
    
    entitiesProductPage <- html_nodes(read_html(responseProductPage), xpath = "//*/text()")
    
    dfProductPage <- data.frame(entity = trimws(entitiesProductPage, "both"), stringsAsFactors = FALSE) %>%
      filter(entity != "") %>%
      filter(startsWith(entity, "window.__INITIAL_STATE_"))
    
    df2ProductPage <- as.data.frame(strsplit(dfProductPage[1,1], "\\},\\{")[[1]])
    
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
    
    if (nrow(TempDF) > 0 || any(grepl("premiseStatement", df2ProductPageSelect$Construction)) || any(grepl("\\{\"copy\"", df2ProductPageSelect$Construction))) {
      print("Extraction Method Two Used")
      df2ProductPageSelect <- Gapsdf2ProductPageSelect %>% 
        mutate(changed = TRUE) 
      
    }
    
    df2ProductPageSelect <- df2ProductPageSelect %>% 
      mutate_all(~ str_replace_all(.x, "componentDesc", "")) %>%
      mutate_all(~ str_replace_all(.x, "specCopy", "")) %>% 
      mutate_all(~ str_replace_all(.x, ".,", "")) %>%
      mutate(WhyWeLoveIt = ifelse(grepl("Why We Love It,secondaryHeaderTxt      ", WhyWeLoveIt), "", WhyWeLoveIt))

    df2ProductPageSelect_transposed <- as.data.frame(t(df2ProductPageSelect)) 
    
    df2ProductPageSelect_transposed <- df2ProductPageSelect_transposed %>%
      separate(col = V1, into = paste0("V", 1:100), sep = '","', remove = FALSE, convert = FALSE)
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
    
    FixLoveAndCare <- as.data.frame(t(df2ProductPageSelect_transposed)) %>%
      mutate(FabricAndCare = ifelse(FabricAndCare == '["Spot clean."]""',
                                    str_replace_all(FabricAndCare, '\\["Spot clean."\\]""', 'Spot clean.'),
                                    FabricAndCare)) %>% 
      filter(WhyWeLoveIt == "{\"copy\"") %>% 
      mutate(NeededRow$BFG) %>%
      rename(BFG = 'NeededRow$BFG') %>% 
      mutate(BFG = str_remove(BFG, ".*whyWeLoveItDesc"),
             BFG = str_replace(BFG, '\\:\\{"copy":\\["', ""),
             WhyWeLoveIt = str_extract(BFG, '"([^"]+)"')) %>% 
      select(-BFG)
    
    TestFixNeeded <- as.data.frame(t(df2ProductPageSelect_transposed)) %>%
      filter(WhyWeLoveIt == "{\"copy\"")
    
    EdgeCaseTest <- as.data.frame(t(df2ProductPageSelect_transposed)) %>%  # Not sure the root cause of why I need this...
      mutate(FabricAndCare = ifelse(FabricAndCare == '["Spot clean."]""',
                                    str_replace_all(FabricAndCare, '\\["Spot clean."\\]""', 'Spot clean.'),
                                    FabricAndCare)) %>% 
      filter(WhyWeLoveIt == '["Why We Love It"]}}"secondaryHeaderTxt"') %>% 
      mutate(NeededRow$BFG) %>%
      rename(BFG = 'NeededRow$BFG') %>% 
      mutate(BFG = str_remove(BFG, ".*whyWeLoveItDesc"),
             BFG = str_replace(BFG, '\\:\\{"copy":\\["', ""),
             WhyWeLoveIt = str_extract(BFG, '"([^"]+)"')) %>% 
      select(-BFG)
    
    EdgeCaseTestSecond <- as.data.frame(t(df2ProductPageSelect_transposed)) %>%  # Not sure the root cause of why I need this...
      mutate(FabricAndCare = ifelse(FabricAndCare == '["Spot clean."]""',
                                    str_replace_all(FabricAndCare, '\\["Spot clean."\\]""', 'Spot clean.'),
                                    FabricAndCare)) %>% 
      filter(WhyWeLoveIt == '["Why We Love It"]}}"secondaryHeaderTxt"') %>% 
      mutate(NeededRow$BFG) %>%
      rename(BFG = 'NeededRow$BFG') %>% 
      mutate(BFG = str_remove(BFG, ".*fabricContentBullet"),
             BFG = str_replace(BFG, '\\:\\{"copy":\\["', ""),
             FabricAndCare = str_extract(BFG, '"([^"]+)"'),
             WhyWeLoveIt = "") %>% 
      select(-BFG)
    
    FinalProductDF <- as.data.frame(t(df2ProductPageSelect_transposed))
    if (nrow(TestFixNeeded) > 0){
      print("Fixed Love & Care")
      FinalProductDF <- FixLoveAndCare
    }
    if (nrow(EdgeCaseTest) > 0){
      print("Fixed Edge Case")
      FinalProductDF <- EdgeCaseTest
      
      if (grepl("Drawstring", EdgeCaseTest$ProductName)) {
        FinalProductDF <- EdgeCaseTestSecond
      }
      
    }
    if (any(grepl("isODSProduct", FinalProductDF$WhyWeLoveIt))) {
      FinalProductDF <- as.data.frame(t(df2ProductPageSelect_transposed)) %>%
        mutate(FabricAndCare = ifelse(FabricAndCare == '["Spot clean."]""',
                                      str_replace_all(FabricAndCare, '\\["Spot clean."\\]""', 'Spot clean.'),
                                      FabricAndCare)) %>% 
        mutate(WhyWeLoveIt = "")
    }
    
    if (any(grepl("Mountain Classic School", FinalProductDF$ProductName))) { # Unfortunately need to hard code this one, can't crack this nut!
      print("Hardcoded: Mountain School")
        FinalProductDF <- FinalProductDF %>%
          mutate(NeededRow$BFG) %>%
          rename(BFG = 'NeededRow$BFG') %>%
          mutate(BFG = str_remove(BFG, ".*fabricContentBullet"),
                 BFG = str_replace(BFG, '\\:\\{"copy":\\["', ""),
                 FabricAndCare = str_extract(BFG, '"([^"]+)"'),) %>%
          select(-BFG)
    }
    
    PriceTwo <- as.data.frame(testdf[grepl('\"default\":false,\"id\":\"', testdf$BFG), ]) %>%
      filter(grepl("price", `testdf[grepl(\"\\\"default\\\":false,\\\"id\\\":\\\"\", testdf$BFG), ]`)) %>% 
      rename(Price = `testdf[grepl(\"\\\"default\\\":false,\\\"id\\\":\\\"\", testdf$BFG), ]`) %>%
      mutate(Price = str_remove(Price, ".*fullPrice")) %>%
      mutate(Price = str_remove(Price, ".*?:"))%>%
      mutate(Price = gsub("potentialDiscounts.*", "", Price)) %>%
      separate(Price, into = c("regular", "sale"), sep = "salePrice") %>%
      mutate(regular = str_replace_all(regular, "[^0-9.]", ""),
             sale = str_replace_all(sale, "[^0-9.]", "")) %>%
      mutate(regular = as.numeric(regular),
             sale = as.numeric(sale)) %>%
      mutate(PriceLowest = ifelse(is.na(regular), sale,
                               ifelse(is.na(sale), regular,
                                      pmin(regular, sale, na.rm = TRUE)))) %>% 
      select(PriceLowest)
    DeterminedPrice = PriceTwo[1,1]
      
    FinalProductDF <- FinalProductDF %>% 
      mutate(Price = DeterminedPrice)
    
    
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
    ) %>% 
    select(-changed)
  
    return(DF_To_Export)
  
}  

ExportDF <- ProductInformation(ExtractBackpacks(URL))

write.csv(ExportDF, "DemoCSV_Bean_June4.csv")
