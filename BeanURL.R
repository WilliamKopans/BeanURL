


ExtractProductInformation <- function(URL_Of_Category) {
  
  ForExport <- data.frame(Images = character(),
                          ProductName = character(),
                          ProductDetails = character(),
                          AdditionalFeatures = character(),
                          Construction = character(),
                          FabricAndCare = character(),
                          WhyWeLoveIt = character(),
                          Specs = character(),
                          ProductLink = character(),
                          Price = character(),
                          stringsAsFactors = FALSE)
  
  response <- httr::content(httr::GET(URL_Of_Category), as = "text")
  
  entities <- rvest::html_nodes(rvest::read_html(response), xpath = "//*/text()")
  
  BasicInfo <- data.frame(entity = trimws(entities, "both"), stringsAsFactors = FALSE) %>%
    dplyr::filter(entity != "") %>%
    dplyr::filter(startsWith(entity, "window.__INITIAL_STATE_")) %>%
    rowwise() %>%
    mutate(entity = list(strsplit(entity, "\\},\\{")[[1]])) %>%
    unnest(entity) %>%
    as.data.frame()%>% 
    tidyr::pivot_longer(everything(), names_to = "Key", values_to = "Value") %>%
    dplyr::select(-Key) %>%
    filter(grepl("page_pageParamValue_s", Value),
           grepl("page_productName_default_s", Value)) %>%
    tidyr::separate(Value, into = paste0("col", 1:40), sep = '","', extra = "drop", fill = "right") %>%
    mutate(Sale = ifelse(grepl("minSalePrice_f", col3), col3, NA)) %>% 
    mutate(Sale = str_replace(Sale, 'minSalePrice_f":', ''),
           Sale = str_replace(Sale, ',"', ''),
           Sale = str_remove(Sale, "fullPriceSwatches_ss.*")) %>% 
    tidyr::separate(col2, into = c("Price","ForLink"), sep = 'page_pageParamValue_s', fill = "right") %>% 
    filter(!grepl("categoryId", Price)) %>% 
    select("col1", "Price", "ForLink", "Sale", everything()) %>% 
    unite(ProductID, col4:last_col(), sep = " ") %>% 
    mutate(ProductID = str_remove(ProductID, ".*pageID_s\":\"")) %>%
    mutate(ProductID = str_sub(ProductID, start = 1, end = 6)) %>% 
    mutate(Price = str_replace(Price, 'minFullPrice_f":', ''),
           Price = str_replace(Price, ',"', ''),
           col1 = str_replace(col1, '"page_productName_default_s":"', ''),
           ForLink = str_replace(ForLink, '":"', '')) %>% 
    mutate(ProductLink = paste0("https://www.llbean.com/llb/shop/",ProductID,"?page=", ForLink)) %>% 
    rename(ProductName = col1) %>% 
    select(-col3, -ProductID) %>%
    mutate(ProductLink = str_remove_all(ProductLink, "\\s"))
  
  # Per Product:
  # Product <- "https://www.llbean.com/llb/shop/127158?page=comfort-carry-laptop-pack-28l-print"
  
  for (Product in BasicInfo$ProductLink) {
    print(Product, quote=FALSE)
    
      tryCatch(
        expr = {
          responseProductPage <- content(GET(Product), as = "text")
          entitiesProductPage <- html_nodes(read_html(responseProductPage), xpath = "//*/text()")
          # Process the entitiesProductPage or perform other operations here
        },
        error = function(e) {
          # Handle the error and exit the loop
          if (grepl("The page was not found", e$message)) {
            message("The requested page does not exist.")
          } else {
            message("An error occurred:", e$message)
          }
          return()   # Move on to the next iteration of the loop
        }
      )
    
    df2ProductPage <- data.frame(entity = trimws(entitiesProductPage, "both"), stringsAsFactors = FALSE) %>%
      filter(entity != "") %>%
      filter(startsWith(entity, "window.__INITIAL_STATE_")) %>%
      slice(1) %>%
      mutate(entity = lapply(strsplit(entity, "\\},\\{"), unlist)) %>%
      unnest(entity) %>%
      as.data.frame()
    
    Images <- df2ProductPage %>%
      filter(grepl("cdni", entity)) %>% 
      mutate(ImageUrl = str_remove(entity, ".*path")) %>%
      mutate(ImageUrl = str_remove_all(ImageUrl, "\"")) %>% 
      mutate(ImageUrl = str_remove_all(ImageUrl, ":")) %>%
      filter(!grepl("propertyogimage|typetag", ImageUrl)) %>% 
      mutate(ImageUrl = str_remove(ImageUrl, 'quantity1.*')) %>%
      mutate(ImageUrl = str_replace_all(ImageUrl, "\\}\\],", "")) %>%
      mutate(ImageUrl = str_replace_all(ImageUrl, "https//", "https://")) %>% 
      select(-entity) %>%
      distinct()
    
    ImageString <- paste(Images$ImageUrl, collapse = " <br> ")
    
    ProductInfo <- df2ProductPage %>%
      filter(grepl("sellingDesc", entity)) %>%
      mutate(ProductName = str_remove(entity, '.*shortDesc')) %>% 
      mutate(ProductName = str_remove(ProductName, "isDormant.*")) %>%
      mutate(ProductDetails = str_remove(entity, '.*premiseStatement":"')) %>% 
      mutate(AdditionalFeatures = str_remove(entity, '.*additionalFeaturesBullet')) %>%
      mutate(AdditionalFeatures = str_replace_all(AdditionalFeatures, "\\:\\{\"copy\":\\[\"", "")) %>%  # I think this will be repeated can I remove `":{"copy":["` altogether?
      mutate(Construction = str_remove(AdditionalFeatures, '.*constructionBullet')) %>% 
      mutate(Construction = str_replace_all(Construction, "\\:\\{\"copy\":\\[\"", "")) %>% 
      mutate(Construction = str_remove(Construction, "constructionHeadline.*")) %>% 
      mutate(FabricAndCare = str_remove(AdditionalFeatures, '.*fabricContentBullet')) %>% 
      mutate(FabricAndCare = str_replace_all(FabricAndCare, "\\:\\{\"copy\":\\[\"", "")) %>% 
      mutate(Specs = str_remove(FabricAndCare, '.*componentDesc')) %>%
      mutate(Specs = str_extract(Specs, "\\[.*")) %>% 
      mutate(WhyWeLoveIt = str_remove(Specs, '.*whyWeLoveItDesc')) %>% 
      mutate(ForLink = str_remove(Specs, '.*pageParam')) %>% 
      select(-entity) %>%
      mutate(ProductDetails = str_remove(ProductDetails, "sellingHeadline.*")) %>%
      mutate(AdditionalFeatures = str_remove(AdditionalFeatures, "additionalFeaturesHeadline.*")) %>%
      mutate(FabricAndCare = str_remove(FabricAndCare, "specs.*")) %>% 
      mutate(Specs = str_remove(Specs, "specsHeadline.*")) %>% 
      mutate(WhyWeLoveIt = str_remove(WhyWeLoveIt, "whyWeLoveItHeadline.*")) %>% 
      mutate(ForLink = str_remove(ForLink, "isODSProduct*")) %>%
      mutate_at(vars(-ProductName), ~ str_replace_all(., "[^a-zA-Z0-9- \"]", "")) %>%
      mutate_all(~ str_replace(., "^\"", "")) %>% 
      mutate(ForLink = str_remove(ForLink, "false.*")) %>%
      mutate(ForLink = str_remove_all(ForLink, "\"")) %>%
      mutate(ProductName = str_remove_all(ProductName, "\"")) %>%
      mutate(ProductName = str_sub(ProductName, start = 2, end = -2)) %>% 
      mutate(Images = ImageString) %>% 
      select(ProductName, ProductDetails, AdditionalFeatures, Construction, FabricAndCare, WhyWeLoveIt, Specs, ForLink, Images)
    
    merged_df <- inner_join(BasicInfo, ProductInfo, by = "ForLink")
    
    
    ForExport <- bind_rows(merged_df, ForExport)
    
  }
  
  ForExport <- ForExport %>%
    mutate_all(~ str_replace_all(., "\"\"", "")) %>%
    mutate(WhyWeLoveIt = ifelse(grepl("Designed For", WhyWeLoveIt), "", WhyWeLoveIt)) %>% 
    select(-ForLink, -ProductName)
  
  colnames(ForExport) <- gsub("\\..*", "", colnames(ForExport))
  
  ForExport <- ForExport %>% 
    select(-which(colnames(.) == "ProductName")[2]) %>% 
    mutate(Specs = str_replace_all(Specs, "upCapacity", "up. Capacity")) %>%
    # mutate(Images = str_extract(Images, "attrValueLime.*")) %>%
    # mutate(Images = ifelse(str_detect(Images, "priceCategoryF"), str_replace(Images, ".*priceCategoryF", "priceCategoryF"), Images)) %>%
    mutate(Images = str_extract(Images, "https.*")) %>% 
    mutate(Images = ifelse(str_detect(Images, "monogram"), str_replace(Images, ".*monogram", "monogram"), Images)) %>% 
    mutate(Images = str_extract(Images, "https.*")) %>% 
    mutate(Images = ifelse(str_detect(Images, "percentageOverall"), str_replace(Images, ".*percentageOverall", "percentageOverall"), Images)) %>%
    mutate(Images = ifelse(str_detect(Images, "Full100"), str_extract(Images, "https.*"), Images))
  
  
  return(ForExport)
  
}


URL <- "https://www.llbean.com/llb/shop/818?page=school-backpacks&csp=f&bc=50-816&start=1&viewCount=48&nav=ln-816"
BackpackInformation <- ExtractProductInformation(URL)

URL = "https://www.llbean.com/llb/shop/516672?page=bags-and-totes&bc=50&csp=f&nav=gnro-594" # Largely works, some columns off
Totes <- ExtractProductInformation(URL)

URL <- "https://www.llbean.com/llb/shop/816?page=school-backpacks&csp=f&bc=50&sort_field=relevance&start=1&viewCount=75"
SchoolBackpacksAndLunchBoxes <- ExtractProductInformation(URL)

#

URL1 <- "https://www.llbean.com/llb/shop/816?page=school-backpacks-and-lunch-boxes&bc=50&csp=f&nav=gnro-516672" # Largely works, some columns off
SchoolBackpacksAndLunchBoxes <- ExtractProductInformation(URL1)

URL2 <- "https://www.llbean.com/llb/shop/1098?page=hiking-backpacks&bc=50&csp=f&nav=gnro-516672" # Largely works, some columns off
HikingBackpacks <- ExtractProductInformation(URL2)

URL3 <- "https://www.llbean.com/llb/shop/516672?page=bags-and-totes&bc=50&csp=f&nav=gnro-516672" # Largely works some bugs
BagsAndTotes <- ExtractProductInformation(URL3)

URL4 <- "https://www.llbean.com/llb/shop/516673?page=luggage-and-duffle-bags&bc=50&csp=f&nav=gnro-516672" # Largely works, some columns off
LuggageAndDuffleBags <- ExtractProductInformation(URL4)

URL5 <- "https://www.llbean.com/llb/shop/203?page=travel-accessories&bc=50&csp=f&nav=gnro-516672" # Largely works some bugs
TravelAccessories <- ExtractProductInformation(URL5)

URL6 <- "https://www.llbean.com/llb/shop/507361?page=toiletry-bags-and-organizers&bc=50&csp=f&nav=gnro-516672" # Doesn't really work
ToiletryBagsAndOrganizers <- ExtractProductInformation(URL6)

#




URL <- "https://www.llbean.com/llb/shop/506673?page=mens-insulated-jackets&bc=516567-593&csp=f&nav=gnro-818" # Does Not Work
MensInsulatedJackets <- ExtractProductInformation(URL)

URL = "https://www.llbean.com/llb/shop/594?page=mens-sweaters&bc=12-26&csp=f&nav=gnro-818"
MensSweaters <- ExtractProductInformation(URL)









