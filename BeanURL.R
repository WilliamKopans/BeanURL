


ExtractProductInformationProd <- function(URL_Of_Category) {
  
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
    as.data.frame()%>% # Everything up until this point is scraping the webpage and should not need any modification unless the site has a full redesign
    filter(grepl("page_pageParamValue_s", entity),
           grepl("page_productName_default_s", entity)) %>%
    tidyr::separate(entity, into = paste0("col", 1:40), sep = ',"', extra = "drop", fill = "right") %>% 
    slice(-1)
  
  # Each cell in BasicInfo contains the information for the products on the category page and is identified with a tag such as `minFullPrice_f` that describes what is in the cell
  # The following statement breaks up the scraped information to split by the ID. They are out of order so more involved than just selecting specific columns
  BasicWithTags <- BasicInfo %>%
    rowid_to_column("id") %>%  # create a unique id for each row
    gather(key, value, -id) %>%  # convert data to long format
    separate(value, into = c("key", "value"), sep = ":", extra = "merge")  %>%  # split keys and values
    pivot_wider(names_from = key, values_from = value, values_fn = list(value = function(x) paste(x, collapse = "; "))) %>%  # concatenate values
    select( # Some columns might be useful that I am getting rid of, but I don't know how to interpret some. Maybe a web developer at LLB could check it out.
      "\"page_productName_default_s\"", "minFullPrice_f\"", "page_pageParamValue_s\"", "fullPriceSwatches_ss\"", "minSalePrice_f\"",
      "pageID_s\"", "swatch_s\"", "skuID_s\"", "page_name_s\"", "page_reviews_averageRating_f\"", "page_isLowInventorySale_b\"",
      "page_isLowInventoryFull_b\"", "liquidatedSwatches_ss\"", "maxFullPrice_f\"", "allSwatches_ss\"", "itemID_s\"",
      "maxSalePrice_f\"", "page_reviews_reviewsCount_i\""
    ) %>% 
    mutate_all(~str_remove_all(., "\"")) %>% 
    rename_all(~str_remove_all(., "\"")) %>% 
    mutate(ProductLink = paste0("https://www.llbean.com/llb/shop/",pageID_s,"?page=", page_pageParamValue_s)) %>% 
    rename(
      Price = minFullPrice_f,
      Sale = minSalePrice_f,
    )
  
  # Up until this point has been collecting all the information given to us on the category screen. Now I will dig into the individual products.
  
  # Product Example that can be used for testing:
  # Product <- "https://www.llbean.com/llb/shop/127158?page=comfort-carry-laptop-pack-28l-print"
  
  for (Product in BasicWithTags$ProductLink) {
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
          message(paste("The requested page does not exist. Please check URL.",))
        } else {
          message("An error occurred:", e$message)
        }
        return()   # Move on to the next iteration of the loop
      }
    )
    
    ProductInfo <- data.frame(Images = character(),
                              ProductName = character(),
                              ProductDetails = character(),
                              AdditionalFeatures = character(),
                              Construction = character(),
                              FabricAndCare = character(),
                              WhyWeLoveIt = character(),
                              Specs = character(),
                              ProductLink = character(),
                              Price = character(),
                              ProductLink = character(),
                              stringsAsFactors = FALSE)
    
    InfoProductPage <- data.frame(entity = trimws(entitiesProductPage, "both"), stringsAsFactors = FALSE) %>%
      dplyr::filter(entity != "") %>%
      dplyr::filter(startsWith(entity, "window.__INITIAL_STATE_")) %>%
      rowwise() %>%
      mutate(entity = list(strsplit(entity, "\\},\\{")[[1]])) %>%
      unnest(entity) %>%
      as.data.frame()%>% # Everything up until this point is scraping the webpage and should not need any modification unless the site has a full redesign
      slice(943) %>% 
      mutate(AdditionalFeatures = sub(".*additionalFeaturesBullet\\s*(.*)", "\\1", entity)) %>% # This and the next line are selecting just the additional features
      mutate(AdditionalFeatures = str_replace(AdditionalFeatures, "additionalFeaturesHeadline.*", "additionalFeaturesHeadline")) %>% # Additional features is not organized the same as other columns so needs to be dealt with seperately
      mutate(AdditionalFeatures = str_extract(AdditionalFeatures, "\\[(.*?)\\]")) %>% # Getting everything within the square brackets
      mutate(AdditionalFeatures = str_replace_all(AdditionalFeatures, "\\\\\"", "")) %>% # Removing backslash which is in some additional features
      mutate(AdditionalFeatures = str_remove_all(AdditionalFeatures, "\\[|\\]|\"")) %>% # Removing square brackets and quotes since they are at the start and end of additional features
      tidyr::separate(entity, into = paste0("col", 1:400), sep = ',"', extra = "drop", fill = "right") %>%
      tidyr::separate(AdditionalFeatures, into = paste0("AdditionalFeatures", 1:10), sep = '","', extra = "drop", fill = "right") %>%
      select(-where(~ all(is.na(.) | . == "")))
    
    # Each cell in BasicInfo contains the information for the products on the category page and is identified with a tag such as `minFullPrice_f` that describes what is in the cell
    # The following statement breaks up the scraped information to split by the ID. They are out of order so more involved than just selecting specific columns
    WithTagsProductPage <- InfoProductPage %>%
      rowid_to_column("id") %>%  # create a unique id for each row
      gather(key, value, -id) %>%  # convert data to long format
      separate(value, into = c("key", "value"), sep = ":", extra = "merge")  %>%  # split keys and values
      pivot_wider(names_from = key, values_from = value, values_fn = list(value = function(x) paste(x, collapse = "; ")))  # concatenate values
      
      
    WithTagsProductPage <- WithTagsProductPage %>%  
      `colnames<-`(gsub("\"", "", colnames(WithTagsProductPage))) %>%  # At this point you should have all of the useful information split into their own distinct columns
      select("href","shortDesc", "sellingDesc", "constructionBullet", "fabricContentBullet", "specs", "Capacity", "Dimensions", "whyWeLoveItDesc", "pageParam", "isLowInventoryAll", "isLowInventorySale", "isLowInventoryFull", "text") %>% 
      mutate_all(~ifelse(str_detect(., ":"), str_extract(., "(?<=:).*"), .)) %>%   # Removing everything before the colon as there are sometimes unnessesary tags
      mutate_all(~str_replace(., "^\"", "")) %>%
      mutate_at(vars(href, shortDesc, sellingDesc, Capacity, Dimensions, pageParam, isLowInventoryAll, isLowInventorySale, isLowInventoryFull, text),
                ~str_replace(., "\\[\".*", "\"\"")) %>% 
      mutate(AgeStart = str_c(str_extract_all(specs, "\\d+"), collapse = "")) %>% 
      tidyr::separate(Dimensions, into = c("Height", "Width", "Depth"), sep = 'x', extra = "drop", fill = "right") %>%
      mutate(Height = str_replace(Height, '"H', ''),
             Width = str_replace(Width, '"W":', ''),
             Depth = str_replace(Depth, '"D":', ''),
        Height = str_replace_all(Height, "[^0-9.]", ""),
        Width = str_replace_all(Width, "[^0-9.]", ""),
        Depth = str_replace_all(Depth, "[^0-9.]", ""),
        Height = str_replace(Height, "\\.$", ""),
        Width = str_replace(Width, "\\.$", ""),
        Depth = str_replace(Depth, "\\.$", "")
      ) %>% 
      select(-specs) %>% 
      mutate(InfoProductPage$AdditionalFeatures1) %>% 
      tidyr::separate(`InfoProductPage$AdditionalFeatures1`, into = paste0("AdditionalFeatures", 1:15), sep = "\\.|,|;", extra = "drop", fill = "right") %>%
      select(where(function(x) any(x !=""))) %>%
      rename_with(~{
        stem <- sub("(\\d+)$", "", .x)
        suffix <- seq_along(.)
        paste0(stem, suffix)
      }, starts_with("AdditionalFeatures"))
    
      
    
    CorrectNames <- WithTagsProductPage %>%
      rename(
        ProductName = shortDesc,
        ProductLink = href,
        ProductDetails = sellingDesc,
        # AdditionalFeatures = col6,
        Construction = constructionBullet,
        FabricAndCare = fabricContentBullet,
        WhyWeLoveIt = whyWeLoveItDesc,
        Age = AgeStart,
        Height = Height,
        Width = Width,
        Depth = Depth,
        Capacity = Capacity,
        # Images1 = col15,
        # Images2 = col16,
        # Images3 = col17,
        # Images4 = col18,
        # Images5 = col19,
        # Lowest_Price = col20,
        # Liters = col21
      )
    
    
    merged_df <- inner_join(BasicWithTags, ProductInfo, by = "ProductLink")
    
    ForExport <- bind_rows(merged_df, ForExport)
    
  }
  
  
  
  return(ForExport)
  
}



URL <- "https://www.llbean.com/llb/shop/818?page=school-backpacks&csp=f&bc=50-816&start=1&viewCount=48&nav=ln-816"
BackpackInformation <- ExtractProductInformationProd(URL)
# write.xlsx(BackpackInformation, "BackpackInformationJune7_Morning.xlsx", rowNames = FALSE)


URL = "https://www.llbean.com/llb/shop/516672?page=bags-and-totes&bc=50&csp=f&nav=gnro-594" # Largely works, some columns off
Totes <- ExtractProductInformation(URL)

URL <- "https://www.llbean.com/llb/shop/816?page=school-backpacks&csp=f&bc=50&sort_field=relevance&start=1&viewCount=75"
SchoolBackpacksAndLunchBoxes <- ExtractProductInformation(URL)
