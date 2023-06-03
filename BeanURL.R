
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



ProductInformation <- function(URLProductPage){
  
  
}  
  




URLProductPage <- URL_List[[1]]$NamesForURL[1]

# Retrieve webpage content from URL
responseProductPage <- content(GET(URLProductPage), as = "text")

# Extract all text nodes from the HTML content
entitiesProductPage <- html_nodes(read_html(responseProductPage), xpath = "//*/text()")

# Clean and transform the data into a structured data frame
dfProductPage <- data.frame(entity = trimws(entitiesProductPage, "both"), stringsAsFactors = FALSE) %>%
  filter(entity != "") %>%
  filter(startsWith(entity, "window.__INITIAL_STATE_"))

# Split the data
df2ProductPage <- as.data.frame(strsplit(dfProductPage[1,1], "\\},\\{")[[1]])

# Prepare and structure data for final output
df3ProductPage <- df2ProductPage %>%
  pivot_longer(everything(), names_to = "Key", values_to = "Value") %>%
  filter(grepl("pageMetaData", Value)) %>%
  select(-Key) %>%
  separate(Value, into = paste0("col", 1:350), sep = '":"') 

# Separate each column at every instance of '","'
for (i in 1:ncol(df3ProductPage)) {
  col_name <- paste0("col", i)
  df3ProductPage <- separate(df3ProductPage, col = col_name, into = col_name, sep = '","')
}




df99ProductPage <- df2ProductPage %>%
  filter(startsWith(`strsplit(dfProductPage[1, 1], \"\\\\},\\\\{\")[[1]]`, '"rel":"alternate","href":"')) %>%
  separate(`strsplit(dfProductPage[1, 1], \"\\\\},\\\\{\")[[1]]`, into = paste0("col", 1:350), sep = '":{"') %>%
  select(where(~ any(. != "", na.rm = TRUE)))

for (i in 1:ncol(df99ProductPage)) {
  col_name <- names(df99ProductPage)[i]
  df99ProductPage <- df99ProductPage %>% 
    separate(col_name, into = c(paste0(col_name,"_1"), paste0(col_name,"_2")), sep = "\\\\", remove = TRUE) %>%
    select(where(~ any(. != "", na.rm = TRUE)))
}

for (i in 1:ncol(df99ProductPage)) {
  col_name <- names(df99ProductPage)[i]
  df99ProductPage <- df99ProductPage %>% 
    separate(col_name, into = c(paste0(col_name,"_1"), paste0(col_name,"_2")), sep = "\":\"", remove = TRUE) %>%
    select(where(~ any(. != "", na.rm = TRUE)))
}

for (i in 1:ncol(df99ProductPage)) {
  col_name <- names(df99ProductPage)[i]
  df99ProductPage <- df99ProductPage %>% 
    separate(col_name, into = c(paste0(col_name,"_1"), paste0(col_name,"_2")), sep = "\",\"", remove = TRUE)%>%
    select(where(~ any(. != "", na.rm = TRUE)))
}


library(purrr)
library(stringr)

df99ProductPage <- df99ProductPage %>%
  select(where(~any(str_detect(., "copy") | str_detect(., "specs"))))

# Identify the column with "premiseStatement" and the column after it
logical_vector <- apply(df99ProductPage, 2, function(x) any(str_detect(x, "premiseStatement")))

# Shift the logical vector one position to the left
logical_vector <- c(logical_vector[-1], FALSE)

# Create a new logical vector that combines the premiseStatement column and the one after it
premise_vector <- logical_vector | apply(df99ProductPage, 2, function(x) any(str_detect(x, "premiseStatement")))

# Create another logical vector for columns containing 'copy' or 'spec'
copy_spec_vector <- apply(df99ProductPage, 2, function(x) any(str_detect(x, c("copy", "spec"))))

# Combine the two logical vectors
combined_vector <- premise_vector | copy_spec_vector

# Subset the dataframe based on the combined vector
df99ProductPage_selected <- df99ProductPage[, combined_vector]

df99ProductPage_selected <- df99ProductPage_selected %>%
  mutate(across(everything(), ~str_replace_all(., ".*\\[", "")))












