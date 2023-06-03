
library(tidyverse)
library(rvest)
library(httr)

URL <- "https://www.llbean.com/llb/shop/816?page=school-backpacks-and-lunch-boxes"
URL <- "https://www.llbean.com/llb/shop/818?page=school-backpacks&csp=f&bc=50-816&start=1&viewCount=48&nav=ln-816"



response <- content(GET(URL), as = "text")

entities <- html_nodes(read_html(response), xpath = "//*/text()")

df <- data.frame(entity = trimws(entities, "both"), stringsAsFactors = FALSE)

df <- as.data.frame(df[df$entity != "", ])

df <- df %>% filter(startsWith(`df[df$entity != \"\", ]`, "window.__INITIAL_STATE_"))

df2 <- as.data.frame(t(strsplit(df[1,1], "\\},\\{")[[1]]))
df3 <- df2 %>%
  pivot_longer(everything(), names_to = "Key", values_to = "Value") %>%
  select(-Key) %>%
  filter(grepl("^\"page_productName", Value)) %>%
  separate(Value, into = paste0("col", 1:15), sep = '":"') 

for (i in 1:ncol(df3)) {
  col_name <- paste0("col", i)
  df3 <- separate(df3, col = col_name, into = col_name, sep = '","')
}

df4 <- df3 %>%
  select(col3, col4, col7, col9) %>% 
  rename(NamesForURL = col3,
         SomeTag = col4,
         NamesReadable = col7,
         InternalIDs = col9) %>% 
  mutate(NamesForURL = paste0("https://www.llbean.com/llb/shop/",SomeTag,"?page=", NamesForURL))



         





# 
# checkUpdate <- function(URL){
# 
#     # Attempt to execute the code within the try block
#     response <- content(GET(URL), as = "text")
#     
#     entities <- html_nodes(read_html(response), xpath = "//*/text()")
#   
#     # Create a dataframe with the entities
#     df <- data.frame(entity = trimws(entities, "both"), stringsAsFactors = FALSE)
#   
#     # Remove empty rows
#     df <- as.data.frame(df[df$entity != "", ])
#   
#     # Add a new column to the dataframe that indicates whether the previous row contains "color"
#     df$prev_row_is_color <- lag(grepl("Colors", df$`df[df$entity != "", ]`, ignore.case = TRUE), default = FALSE)
#     
#     # Find rows where the previous row contains "color"
#     target_row <- which(df$prev_row_is_color)
#     
#     filtered_df2 <- as.data.frame(df[,1]) %>%
#       filter(row_number() %in% target_row,
#       is.na(as.numeric(`df[, 1]`)),
#       !grepl("^window.__llbeanUdal", `df[, 1]`)) %>%
#       mutate(`df[, 1]` = str_remove(`df[, 1]`, ", 32L|, 40L|, 24L|, 42L|, 16L")) %>% 
#       mutate(`df[, 1]` = str_remove(`df[, 1]`, ","),
#       URLs = paste0("https://www.llbean.com/llb/shop/118985?page=", gsub(" ", "-", `df[, 1]`)))
#     
#     
#   
# }
# 
# 
# 
# 
# split_row2 <- strsplit(as.character(df[1139,1]), "`\`")
# 
# split_df2 <- data.frame(split_row2 = unlist(split_row2), stringsAsFactors = FALSE)
# 
# split_row2 <- strsplit(as.character(split_df2[1,]), ",")
# 
# split_df2 <- data.frame(split_row2 = unlist(split_row2), stringsAsFactors = FALSE)
# 
# 
# 
# 
# # elements <- strsplit(df[1139,1], "\\\\")[[1]]
# elements <- strsplit(df[1139,1], "\\},\\{")[[1]]
# 
# # Transpose the list to create a data frame
# df2 <- as.data.frame(t(elements))
# 
# # df22 <- df22 %>%
# #   pivot_longer(everything(), names_to = "Key", values_to = "Value") %>%
# #   select(-Key) %>%
# #   filter(grepl("^\"page_productName", Value)) %>%
# #   separate(Value, into = paste0("col", 1:1000), sep = '":"')
# 
# 
# df22 <- df2 %>%
#   pivot_longer(everything(), names_to = "Key", values_to = "Value") %>%
#   select(-Key) %>%
#   filter(grepl("^\"page_productName", Value)) %>%
#   separate(Value, into = paste0("col", 1:15), sep = '":"') %>% 
#   separate(everything(), into = paste0("col", 1:15), sep = '","') %>% 
#   separate(col2, into = c("col2", paste0("col", 1:4)), sep = '","minF') 
# 
# # %>%
#   # select(starts_with("minFullPrice_f"))
# 
# 
# 
# 
# 
# df2 <- as.data.frame(t(strsplit(df[1139, 1], "\\},\\{")[[1]]))
# df22 <- df2 %>%
#   pivot_longer(everything(), names_to = "Key", values_to = "Value") %>%
#   select(-Key) %>%
#   filter(grepl("^\"page_productName", Value)) %>%
#   separate(Value, into = paste0("col", 1:15), sep = '":"') 
# 
# # Loop through each column and apply the separate function
# for (i in 1:ncol(df22)) {
#   col_name <- paste0("col", i)
#   df22 <- separate(df22, col = col_name, into = col_name, sep = '","')
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
#     