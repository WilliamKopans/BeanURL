
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



         
