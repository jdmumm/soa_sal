library(rvest)
library (httr)
library(tidyverse)

# func to create url given url_prefix and page num
paste_url <- function (url_pre, pg){
  paste0(url_pre, pg)
}

# func to extract first table from url 
scrape_table <- function(url) {
  html <- read_html(url)
  table_nodes <- html_nodes(html, "table") # select all tables
  table_nodes_1 <- table_nodes[1] 
  table_1 <- html_table(table_nodes_1, header = TRUE, fill = TRUE)
  table_df <- as.data.frame(table_1)
  return(table_df)
}

url_pre <- "https://www.openthebooks.com/alaska-state-employees/?Year_S=2022&PG="
p1 <- "1"
num_pages <- 100 

paste_url(url_pre, p1) -> url
scrape_table(url) -> dat

for (i in 2:num_pages) {
  paste_url(url_pre, i) -> url
  scrape_table(url) -> dat_i 
  rbind(dat,dat_i) -> dat
  cat("Page", i, "downloaded\n")
}

write.csv(dat, './data/sal_allDepts_22_10K.csv')





