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

# func to loop through pages and scrape
scrape_loop <- function(url_pre, num_pages){
  for (i in 2:num_pages) {
    paste_url(url_pre, i) -> url
    scrape_table(url) -> dat_i 
    rbind(dat,dat_i) -> dat
    cat("Page", i, "downloaded\n")
  }
  return(dat)
}

# Scrape to 10K recs
url_pre <- "https://www.openthebooks.com/alaska-state-employees/?Year_S=2022&PG="
p1 <- "1"
num_pages <- 100 

paste_url(url_pre, p1) -> url
scrape_table(url) -> dat
scrape_loop(url_pre, num_pages) -> top10K

# Scrape bottom 3,274. Above only returned top 10K of 13,274 tot recs. 
url_pre_2 <- 'https://www.openthebooks.com/alaska-state-employees/?Year_S=2022&F_Min_Amount_S=2.35&F_Max_Amount_S=' 
max <- as.numeric(gsub("\\$|,", "", tail(top10K, n = 1)$Annual.Wages)) - .01 
url_pre_3 <- paste0(url_pre_2, max, '&pg=')

num_pages <- 33
paste_url(url_pre_3, p1) -> url
scrape_table(url) -> dat
scrape_loop(url_pre_3, num_pages) -> bot3K

# Append and write
rbind(top10K, bot3K) -> all
write.csv(all, './data/sal_allDepts_22.csv')





