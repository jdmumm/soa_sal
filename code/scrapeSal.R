library(rvest)
library (httr)
library(tidyverse)

# Functions ----
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
  dat <- data.frame()
  for (i in 1:num_pages) {
    paste0(url_pre, i) -> url
    scrape_table(url) -> dat_i 
    rbind(dat,dat_i) -> dat
    cat("Page", i, "downloaded\n")
  }
  return(dat)
}

# Scrape ----
#Top 10K recs 
url_pre <- "https://www.openthebooks.com/alaska-state-employees/?Year_S=2022&PG="
num_pages <- 100 
scrape_loop(url_pre, num_pages) -> top10K

#Bottom 3,274 recs. Add max parameter to url to return additional recs. 
url_pre_2 <- 'https://www.openthebooks.com/alaska-state-employees/?Year_S=2022&F_Min_Amount_S=2.35&F_Max_Amount_S=' 
max <- as.numeric(gsub("\\$|,", "", tail(top10K, n = 1)$Annual.Wages)) - .01 
url_pre_2 <- paste0(url_pre_2, max, '&pg=')
num_pages_2 <- 33

scrape_loop(url_pre_2, num_pages_2) -> bot3K

# Append and write
rbind(top10K, bot3K) -> all
all %>% write.csv('./data/sal_allDepts_22.csv')
