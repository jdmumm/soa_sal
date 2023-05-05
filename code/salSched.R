library(pdftools)
library(stringr)
library (tidyverse)
library(plotly)

pdf_path <- "0722gpw_200.pdf"
pdf_text <- pdf_text(pdf_path)
table_text <- str_extract_all(pdf_text, "(?<=\\s)[0-9,]{5}+\\.[0-9]{2}(?=\\s)")[[1]]
num_cols <- 24 
table_matrix <- matrix(table_text, ncol = num_cols, byrow = TRUE)
table_df <- as.data.frame(table_matrix, stringsAsFactors = FALSE)
table_df <- data.frame(lapply(table_df, function(x) as.numeric(gsub(",", "", x))))
colnames(table_df) <- setdiff(LETTERS, c("H", "I"))
table_df$Range <- 5:27

write.csv(table_df, "salSched_c23_220701.csv", row.names =FALSE)
read.csv("salSched_c23_220701.csv") -> sal

sal[,1:24] <- sal[,1:24] * 26 # biweekly to annual. Exclude range column

# add extra steps to reflect biennial after step j
sal %>% 
  mutate(J2 = J, K2 = K, L2 = L, M2 = M, N2 = N, O2 = O, P2 = P, Q2 = Q, R2 = R, S2 = S, T2 = T, U2 = U,
         V2 = V, W2 = W, X2 = X, Y2 = Y, Z2 = Z) -> sal
sal_long <- gather(sal, key = "Step", value = "Salary", -Range)

write.csv(sal_long, "salSched_c23_220701_ann_long.csv")

