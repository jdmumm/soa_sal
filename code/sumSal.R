library(tidyverse)

dat <- read.csv('./data/sal_allDepts_22.csv')

#summarize
dat %>% mutate(wages = as.numeric(gsub("\\$|,", "", Annual.Wages))) -> dat
dat %>% group_by (Title) %>% summarize (
    mean = mean(wages),
    min = min(wages),
    max = max(wages),
    n = n()
) -> summ
summ

write.csv(summ,"./data/sal_allDepts_22_sum.csv")
