library(tidyverse)
library(RSocrata)
library(zoo)


df2 <- read.socrata("https://opendata.maryland.gov/resource/t7ek-pn7n.json")

df2 <- df2 %>% select(date, count) %>% mutate(date = as.Date(date))

df2 <- df2 %>% tail(n = 365) %>% mutate(count = as.numeric(count))

md_cases <- df2 %>% mutate(Diff = count - lag(count))

md_cases <- transform(md_cases, avg7 = rollmeanr(Diff, 7, fill = NA)) %>% select(date, avg7)



write_csv(md_cases, "md_cases.csv")







