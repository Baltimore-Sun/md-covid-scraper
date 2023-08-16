library(tidyverse)
library(RSocrata)
library(zoo)

df2 <- read.socrata("https://opendata.maryland.gov/resource/t7ek-pn7n.json")

df2 <- df2 %>% select(date, count) %>% mutate(date = as.Date(date))

df2 <- df2 %>% tail(n = 365) %>% mutate(count = as.numeric(count))

md_cases <- df2 %>% mutate(Diff = count - lag(count))

md_cases <- transform(md_cases, avg7 = rollmeanr(Diff, 7, fill = NA)) %>% select(date, avg7)

write_csv(md_cases, "md_cases.csv")



df6 <- read.socrata("https://opendata.maryland.gov/resource/hd2f-3amb.json")

df6 <- df6 %>% select(reportdate, acute, icu) %>% mutate(reportdate = as.Date(reportdate))

df6 <- df6 %>% tail(n = 365) %>% mutate(acute = as.numeric(acute)) %>% mutate(icu = as.numeric(icu))

md_hosp <- transform(df6, avg7_acute = rollmeanr(acute, 7, fill = NA)) %>% transform(df6, avg7_icu = rollmeanr(icu, 7, fill = NA)) %>% select(reportdate, avg7_acute, avg7_icu)

write_csv(md_hosp, "md_hosp.csv")


df7 <- read.socrata("https://opendata.maryland.gov/resource/65qq-j35q.json")

df7 <- df7 %>% select(date, count) %>% mutate(date = as.Date(date))

df7 <- df7 %>% tail(n = 365) %>% mutate(count = as.numeric(count))

md_deaths <- transform(df7, avg7 = rollmeanr(count, 7, fill = NA)) %>% select(date, avg7)

write_csv(md_deaths, "md_deaths.csv")


