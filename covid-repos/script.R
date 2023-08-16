library(tidyverse)
library(RSocrata)

df <- read.socrata("https://opendata.maryland.gov/resource/mgd3-qk8t.json")

write_csv(df, "example.csv")

## cumulative positive cases, Maryland

df2 <- read.socrata("https://opendata.maryland.gov/resource/t7ek-pn7n.json")

write_csv(df2, "md-cases.csv")

## cases by county

df3 <- read.socrata("https://opendata.maryland.gov/resource/tm86-dujs.json")

write_csv(df3, "county-cases.csv")

## cases by age

df4 <- read.socrata("https://opendata.maryland.gov/resource/sjqg-bqsu.json")

write_csv(df4, "age-cases.csv")

## cases by race/ethnicity

df5 <- read.socrata("https://opendata.maryland.gov/resource/xnfm-sgpt.json")

write_csv(df5, "race-cases.csv")

## hospitalizations

df6 <- read.socrata("https://opendata.maryland.gov/resource/hd2f-3amb.json")

write_csv(df6, "md-hosp.csv")

## deaths by day

df7 <- read.socrata("https://opendata.maryland.gov/resource/65qq-j35q.json")

write_csv(df7, "md-deaths.csv")

## deaths by county

df8 <- read.socrata("https://opendata.maryland.gov/resource/x28q-kc4a.json")

write_csv(df8, "county-deaths.csv")

## deaths by age

df9 <- read.socrata("https://opendata.maryland.gov/resource/ix2d-fenx.json")

write_csv(df9, "age-deaths.csv")

## deaths by race

df10 <- read.socrata("https://opendata.maryland.gov/resource/qwhp-7983.json")

write_csv(df10, "race-deaths.csv")


