library(tidyverse)
library(googlesheets4)
library(RSocrata)
library(gargle)


gs4_deauth()

ss <- "1Nss0y2xUcMBt5cfWR2xXJX34d2Jb2KeaSQzAaabPVkQ"
dat <- read_sheet(ss)
df <- read.socrata("https://opendata.maryland.gov/resource/mgd3-qk8t.json")

df %>% sheet_write("1Nss0y2xUcMBt5cfWR2xXJX34d2Jb2KeaSQzAaabPVkQ", sheet = "df")


## cumulative positive cases, Maryland

df2 <- read.socrata("https://opendata.maryland.gov/resource/t7ek-pn7n.json")

df2 %>% sheet_write("1Nss0y2xUcMBt5cfWR2xXJX34d2Jb2KeaSQzAaabPVkQ", sheet = "cases_state")

## cases by county

df3 <- read.socrata("https://opendata.maryland.gov/resource/tm86-dujs.json")

df3 %>% sheet_write("1Nss0y2xUcMBt5cfWR2xXJX34d2Jb2KeaSQzAaabPVkQ", sheet = "cases_county")

## cases by age

df4 <- read.socrata("https://opendata.maryland.gov/resource/sjqg-bqsu.json")

df4 %>% sheet_write("1Nss0y2xUcMBt5cfWR2xXJX34d2Jb2KeaSQzAaabPVkQ", sheet = "cases_age")

## cases by race/ethnicity

df5 <- read.socrata("https://opendata.maryland.gov/resource/xnfm-sgpt.json")

df5 %>% sheet_write("1Nss0y2xUcMBt5cfWR2xXJX34d2Jb2KeaSQzAaabPVkQ", sheet = "cases_race")

## hospitalizations

df6 <- read.socrata("https://opendata.maryland.gov/resource/hd2f-3amb.json")

df6 %>% sheet_write("1Nss0y2xUcMBt5cfWR2xXJX34d2Jb2KeaSQzAaabPVkQ", sheet = "hospitalizations_by_day")

## deaths by day

df7 <- read.socrata("https://opendata.maryland.gov/resource/65qq-j35q.json")

df7 %>% sheet_write("1Nss0y2xUcMBt5cfWR2xXJX34d2Jb2KeaSQzAaabPVkQ", sheet = "deaths_by_day")

## deaths by county

df8 <- read.socrata("https://opendata.maryland.gov/resource/x28q-kc4a.json")

df8 %>% sheet_write("1Nss0y2xUcMBt5cfWR2xXJX34d2Jb2KeaSQzAaabPVkQ", sheet = "deaths_by_county")

## deaths by age

df9 <- read.socrata("https://opendata.maryland.gov/resource/ix2d-fenx.json")

df9 %>% sheet_write("1Nss0y2xUcMBt5cfWR2xXJX34d2Jb2KeaSQzAaabPVkQ", sheet = "deaths_by_age")

## deaths by race

df10 <- read.socrata("https://opendata.maryland.gov/resource/qwhp-7983.json")

df10 %>% sheet_write("1Nss0y2xUcMBt5cfWR2xXJX34d2Jb2KeaSQzAaabPVkQ", sheet = "deaths_by_race")

## deaths from cdc on age and race

df11 <- read.socrata("https://data.cdc.gov/resource/ks3g-spdg.json") %>% filter(state == "Maryland")

df11 %>% sheet_write("1Nss0y2xUcMBt5cfWR2xXJX34d2Jb2KeaSQzAaabPVkQ", sheet = "CDC_deats")
