library(tidyverse)
library(googlesheets4)
library(RSocrata)
library(gargle)
library(googledrive)

df <- read.socrata("https://opendata.maryland.gov/resource/mgd3-qk8t.json")

write_csv(df, "example.csv")
