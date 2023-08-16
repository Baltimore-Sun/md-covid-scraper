library(tidyverse)
library(RSocrata)

df <- read.socrata("https://opendata.maryland.gov/resource/mgd3-qk8t.json")

write_csv(df, "example.csv")



