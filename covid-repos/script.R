library(tidyverse)
library(RSocrata)

df <- read.socrata("https://opendata.maryland.gov/resource/mgd3-qk8t.json")

write_csv(df, "example.csv")

df2 <- read.socrata("https://opendata.maryland.gov/resource/t7ek-pn7n.json")

write_csv(df2, "example2.csv")


