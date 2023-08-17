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



df6_sum <- df6 %>% mutate(count = acute+icu)

df6_sum <- transform(df6_sum, avg7 = rollmeanr(count, 7, fill = NA)) %>% select(reportdate, avg7)




df7 <- read.socrata("https://opendata.maryland.gov/resource/65qq-j35q.json")

df7 <- df7 %>% select(date, count) %>% mutate(date = as.Date(date))

df7 <- df7 %>% tail(n = 365) %>% mutate(count = as.numeric(count))

md_deaths <- transform(df7, avg7 = rollmeanr(count, 7, fill = NA)) %>% select(date, avg7)

write_csv(md_deaths, "md_deaths.csv")




## first part is creating the text in the first cell with the latest date and the most recent data for the second column

md_hosp2 <- df6_sum %>% tail(n = 1)

md_hosp2 <- md_hosp2 %>% mutate(txt = "Hospitalizations as of")

md_hosp2$MY <- paste(md_hosp2$txt, md_hosp2$reportdate)

md_hosp2  <- md_hosp2 %>% select(MY, avg7)



md_deaths2 <- md_deaths %>% tail(n = 1)

md_deaths2 <- md_deaths2 %>% mutate(txt = "Deaths reported as of")

md_deaths2$MY <- paste(md_deaths2$txt, md_deaths2$date)

md_deaths2  <- md_deaths2 %>% select(MY, avg7)



md_deaths2 <- md_deaths2 %>% mutate(extra = "deaths")
md_hosp2 <- md_hosp2 %>% mutate(extra = "hosp")

topper <- md_deaths2 %>% full_join(md_hosp2, by=c("extra", "MY", "avg7"))



md_hosp3 <- md_hosp %>% tail(n=1)

md_hosp3 <- md_hosp3 %>% pivot_longer(!reportdate, names_to = "type", values_to = "count") %>% summarise(count1 = sum(count)) %>% mutate(extra = "extra")

md_hosp4 <- md_hosp %>% tail(n=15) %>% arrange(desc(reportdate)) %>% tail(n=1)

md_hosp4 <- md_hosp4 %>% pivot_longer(!reportdate, names_to = "type", values_to = "count") %>% summarise(count14 = sum(count)) %>% mutate(extra = "extra")



md_hosp5 <- md_hosp3 %>% full_join(md_hosp4, by = "extra")

md_hosp5 <- md_hosp5 %>% summarise(two_week_chg = ((count1-count14)/count14)*100) %>% mutate(extra = "hosp")


md_deaths3 <- md_deaths %>% tail(n=1)

md_deaths3 <- md_deaths3 %>% mutate(extra = "extra") %>% select(avg7, extra)


md_deaths4 <- md_deaths %>% tail(n=15) %>% arrange(desc(date)) %>% tail(n=1)

md_deaths4 <- md_deaths4 %>% mutate(extra = "extra") %>% select(avg7, extra)



md_deaths5 <- md_deaths3 %>% full_join(md_deaths4, by = "extra")

md_deaths5 <- md_deaths5 %>% summarise(two_week_chg = ((avg7.x-avg7.y)/avg7.y)*100) %>% mutate(extra = "deaths")




topper2 <- md_hosp5 %>% full_join(md_deaths5, by=c("extra", "two_week_chg"))

topper <- topper %>% inner_join(topper2, by="extra")




hosp_mini <- df6_sum %>% tail(n=14) %>% mutate(order= c("a","b","c","d","e","f","g","h","i","j","k","l","m","n"))


hosp_mini <- hosp_mini %>% select(order, avg7) %>% pivot_wider(names_from = order, values_from = avg7) %>% mutate(extra = "hosp")



dea_mini <- md_deaths %>% tail(n=14) %>% mutate(order= c("a","b","c","d","e","f","g","h","i","j","k","l","m","n"))

dea_mini <- dea_mini  %>% select(order, avg7) %>% pivot_wider(names_from = order, values_from = avg7) %>% mutate(extra = "deaths")


mini <- hosp_mini %>% full_join(dea_mini, by=c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","extra"))



topper <- topper %>% full_join(mini, by="extra")

topper <- topper %>% select(MY, avg7, two_week_chg, a, b, c, d, e, f, g, h, i, j, k, l, m, n)

topper <- topper %>% rename(" "="MY") %>% rename("7-day average"="avg7") %>% rename("Two-week change"="two_week_chg")

write_csv(topper, "graphic-top.csv")




