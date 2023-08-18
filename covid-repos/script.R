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




df8 <- read.socrata("https://opendata.maryland.gov/resource/x28q-kc4a.json")

df8 <- df8 %>% select(date:worcester) %>% mutate(date = as.Date(date))

df8 <- df8 %>% tail(n = 21) %>% mutate_if(is.character, as.numeric)

df8 <- df8 %>% fill(allegany:worcester, .direction = "down")

df8 <- df8 %>% mutate(allegany = allegany - lag(allegany)) %>%
  mutate(anne_arundel = anne_arundel - lag(anne_arundel))  %>%
  mutate(baltimore = baltimore - lag(baltimore))  %>%
  mutate(baltimore_city = baltimore_city - lag(baltimore_city))  %>%
  mutate(calvert = calvert - lag(calvert))  %>%
  mutate(caroline = caroline - lag(caroline))  %>%
  mutate(carroll = carroll - lag(carroll))  %>%
  mutate(cecil = cecil - lag(cecil))  %>%
  mutate(charles = charles - lag(charles))  %>%
  mutate(dorchester = dorchester - lag(dorchester))  %>%
  mutate(frederick = frederick- lag(frederick))  %>%
  mutate(garrett = garrett - lag(garrett))  %>%
  mutate(harford = harford - lag(harford))  %>%
  mutate(howard = howard - lag(howard))  %>%
  mutate(kent = kent- lag(kent))  %>%
  mutate(montgomery = montgomery - lag(montgomery))  %>%
  mutate(prince_georges = prince_georges - lag(prince_georges)) %>%
  mutate(queen_annes = queen_annes - lag(queen_annes))  %>%
  mutate(somerset= somerset - lag(somerset))  %>%
  mutate(st_marys = st_marys - lag(st_marys))  %>%
  mutate(talbot = talbot - lag(talbot))  %>%
  mutate(washington = washington- lag(washington))  %>%
  mutate(wicomico = wicomico - lag(wicomico))  %>%
  mutate(worcester = worcester - lag(worcester))

county_deaths <- df8 %>% select(allegany:worcester)

county_deaths <- transform(county_deaths, avg7 = rollmeanr(county_deaths, k = 7, fill = NA))


map_deaths <- county_deaths %>% tail(n=1) %>% select(`avg7.allegany`:`avg7.worcester`) %>% mutate(blank = "blank")

map_deaths <- map_deaths %>% rename("Allegany"="avg7.allegany") %>% 
  rename("Anne Arundel"="avg7.anne_arundel") %>% 
  rename("Baltimore"="avg7.baltimore") %>% 
  rename("Baltimore City"="avg7.baltimore_city") %>% 
  rename("Calvert"="avg7.calvert") %>% 
  rename("Caroline"="avg7.caroline") %>% 
  rename("Carroll"="avg7.carroll") %>% 
  rename("Cecil"="avg7.cecil") %>% 
  rename("Charles"="avg7.charles") %>% 
  rename("Dorchester"="avg7.dorchester") %>% 
  rename("Frederick"="avg7.frederick") %>% 
  rename("Garrett"="avg7.garrett") %>% 
  rename("Harford"="avg7.harford") %>% 
  rename("Howard"="avg7.howard") %>% 
  rename("Kent"="avg7.kent") %>% 
  rename("Montgomery"="avg7.montgomery") %>% 
  rename("Prince George's"="avg7.prince_georges") %>% 
  rename("Queen Anne's"="avg7.queen_annes") %>% 
  rename("Somerset"="avg7.somerset") %>% 
  rename("St. Mary's"="avg7.st_marys") %>% 
  rename("Talbot"="avg7.talbot") %>% 
  rename("Washington"="avg7.washington") %>% 
  rename("Wicomico"="avg7.wicomico") %>% 
  rename("Worcester"="avg7.worcester")

county_table <- df8 %>% select(date) %>% tail(n=1) %>% mutate(blank = "blank")

map_deaths <- map_deaths %>% full_join(county_table, by="blank")

map_deaths <- map_deaths %>% select(-c(blank))

map_deaths <- map_deaths %>% pivot_longer(!date, names_to = "county", values_to = "seven_day_average")

county <- c("Allegany","Anne Arundel","Baltimore","Baltimore City","Calvert" ,"Caroline","Carroll","Cecil","Charles","Dorchester","Frederick","Garrett" ,"Harford","Howard","Kent","Montgomery" ,"Prince George's","Queen Anne's","Somerset","St. Mary's","Talbot","Washington","Wicomico","Worcester")
population <- c("67267","593286","846161","569931","94573","33433","175305","104942","170102","32726","287079","28579","263867","335411","19320","1052521","946971","51711","24546","114877","37932","155590","104664","53866")

pop <- data.frame(county, population)

map_deaths <- map_deaths %>% full_join(pop, by="county")

map_deaths <- map_deaths %>% mutate(population = as.numeric(population))

map_deaths <- map_deaths %>% mutate(avg7_per10k = seven_day_average/population*10000)

map_deaths <- map_deaths %>% select(date, county, avg7_per10k)

map_deaths <- map_deaths %>% rename("Date"="date") %>% rename("County"="county") %>% rename("7-day average per 10,000"="avg7_per10k")

write_csv(map_deaths, "map_deaths.csv")




df3 <- read.socrata("https://opendata.maryland.gov/resource/tm86-dujs.json")

df3 <- df3 %>% select(date:worcester) %>% mutate(date = as.Date(date))

df3 <- df3 %>% tail(n = 21) %>% mutate_if(is.character, as.numeric)

df3 <- df3 %>% fill(allegany:worcester, .direction = "down")

df3 <- df3 %>% mutate(allegany = allegany - lag(allegany)) %>%
  mutate(anne_arundel = anne_arundel - lag(anne_arundel))  %>%
  mutate(baltimore = baltimore - lag(baltimore))  %>%
  mutate(baltimore_city = baltimore_city - lag(baltimore_city))  %>%
  mutate(calvert = calvert - lag(calvert))  %>%
  mutate(caroline = caroline - lag(caroline))  %>%
  mutate(carroll = carroll - lag(carroll))  %>%
  mutate(cecil = cecil - lag(cecil))  %>%
  mutate(charles = charles - lag(charles))  %>%
  mutate(dorchester = dorchester - lag(dorchester))  %>%
  mutate(frederick = frederick- lag(frederick))  %>%
  mutate(garrett = garrett - lag(garrett))  %>%
  mutate(harford = harford - lag(harford))  %>%
  mutate(howard = howard - lag(howard))  %>%
  mutate(kent = kent- lag(kent))  %>%
  mutate(montgomery = montgomery - lag(montgomery))  %>%
  mutate(prince_georges = prince_georges - lag(prince_georges)) %>%
  mutate(queen_annes = queen_annes - lag(queen_annes))  %>%
  mutate(somerset= somerset - lag(somerset))  %>%
  mutate(st_marys = st_marys - lag(st_marys))  %>%
  mutate(talbot = talbot - lag(talbot))  %>%
  mutate(washington = washington- lag(washington))  %>%
  mutate(wicomico = wicomico - lag(wicomico))  %>%
  mutate(worcester = worcester - lag(worcester))

county_cases <- df3 %>% select(allegany:worcester)

county_cases <- transform(county_cases, avg7 = rollmeanr(county_cases, k = 7, fill = NA))

map_cases <- county_cases %>% tail(n=1) %>% select(`avg7.allegany`:`avg7.worcester`) %>% mutate(blank = "blank")

map_cases <- map_cases %>% rename("Allegany"="avg7.allegany") %>% 
  rename("Anne Arundel"="avg7.anne_arundel") %>% 
  rename("Baltimore"="avg7.baltimore") %>% 
  rename("Baltimore City"="avg7.baltimore_city") %>% 
  rename("Calvert"="avg7.calvert") %>% 
  rename("Caroline"="avg7.caroline") %>% 
  rename("Carroll"="avg7.carroll") %>% 
  rename("Cecil"="avg7.cecil") %>% 
  rename("Charles"="avg7.charles") %>% 
  rename("Dorchester"="avg7.dorchester") %>% 
  rename("Frederick"="avg7.frederick") %>% 
  rename("Garrett"="avg7.garrett") %>% 
  rename("Harford"="avg7.harford") %>% 
  rename("Howard"="avg7.howard") %>% 
  rename("Kent"="avg7.kent") %>% 
  rename("Montgomery"="avg7.montgomery") %>% 
  rename("Prince George's"="avg7.prince_georges") %>% 
  rename("Queen Anne's"="avg7.queen_annes") %>% 
  rename("Somerset"="avg7.somerset") %>% 
  rename("St. Mary's"="avg7.st_marys") %>% 
  rename("Talbot"="avg7.talbot") %>% 
  rename("Washington"="avg7.washington") %>% 
  rename("Wicomico"="avg7.wicomico") %>% 
  rename("Worcester"="avg7.worcester")

county_table2 <- df3 %>% select(date) %>% tail(n=1) %>% mutate(blank = "blank")

map_cases <- map_cases %>% full_join(county_table2, by="blank")

map_cases <- map_cases %>% select(-c(blank))

map_cases <- map_cases %>% pivot_longer(!date, names_to = "county", values_to = "seven_day_average")



map_cases <- map_cases %>% full_join(pop, by="county")

map_cases <- map_cases %>% mutate(population = as.numeric(population))

map_cases <- map_cases %>% mutate(avg7_per10k = seven_day_average/population*10000)

map_cases <- map_cases %>% select(date, county, avg7_per10k)

map_cases <- map_cases %>% rename("Date"="date") %>% rename("County"="county") %>% rename("7-day average per 10,000"="avg7_per10k")

write_csv(map_cases, "map_cases.csv")





