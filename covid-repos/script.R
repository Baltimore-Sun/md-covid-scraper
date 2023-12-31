library(tidyverse)
library(RSocrata)
library(zoo)
library(lubridate)


df2 <- read.socrata("https://opendata.maryland.gov/resource/t7ek-pn7n.json") %>% select(date, count) %>% mutate(date = as.Date(date)) %>% tail(n = 365) %>% mutate(count = as.numeric(count))

md_cases <- df2 %>% mutate(Diff = count - lag(count))

md_cases <- transform(md_cases, avg7 = rollmeanr(Diff, 7, fill = NA)) %>% select(date, avg7)



write_csv(md_cases, "md_cases.csv")



df6 <- read.socrata("https://opendata.maryland.gov/resource/hd2f-3amb.json") %>% select(reportdate, acute, icu) %>% mutate(reportdate = as.Date(reportdate)) %>% tail(n = 365) %>% mutate(acute = as.numeric(acute)) %>% mutate(icu = as.numeric(icu))


md_hosp <- transform(df6, avg7_acute = rollmeanr(acute, 7, fill = NA)) %>% transform(df6, avg7_icu = rollmeanr(icu, 7, fill = NA)) %>% select(reportdate, avg7_acute, avg7_icu)

md_hosp <- md_hosp %>% rename("Acute" = "avg7_acute") %>% rename("ICU" = "avg7_icu")

write_csv(md_hosp, "md_hosp.csv")





df6_sum <- df6 %>% mutate(count = acute+icu)

df6_sum <- transform(df6_sum, avg7 = rollmeanr(count, 7, fill = NA)) %>% select(reportdate, avg7)




df7 <- read.socrata("https://opendata.maryland.gov/resource/65qq-j35q.json") %>% select(date, count) %>% mutate(date = as.Date(date)) %>% tail(n = 365) %>% mutate(count = as.numeric(count))



md_deaths <- transform(df7, avg7 = rollmeanr(count, 7, fill = NA)) %>% select(date, avg7)

write_csv(md_deaths, "md_deaths.csv")



md_hosp2 <- df6_sum %>% tail(n = 1)

md_hosp2 <- md_hosp2 %>% mutate(reportdate = format(as.Date(md_hosp2$reportdate, "%Y-%m-%d"), "%m-%d-%Y"))

md_hosp2 <- md_hosp2 %>% mutate(txt = "Hospitalizations as of")

md_hosp2$MY <- paste(md_hosp2$txt, md_hosp2$reportdate)

md_hosp2  <- md_hosp2 %>% select(MY, avg7)



md_deaths2 <- md_deaths %>% tail(n = 1)

md_deaths2 <- md_deaths2 %>% mutate(date = format(as.Date(md_deaths2$date, "%Y-%m-%d"), "%m-%d-%Y"))


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


df8 <- read.socrata("https://opendata.maryland.gov/resource/x28q-kc4a.json") %>% select(date:worcester) %>% mutate(date = as.Date(date)) %>% tail(n = 21) %>% mutate_if(is.character, as.numeric) %>% fill(allegany:worcester, .direction = "down")

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

county_deaths1 <- df8 %>% select(allegany:worcester)

county_deaths1 <- transform(county_deaths1, avg7 = rollmeanr(county_deaths1, k = 7, fill = NA))



county_deaths <- county_deaths1 %>% tail(n=1) %>% select(`avg7.allegany`:`avg7.worcester`) %>% mutate(blank = "blank")

county_deaths <- county_deaths %>% rename("Allegany"="avg7.allegany") %>% 
  rename("Anne Arundel"="avg7.anne_arundel") %>% 
  rename("Baltimore County"="avg7.baltimore") %>% 
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
county_deaths <- county_deaths %>% full_join(county_table, by="blank")

county_deaths <- county_deaths %>% select(-c(blank))

county_deaths <- county_deaths %>% pivot_longer(!date, names_to = "county", values_to = "value")

county <- c("Allegany","Anne Arundel","Baltimore County","Baltimore City","Calvert" ,"Caroline","Carroll","Cecil","Charles","Dorchester","Frederick","Garrett" ,"Harford","Howard","Kent","Montgomery" ,"Prince George's","Queen Anne's","Somerset","St. Mary's","Talbot","Washington","Wicomico","Worcester")
population <- c("67267","593286","846161","569931","94573","33433","175305","104942","170102","32726","287079","28579","263867","335411","19320","1052521","946971","51711","24546","114877","37932","155590","104664","53866")

pop <- data.frame(county, population)

county_deaths2 <- county_deaths %>% full_join(pop, by="county")

county_deaths2 <- county_deaths2 %>% mutate(population = as.numeric(population))

county_deaths2 <- county_deaths2 %>% mutate(avg7_per10k = value/population*10000)

county_deaths2 <- county_deaths2 %>% select(date, county, avg7_per10k)

county_deaths2 <- county_deaths2 %>% rename("Date"="date") %>% rename("County"="county") %>% rename("7-day avg. deaths per 10,000"="avg7_per10k")



### starting county cases

df3 <- read.socrata("https://opendata.maryland.gov/resource/tm86-dujs.json") %>% select(date:worcester) %>% mutate(date = as.Date(date)) %>% tail(n = 21) %>% mutate_if(is.character, as.numeric) %>% fill(allegany:worcester, .direction = "down")

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



county_cases <- county_cases %>% tail(n=1) %>% select(`avg7.allegany`:`avg7.worcester`) %>% mutate(blank = "blank")

county_cases <- county_cases %>% rename("Allegany"="avg7.allegany") %>% 
  rename("Anne Arundel"="avg7.anne_arundel") %>% 
  rename("Baltimore County"="avg7.baltimore") %>% 
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

county_cases <- county_cases %>% full_join(county_table2, by="blank")

county_cases <- county_cases %>% select(-c(blank))

county_cases <- county_cases %>% pivot_longer(!date, names_to = "county", values_to = "seven_day_average")

county_cases2 <- county_cases %>% full_join(pop, by="county")

county_cases2 <- county_cases2 %>% mutate(population = as.numeric(population))

county_cases2 <- county_cases2 %>% mutate(avg7_per10k = seven_day_average/population*10000)

county_cases2 <- county_cases2 %>% select(date, county, avg7_per10k)

county_cases2 <- county_cases2 %>% rename("Date"="date") %>% rename("County"="county") %>% rename("7-day avg. cases per 10,000"="avg7_per10k")




###take last two weeks and pivot wider for deaths

county_death_chg <- county_deaths1 %>% tail(n=14) %>% select(`avg7.allegany`:`avg7.worcester`) %>% mutate(order= c("a","b","c","d","e","f","g","h","i","j","k","l","m","n"))


county_death_chg <- county_death_chg %>% mutate(avg7.allegany = avg7.allegany/67267*10000) %>% 
  mutate(avg7.anne_arundel = avg7.anne_arundel/593286*10000) %>% 
  mutate(avg7.baltimore = avg7.baltimore/846161*10000) %>%
  mutate(avg7.baltimore_city = avg7.baltimore_city/569931*10000) %>%
  mutate(avg7.calvert = avg7.calvert/94573*10000) %>%
  mutate(avg7.caroline = avg7.caroline/33433*10000) %>%
  mutate(avg7.carroll = avg7.carroll/175305*10000) %>%
  mutate(avg7.cecil = avg7.cecil/104942*10000) %>%
  mutate(avg7.charles = avg7.charles/170102*10000) %>%
  mutate(avg7.dorchester = avg7.dorchester/32726*10000) %>%
  mutate(avg7.frederick = avg7.frederick/287079*10000) %>%
  mutate(avg7.garrett = avg7.garrett/28579*10000) %>%
  mutate(avg7.harford = avg7.harford/263867*10000) %>%
  mutate(avg7.howard = avg7.howard/335411*10000) %>%
  mutate(avg7.kent = avg7.kent/19320*10000) %>%
  mutate(avg7.montgomery = avg7.montgomery/1052521*10000) %>%
  mutate(avg7.prince_georges = avg7.prince_georges/946971*10000) %>%
  mutate(avg7.queen_annes = avg7.queen_annes/51711*10000) %>%
  mutate(avg7.somerset = avg7.somerset/24546*10000) %>%
  mutate(avg7.st_marys = avg7.st_marys/114877*10000) %>%
  mutate(avg7.talbot = avg7.talbot/37932*10000) %>%
  mutate(avg7.washington = avg7.washington/155590*10000) %>%
  mutate(avg7.wicomico = avg7.wicomico/104664*10000) %>%
  mutate(avg7.worcester = avg7.worcester/53866*10000)

county_death_chg <- county_death_chg %>% rename("Allegany"="avg7.allegany") %>% 
  rename("Anne Arundel"="avg7.anne_arundel") %>% 
  rename("Baltimore County"="avg7.baltimore") %>% 
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

county_death_chg2 <- county_death_chg %>% pivot_longer(!order, names_to = "County", values_to = "Value")

county_death_chg2 <- county_death_chg2 %>% pivot_wider(names_from = order, values_from =  Value)


big_table <- county_deaths2 %>% full_join(county_death_chg2, by="County")




big_table <- big_table %>% full_join(county_cases2, by="County")


big_table <- big_table %>% select(-c(Date.y)) %>% select(-c(Date.x)) %>% rename("Jurisdiction"="County")


write_csv(big_table, "big-table.csv")




df9 <- read.socrata("https://opendata.maryland.gov/resource/ix2d-fenx.json") %>% mutate(date = as.Date(date)) %>% tail(n = 1) %>% select(date:age_unknown)

df9 <- df9 %>% rename("Age 0-9"="age_0_to_9") %>% 
  rename("Age 10-19"="age_10_to_19") %>%
  rename("Age 20-29"="age_20_to_29") %>% 
  rename("Age 30-39"="age_30_to_39") %>%
  rename("Age 40-49"="age_40_to_49") %>%
  rename("Age 50-59"="age_50_to_59") %>%
  rename("Age 60-69"="age_60_to_69") %>%
  rename("Age 70-79"="age_70_to_79") %>%
  rename("Age 80+"="age_80plus") %>%
  rename("Unknown"="age_unknown")
df9 <- df9 %>% pivot_longer(!date, names_to = "age", values_to = "count") %>% mutate(count = as.numeric(count))

df9 <- df9 %>% mutate(total_deaths = sum(count))


df9  <- df9 %>% mutate(pct_age_deaths = count/total_deaths*100)

age <- c("Age 0-9",
         "Age 10-19",
         "Age 20-29",
         "Age 30-39",
         "Age 40-49",
         "Age 50-59",
         "Age 60-69",
         "Age 70-79",
         "Age 80+"
)


age_population <- c("737067",
                    "772508",
                    "809165",
                    "833654",
                    "738594",
                    "824760",
                    "742474",
                    "466028",
                    "240410")


age_pop <- data.frame(age, age_population) %>% mutate(age_population = as.numeric(age_population))

age_pop <- age_pop %>% mutate(pct_pop = (age_population/6164660)*100)

age_graphic <- df9 %>% left_join(age_pop, by="age") %>% select(age, pct_age_deaths, pct_pop)

age_graphic  <- age_graphic %>% rename("Percentage of deaths"="pct_age_deaths") %>% rename("Percentage of population"="pct_pop")

write_csv(age_graphic, "age_graphic.csv")


df10 <- read.socrata("https://opendata.maryland.gov/resource/qwhp-7983.json") %>% tail(n = 1) %>% select(date:not_available)


df10 <- df10 %>% rename("Black or African American"="african_american") %>%
  rename("White"="white") %>% 
  rename("Hispanic"="hispanic") %>%
  rename("Asian"="asian") %>% 
  rename("Other"="other") %>%
  rename("Unknown"="not_available")


df10 <- df10 %>% pivot_longer(!date, names_to = "race", values_to = "count") %>% mutate(count = as.numeric(count))


df10 <- df10 %>% mutate(total_deaths = sum(count))


df10  <- df10 %>% mutate(pct_race_deaths = count/total_deaths*100)

race <- c("Black or African American",
          "White",
          "Hispanic",
          "Asian",
          "Other"
)


race_ethn_population <- c("1951658",
                          "3530719",
                          "706816",
                          "435183",
                          "247100"
)


race_ethn_pop <- data.frame(race, race_ethn_population) %>% mutate(race_ethn_population = as.numeric(race_ethn_population))

race_ethn_pop <- race_ethn_pop %>% mutate(pct_pop = (race_ethn_population/6164660)*100)

race_ethn_graphic <- df10 %>% left_join(race_ethn_pop, by="race") %>% select(race, pct_race_deaths, pct_pop)

race_ethn_graphic <- race_ethn_graphic %>% rename("Percentage of deaths"="pct_race_deaths") %>% rename("Percentage of population"="pct_pop")

write_csv(race_ethn_graphic, "race_ethn_graphic.csv")



cdc <- read.socrata("https://data.cdc.gov/resource/akn2-qxic.json") %>% filter(state == "Maryland") %>% mutate(week_end_date = as.Date(week_end_date)) %>% arrange(week_end_date) %>% tail(n=24)

md_cdc <- cdc %>% select(county, fips_code, week_end_date, total_adm_all_covid_confirmed_level, total_adm_all_covid_confirmed_per_100k)


total_adm_all_covid_confirmed_level <- c("Medium (10.9-19.9)","High (≥20.0)")



additions <- data.frame(total_adm_all_covid_confirmed_level)

md_cdc <- md_cdc %>% full_join(additions, by="total_adm_all_covid_confirmed_level")



write_csv(md_cdc, "md_cdc.csv")









