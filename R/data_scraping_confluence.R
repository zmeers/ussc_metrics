##################################################
## Project: Scrape data from USSC confluence page
## Date: Fri Mar 22 11:47:59 2019
## Author: Zoe Meers
##################################################


library(ussc)
library(tidyverse)
library(janitor)
library(lubridate)
library(here)

# page IDs

# Impact

impact_id <- "950239621"

grab_impact_data <- ussc_confluence_excel(impact_id)

# Key Performance Indicators

kpi_2019_data <- ussc_kpi_table(id = "950075519") %>% 
  mutate(publication_date = gsub("13 November", "13 November 2018", publication_date),
         publication_date = as_date(dmy(publication_date),"%Y-%m-%d"),
         tracking_date = gsub(" 19", "2019", tracking_date),
         tracking_date = zoo::as.yearmon(tracking_date),
         tracking_date = dmy(paste("01-", tracking_date , sep ="")))
 
kpi_2018_2_data <- ussc_kpi_table("790822968") %>% 
  mutate(publication_date = gsub("13 November", "13 November 2018", publication_date),
         publication_date = gsub("2 November", "2 November 2018", publication_date),
         publication_date = gsub("5 December", "5 December 2018", publication_date),
         publication_date = gsub("12 October", "12 October 2018", publication_date),
         publication_date = gsub("26 October", "26 October 2018", publication_date),
         publication_date = gsub("3 August", "3 August 2018", publication_date),
         publication_date = gsub("9 November", "9 November 2018", publication_date),
         publication_date = gsub("10 October", "10 October 2018", publication_date),
         publication_date = gsub("16 November", "16 November 2018", publication_date),
         publication_date = gsub("28 August", "28 August 2018", publication_date),
         publication_date = gsub("22 September", "22 September 2018", publication_date),
         publication_date = as_date(dmy(publication_date),"%Y-%m-%d"),
         tracking_date = gsub(" 18", "2018", tracking_date),
         tracking_date = zoo::as.yearmon(tracking_date),
         tracking_date = dmy(paste("01-", tracking_date , sep ="")))
  

kpi_2018_1_data <- ussc_kpi_table("464322561") %>% 
  mutate(publication_date = gsub("2 November", "2 November 2017", publication_date),
         publication_date = gsub("13 November", "13 November 2017", publication_date),
         publication_date = gsub("18 July", "18 July 2017", publication_date),
         publication_date = gsub("18 August", "18 August 2017", publication_date),
         publication_date = gsub("25 July", "25 July 2017", publication_date),
         publication_date = gsub("5 December", "5 December 2017", publication_date),
         publication_date = gsub("12 October", "12 October 2017", publication_date),
         publication_date = gsub("26 October", "26 October 2017", publication_date),
         publication_date = gsub("3 August", "3 August 2017", publication_date),
         publication_date = gsub("9 November", "9 November 2017", publication_date),
         publication_date = gsub("10 October", "10 October 2017", publication_date),
         publication_date = gsub("16 November", "16 November 2017", publication_date),
         publication_date = gsub("28 August", "28 August 2017", publication_date),
         publication_date = gsub("22 September", "22 September 2017", publication_date),
         publication_date = as_date(dmy(publication_date),"%Y-%m-%d"),
         tracking_date = gsub(" 18", "2018", tracking_date),
         tracking_date = gsub(" 17", "2017", tracking_date),
         tracking_date = zoo::as.yearmon(tracking_date),
         tracking_date = dmy(paste("01-", tracking_date , sep ="")))


kpi_2017_data <- ussc_kpi_table("30867457") %>% 
  mutate(
    tracking_date = case_when(
    tracking_date == "f" ~ as_date("2017-02-01"),
    tracking_date == "m" ~ as_date("2017-03-01"),
    tracking_date == "a" ~ as_date("2017-04-01"),
    tracking_date == "m 2" ~ as_date("2017-05-01"),
    tracking_date == "j" ~ as_date("2017-06-01"),
    tracking_date == "j 2" ~ as_date("2017-07-01"),
    tracking_date == "a 2" ~ as_date("2017-08-01"),
    tracking_date == "s" ~ as_date("2017-09-01"),
    tracking_date == "o" ~ as_date("2017-10-01"),
    tracking_date == "n" ~ as_date("2017-11-01"),
    tracking_date == "d" ~ as_date("2017-12-01")
  ),
  publication_date = dmy(paste(publication_date, " 2017", sep =""))
  )


kpi_data <- bind_rows(kpi_2017_data, kpi_2018_1_data, kpi_2018_2_data, kpi_2019_data) %>%
    arrange(report_title, tracking_date) %>% 
    distinct() %>% 
    filter(!str_detect(report_type, "(Reports)|(Total)")) %>% 
    mutate(value = str_remove(value, "[.]"))

write_rds(kpi_data, here("data/kpi_publications_17_19.RDS"))





# Digital KPIs


april_2018 <- "657981441"

dig_kpi_april_18 <- ussc_confluence_table(april_2018) 


may_2018 <- "708313113"

dig_kpi_may_18 <- ussc_confluence_table(may_2018) 


june_2018 <- "763396101"

dig_kpi_june_18 <- ussc_confluence_table(june_2018) 


july_2018 <- "790986761"

dig_kpi_july_18 <- ussc_confluence_table(july_2018) 


august_2018 <- "817397775"

dig_kpi_august_18 <- ussc_confluence_table(august_2018) 

september_2018 <- "938835975"

dig_kpi_sep_18 <- ussc_confluence_table(september_2018) 


october_2018 <- "938967081"

dig_kpi_oct_18 <- ussc_confluence_table(october_2018) 


november_2018 <- "938836145"

dig_kpi_nov_18 <- ussc_confluence_table(november_2018) 


december_2018 <- "942931969"

dig_kpi_dec_18 <- ussc_confluence_table(december_2018) 

digital_kpi_stats_april_dec_18 <- list(dig_kpi_april_18, dig_kpi_may_18, dig_kpi_june_18, dig_kpi_july_18, dig_kpi_august_18, dig_kpi_sep_18, dig_kpi_oct_18, dig_kpi_nov_18, dig_kpi_dec_18)


kpi_pageviews_section <- list(dig_kpi_april_18, dig_kpi_may_18, dig_kpi_june_18, dig_kpi_july_18, dig_kpi_august_18, dig_kpi_sep_18, dig_kpi_oct_18, dig_kpi_nov_18, dig_kpi_dec_18) %>% 
  map(~.[[2]]) %>% 
  bind_rows(.id = "months") %>% 
  mutate(date = case_when(
    months == '1' ~ as_date("2018-04-01"),
    months == '2' ~ as_date("2018-05-01"),
    months == '3' ~ as_date("2018-06-01"),
    months == '4' ~ as_date("2018-07-01"),
    months == '5' ~ as_date("2018-08-01"),
    months == '6' ~ as_date("2018-09-01"),
    months == '7' ~ as_date("2018-10-01"),
    months == '8' ~ as_date("2018-11-01"),
    months == '9' ~ as_date("2018-12-01")
  )) %>% 
  select(-months) %>% 
  mutate(pageviews = gsub(",", "", pageviews),
         pageviews = as.numeric(pageviews)) %>% 
  rename('section' = 'x')


kpi_pageviews_page <- list(dig_kpi_april_18, dig_kpi_may_18, dig_kpi_june_18, dig_kpi_july_18, dig_kpi_august_18, dig_kpi_sep_18, dig_kpi_oct_18, dig_kpi_nov_18, dig_kpi_dec_18) %>% 
  map(~.[[3]]) %>% 
  bind_rows(.id = "months") %>% 
  mutate(date = case_when(
    months == '1' ~ as_date("2018-04-01"),
    months == '2' ~ as_date("2018-05-01"),
    months == '3' ~ as_date("2018-06-01"),
    months == '4' ~ as_date("2018-07-01"),
    months == '5' ~ as_date("2018-08-01"),
    months == '6' ~ as_date("2018-09-01"),
    months == '7' ~ as_date("2018-10-01"),
    months == '8' ~ as_date("2018-11-01"),
    months == '9' ~ as_date("2018-12-01")
  )) %>% 
  select(-months) %>% 
  mutate(pageviews = gsub(",", "", pageviews),
         pageviews = as.numeric(pageviews)) %>% 
  rename('page' = 'x')

kpi_pageviews_people <- list(dig_kpi_april_18, dig_kpi_may_18, dig_kpi_june_18, dig_kpi_july_18, dig_kpi_august_18, dig_kpi_sep_18, dig_kpi_oct_18, dig_kpi_nov_18, dig_kpi_dec_18) %>% 
  map(~.[[6]]) %>% 
  bind_rows(.id = "months") %>% 
  mutate(date = case_when(
    months == '1' ~ as_date("2018-04-01"),
    months == '2' ~ as_date("2018-05-01"),
    months == '3' ~ as_date("2018-06-01"),
    months == '4' ~ as_date("2018-07-01"),
    months == '5' ~ as_date("2018-08-01"),
    months == '6' ~ as_date("2018-09-01"),
    months == '7' ~ as_date("2018-10-01"),
    months == '8' ~ as_date("2018-11-01"),
    months == '9' ~ as_date("2018-12-01")
  )) %>% 
  select(-months) %>% 
  mutate(pageviews = gsub(",", "", pageviews),
         pageviews = as.numeric(pageviews)) %>% 
  rename('person' = 'x')

save(digital_kpi_stats_april_dec_18, kpi_pageviews_page, kpi_pageviews_section,kpi_pageviews_people, file = here("data", "digital_kpi_2018.RData"))
