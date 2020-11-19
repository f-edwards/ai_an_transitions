rm(list=ls()); gc()
library(tidyverse)
library(lubridate)

setwd("~/Projects/ai_an_transitions/")

pop<-read_fwf("./data/us.1990_2017.singleages.adjusted.txt",
              fwf_widths(c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8),
                         c("year", "state", "st_fips",
                           "cnty_fips", "reg", "race",
                           "hisp", "sex", "age", "pop")))
pop<-pop%>%
  mutate(pop = as.integer(pop))%>%
  mutate(race_ethn =
           case_when(
             race==1 & hisp ==0 ~ "White",
             race==2 ~ "Black",
             race==3 ~ "AI/AN",
             race==4 ~ "Asian/PI",
             hisp==1 ~ "Hispanic")) %>%
  mutate(age = as.integer(age)) %>%
  filter(year>=2000)

pop<-pop %>% 
  group_by(year, age, race_ethn) %>% 
  summarise(pop = sum(pop))

afcars<-read_csv("./data/afcars_imputed_all_cases.csv",
                 col_types = cols(stfcid = "c")) 

### for first cut, add imp uncertainty later
afcars<-afcars %>% 
  filter(.imp==1)

ncands_xwalk<-read_csv("./data/ncands_xwalk.csv",
                       col_types = cols(stfcid = "c"))

afcars_first<-afcars %>% 
  filter(entered==1, totalrem==1) %>% 
  filter(!(duplicated(stfcid)))

afcars_first_nat<-afcars_first %>% 
  group_by(.imp, age, race_ethn, year) %>% 
  summarise(var = n()) %>% 
  left_join(pop) %>% 
  ungroup() %>% 
  write_csv("./data/afcars_first_fc.csv")

afcars_inv<-afcars_first %>% 
  left_join(ncands_xwalk %>% 
              select(stfcid, rptdt) %>% 
              arrange(rptdt) %>% 
              group_by(stfcid) %>% 
              slice(1) %>% 
              ungroup())

afcars_inv<-afcars_inv %>% 
  mutate(rpt_year = year(rptdt)) %>% 
  filter(!(is.na(rptdt))) 

# remove when year(rptdt)>year, get sequence of report -> fc, not fc -> report

afcars_inv<-afcars_inv %>%
  filter(year>=rpt_year)

afcars_inv_nat<-afcars_inv %>%
  group_by(.imp, age, race_ethn, year) %>%
  summarise(var = n()) %>%
  left_join(pop) %>%
  ungroup() %>%
  filter(year>=2003) %>% 
  write_csv("./data/afcars_fc_post_inv.csv")

afcars_tpr<-afcars %>%
  filter(istpr==1) %>%
  arrange(.imp, year) %>% 
  group_by(.imp, stfcid) %>%
  slice(1) %>% 
  ungroup() 

afcars_tpr_nat<-afcars_tpr %>%
  group_by(.imp, age, race_ethn, year) %>%
  summarise(var = n()) %>%
  ungroup() %>%
  ### fill in zeroes
  complete(.imp, age, race_ethn, year,
           fill = list(var=0)) %>% 
  left_join(pop) %>%
  write_csv("./data/afcars_tpr.csv")

afcars_icwa<-afcars %>%
  filter(stfcid%in%afcars_first$stfcid) %>% 
  filter(race_ethn=="AI/AN") %>%
  mutate(icwa_compliant = case_when(
    curplset==2 ~ T,
    rf1amakn==1 ~ T,
    rf2amakn==1 ~ T,
    T ~ F)) %>%
  filter(icwa_compliant == F) %>%
  arrange(.imp, year) %>% 
  group_by(.imp, stfcid) %>%
  slice(1) %>% 
  ungroup() 

afcars_icwa_nat<-afcars_icwa %>%
  group_by(.imp, age, race_ethn, year) %>%
  summarise(var = n()) %>%
  ungroup() %>% 
  complete(.imp, age, race_ethn, year,
           fill = list(var=0)) %>% 
  left_join(pop) %>%
  ungroup() %>%
  write_csv("./data/afcars_non_icwa.csv")

# pop_nat<-pop %>%
#   filter(race_ethn=="AI/AN" | race_ethn=="White") %>%
#   group_by(year, age, race_ethn) %>%
#   summarise(pop = sum(pop))
# 
# write.csv(pop_nat, "./data/pop_nat.csv", row.names=F)
# 
# ncands_aian<-ncands %>%
#   filter(race_ethn=="AI/AN" | race_ethn=="White") %>%
#   filter(.imp==1) %>%
#   write.csv("./data/ncands_subset.csv", row.names=F)
# 
# afcars_aian<-afcars %>%
#   filter(race_ethn=="AI/AN" | race_ethn=="White") %>%
#   filter(.imp==1) %>%
#   write.csv("./data/afcars_subset.csv", row.names=F)
