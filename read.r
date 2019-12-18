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

afcars<-read_csv("./data/afcars_imputed_all_cases.csv",
                 col_types = cols(stfcid = "c")) 

ncands<-read_csv("./data/ncands_imputed.csv")

ncands_xwalk<-read_csv("./data/ncands_xwalk.csv",
                       col_types = cols(stfcid = "c"))

library(maps)
data(state.fips)
state.fips<-state.fips %>% 
  select(fips, abb) %>% 
  distinct() %>% 
  rename(staterr = abb)

### attach stfcid
ncands<-ncands %>% 
  left_join(ncands_xwalk)

### index files to crosswalk with varying imputations
# afcars_all<-read_csv("./data/afcars_id_xwalk.csv",
#                      col_types = cols(stfcid="c"))
# ncands_xwalk<-read_csv("./data/ncands_xwalk.csv")
# xwalk<-ncands_xwalk %>% 
#   left_join(afcars_all) %>% 
#   filter(fy<=2003, fy<2017) %>% 
#   filter(!(is.na(fy))) %>% 
#   select(stfcid, rptdt, fy) %>% 
#   rename(fc_year = fy)
# 
# write_csv(xwalk, "./data/xwalk_merged.csv")

### remove non-matches (prior to 2003, 2017)

ncands<-ncands %>% 
  filter(!(is.na(subyr)))

pop_aian<-pop %>% 
  filter(race_ethn=="AI/AN" | race_ethn=="White") %>% 
  group_by(year, age, race_ethn) %>% 
  summarise(pop = sum(pop)) %>% 
  write.csv("./data/pop_nat.csv", row.names=F)

ncands_aian<-ncands %>% 
  filter(race_ethn=="AI/AN" | race_ethn=="White") %>% 
  filter(.imp==1) %>% 
  write.csv("./data/ncands_subset.csv", row.names=F)

afcars_aian<-afcars %>% 
  filter(race_ethn=="AI/AN" | race_ethn=="White") %>% 
  filter(.imp==1) %>% 
  write.csv("./data/afcars_subset.csv", row.names=F)

q("no")
  