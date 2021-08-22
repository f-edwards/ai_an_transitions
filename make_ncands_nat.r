rm(list=ls()); gc()
library(tidyverse)
library(lubridate)

setwd("~/Projects/ai_an_transitions/")

# ncands<-read_csv("./data/ncands_imputed.csv")
# 
# ncands<-ncands %>% 
#   filter(.imp==1)

ncands_xwalk<-read_csv("./data/ncands_xwalk.csv",
                       col_types = cols(stfcid = "c")) %>%
  mutate(st_id = paste(staterr, chid, sep = "")) %>% 
  filter(!duplicated(st_id))
  
library(maps)
data(state.fips)
state.fips<-state.fips %>% 
  select(fips, abb) %>% 
  distinct() %>% 
  rename(staterr = abb)


### attach stfcid
ncands<-ncands %>%
  right_join(ncands_xwalk)

### get needed first events

ncands<-ncands %>%
  filter(!(is.na(subyr)))

ncands_first<-ncands %>%
  arrange(.imp, rptdt) %>% 
  group_by(.imp, stfcid) %>%
  slice(1) %>% 
  ungroup() %>%
  mutate(year = year(rptdt))

ncands_first_nat<-ncands_first %>%
  group_by(.imp, chage, race_ethn, year) %>%
  summarise(var = n()) %>%
  rename(age = chage) %>%
  left_join(pop) %>%
  ungroup() %>%
  write_csv("./data/ncands_first_inv.csv")

ncands_first_subst<-ncands %>%
  filter(rptvictim==1) %>%
  arrange(.imp, rptdt) %>% 
  group_by(.imp, stfcid) %>%
  slice(1) %>% 
  ungroup() %>%
  mutate(year = year(rptdt)) 

#### make ncandsID xwalk here to set up fc|sub

ncands_first_subst_nat<-ncands_first_subst %>%
  group_by(.imp, chage, race_ethn, year) %>%
  summarise(var = n()) %>%
  rename(age = chage) %>%
  left_join(pop) %>%
  ungroup() %>%
  write_csv("./data/ncands_first_sub.csv")


