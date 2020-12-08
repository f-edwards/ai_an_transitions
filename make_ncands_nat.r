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

ncands<-read_csv("./data/ncands_imputed.csv")

ncands<-ncands %>% 
  filter(.imp==1)

ncands_xwalk1<-read_csv("./data/ncands_xwalk.csv",
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


