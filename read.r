rm(list=ls()); gc()
library(tidyverse)
library(lubridate)

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
                 col_types = cols(stfcid = "c")) %>% 
  filter(year>2003) 

ncands<-read_csv("./data/ncands_imputed.csv")
ncands_xwalk<-read_csv("./data/ncands_xwalk.csv")

library(maps)
data(state.fips)
state.fips<-state.fips %>% 
  select(fips, abb) %>% 
  distinct() %>% 
  rename(staterr = abb)

### attach stfcid
ncands<-ncands %>% 
  left_join(ncands_xwalk)

afcars_xwalk<-read_csv("./data/afcars_xwalk.csv")

pop_aian<-pop %>% 
  filter(race_ethn=="AI/AN") %>% 
  group_by(year, age) %>% 
  summarise(pop = sum(pop)) %>% 
  write.csv("./data/pop_aian_nat.csv", row.names=F)

ncands_aian<-ncands %>% 
  filter(race_ethn=="AI/AN") %>% 
  filter(.imp==1) %>% 
  write.csv("./data/ncands_subset.csv", row.names=F)

afcars_aian<-afcars %>% 
  filter(race_ethn=="AI/AN") %>% 
  filter(.imp==1) %>% 
  write.csv("./data/afcars_subset.csv", row.names=F)
  