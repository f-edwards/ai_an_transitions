rm(list=ls()); gc()
library(tidyverse)
library(lubridate)

### make point-in-time state caseloads for ICWA comps
#### read in 2019 afcars

afcars_st<-read_csv("~/Projects/ndacan_processing/data/afcars_all_events_state.csv")

pop<-read_fwf("~/Projects/data/us.1990_2019.singleages.adjusted.txt",
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
  group_by(year, state, st_fips, age, race_ethn) %>% 
  summarise(pop = sum(pop))


### compute total event rates cross age
### then imputed min/max for each state for AIAN for contact
totals_15_19<-afcars_st %>% 
  group_by(.imp, state, year, race_ethn) %>% 
  summarize(across(fc_entries:fc_tpr, sum)) %>% 
  filter(year>=2015, race_ethn == "AI/AN") %>% 
  group_by(state) %>% 
  summarize(fc_contact_mean = mean(fc_total_contact),
            fc_contact_mean_sd = sd(fc_total_contact)/sqrt(n()))

### join pop, compute per cap for states 
totals_15_19<-totals_15_19 %>% 
  rename(st_fips = state) %>% 
  left_join(pop %>% 
              mutate(st_fips = as.numeric(st_fips)) %>% 
              filter(year>=2015, 
                     race_ethn == "AI/AN",
                     age<18) %>% 
              group_by(state, st_fips) %>% 
              summarize(pop_5yr = sum(pop)/5)) 

### standardize to rates per 1k
totals_15_19<-totals_15_19 %>% 
  mutate(fc_contact_rt = fc_contact_mean / pop_5yr * 1e3,
         fc_contact_rt_sd = fc_contact_mean_sd / pop_5yr * 1e3) %>% 
  filter(state!="HI" & state!="DC") 

### OK - The top 10 are reasonable states to work with, after removing HI
### HI pop counts are strange, likely counting some Native HI as AIAN in FC
### ADD THAT AS A FOOTNOTE

### make FIG 1: COUNTS AND RATES

plot_dat<-


