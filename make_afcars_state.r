rm(list=ls()); gc()
library(tidyverse)
library(lubridate)

setwd("~/Projects/ai_an_transitions/")

afcars<-read_csv("./data/afcars_imputed_all_cases.csv",
                 col_types = cols(stfcid = "c")) %>% 
  filter(year>=2014)

ncands_xwalk<-read_csv("./data/ncands_xwalk.csv",
                       col_types = cols(stfcid = "c")) %>% 
  distinct() %>% 
  filter(!(is.na(stfcid)),
         nchar(stfcid)>2) %>% 
  filter(staterr!="XX")

afcars_first<-afcars %>% 
  filter(entered==1, totalrem==1) %>% 
  group_by(.imp, stfcid) %>% 
  slice(1)

afcars_first_st<-afcars_first %>% 
  group_by(.imp, state, age, race_ethn, year) %>% 
   summarise(var = n()) %>% 
   write_csv("./data/afcars_first_fc_st.csv")

afcars_inv<-afcars_first %>% 
  left_join(ncands_xwalk %>% 
              select(staterr, stfcid, rptdt) %>% 
              arrange(rptdt) %>% 
              group_by(stfcid) %>% 
              slice(1) %>% 
              ungroup())

afcars_inv<-afcars_inv %>% 
  mutate(rpt_year = year(rptdt)) %>% 
  filter(!(is.na(rptdt))) 

# remove when year(rptdt)>year, get sequence of report -> fc, not fc -> report

### for apples to apples, filter
### this for matches on stfcid in afcars_first

afcars_inv<-afcars_inv %>%
  filter(year>=rpt_year)

afcars_inv_nat<-afcars_inv %>%
  group_by(.imp, state, age, race_ethn, year) %>%
  summarise(var = n()) %>%
  filter(year>=2003) %>% 
  write_csv("./data/afcars_fc_post_inv_st.csv")


#### make afcars_sub table
ncands_first_sub<-read_csv("./data/ncands_first_victim_index.csv")
### join to xwalk to get stfcid
ncands_first_sub_walk<-ncands_xwalk %>% 
  mutate(st_id = paste(staterr, chid, sep = "")) %>% 
  left_join(ncands_first_sub %>% 
              mutate(first_sub = T))

## eval matches
# nrow(ncands_first_sub)
# sum(ncands_first_sub_walk$first_sub, na.rm = T)
# head(ncands_first_sub_walk)

afcars_sub_nat<-afcars_first %>% 
  left_join(ncands_first_sub_walk) %>% 
  filter(!(is.na(first_sub))) %>% 
  group_by(.imp, state, age, race_ethn, year) %>%
  summarise(var = n()) %>%
  filter(year>=2003) %>% 
  write_csv("./data/afcars_fc_post_sub_st.csv")


#### PAUSE HERE AND CHECK ON fc|inv, fc|sub. Do they make sense?


afcars_tpr<-afcars %>%
  filter(stfcid%in%afcars_first$stfcid) %>% 
  filter(istpr==1) %>%
  arrange(.imp, year) %>% 
  group_by(.imp, stfcid) %>%
  slice(1) %>% 
  ungroup() 

afcars_tpr_state<-afcars_tpr %>%
  group_by(.imp, state, age, race_ethn, year) %>%
  summarise(var = n()) %>%
  ungroup() %>%
  write_csv("./data/afcars_tpr_st.csv")

### for apples to apples, filter
### this for matches on stfcid in afcars_first
### some states have non_icwa/foster_case ratios > 1
afcars_first_aian<-afcars_first %>% 
  filter(race_ethn == "AI/AN")

#### look at length of time in placement?

afcars_icwa<-afcars %>%
  filter(stfcid %in% afcars_first_aian$stfcid)%>% 
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
### check for white kids in here
### CHECK UNIQUE IDs HERE VS AIAN FIRST ENTRIES

afcars_icwa_state<-afcars_icwa %>%
  group_by(.imp, state, age, race_ethn, year) %>%
  summarise(var = n()) %>%
  write_csv("./data/afcars_non_icwa_st.csv")
