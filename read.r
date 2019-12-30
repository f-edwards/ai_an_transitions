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

ncands<-read_csv("./data/ncands_imputed.csv")

ncands_xwalk<-read_csv("./data/ncands_xwalk.csv",
                       col_types = cols(stfcid = "c"))
  
library(maps)
data(state.fips)
state.fips<-state.fips %>% 
  select(fips, abb) %>% 
  distinct() %>% 
  rename(staterr = abb)

zz <- file("all.Rout", open = "wt")
sink(zz)
sink(zz, type = "message")
### attach stfcid
ncands<-ncands %>%
  left_join(ncands_xwalk)

### get needed first events

ncands<-ncands %>%
  filter(!(is.na(subyr)))

ncands_first<-ncands %>%
  arrange(.imp, rptdt) %>% 
  group_by(.imp, stfcid) %>%
  slice(1)
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

ncands_first_subst_nat<-ncands_first_subst %>%
  group_by(.imp, chage, race_ethn, year) %>%
  summarise(var = n()) %>%
  rename(age = chage) %>%
  left_join(pop) %>%
  ungroup() %>%
  write_csv("./data/ncands_first_sub.csv")

afcars_first <- afcars %>% 
  arrange(.imp, year, age) %>% 
  group_by(.imp, stfcid) %>% 
  slice(1) %>% 
  ungroup()

sink("log.txt")
print("afcars_first length")
print(nrow(afcars_first))
print("unique fcid")
print(length(unique(afcars$stfcid)))
sink()

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
              slice(1) %>% 
              mutate(ncands_inv = T))

sink("log2.txt")
print("afcars_inv post join length")
print(nrow(afcars_inv))
print("unique ids")
print(length(unique(afcars_inv$stfcid)))
sink()

afcars_inv<-afcars_inv %>% 
  mutate(rpt_year = year(rptdt)) %>% 
  filter(!(is.na(ncands_inv))) 

# remove when year(rptdt)>year

afcars_inv<-afcars_inv %>%
  filter(year>=rpt_year)

afcars_inv_nat<-afcars_inv %>%
  group_by(.imp, age, race_ethn, year) %>%
  summarise(var = n()) %>%
  left_join(pop) %>%
  ungroup() %>%
  write_csv("./data/afcars_fc_post_inv.csv")

afcars_sub<-afars_first %>% 
  left_join(ncands_first_subst %>% 
              select(.imp, stfcid, rptdt)) %>%
  mutate(rptyr=year(rptdt)) %>% 
  filter(!(is.na(rptyr))) %>% 
  filter(year>=rptyr)
  
afcars_sub_nat<-afcars_sub %>% 
  group_by(.imp, age, race_ethn, year) %>%
  summarise(var = n()) %>%
  left_join(pop) %>%
  ungroup() %>%
  write_csv("./data/afcars_fc_post_sub.csv")
  
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
  left_join(pop) %>%
  write_csv("./data/afcars_tpr.csv")

afcars_icwa<-afcars %>%
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
sink(type="message")
sink()
q("no")
  