library(tidyverse)

path<-"~/Projects/ndacan_data/afcars_adopt_fc19/Data/Text Files/"

#### list and read in adoption afcars flat files using .tab extension
files<-list.files(path)
files<-files[str_sub(files, -4, -1)==".tab"]
files<-paste(path, files, sep = "")

#### use purrr:map_dfr() to read and join into single df
afcars_adopt<-files %>% 
  map_dfr(read_tsv)




#### make state_year adoption counts for
### aian / non-aian / white 
### by kin/non-kin
### by foster parent aian / non-aian

afcars_adopt_st<-afcars_adopt %>% 
  mutate(race_ethn = case_when(
    AMIAKN == 1 ~ "AIAN",
    BLKAFRAM == 1 ~ "Black",
    ASIAN == 1 ~ "API",
    HAWAIIPI == 1 ~ "API",
    HISORGIN == 1 ~ "Latinx",
    WHITE == 1 ~ "White"
  )) %>% 
  group_by(FY, STATE, St, race_ethn) %>% 
  summarise(n = n(),
            n_kin = sum(STPARENT == 1 | OTHEREL == 1, na.rm=T),
            n_aian = sum(AMIAKND==1 | AMIAKND==1, na.rm=T),
            n_kin_or_aian = sum((STPARENT == 1 | OTHEREL == 1)|
                                     (AMIAKND==1 | AMIAKND==1), na.rm=T))

### for ICWA comps want annual adoption placement rates and under 21 point in time
afcars_adopt_mean<-afcars_adopt_st %>% 
  ungroup() %>% 
  mutate(race_ethn = ifelse(race_ethn=="AIAN", "AIAN", "non-AIAN"),
         race_ethn = ifelse(is.na(race_ethn), "non-AIAN", race_ethn)) %>%
  select(-FY) %>% 
  group_by(St, STATE, race_ethn) %>% 
  summarise_all(mean)

### going to use the caseload measure for cjlr
### will probe the annual x age numbers later

afcars_adopt_caseload_19<-afcars_adopt %>% 
  filter(AgeAtAdopt + (2019 - FY)  < 21) %>% 
  mutate(race_ethn = case_when(
    AMIAKN == 1 ~ "AIAN",
    BLKAFRAM == 1 ~ "Black",
    ASIAN == 1 ~ "API",
    HAWAIIPI == 1 ~ "API",
    HISORGIN == 1 ~ "Latinx",
    WHITE == 1 ~ "White"
  )) %>% 
  mutate(race_ethn = ifelse(race_ethn=="AIAN", "AIAN", "non-AIAN"),
         race_ethn = ifelse(is.na(race_ethn), "non-AIAN", race_ethn)) %>% 
  group_by(STATE, St, race_ethn) %>% 
  summarise(n = n(),
            n_kin = sum(STPARENT == 1 | OTHEREL == 1, na.rm=T),
            n_aian = sum(AMIAKND==1 | AMIAKND==1, na.rm=T),
            n_kin_or_aian = sum((STPARENT == 1 | OTHEREL == 1)|
                                  (AMIAKND==1 | AMIAKND==1), na.rm=T)) 


