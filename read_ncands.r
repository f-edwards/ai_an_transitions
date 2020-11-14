rm(list=ls()); gc()
library(data.table)
library(tidyverse)
library(lubridate)
library(mice)

ncands_path<-"~/Projects/ndacan_data/ncands/"
ncands_files<-paste(ncands_path,
                    list.files(ncands_path),
                    sep = "")[1:16]

ncands<-lapply(ncands_files, fread)

for(i in 1:length(ncands_files)){
  ncands[[i]]<-ncands[[i]]%>%
    rename_all(tolower) %>%
    mutate(rptdt = ymd(rptdt)) %>%
    mutate(race_ethn =
             ifelse(chracbl==1,
                    "Black",
                    ifelse(chracai==1, "AI/AN",
                           ifelse(chracas==1 | chracnh==1,
                                  "Asian/PI",
                                  ifelse(cethn==1, "Hispanic",
                                          ifelse(chracwh == 1, "White",
                                                 NA)))))) 
  }

##### make an FCID chid xwalk file
ncands_xwalk<-list()
for(i in 1:length(ncands_files)){
  temp<-ncands[[i]] %>% 
    select(chid, staterr, 
           afcarsid, rptdt, subyr) 
  ncands_xwalk[[i]]<-temp
}

ncands_xwalk<-bind_rows(ncands_xwalk) 

library(maps)
data(state.fips)
state.fips<-state.fips %>% 
  select(fips, abb) %>% 
  distinct() %>% 
  rename(staterr = abb) 

## add missings, HI AND AK
state.fips<-state.fips %>% 
  bind_rows(data.frame(fips = c(2, 15), staterr = c("AK", "HI")))


ncands_xwalk<-ncands_xwalk %>% 
  left_join(state.fips) %>% 
  filter(afcarsid!="",
         staterr!="XX") 

ncands_xwalk<-ncands_xwalk%>% 
  mutate(stfcid = paste(fips, afcarsid, sep="")) 

ncands_xwalk<-ncands_xwalk %>% 
  select(staterr, chid, stfcid, rptdt, subyr)

write_csv(ncands_xwalk, 
          "~/Projects/ai_an_transitions/data/ncands_xwalk.csv")

#q("no")