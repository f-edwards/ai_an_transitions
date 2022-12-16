library(data.table)
library(tidyverse)

files<-c("~/Projects/ndacan_data/ncands/CF2015v4.tab",
         "~/Projects/ndacan_data/ncands/CF2016v3.tab",
         "~/Projects/ndacan_data/ncands/CF2017v2.tab",
         "~/Projects/ndacan_data/ncands/CF2018v1.tab",
         "~/Projects/ndacan_data/ncands/CF2019v1.tab")
dat_out<-list()
for(i in 1:length(files)){
  temp<-fread(files[i])
  ### subset to select states, MN, AK, MT
  temp<-temp %>% 
    rename_all(tolower) %>% 
    filter(staterr %in% c("MN", "MT", "AK")) %>% 
    mutate(race_ethn = case_when(
      chracai==1 ~ "AIAN",
      chracbl==1 ~ "Black",
      chracas==1 | chracnh==1 ~ "API",
      cethn==1 ~ "Latinx",
      chracwh == 1 ~ "White"
    )) %>%
    mutate(neglect = chmal1 == 2 | chmal2 == 2 | chmal3 == 2 | chmal4 == 2 |
             chmal1 == 3 | chmal2 == 3 | chmal3 == 3 | chmal4 == 3 ,
           phy_abuse = chmal1 == 1 | chmal2 == 1 | chmal3 == 1 | chmal4 == 1,
           sex_abuse = chmal1 == 4 | chmal2 == 4 | chmal3 == 4 | chmal4 == 4,
           other = chmal1 >4 | chmal2 >4 | chmal3 >4 | chmal4 >4 ) %>% 
    mutate(neglect = ifelse(is.na(neglect), F, neglect),
           phy_abuse = ifelse(is.na(phy_abuse), F, phy_abuse),
           sex_abuse = ifelse(is.na(sex_abuse), F, sex_abuse),
           other = ifelse(is.na(other), F, other)) %>% 
  select(subyr, staterr, chid, rptsrc, 
         chage, race_ethn, neglect, 
         phy_abuse, sex_abuse, other)
  
  ### deduplicate within years, preserve only first report annually per child
  temp<-temp %>% 
    mutate(stchid = paste(staterr, chid, sep = "")) %>% 
    distinct(stchid, .keep_all=T)
  
  dat_out[[i]]<-temp
}

ncands<-bind_rows(dat_out)

### convert rptsrc missings
ncands<-ncands %>% 
  mutate(rptsrc = ifelse(rptsrc==99, NA, rptsrc)) %>% 
  mutate(chage = ifelse(chage==77, 0, chage),
         chage = ifelse(chage>=99, NA, chage))

### 

#### MI
library(mice)
### convert categoricals to factors
ncands<-ncands %>% 
  mutate(across(c(subyr, staterr, rptsrc, race_ethn), as_factor)) 
### impute (TRIAL)
ncands_imp<-parlmice(ncands,
                 m = 10,
                 n.imp.core = 2,
                 n.core = 5)

dat_out<-mice::complete(ncands_imp, action = "long", include = F)

write_csv(dat_out, "./data/ncands_subset_imputed.csv")
