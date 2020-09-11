rm(list=ls()); gc()
library(tidyverse)
library(lubridate)
library(gridExtra)
library(RColorBrewer)
theme_set(theme_bw())
source("lifetable.r")
pop<-read_fwf("./data/us.1990_2018.singleages.adjusted.txt",
              fwf_widths(c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8),
                         c("year", "state", "st_fips",
                           "cnty_fips", "reg", "race",
                           "hisp", "sex", "age", "pop")),
              col_types = "iccciiiiii")

pop<-pop %>% 
  filter(age<=18, race == 3 | (race==1 & hisp==0), year >=2003) %>% 
  mutate(pop = as.integer(pop),
         age = as.integer(age),
         race_ethn = 
           case_when(
             race==1 & hisp ==0 ~ "White",
             race==3 ~ "AI/AN"))%>%
  group_by(year, age, race_ethn) %>% 
  summarise(pop = sum(pop))

format_data<-function(dat){
  dat<-dat %>% 
    filter(year>=2003, year<2017)%>% 
    filter(age<=18) %>% 
    filter(.imp!=0) %>% 
    filter(race_ethn == "AI/AN" | race_ethn == "White") %>% 
    select(-pop) %>% 
    complete(.imp, age, race_ethn, year, fill = list(var = 0)) %>% 
    left_join(pop)
}

ncands_inv<-read_csv("./data/ncands_first_inv.csv") %>% 
  format_data()
ncands_sub<-read_csv("./data/ncands_first_sub.csv")%>% 
  format_data()


afcars_fc<-read_csv("./data/afcars_first_fc.csv")%>% 
  format_data()
afcars_tpr<-read_csv("./data/afcars_tpr.csv")%>% 
  format_data()
afcars_non_icwa<-read_csv("./data/afcars_non_icwa.csv")%>% 
  format_data()
afcars_fc_inv<-read_csv("./data/afcars_fc_post_inv.csv")%>% 
  format_data()
afcars_fc_sub<-read_csv("./data/afcars_fc_post_sub.csv")%>% 
  format_data()

### read in new first investigation csv
### run lifetable by year

investigation_tables<-list()
years<-unique(ncands_inv$year)
race_id<-unique(ncands_inv$race_ethn)
index<-1
for(i in 1:5){
  for(j in 1:length(years)){
    for(r in 1:length(race_id)){
      temp<-ncands_inv %>% 
        filter(year==years[j])
      temp<-temp %>% 
        filter(race_ethn == race_id[r])
      temp<-temp %>% 
        filter(.imp==i)
      investigation_tables[[index]]<-make_life_table(temp)
      index<-index+1
    }
  }
}
investigation_tables<-bind_rows(investigation_tables)

investigation_tables_c<-investigation_tables %>% 
  filter(age==18) %>% 
  group_by(year, race_ethn) %>% 
  summarise(c_mn = mean(c), cmax = max(c), cmin = min(c))


investigation_tables_pooled<-list()
race_id<-unique(ncands_inv$race_ethn)
imps<-unique(ncands_inv$.imp)
index<-1
for(i in 1:length(imps)){
  for(r in 1:length(race_id)){
    temp<-ncands_inv %>% 
      filter(year>2011) 
    temp<-temp %>% 
      filter(race_ethn == race_id[r])
    temp<-temp %>% 
      filter(.imp==i)
    temp<-temp %>% 
      group_by(.imp, age, race_ethn) %>% 
      summarise(var = sum(var), pop = sum(pop)) %>% 
      ungroup()
    investigation_tables_pooled[[index]]<-make_life_table(temp)
    index<-index+1
  }
}
investigation_tables_pooled<-bind_rows(investigation_tables_pooled)

## ## ## ## ## ## ## ## ## ## ## ## ## 
## substantiation
## ## ## ## ## ## ## ## ## ## ## ## ## 
### run lifetable by year
subst_tables<-list()
years<-unique(ncands_sub$year)
race_id<-unique(ncands_inv$race_ethn)
index<-1
for(i in 1:5){
  for(j in 1:length(years)){
    for(r in 1:length(race_id)){
      temp<-ncands_sub %>%
        filter(year==years[j])
      temp<-temp %>%
        filter(race_ethn == race_id[r])
      temp<-temp %>%
        filter(.imp==i)
      subst_tables[[index]]<-make_life_table(temp)
      index<-index+1
    }
  }
}
subst_tables<-bind_rows(subst_tables)

subst_tables_c<-subst_tables %>%
  filter(age==18) %>%
  group_by(year, race_ethn) %>%
  summarise(c_mn = mean(c), cmax = max(c), cmin = min(c))

sub_tables_pooled<-list()
race_id<-unique(ncands_sub$race_ethn)
imps<-unique(ncands_sub$.imp)
index<-1
for(i in 1:length(imps)){
  for(r in 1:length(race_id)){
    temp<-ncands_sub %>% 
      filter(year>2011) 
    temp<-temp %>% 
      filter(race_ethn == race_id[r])
    temp<-temp %>% 
      filter(.imp==i)
    temp<-temp %>% 
      group_by(.imp, age, race_ethn) %>% 
      summarise(var = sum(var), pop = sum(pop)) %>% 
      ungroup()
    sub_tables_pooled[[index]]<-make_life_table(temp)
    index<-index+1
  }
}
sub_tables_pooled<-bind_rows(sub_tables_pooled)

##############################################
## p(sub|inv) = p(inv, sub)/p(inv)
## p(sub) / p(inv)
##############################################

ncands_sub_inv<-sub_tables_pooled %>%
  mutate(sub_inv = q / investigation_tables_pooled$q)

##############################################
## foster care
##############################################
fc_tables<-list()
years<-unique(afcars_fc$year)
race_id<-unique(afcars_fc$race_ethn)
index<-1
for(i in 1:length(unique(afcars_fc$.imp))){
  for(j in 1:length(years)){
    for(r in 1:length(race_id)){
      temp<-afcars_fc %>%
        filter(year==years[j])
      temp<-temp %>%
        filter(race_ethn == race_id[r])
      temp<-temp %>%
        filter(.imp==i)
      fc_tables[[index]]<-make_life_table(temp)
      index<-index+1
    }
  }
}
fc_tables<-bind_rows(fc_tables)

fc_tables_c<-fc_tables %>%
  filter(age==18) %>%
  group_by(year, race_ethn) %>%
  summarise(c_mn = mean(c), cmax = max(c), cmin = min(c))

fc_tables_pooled<-list()
race_id<-unique(afcars_fc$race_ethn)
imps<-unique(afcars_fc$.imp)
index<-1
for(i in 1:length(imps)){
  for(r in 1:length(race_id)){
    temp<-afcars_fc %>% 
      filter(year>2011) 
    temp<-temp %>% 
      filter(race_ethn == race_id[r])
    temp<-temp %>% 
      filter(.imp==i)
    temp<-temp %>% 
      group_by(.imp, age, race_ethn) %>% 
      summarise(var = sum(var), pop = sum(pop)) %>% 
      ungroup()
    fc_tables_pooled[[index]]<-make_life_table(temp)
    index<-index+1
  }
}
fc_tables_pooled<-bind_rows(fc_tables_pooled)

########################
## P(FC,inv)
########################

fc_post_inv_tables<-list()
years<-unique(afcars_fc_inv$year)
race_id<-unique(afcars_fc_inv$race_ethn)
index<-1
#for(i in 1:length(unique(afcars_fc_inv$.imp))){
for(r in 1:length(race_id)){
  temp<-afcars_fc_inv %>%
    filter(year>2011)
  temp<-temp %>% 
    filter(race_ethn == race_id[r])
  temp<-temp %>% 
    filter(.imp==1)
  temp<-temp %>% 
    group_by(.imp, age, race_ethn) %>% 
    summarise(var = sum(var), pop = sum(pop)) %>% 
    ungroup()
  fc_post_inv_tables[[index]]<-make_life_table(temp)
  index<-index+1
}
#}
# P(A,B)
fc_post_inv_tables<-bind_rows(fc_post_inv_tables)

# P(A|B) = P(A,B)/P(B)
fc_cond_inv<-fc_post_inv_tables %>%
  mutate(fc_inv = q / investigation_tables_pooled$q)

##P(FC,sub)
fc_sub_tables<-list()
years<-unique(afcars_fc_sub$year)
race_id<-unique(afcars_fc_sub$race_ethn)
index<-1
#for(i in 1:length(unique(afcars_fc_sub$.imp))){
for(r in 1:length(race_id)){
  temp<-afcars_fc_sub %>%
    filter(year>2011)
  temp<-temp %>% 
    filter(race_ethn == race_id[r])
  temp<-temp %>% 
    filter(.imp==1)
  temp<-temp %>% 
    group_by(.imp, age, race_ethn) %>% 
    summarise(var = sum(var), pop = sum(pop)) %>% 
    ungroup()
  fc_sub_tables[[index]]<-make_life_table(temp)
  index<-index+1
}
#}
# P(A,B)
fc_sub_tables<-bind_rows(fc_sub_tables)
# P(A|B) = P(A,B)/P(B)
fc_sub_cond<-fc_sub_tables %>%
  mutate(fc_sub = q / sub_tables_pooled$q)

########################
## ICWA placements
########################

icwa_tables<-list()
years<-unique(afcars_non_icwa$year)
race_id<-unique(afcars_non_icwa$race_ethn)
index<-1
for(i in 1:length(unique(afcars_non_icwa$.imp))){
  for(j in 1:length(years)){
    for(r in 1:length(race_id)){
      temp<-afcars_non_icwa %>%
        filter(year==years[j])
      temp<-temp %>%
        filter(race_ethn == race_id[r])
      temp<-temp %>%
        filter(.imp==i)
      icwa_tables[[index]]<-make_life_table(temp)
      index<-index+1
    }
  }
}
icwa_tables<-bind_rows(icwa_tables)


icwa_tables_pooled<-list()
race_id<-unique(afcars_non_icwa$race_ethn)
imps<-unique(afcars_non_icwa$.imp)
index<-1
for(i in 1:length(imps)){
  for(r in 1:length(race_id)){
    temp<-afcars_non_icwa %>% 
      filter(year>2011) 
    temp<-temp %>% 
      filter(race_ethn == race_id[r])
    temp<-temp %>% 
      filter(.imp==i)
    temp<-temp %>% 
      group_by(.imp, age, race_ethn) %>% 
      summarise(var = sum(var), pop = sum(pop)) %>% 
      ungroup()
    icwa_tables_pooled[[index]]<-make_life_table(temp)
    index<-index+1
  }
}
icwa_tables_pooled<-bind_rows(icwa_tables_pooled)


icwa_tables_c<-icwa_tables %>%
  filter(age==18) %>%
  group_by(year, race_ethn) %>%
  summarise(c_mn = mean(c), cmax = max(c), cmin = min(c))

## P(non-icwa|placement)
## P(non-icwa, placement)/P(placement)
fc_tables_pooled_aian<-fc_tables_pooled %>% 
  filter(race_ethn=="AI/AN")

icwa_cond<-icwa_tables_pooled %>% 
  mutate(icwa_cond = q / fc_tables_pooled_aian$q)


###########################
# TPR
###########################

tpr_tables<-list()
years<-unique(afcars_tpr$year)
race_id<-unique(afcars_tpr$race_ethn)
index<-1
for(i in 1:length(unique(afcars_tpr$.imp))){
  for(j in 1:length(years)){
    for(r in 1:length(race_id)){
      temp<-afcars_tpr %>%
        filter(year==years[j])
      temp<-temp %>%
        filter(race_ethn == race_id[r])
      temp<-temp %>%
        filter(.imp==i)
      tpr_tables[[index]]<-make_life_table(temp)
      index<-index+1
    }
  }
}
tpr_tables<-bind_rows(tpr_tables)

tpr_tables_pooled<-list()
race_id<-unique(afcars_tpr$race_ethn)
imps<-unique(afcars_tpr$.imp)
index<-1
for(i in 1:length(imps)){
  for(r in 1:length(race_id)){
    temp<-afcars_tpr %>% 
      filter(year>2011) 
    temp<-temp %>% 
      filter(race_ethn == race_id[r])
    temp<-temp %>% 
      filter(.imp==i)
    temp<-temp %>% 
      group_by(.imp, age, race_ethn) %>% 
      summarise(var = sum(var), pop = sum(pop)) %>% 
      ungroup()
    tpr_tables_pooled[[index]]<-make_life_table(temp)
    index<-index+1
  }
}
tpr_tables_pooled<-bind_rows(tpr_tables_pooled)

tpr_tables_c<-tpr_tables %>%
  filter(age==18) %>%
  group_by(year, race_ethn) %>%
  summarise(c_mn = mean(c), cmax = max(c), cmin = min(c))

## conditionals
tpr_cond<-tpr_tables_pooled %>% 
  mutate(tpr_cond = q / fc_tables_pooled$q)


### make a cumulative risk visual by outcome

c_plot_dat<-bind_rows(
  investigation_tables_c %>% 
    mutate(outcome = "Investigation"),
  subst_tables_c %>% 
    mutate(outcome = "Substantiation"),
  fc_tables_c %>% 
    mutate(outcome = "Foster care"),
  tpr_tables_c %>% 
    mutate(outcome = "Parental rights terminated")) %>% 
  filter(year==2016) %>% 
  mutate(outcome = factor(outcome, 
                          levels= c("Investigation", "Substantiation",
                                    "Foster care", "Parental rights terminated")))

cumulative_plot<-c_plot_dat %>% 
  ggplot(aes(y = c_mn * 100, x = outcome, fill = race_ethn)) +
  geom_col(position = position_dodge()) + 
  scale_fill_brewer(palette="Dark2") + 
  xlab("") + 
  ylab("Percent of children experiencing event by age 18") + 
  labs(fill = "")


##### TO DO 
### FINISH STATE FILE ON SERVER WITH CURRENT DATA
### 

icwa_dat<-read_csv("./data/icwa_data.csv")
