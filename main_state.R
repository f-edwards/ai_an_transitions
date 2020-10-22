rm(list=ls()); gc()
library(tidyverse)
library(lubridate)
library(gridExtra)
library(RColorBrewer)
library(geofacet)

theme_set(theme_bw())
source("lifetable.r")
pop<-read_fwf("~/Projects/cps_lifetables/data/us.1990_2018.singleages.adjusted.txt",
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
  group_by(year,state, st_fips, age, race_ethn) %>%
  summarise(pop = sum(pop)) %>%
  ungroup() %>%
  mutate(st_fips = as.numeric(st_fips))

### read in AIAN alone or in combination 2010 census data by state
pop_alone_comb<-read_csv("./data/nhgis0053_ds181_2010_state.csv") %>% 
  select(YEAR, STATE, STATEA,
         LGJAAE003:LGJAAE021, ## male 0 - 18 AIAN alone
         LGJAAE107:LGJAAE125, ## female 0 - 18 AIAN alone
         LGJACF003:LGJACF021, ## male 0-18 AIAN alone or in comb
         LGJACF107:LGJACF125 ## female 0-18 AIAN alone or in comb
         )

pop_alone_m<-pop_alone_comb %>% 
  select(YEAR, STATE, STATEA,
         LGJAAE003:LGJAAE021) %>% 
  pivot_longer(cols = LGJAAE003:LGJAAE021,
               names_to = "age",
               values_to = "pop_alone") %>% 
  mutate(age = substr(age, 7, 9),
         age = as.numeric(age) - 3,
         sex = "M")

pop_alone_f<-pop_alone_comb %>% 
  select(YEAR, STATE, STATEA,
         LGJAAE107:LGJAAE125) %>% 
  pivot_longer(cols = LGJAAE107:LGJAAE125,
               names_to = "age",
               values_to = "pop_alone") %>% 
  mutate(age = substr(age, 7, 9),
         age = as.numeric(age) - 107,
         sex = "F")

pop_alone_comb_m<-pop_alone_comb %>% 
  select(YEAR, STATE, STATEA,
         LGJACF003:LGJACF021) %>% 
  pivot_longer(cols = LGJACF003:LGJACF021,
               names_to = "age",
               values_to = "pop_alone_comb") %>% 
  mutate(age = substr(age, 7, 9),
         age = as.numeric(age) - 3,
         sex = "M")

pop_alone_comb_f<-pop_alone_comb %>% 
  select(YEAR, STATE, STATEA,
         LGJACF107:LGJACF125) %>% 
  pivot_longer(cols = LGJACF107:LGJACF125,
               names_to = "age",
               values_to = "pop_alone_comb") %>% 
  mutate(age = substr(age, 7, 9),
         age = as.numeric(age) - 107,
         sex = "F")

pop_alone_comb<-pop_alone_m %>% 
  bind_rows(pop_alone_f) %>% 
  left_join(pop_alone_comb_f %>% 
              bind_rows(pop_alone_comb_m)) %>% 
  filter(STATEA!=72) %>% 
  mutate(st_fips = as.numeric(STATEA)) %>% 
  select(-STATE,-STATEA, -YEAR) 
  
pop_alone_comb<-pop_alone_comb %>% 
  pivot_wider(id_cols = c(st_fips, age),
              names_from = sex,
              names_sep = "_",
              values_from = c(pop_alone, pop_alone_comb)) %>% 
  mutate(pop_alone = pop_alone_M + pop_alone_F,
         pop_alone_comb = pop_alone_comb_M + 
           pop_alone_comb_F) %>% 
  select(st_fips, age, pop_alone, 
         pop_alone_comb) %>% 
  mutate(race_ethn = "AI/AN")

######## join alone / alone_comb to seer for sensitivity
pop<-pop %>% 
  left_join(pop_alone_comb)


#### pop sensitivity

pop_sens<-pop %>% 
  filter(year==2014,
         race_ethn=="AI/AN") %>% 
  group_by(state) %>% 
  summarise(pop = sum(pop),
            pop_alone = sum(pop_alone),
            pop_alone_comb = sum(pop_alone_comb)) %>% 
  mutate(r_pop_alone = pop/pop_alone,
         r_pop_alone_comb = pop/pop_alone_comb) %>% 
  select(state, r_pop_alone, r_pop_alone_comb) %>% 
  pivot_longer(cols = r_pop_alone:r_pop_alone_comb,
               names_to = "type",
               values_to = "ratio")

### vis of possible bias from alone / alone_comb
ggplot(pop_sens,
       aes(x = ratio, fill = type)) + 
  geom_density(alpha = 0.5, color = "black")
####

format_data<-function(dat){
  dat1<-dat %>%
    filter(year>=2014)%>% filter(.imp!=0) %>%
    filter(race_ethn == "AI/AN" | race_ethn == "White") %>%
    ungroup() %>%
    complete(.imp, age, race_ethn, year, state,fill = list(var = 0)) %>%
    left_join(pop) %>% 
    mutate(race_ethn = ifelse(race_ethn == "AI/AN", "AIAN", race_ethn))
}

ncands_inv<-read_csv("~/Projects/cps_lifetables/data/state_first_inv.csv") %>%
  rename(year = subyr,
         var = first_inv,
         state = staterr) %>%
  format_data() %>%
  group_by(.imp, age, race_ethn, state) %>%
  summarise(var = sum(var),
            pop = sum(pop), 
            pop_min = sum(pop_alone), 
            pop_max = sum(pop_alone_comb)) %>%
  ungroup()

ncands_sub<-read_csv("~/Projects/cps_lifetables/data/state_first_victim_out.csv")%>%
  rename(year = subyr,
         var = first_victim,
         state = staterr) %>%
  format_data() %>%
  group_by(.imp, age, race_ethn, state) %>%
  summarise(var = sum(var),
            pop = sum(pop), 
            pop_min = sum(pop_alone),
            pop_max = sum(pop_alone_comb)) %>%
  ungroup()


afcars_fc<-read_csv("./data/state_first_fc.csv")%>%
  rename(year = fy) %>%
  filter(state!=72) %>%
  rename(var = first_entry) %>%
  rename(st_fips = state) %>%
  left_join(pop %>% select(st_fips, state) %>% distinct()) %>%
  select(-st_fips) %>%
  format_data() %>%
  group_by(.imp, age, race_ethn, state) %>%
  summarise(var = sum(var),
            pop = sum(pop), 
            pop_min = sum(pop_alone), 
            pop_max = sum(pop_alone_comb)) %>%
  ungroup()

afcars_tpr<-read_csv("./data/afcars_tpr_st.csv")%>%
  filter(state!=72) %>%
  rename(st_fips = state) %>%
  left_join(pop %>% select(st_fips, state) %>% distinct()) %>%
  select(-st_fips) %>%
  format_data() %>%
  group_by(.imp, age, race_ethn, state) %>%
  summarise(var = sum(var),
            pop = sum(pop), 
            pop_min = sum(pop_alone), 
            pop_max = sum(pop_alone_comb)) %>%
  ungroup()

afcars_non_icwa<-read_csv("./data/afcars_non_icwa_st.csv")%>%
  filter(state!=72) %>%
  rename(st_fips = state) %>%
  left_join(pop %>% select(st_fips, state) %>% distinct()) %>%
  select(-st_fips) %>%
  format_data() %>%
  group_by(.imp, age, race_ethn, state) %>%
  summarise(var = sum(var),
            pop = sum(pop), 
            pop_min = sum(pop_alone), 
            pop_max = sum(pop_alone_comb)) %>%
  ungroup()

afcars_fc_inv<-read_csv("./data/afcars_fc_post_inv_st.csv")%>%
  rename(st_fips = state) %>%
  left_join(pop %>% select(st_fips, state) %>% distinct()) %>%
  select(-st_fips) %>%
  format_data() %>%
  group_by(.imp, age, race_ethn, state) %>%
  summarise(var = sum(var),
            pop = sum(pop), 
            pop_min = sum(pop_alone), 
            pop_max = sum(pop_alone_comb)) %>%
  ungroup()

afcars_fc_sub<-read_csv("./data/afcars_fc_post_sub_st.csv")%>%
  select(-pop, -staterr) %>%
  rename(st_fips = state) %>%
  left_join(pop %>% select(st_fips, state) %>% distinct()) %>%
  select(-st_fips) %>%
  format_data() %>%
  group_by(.imp, age, race_ethn, state) %>%
  summarise(var = sum(var),
            pop = sum(pop), 
            pop_min = sum(pop_alone), 
            pop_max = sum(pop_alone_comb)) %>%
  ungroup()


### read in new first investigation csv
### run lifetable by year


investigation_tables<-list()
investigation_tables_ac<-list()
states<-unique(ncands_inv$state)
race_id<-unique(ncands_inv$race_ethn)
imps<-unique(ncands_inv$.imp)
index<-1
for(i in 1:length(imps)){
  for(j in 1:length(states)){
    for(r in 1:length(race_id)){
      temp<-ncands_inv %>% 
        filter(state==states[j])
      temp<-temp %>% 
        filter(race_ethn == race_id[r])
      temp<-temp %>% 
        filter(.imp==i)
      investigation_tables[[index]]<-make_life_table(temp)
      investigation_tables_ac[[index]]<-make_life_table(temp %>% 
                                                         mutate(pop = pop_max))
      index<-index+1
    }
  }
}
investigation_tables<-bind_rows(investigation_tables)
investigation_tables_ac<-bind_rows(investigation_tables_ac) %>% 
  rename(q_c = q, c_c = c) %>% 
  select(.imp, age, race_ethn, state, 
         var, q_c, c_c)

investigation_tables<-investigation_tables %>% 
  left_join(investigation_tables_ac)


investigation_tables_c<-investigation_tables %>% 
  filter(age==18) %>%
  select(.imp, state, race_ethn, c, c_c) 





## ## ## ## ## ## ## ## ## ## ## ## ## 
## substantiation
## ## ## ## ## ## ## ## ## ## ## ## ## 
### run lifetable by year
subst_tables<-list()
subst_tables_ac<-list()
states<-unique(ncands_sub$state)
race_id<-unique(ncands_sub$race_ethn)
imps<-unique(ncands_sub$.imp)
index<-1
for(i in 1:length(imps)){
  for(j in 1:length(states)){
    for(r in 1:length(race_id)){
      temp<-ncands_sub %>% 
        filter(state==states[j])
      temp<-temp %>% 
        filter(race_ethn == race_id[r])
      temp<-temp %>% 
        filter(.imp==i)
      subst_tables[[index]]<-make_life_table(temp)
      subst_tables_ac[[index]]<-make_life_table(temp %>% 
                                               mutate(pop = pop_max))
      index<-index+1
    }
  }
}
subst_tables<-bind_rows(subst_tables)
subst_tables_ac<-bind_rows(subst_tables_ac) %>% 
  rename(q_c = q, c_c = c) %>% 
  select(.imp, age, race_ethn, state, 
         var, q_c, c_c)

subst_tables<-subst_tables %>% 
  left_join(subst_tables_ac)

subst_tables_c<-subst_tables %>%
  filter(age==18) %>%
  select(.imp, state, race_ethn, c, c_c) 

##############################################
## p(sub|inv) = p(inv, sub)/p(inv)
## p(sub) / p(inv)
##############################################

### 3 states missing on investigation tables because
### of failed ID matches. run a join instead.

#### MAKE THESE CUMULATIVE. AGE SPECIFIC NOT AS USEFUL

ncands_sub_inv<-subst_tables_c %>%
  rename(sub = c) %>% 
  select(.imp , race_ethn, state, sub) %>% 
  left_join(investigation_tables_c %>% 
              rename(inv = c) %>% 
              select(.imp, race_ethn, state, inv)) %>% 
  mutate(sub_inv = sub / inv)

##############################################
## foster care
##############################################
fc_tables<-list()
states<-unique(afcars_fc$state)
race_id<-unique(afcars_fc$race_ethn)
imps<-unique(afcars_fc$.imp)
index<-1
for(i in 1:length(imps)){
  for(j in 1:length(states)){
    for(r in 1:length(race_id)){
      temp<-afcars_fc %>% 
        filter(state==states[j])
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
  select(.imp, state, race_ethn, c) 


########################
## P(FC,inv)
########################

fc_post_inv_tables<-list()
states<-unique(afcars_fc_inv$state)
race_id<-unique(afcars_fc_inv$race_ethn)
imps<-unique(afcars_fc_inv$.imp)
index<-1
for(i in 1:length(imps)){
  for(j in 1:length(states)){
    for(r in 1:length(race_id)){
      temp<-afcars_fc_inv %>% 
        filter(state==states[j])
      temp<-temp %>% 
        filter(race_ethn == race_id[r])
      temp<-temp %>% 
        filter(.imp==i)
      fc_post_inv_tables[[index]]<-make_life_table(temp)
      index<-index+1
    }
  }
}

# P(A,B)
fc_post_inv_tables<-bind_rows(fc_post_inv_tables)

fc_post_inv_tables_c<-fc_post_inv_tables %>%
  filter(age==18) %>%
  select(.imp, state, race_ethn, c) 

# P(A|B) = P(A,B)/P(B)
### 3 states missing on investigation tables because
### of failed ID matches. run a join instead.

fc_cond_inv<-fc_post_inv_tables_c %>%
  rename(fc = c) %>% 
  select(.imp, race_ethn, state, fc) %>% 
  left_join(investigation_tables_c %>% 
              rename(inv = c) %>% 
              select(.imp, race_ethn, state, inv)) %>% 
  mutate(fc_inv = fc / inv)

##P(FC,sub)
fc_sub_tables<-list()
states<-unique(afcars_fc_sub$state)
race_id<-unique(afcars_fc_sub$race_ethn)
imps<-unique(afcars_fc_sub$.imp)
index<-1

for(i in 1:length(imps)){
  for(j in 1:length(states)){
    for(r in 1:length(race_id)){
      temp<-afcars_fc_sub %>% 
        filter(state==states[j])
      temp<-temp %>% 
        filter(race_ethn == race_id[r])
      temp<-temp %>% 
        filter(.imp==i)
      fc_sub_tables[[index]]<-make_life_table(temp)
      index<-index+1
    }
  }
}

fc_sub_tables<-bind_rows(fc_sub_tables)

fc_sub_tables_c<-fc_sub_tables %>%
  filter(age==18) %>%
  select(.imp, state, race_ethn, c) 

# P(A|B) = P(A,B)/P(B)
### 3 states missing on investigation tables because
### of failed ID matches. run a join instead.

fc_cond_sub<-fc_sub_tables_c %>%
  rename(fc = c) %>% 
  select(.imp, race_ethn, state, fc) %>% 
  left_join(subst_tables_c %>% 
              rename(sub = c) %>% 
              select(.imp, race_ethn, state, sub)) %>% 
  mutate(fc_sub = fc / sub)

########################
## Non-ICWA placements
########################

non_icwa_tables<-list()
states<-unique(afcars_non_icwa$state)
race_id<-unique(afcars_non_icwa$race_ethn)
imps<-unique(afcars_non_icwa$.imp)
index<-1
for(i in 1:length(imps)){
  for(j in 1:length(states)){
    for(r in 1:length(race_id)){
      temp<-afcars_non_icwa %>% 
        filter(state==states[j])
      temp<-temp %>% 
        filter(race_ethn == race_id[r])
      temp<-temp %>% 
        filter(.imp==i)
      non_icwa_tables[[index]]<-make_life_table(temp)
      index<-index+1
    }
  }
}
non_icwa_tables<-bind_rows(non_icwa_tables)

non_icwa_c<-non_icwa_tables %>%
  filter(age==18) %>%
  select(.imp, state, race_ethn, c) 

## P(non-icwa|placement)
## P(non-icwa, placement)/P(placement)

non_icwa_cond_fc<-non_icwa_c %>%
  rename(non_icwa = c) %>% 
  select(.imp, race_ethn, state, non_icwa) %>% 
  left_join(fc_tables_c%>% 
              rename(fc = c) %>% 
              select(.imp, race_ethn, state, fc)) %>% 
  mutate(non_icwa_fc = non_icwa / fc)
 
###########################
# TPR
###########################

tpr_tables<-list()
states<-unique(afcars_tpr$state)
race_id<-unique(afcars_tpr$race_ethn)
imps<-unique(afcars_tpr$.imp)
index<-1
for(i in 1:length(imps)){
  for(j in 1:length(states)){
    for(r in 1:length(race_id)){
      temp<-afcars_tpr %>% 
        filter(state==states[j])
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

tpr_tables_c<-tpr_tables %>%
  filter(age==18) %>%
  select(.imp, state, race_ethn, c) 

## conditionals
tpr_cond<-tpr_tables_c%>% 
  rename(tpr = c) %>% 
  select(.imp, race_ethn, state, tpr) %>% 
  left_join(fc_tables_c%>% 
              rename(fc = c) %>% 
              select(.imp, race_ethn, state, fc)) %>% 
  mutate(tpr_fc = tpr / fc)


############################################################
#### VISUALS
############################################################

### small multiple traceplot by state of age-specific risk of each major outcome
### inv; subst; fc; tpr: white / AIAN as color
## like this using geofacet package
## https://towardsdatascience.com/how-to-make-beautiful-small-multiple-us-maps-in-r-ad7e557cd463

ggplot(investigation_tables %>% 
         group_by(state, race_ethn, age) %>% 
         summarise(qmax = max(q), qmin=min(q), q = mean(q)),
       aes(x = age, y = q, ymin = qmin, ymax = qmax)) + 
  geom_line(aes(color = race_ethn)) + 
  geom_ribbon(aes(fill = race_ethn), alpha = 0.3) + 
  facet_geo(~state) + 
  scale_color_brewer(palette = "Set2") + 
  scale_fill_brewer(palette = "Set2") + 
  labs(color = "", fill = "") + 
  theme_bw() + 
  ggsave("./vis/age_inv_states.pdf")

ggplot(subst_tables %>% 
         group_by(state, race_ethn, age) %>% 
         summarise(qmax = max(q), qmin=min(q), q = mean(q)),
       aes(x = age, y = q, ymin = qmin, ymax = qmax, color = race_ethn, fill = race_ethn)) + 
  geom_line() + 
  geom_ribbon(alpha = 0.3) + 
  scale_color_brewer(palette = "Set2") + 
  scale_fill_brewer(palette = "Set2") + 
  facet_geo(~state) + 
  labs(color = "", fill = "") + 
  ggsave("./vis/age_sub_states.pdf")

ggplot(fc_tables %>% 
         group_by(state, race_ethn, age) %>% 
         summarise(qmax = max(q), qmin=min(q), q = mean(q)),
       aes(x = age, y = q, ymin = qmin, ymax = qmax, 
           color = race_ethn, fill = race_ethn)) + 
  geom_line() + 
  geom_ribbon(alpha = 0.3) + 
  scale_color_brewer(palette = "Set2") + 
  scale_fill_brewer(palette = "Set2") + 
  facet_geo(~state) + 
  labs(color = "", fill = "") + 
  ggsave("./vis/age_fc_states.pdf")

ggplot(tpr_tables %>% 
         group_by(state, race_ethn, age) %>% 
         summarise(qmax = max(q), qmin=min(q), q = mean(q)),
       aes(x = age, y = q, ymin = qmin, ymax = qmax, 
           color = race_ethn, fill = race_ethn)) + 
  geom_line() + 
  geom_ribbon(alpha = 0.3) + 
  scale_color_brewer(palette = "Set2") + 
  scale_fill_brewer(palette = "Set2") + 
  facet_geo(~state) + 
  labs(color = "", fill = "") + 
  ggsave("./vis/age_tpr_states.pdf")

#### now cumulative numbers

ggplot(investigation_tables_c %>% 
         group_by(state, race_ethn) %>% 
         summarise(cmin = min(c),
                   cmax = max(c),
                   c = mean(c)),
       aes(x = c, 
           xmin = cmin,
           xmax = cmax,
           y = reorder(state, c),
           color = race_ethn)) + 
  labs(color = "", fill = "",
       y = "",
       x = "Risk of event by age 18") + 
  geom_point() + 
  geom_linerange() + 
  ggsave("./vis/c_inv_states.pdf")

ggplot(subst_tables_c %>% 
         group_by(state, race_ethn) %>% 
         summarise(cmin = min(c),
                   cmax = max(c),
                   c = mean(c)),
       aes(x = c, 
           xmin = cmin,
           xmax = cmax,
           y = reorder(state, c),
           color = race_ethn)) + 
  labs(color = "", fill = "",
       y = "",
       x = "Risk of event by age 18") + 
  geom_point() + 
  geom_linerange() + 
  ggsave("./vis/c_sub_states.pdf")

ggplot(fc_tables_c %>% 
         group_by(state, race_ethn) %>% 
         summarise(cmin = min(c),
                   cmax = max(c),
                   c = mean(c)),
       aes(x = c, 
           xmin = cmin,
           xmax = cmax,
           y = reorder(state, c),
           color = race_ethn)) + 
  labs(color = "", fill = "",
       y = "",
       x = "Risk of event by age 18") + 
  geom_point() + 
  geom_linerange() + 
  ggsave("./vis/c_fc_states.pdf")

ggplot(tpr_tables_c %>% 
         group_by(state, race_ethn) %>% 
         summarise(cmin = min(c),
                   cmax = max(c),
                   c = mean(c)),
       aes(x = c, 
           xmin = cmin,
           xmax = cmax,
           y = reorder(state, c),
           color = race_ethn)) + 
  labs(color = "", fill = "",
       y = "",
       x = "Risk of event by age 18") + 
  geom_point() + 
  geom_linerange() + 
  ggsave("./vis/c_tpr_states.pdf")

### make a cumulative risk visual by outcome

