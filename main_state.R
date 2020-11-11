rm(list=ls()); gc()
library(tidyverse)
library(lubridate)
library(gridExtra)
library(RColorBrewer)
library(geofacet)

theme_set(theme_bw())
source("lifetable.r")

### CHECKING THE CENSUS PEP ALONE/COMB DATA FOR THIS PERIOD
pop_new<-read_csv("./data/cc-est2019-alldata.csv")
### code age groups
pop_new<-pop_new %>% 
  mutate(age = case_when(
    AGEGRP==0 ~ "Total",
    AGEGRP==1 ~ "0-4",
    AGEGRP==2 ~ "5-9",
    AGEGRP==3 ~ "10-14",
    AGEGRP==4 ~ "15-19",
    AGEGRP>4 ~ "20+"
  ))

pop_new<-pop_new %>% 
  mutate(aian = NHIAC_MALE + NHIAC_FEMALE + 
           HIAC_MALE + HIAC_FEMALE)

pop_new<-pop_new %>% 
  select(STATE, COUNTY, STNAME, CTYNAME, 
         YEAR, AGEGRP, TOT_POP, AGEGRP,
         age, aian) %>% 
  filter(YEAR>2) %>% 
  filter(age!="Total") %>% 
  mutate(year = YEAR + 2007) %>% 
  select(-YEAR, -AGEGRP) %>% 
  group_by(STATE, year, age) %>% 
  summarise(pop = sum(aian)) %>% 
  mutate(race_ethn = "AIAN")

pop<-read_fwf("~/Projects/cps_lifetables/data/us.1990_2018.singleages.adjusted.txt",
              fwf_widths(c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8),
                         c("year", "state", "st_fips",
                           "cnty_fips", "reg", "race",
                           "hisp", "sex", "age", "pop")),
              col_types = "iccciiiiii")

pop_seer_nat<-pop %>%
  filter(year==2018)%>% 
  mutate(pop = as.integer(pop),
         age = as.integer(age),
         race_ethn =
           case_when(
             race==3 ~ "AIAN"),
         race_ethn = ifelse(is.na(race_ethn),
                            "other",
                            race_ethn))%>%
  group_by(year,state, st_fips, age, race_ethn) %>%
  summarise(pop = sum(pop)) %>%
  ungroup() %>%
  mutate(st_fips = as.numeric(st_fips))


pop<-pop %>%
  filter(race == 3 | (race==1 & hisp==0), year >=2010) %>%
  mutate(pop = as.integer(pop),
         age = as.integer(age),
         race_ethn =
           case_when(
             (race==1 & hisp ==0) ~ "White",
             race==3 ~ "AIAN"))%>%
  group_by(year,state, st_fips, age, race_ethn) %>%
  summarise(pop = sum(pop)) %>%
  ungroup() %>%
  mutate(st_fips = as.numeric(st_fips))



########## use PEP for AIAN totals, 
### portion single years 
### based on proportions in SEER

pop_seer_aian<-pop %>% 
  filter(race_ethn=="AIAN") %>% 
  filter(year>=2010) %>% 
  mutate(age_grp = 
           case_when(
             age<5 ~ "0-4",
             age<10 ~ "5-9",
             age<15 ~ "10-14",
             age<20 ~ "15-19",
             age>=20 ~ "20+"
           ))

pop_seer_aian_grp <- pop_seer_aian %>% 
  group_by(st_fips, state, year, age_grp) %>% 
  summarise(pop_grp = sum(pop)) %>% 
  right_join(pop_seer_aian) %>% 
  mutate(pct = pop / pop_grp) %>% 
  rename(seer_pop = pop)

### use age distribution in SEER to disaggregate
### 5 year pep categories
### assumes bridged race age distribution == alone/comb age distribution

pop_aian<-pop_new %>% 
  mutate(st_fips = as.numeric(STATE)) %>% 
  rename(age_grp = age) %>% 
  right_join(pop_seer_aian_grp) %>% 
  mutate(pop_adj = pop * pct) %>% 
  ungroup() %>% 
  select(year, race_ethn, st_fips, state,
         age, pop_adj, seer_pop) %>% 
  arrange(year, state, age) 

pop<-pop %>% 
  left_join(pop_aian %>% 
              select(-seer_pop)) %>% 
  mutate(pop_adj = ifelse(race_ethn == "White",
                          pop,
                          pop_adj)) %>% 
  filter(age<=21)

ggplot(pop_aian %>% 
         filter(year == 2018),
       aes(x = age, ymin=seer_pop, ymax=pop_adj)) + 
  geom_ribbon(fill = "green", alpha = 0.5,
              color = "black") + 
  facet_wrap(~state) + 
  ggsave("./vis/seer_pep_adjusted.png", width = 10, height = 6)


format_data<-function(dat){
  dat1<-dat %>%
    filter(year>=2014)%>% filter(.imp!=0) %>%
    filter(race_ethn == "AI/AN" | race_ethn == "White") %>%
    mutate(race_ethn = ifelse(race_ethn == "AI/AN",
                              "AIAN",
                              race_ethn)) %>% 
    complete(.imp, age, race_ethn, year, state,fill = list(var = 0)) %>%
    left_join(pop) 
}

ncands_inv<-read_csv("~/Projects/cps_lifetables/data/state_first_inv.csv") %>%
  rename(year = subyr,
         var = first_inv,
         state = staterr) %>%
  format_data() %>%
  group_by(.imp, age, race_ethn, state) %>%
  summarise(var = sum(var),
            pop = sum(pop), 
            pop_max = sum(pop_adj)) %>%
  ungroup()

ncands_sub<-read_csv("~/Projects/cps_lifetables/data/state_first_victim_out.csv")%>%
  rename(year = subyr,
         var = first_victim,
         state = staterr) %>%
  format_data() %>%
  group_by(.imp, age, race_ethn, state) %>%
  summarise(var = sum(var),
            pop = sum(pop), 
            pop_max = sum(pop_adj)) %>%
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
            pop_max = sum(pop_adj)) %>%
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
            pop_max = sum(pop_adj)) %>%
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
            pop_max = sum(pop_adj)) %>%
  ungroup()

afcars_fc_inv<-read_csv("./data/afcars_fc_post_inv_st.csv")%>%
  rename(st_fips = state) %>%
  left_join(pop %>% select(st_fips, state) %>% distinct()) %>%
  select(-st_fips) %>%
  format_data() %>%
  group_by(.imp, age, race_ethn, state) %>%
  summarise(var = sum(var),
            pop = sum(pop), 
             
            pop_max = sum(pop_adj)) %>%
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
             
            pop_max = sum(pop_adj)) %>%
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
  rename(sub = c,
         sub_c = c_c) %>% 
  select(.imp , race_ethn, state, sub, sub_c) %>% 
  left_join(investigation_tables_c %>% 
              rename(inv = c,
                     inv_c = c_c) %>% 
              select(.imp, race_ethn, state, inv, inv_c)) %>% 
  mutate(sub_inv = sub / inv,
         sub_inv_c = sub_c/inv_c)

##############################################
## foster care
##############################################
fc_tables<-list()
fc_tables_ac<-list()
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
      fc_tables_ac[[index]]<-make_life_table(temp %>% 
                                               mutate(pop = pop_max))
      index<-index+1
    }
  }
}
fc_tables<-bind_rows(fc_tables)

fc_tables_ac<-bind_rows(fc_tables_ac) %>% 
  rename(q_c = q, c_c = c) %>% 
  select(.imp, age, race_ethn, state, 
         var, q_c, c_c)

fc_tables<-fc_tables %>% 
  left_join(fc_tables_ac)


fc_tables_c<-fc_tables %>%
  filter(age==18) %>%
  select(.imp, state, race_ethn, c, c_c) 


########################
## P(FC,inv)
########################

fc_post_inv_tables<-list()
fc_post_inv_tables_ac<-list()
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
      fc_post_inv_tables_ac[[index]]<-make_life_table(temp %>% 
        mutate(pop = pop_max))
      index<-index+1
    }
  }
}


fc_post_inv_tables<-bind_rows(fc_post_inv_tables)

fc_post_inv_tables_ac<-bind_rows(fc_post_inv_tables_ac) %>% 
  rename(q_c = q, c_c = c) %>% 
  select(.imp, age, race_ethn, state, 
         var, q_c, c_c)

fc_post_inv_tables<-fc_post_inv_tables %>% 
  left_join(fc_post_inv_tables_ac)


fc_post_inv_tables_c<-fc_post_inv_tables %>%
  filter(age==18) %>%
  select(.imp, state, race_ethn, c, c_c) 


# P(A|B) = P(A,B)/P(B)
### 3 states missing on investigation tables because
### of failed ID matches. run a join instead.

fc_cond_inv<-fc_post_inv_tables_c %>%
  rename(fc = c,
         fc_c = c_c) %>% 
  select(.imp, race_ethn, state, fc, fc_c) %>% 
  left_join(investigation_tables_c %>% 
              rename(inv = c,
                     inv_c = c_c) %>% 
              select(.imp, race_ethn, state, inv, inv_c)) %>% 
  mutate(fc_inv = fc / inv,
         fc_inv_c = fc_c/inv_c)

##P(FC,sub)
fc_sub_tables<-list()
fc_sub_tables_ac<-list()
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
      fc_sub_tables_ac[[index]]<-make_life_table(temp %>% 
                                                   mutate(pop = pop_max))
      index<-index+1
    }
  }
}

fc_sub_tables<-bind_rows(fc_sub_tables)

fc_sub_tables_ac<-bind_rows(fc_sub_tables_ac) %>% 
  rename(q_c = q, c_c = c) %>% 
  select(.imp, age, race_ethn, state, 
         var, q_c, c_c)

fc_sub_tables<-fc_sub_tables %>% 
  left_join(fc_sub_tables_ac)

fc_sub_tables_c<-fc_sub_tables %>%
  filter(age==18) %>%
  select(.imp, state, race_ethn, c, c_c) 

# P(A|B) = P(A,B)/P(B)
### 3 states missing on investigation tables because
### of failed ID matches. run a join instead.

fc_cond_sub<-fc_sub_tables_c %>%
  rename(fc = c, fc_c = c_c) %>% 
  select(.imp, race_ethn, state, fc, fc_c) %>% 
  left_join(subst_tables_c %>% 
              rename(sub = c, sub_c = c_c) %>% 
              select(.imp, race_ethn, state, sub, sub_c)) %>% 
  mutate(fc_sub = fc / sub,
         fc_sub_c = fc_c/sub_c) %>% 
  mutate(fc_sub = ifelse(is.infinite(fc_sub),
                         0,
                         fc_sub),
         fc_sub_c = ifelse(is.infinite(fc_sub_c),
                           0,
                           fc_sub_c))

########################
## Non-ICWA placements
########################

non_icwa_tables<-list()
non_icwa_tables_ac<-list()
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
      non_icwa_tables_ac[[index]]<-make_life_table(temp %>% 
                                                     mutate(pop = pop_max))
      
      index<-index+1
    }
  }
}
non_icwa_tables<-bind_rows(non_icwa_tables)

non_icwa_tables_ac<-bind_rows(non_icwa_tables_ac) %>% 
  rename(q_c = q, c_c = c) %>% 
  select(.imp, age, race_ethn, state, 
         var, q_c, c_c)

non_icwa_tables<-non_icwa_tables %>% 
  left_join(non_icwa_tables_ac)

non_icwa_c<-non_icwa_tables %>%
  filter(age==18) %>%
  select(.imp, state, race_ethn, c, c_c) 

## P(non-icwa|placement)
## P(non-icwa, placement)/P(placement)

non_icwa_cond_fc<-non_icwa_c %>%
  rename(non_icwa = c,
         non_icwa_c = c_c) %>% 
  select(.imp, race_ethn, state, non_icwa, non_icwa_c) %>% 
  left_join(fc_tables_c%>% 
              rename(fc = c,
                     fc_c = c_c) %>% 
              select(.imp, race_ethn, state, fc, fc_c)) %>% 
  mutate(non_icwa_fc = non_icwa / fc,
         non_icwa_fc_c = non_icwa_c / fc_c)
 
###########################
# TPR
###########################

tpr_tables<-list()
tpr_tables_ac<-list()
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
      tpr_tables_ac[[index]]<-make_life_table(temp %>% 
                                             mutate(pop = pop_max))
      index<-index+1
    }
  }
}
tpr_tables<-bind_rows(tpr_tables)

tpr_tables_ac<-bind_rows(tpr_tables_ac) %>% 
  rename(q_c = q, c_c = c) %>% 
  select(.imp, age, race_ethn, state, 
         var, q_c, c_c)

tpr_tables<-tpr_tables %>% 
  left_join(tpr_tables_ac)

tpr_tables_c<-tpr_tables %>%
  filter(age==18) %>%
  select(.imp, state, race_ethn, c, c_c) 

## conditionals
tpr_cond<-tpr_tables_c%>% 
  rename(tpr = c,
         tpr_c = c_c) %>% 
  select(.imp, race_ethn, state, tpr, tpr_c) %>% 
  left_join(fc_tables_c%>% 
              rename(fc = c,
                     fc_c = c_c) %>% 
              select(.imp, race_ethn, state, fc, fc_c)) %>% 
  mutate(tpr_fc = tpr / fc,
         tpr_fc_c = tpr_c/fc_c)

#######################
## AAIA ICWA data

icwa<-read_csv("./data/icwa_data.csv") 
### add state abbrev
xwalk<-data.frame(State = as.character(state.name), 
                  state.abb = as.character(state.abb))
icwa<-icwa %>% 
  left_join(xwalk)

icwa<-icwa %>% 
  mutate(AIAN = fc_aian,
         White = fc_non)

icwa_fc<-icwa %>% 
  select(state.abb, AIAN, White) %>% 
  rename(state = state.abb) %>% 
  pivot_longer(col = AIAN:White, 
               names_to = "race_ethn",
               values_to = "fc") %>% 
  mutate(period = "1976") 



# 
# ############################################################
# #### VISUALS
# ############################################################
# 
# ### small multiple traceplot by state of age-specific risk of each major outcome
# ### inv; subst; fc; tpr: white / AIAN as color
# ## like this using geofacet package
# ## https://towardsdatascience.com/how-to-make-beautiful-small-multiple-us-maps-in-r-ad7e557cd463
# 
# ggplot(investigation_tables %>% 
#          group_by(state, race_ethn, age) %>% 
#          summarise(qmax = max(q), qmin=min(q), q = mean(q)),
#        aes(x = age, y = q, ymin = qmin, ymax = qmax)) + 
#   geom_line(aes(color = race_ethn)) + 
#   geom_ribbon(aes(fill = race_ethn), alpha = 0.3) + 
#   facet_geo(~state) + 
#   scale_color_brewer(palette = "Set2") + 
#   scale_fill_brewer(palette = "Set2") + 
#   labs(color = "", fill = "") + 
#   theme_bw() + 
#   ggsave("./vis/age_inv_states.pdf")
# 
# ggplot(subst_tables %>% 
#          group_by(state, race_ethn, age) %>% 
#          summarise(qmax = max(q), qmin=min(q), q = mean(q)),
#        aes(x = age, y = q, ymin = qmin, ymax = qmax, color = race_ethn, fill = race_ethn)) + 
#   geom_line() + 
#   geom_ribbon(alpha = 0.3) + 
#   scale_color_brewer(palette = "Set2") + 
#   scale_fill_brewer(palette = "Set2") + 
#   facet_geo(~state) + 
#   labs(color = "", fill = "") + 
#   ggsave("./vis/age_sub_states.pdf")
# 
# ggplot(fc_tables %>% 
#          group_by(state, race_ethn, age) %>% 
#          summarise(qmax = max(q), qmin=min(q), q = mean(q)),
#        aes(x = age, y = q, ymin = qmin, ymax = qmax, 
#            color = race_ethn, fill = race_ethn)) + 
#   geom_line() + 
#   geom_ribbon(alpha = 0.3) + 
#   scale_color_brewer(palette = "Set2") + 
#   scale_fill_brewer(palette = "Set2") + 
#   facet_geo(~state) + 
#   labs(color = "", fill = "") + 
#   ggsave("./vis/age_fc_states.pdf")
# 
# ggplot(tpr_tables %>% 
#          group_by(state, race_ethn, age) %>% 
#          summarise(qmax = max(q), qmin=min(q), q = mean(q)),
#        aes(x = age, y = q, ymin = qmin, ymax = qmax, 
#            color = race_ethn, fill = race_ethn)) + 
#   geom_line() + 
#   geom_ribbon(alpha = 0.3) + 
#   scale_color_brewer(palette = "Set2") + 
#   scale_fill_brewer(palette = "Set2") + 
#   facet_geo(~state) + 
#   labs(color = "", fill = "") + 
#   ggsave("./vis/age_tpr_states.pdf")
# 
# #### now cumulative numbers
# 
# ggplot(investigation_tables_c %>% 
#          group_by(state, race_ethn) %>% 
#          summarise(cmin = min(c),
#                    cmax = max(c),
#                    c = mean(c)),
#        aes(x = c, 
#            xmin = cmin,
#            xmax = cmax,
#            y = reorder(state, c),
#            color = race_ethn)) + 
#   labs(color = "", fill = "",
#        y = "",
#        x = "Risk of event by age 18") + 
#   geom_point() + 
#   geom_linerange() + 
#   ggsave("./vis/c_inv_states.pdf")
# 
# ggplot(subst_tables_c %>% 
#          group_by(state, race_ethn) %>% 
#          summarise(cmin = min(c),
#                    cmax = max(c),
#                    c = mean(c)),
#        aes(x = c, 
#            xmin = cmin,
#            xmax = cmax,
#            y = reorder(state, c),
#            color = race_ethn)) + 
#   labs(color = "", fill = "",
#        y = "",
#        x = "Risk of event by age 18") + 
#   geom_point() + 
#   geom_linerange() + 
#   ggsave("./vis/c_sub_states.pdf")
# 
# ggplot(fc_tables_c %>% 
#          group_by(state, race_ethn) %>% 
#          summarise(cmin = min(c),
#                    cmax = max(c),
#                    c = mean(c)),
#        aes(x = c, 
#            xmin = cmin,
#            xmax = cmax,
#            y = reorder(state, c),
#            color = race_ethn)) + 
#   labs(color = "", fill = "",
#        y = "",
#        x = "Risk of event by age 18") + 
#   geom_point() + 
#   geom_linerange() + 
#   ggsave("./vis/c_fc_states.pdf")
# 
# ggplot(tpr_tables_c %>% 
#          group_by(state, race_ethn) %>% 
#          summarise(cmin = min(c),
#                    cmax = max(c),
#                    c = mean(c)),
#        aes(x = c, 
#            xmin = cmin,
#            xmax = cmax,
#            y = reorder(state, c),
#            color = race_ethn)) + 
#   labs(color = "", fill = "",
#        y = "",
#        x = "Risk of event by age 18") + 
#   geom_point() + 
#   geom_linerange() + 
#   ggsave("./vis/c_tpr_states.pdf")
# 
# ### make a cumulative risk visual by outcome

