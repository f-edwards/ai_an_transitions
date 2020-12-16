### make state tables

source("lifetable.r")

### CHECKING THE CENSUS PEP ALONE/COMB DATA FOR THIS PERIOD
### previously used county with age group data "cc-est2019-alldata.csv"
### 12/16, updating to state file with single ages: "sc-est2019-alldata5.csv"
### for alone or in combination
### and alldata6 for alone (white non-hispanic)
pop_new<-read_csv("https://www2.census.gov/programs-surveys/popest/tables/2010-2019/state/asrh/sc-est2019-alldata5.csv")

### obtain AIAN alone or in combination
pop_aian<-pop_new %>% 
  filter(RACE==3 & ORIGIN == 0 & SEX == 0) %>% 
  select(-SUMLEV, -REGION, -DIVISION, -CENSUS2010POP, -ESTIMATESBASE2010) %>% 
  pivot_longer(cols=POPESTIMATE2010:POPESTIMATE2019,
               names_to = "YEAR"
               ) %>% 
  mutate(YEAR = str_sub(YEAR, -4, -1)) %>% 
  group_by(STATE, NAME, AGE, YEAR) %>% 
  summarise(pop = sum(value)) %>% 
  mutate(race_ethn = "AIAN")

pop_non<-pop_new %>% 
  filter(RACE !=3 & ORIGIN == 0 & SEX == 0) %>% 
  select(-SUMLEV, -REGION, -DIVISION, -CENSUS2010POP, -ESTIMATESBASE2010) %>% 
  pivot_longer(cols=POPESTIMATE2010:POPESTIMATE2019,
               names_to = "YEAR"
  ) %>% 
  mutate(YEAR = str_sub(YEAR, -4, -1)) %>% 
  group_by(STATE, NAME, AGE, YEAR) %>% 
  summarise(pop = sum(value)) %>% 
  mutate(race_ethn = "Non-AIAN")

### alone data for white pop
pop_new<-read_csv("https://www2.census.gov/programs-surveys/popest/tables/2010-2019/state/asrh/sc-est2019-alldata6.csv")
  
pop_white<-pop_new %>% 
  filter(RACE == 1 & ORIGIN == 1 & SEX == 0) %>% 
  select(-SUMLEV, -REGION, -DIVISION, -CENSUS2010POP, -ESTIMATESBASE2010) %>% 
  pivot_longer(cols=POPESTIMATE2010:POPESTIMATE2019,
               names_to = "YEAR"
  ) %>% 
  mutate(YEAR = str_sub(YEAR, -4, -1)) %>% 
  group_by(STATE, NAME, AGE, YEAR) %>% 
  summarise(pop = sum(value)) %>% 
  mutate(race_ethn = "White")

pop_new<-pop_aian %>% 
  bind_rows(pop_white) %>% 
  bind_rows(pop_non)

### format for join
pop<-pop_new %>% 
  rename(st_fips = STATE, year = YEAR, age = AGE)

## for state names -> abb
state_walk<-data.frame("NAME"=str_to_title(state.name),
                       "state"=state.abb)

state_walk<-state_walk %>% 
  bind_rows(data.frame("NAME" = "District of Columbia",
                       "state" = "DC"))

pop<-pop %>% 
  ungroup() %>% 
  left_join(state_walk) %>% 
  select(-NAME) %>% 
  mutate(year = as.numeric(year),
         st_fips = as.numeric(st_fips))

format_data<-function(dat){
  dat1<-dat %>%
    filter(year>=2014)%>% filter(.imp!=0) %>%
    filter(race_ethn == "AI/AN" | race_ethn == "White") %>%
    mutate(race_ethn = ifelse(race_ethn == "AI/AN",
                              "AIAN",
                              race_ethn)) %>% 
    ungroup() %>% 
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
            pop = sum(pop)) %>%
  ungroup() 

ncands_sub<-read_csv("~/Projects/cps_lifetables/data/state_first_victim_out.csv")%>%
  rename(year = subyr,
         var = first_victim,
         state = staterr) %>%
  format_data() %>%
  group_by(.imp, age, race_ethn, state) %>%
  summarise(var = sum(var),
            pop = sum(pop)) %>%
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
            pop = sum(pop)) %>%
  ungroup()

afcars_tpr<-read_csv("./data/afcars_tpr_st.csv")%>%
  filter(state!=72) %>%
  rename(st_fips = state) %>%
  left_join(pop %>% select(st_fips, state) %>% distinct()) %>%
  select(-st_fips) %>%
  format_data() %>%
  group_by(.imp, age, race_ethn, state) %>%
  summarise(var = sum(var),
            pop = sum(pop)) %>%
  ungroup()

afcars_non_icwa<-read_csv("./data/afcars_non_icwa_st.csv")%>%
  filter(state!=72) %>%
  rename(st_fips = state) %>%
  left_join(pop %>% select(st_fips, state) %>% distinct()) %>%
  select(-st_fips) %>%
  format_data() %>%
  group_by(.imp, age, race_ethn, state) %>%
  summarise(var = sum(var),
            pop = sum(pop)) %>%
  ungroup()

afcars_fc_inv<-read_csv("./data/afcars_fc_post_inv_st.csv")%>%
  rename(st_fips = state) %>%
  left_join(pop %>% select(st_fips, state) %>% distinct()) %>%
  select(-st_fips) %>%
  format_data() %>%
  group_by(.imp, age, race_ethn, state) %>%
  summarise(var = sum(var),
            pop = sum(pop)) %>%
  ungroup()

afcars_fc_sub<-read_csv("./data/afcars_fc_post_sub_st.csv")%>%
  rename(st_fips = state) %>%
  left_join(pop %>% select(st_fips, state) %>% distinct()) %>%
  select(-st_fips) %>%
  format_data() %>%
  group_by(.imp, age, race_ethn, state) %>%
  summarise(var = sum(var),
            pop = sum(pop)) %>% 
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
      index<-index+1
    }
  }
}
investigation_tables<-bind_rows(investigation_tables)

investigation_tables_c<-investigation_tables %>% 
  filter(age==18) %>%
  select(.imp, state, race_ethn, c) 

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
      index<-index+1
    }
  }
}
subst_tables<-bind_rows(subst_tables)

subst_tables_c<-subst_tables %>%
  filter(age==18) %>%
  select(.imp, state, race_ethn, c) 

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
  mutate(fc_sub = fc / sub) %>% 
  mutate(fc_sub = ifelse(is.infinite(fc_sub),
                         0,
                         fc_sub))

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

#######################
## AAIA ICWA data

icwa<-read_csv("./data/icwa_data.csv") 

### add state abbrev
xwalk<-data.frame(State = as.character(state.name), 
                  state.abb = as.character(state.abb)) 

icwa_adopt<-icwa %>% 
  left_join(xwalk) %>% 
  mutate(AIAN = adopted_aian,
         White = adopted_non)

icwa_boarding<-icwa %>% 
  left_join(xwalk) %>% 
  mutate(AIAN = bia_boarding74,
         White = NA)

icwa_fc<-icwa %>% 
  mutate(AIAN = fc_aian,
         White = fc_non) %>% 
  left_join(xwalk) %>% 
  ungroup() %>% 
  mutate(AIAN = fc_aian,
         White = fc_non) %>% 
  select(State, state.abb, AIAN, White) %>% 
  rename(state = state.abb,
         St = State) %>% 
  pivot_longer(col = AIAN:White, 
               names_to = "race_ethn",
               values_to = "fc") %>% 
  mutate(period = "1976") %>% 
  left_join(icwa_adopt %>% 
              select(State, state.abb, AIAN, White) %>% 
              rename(state = state.abb,
                     St = State) %>% 
              pivot_longer(col = AIAN:White, 
                           names_to = "race_ethn",
                           values_to = "adopted")) %>% 
  left_join(icwa_boarding %>% 
              select(State, state.abb, AIAN, White) %>% 
              rename(state = state.abb,
                     St = State) %>% 
              pivot_longer(col = AIAN:White, 
                           names_to = "race_ethn",
                           values_to = "boarding"))

