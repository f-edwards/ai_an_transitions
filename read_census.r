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
