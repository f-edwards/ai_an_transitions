### load packages and suppress warnings
library(tidyverse)

load("./cfp_final_report.RData")

### merge _c tables
cumul_out<-investigation_tables_c %>% 
  rename(investigation = c) %>% 
  left_join(
    subst_tables_c %>% 
      rename(substantiation = c)
  ) %>% 
  left_join(
    fc_tables_c %>% 
      rename(fc = c)
  ) %>% 
  left_join(
    tpr_tables_c %>% 
      rename(tpr = c)
  ) %>% 
  group_by(state, race_ethn) %>% 
  summarise(investigation = mean(investigation),
            substantiation = mean(substantiation),
            fc = mean(fc),
            tpr = mean(tpr))

write_csv(cumul_out, "./data/cfp_datavis_cumulative.csv")

### merge age-spec tables

age_out<-investigation_tables %>% 
  select(.imp, age, race_ethn, state, q) %>% 
  rename(investigation = q) %>% 
  left_join(
    subst_tables %>% 
      select(.imp, age, race_ethn, state, q) %>% 
      rename(substantiation = q)
  ) %>% 
  left_join(
    fc_tables %>% 
      select(.imp, age, race_ethn, state, q) %>% 
      rename(fc = q)
  ) %>% 
  left_join(
    tpr_tables %>% 
      select(.imp, age, race_ethn, state, q) %>% 
      rename(tpr = q)
  ) %>% 
  group_by(age, state, race_ethn) %>% 
  summarise(investigation = mean(investigation),
            substantiation = mean(substantiation),
            fc = mean(fc),
            tpr = mean(tpr))

write_csv(age_out, "./data/cfp_datavis_agespec.csv")
 