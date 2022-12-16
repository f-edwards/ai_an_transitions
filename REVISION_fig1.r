library(tidyverse)
library(lubridate)
library(RColorBrewer)

### make point-in-time state caseloads for ICWA comps
#### read in 2019 afcars

afcars_st<-read_csv("~/Projects/ndacan_processing/data/afcars_all_events_state_15_19.csv")

afcars_st<-afcars_st %>% 
  mutate(race_ethn = ifelse(race_ethn== "AI/AN", "AIAN", race_ethn))

pop<-read_fwf("~/Projects/data/us.1990_2019.singleages.adjusted.txt",
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
             race==3 ~ "AIAN",
             race==4 ~ "Asian/PI",
             hisp==1 ~ "Hispanic")) %>%
  mutate(age = as.integer(age)) %>%
  filter(year>=2000)

pop<-pop %>% 
  group_by(year, state, st_fips, age, race_ethn) %>% 
  summarise(pop = sum(pop))


### compute total event rates cross age
### then imputed min/max for each state for AIAN for contact
totals_15_19<-afcars_st %>% 
  filter(year>=2015, race_ethn == "AIAN", age<18) %>% 
  group_by(.imp, state, year) %>% 
  summarize(fc_total_contact = sum(fc_total_contact))

### MI Std Error using rubin's rules
v_within<-totals_15_19 %>% 
  group_by(state, .imp) %>% 
  summarize(v = var(fc_total_contact)) %>% 
  summarize(v_bar = mean(v))

### between imputation variance is 1/(m-1) \sum_1^m (\hat{x} - \bar{x})^2
# \bar{x} = fc_contact_mean
# \hat{x} = 5-yr mean per imputation
m<-max(afcars_st$.imp)

v_between<-totals_15_19 %>% 
  group_by(state, .imp) %>% 
  summarize(xhat = mean(fc_total_contact)) %>% 
  left_join(totals_15_19 %>% 
              group_by(state) %>% 
              summarize(fc_contact_mean = mean(fc_total_contact))) %>% 
  summarize(b = 1 / (m-1) * sum((xhat - fc_contact_mean)^2))

### total variance is v_bar + (1 + 1/m)b
v_total<-v_within %>% 
  left_join(v_between) %>% 
  mutate(v_total = v_bar + (1 + 1/m) * b,
         fc_contact_mean_se = sqrt(v_total)) %>% 
  select(state, fc_contact_mean_se)

### join pop, compute per cap for states 
totals_15_19<-totals_15_19 %>% 
  group_by(state) %>% 
  summarize(fc_contact_mean = mean(fc_total_contact)) %>% 
  left_join(v_total) %>% 
  rename(st_fips = state) %>% 
  left_join(pop %>% 
              mutate(st_fips = as.numeric(st_fips)) %>% 
              filter(year>=2015, 
                     race_ethn == "AIAN",
                     age<18) %>% 
              group_by(state, st_fips) %>% 
              summarize(pop_5yr = sum(pop)/5)) 

### standardize to rates per 1k
totals_15_19<-totals_15_19 %>% 
  mutate(fc_contact_rt = fc_contact_mean / pop_5yr * 1e2,
         fc_contact_rt_se = fc_contact_mean_se / pop_5yr * 1e2) 
### OK - The top 10 are reasonable states to work with, after removing HI
### HI pop counts are strange, likely counting some Native HI as AIAN in FC
### ADD THAT AS A FOOTNOTE

### make FIG 1: COUNTS AND RATES

counts<-totals_15_19 %>% 
  select(st_fips, state, fc_contact_mean, fc_contact_mean_se) %>% 
  rename(var = fc_contact_mean, var_sd = fc_contact_mean_se) %>% 
  mutate(varname = "2. Children in foster care (count)")

rates<-totals_15_19 %>% 
  select(st_fips, state, fc_contact_rt, fc_contact_rt_se) %>% 
  rename(var = fc_contact_rt, var_sd = fc_contact_rt_se) %>% 
  mutate(varname = "1. Children in foster care (percent of population)") 

st_levels<-rates %>% 
  arrange(var)

plot_dat<-bind_rows(rates, counts) %>% 
  mutate(state = factor(state, 
                        levels = st_levels$state))

### add AAIA numbers to fig 1

pre_icwa<-read_csv("./data/icwa_data.csv") %>% 
  left_join(data.frame(state = state.abb, State = state.name)) %>% 
  mutate("FC" = fc_aian,
         "FC+adopted" = fc_aian + adopted_aian) %>% 
  select(state, pop_aian21, 'FC', 'FC+adopted') %>% 
  pivot_longer(FC:'FC+adopted',
               names_to = "var",
               values_to = "n") %>% 
  mutate(rate = n / pop_aian21 * 1e2) %>% 
  select(-pop_aian21) %>% 
  filter(state!="MS") 

levels<-pre_icwa %>% 
  filter(var=="FC") %>% 
  arrange(rate) %>% 
  select(state)

pre_icwa<-pre_icwa %>% 
  mutate(state = factor(state, levels = levels$state))


#### SET UP JOINT ICWA / CONTEMP DATA FIGURE HERE
pre_icwa_long<-pre_icwa %>% 
  rename(pre_icwa_varname = var) %>% 
  pivot_longer(n:rate,
               values_to = "pre_icwa_var",
               names_to = "varname") %>% 
  mutate(varname = case_when(
    varname == "n"~ "2. Children in foster care (count)",
    varname == "rate" ~ "1. Children in foster care (percent of population)"
  ))


plot_dat<-plot_dat %>% 
  left_join(pre_icwa_long)

ggplot(plot_dat %>% 
         filter(state!="HI"),
       aes(x = var, y = state)) + 
  geom_point() + 
  geom_linerange(aes(xmin = var - var_sd,
                     xmax = var + var_sd)) + 
  geom_point(aes(y = state, x = pre_icwa_var, color = pre_icwa_varname),
             alpha = 0.7)+
  scale_fill_brewer(palette = "Dark2", na.translate = F) + 
  scale_color_brewer(palette = "Dark2", na.translate = F) +
  labs(y = "", x = "AIAN children in foster care",
       fill = "1976 level",
       color = "1976 level") + 
  facet_wrap(~varname, scales = "free_x") 

ggsave("./vis/fig1.png",
       height = 8, width = 8)
