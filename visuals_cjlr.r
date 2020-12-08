#### make visuals for cjlr icwa paper

rm(list=ls())
gc()
### suppress messages
library(tidyverse, warn.conflicts = FALSE)
options(dplyr.summarise.inform = FALSE)
# Suppress summarise info

library(geofacet)
library(RColorBrewer)
library(mapproj)
library(usmap)
library(gridExtra)
theme_set(theme_bw())


#### make needed state and national tables

# source("make_state_tables.R")
# source("make_national_tables.r")
# source("read_adopt.r")
# 
# save.image("./cjlr.RData")

load("./cjlr.RData")

####################################################################################
############## ICWA figures
####################################################################################

### ICWA AAIA data STATES

icwa_fc<-icwa_fc %>% 
  filter(!is.na(fc))


### ICWA DATA NAT

icwa_nat<-icwa %>% 
  filter(!(is.na(fc_aian))) %>% 
  summarize(fc_aian = sum(fc_aian),
            fc_non = sum(fc_non),
            pop_aian21 = sum(pop_aian21),
            pop_non = sum(pop_nonAIAN21))

icwa_st<-icwa %>% 
  filter(!(is.na(fc_aian))) 

## 


afcars_caseload19<-read_csv("./data/afcars19_caseload.csv")
afcars_caseload19<-afcars_caseload19 %>% 
  mutate(race_ethn = ifelse(AMIAKN==1,
                            "AIAN",
                            "other")) %>% 
  rename(state = St,
         st_fips = STATE,
         fc = caseload) %>% 
  filter(st_fips!=72) %>% 
  select(st_fips, race_ethn, fc) %>% 
  filter(!is.na(race_ethn))

# afcars_caseload<-read_csv("./data/state_first_fc.csv")%>%
#   filter(.imp==1) %>% 
#   rename(year = fy) %>%
#   filter(state!=72) %>%
#   rename(var = first_entry) %>%
#   rename(st_fips = state) %>% 
#   filter(year == 2018) %>% 
#   mutate(race_ethn = ifelse(race_ethn=="AI/AN",
#                             "AIAN",
#                             "other")) %>% 
#   group_by(st_fips, race_ethn) %>% 
#   summarise(fc = sum(var)) %>% 
#   ungroup() %>% 
#   left_join(pop_seer_nat %>% 
#               filter(age<=21) %>% 
#               select(state, st_fips, age, race_ethn, pop) %>% 
#               group_by(state, st_fips, race_ethn) %>% 
#               summarize(pop = sum(pop)) %>% 
#               ungroup()) %>% 
#   filter(state%in%icwa_fc$state) %>% 
#   ungroup()

### add adoption data

### join with afcars adoption data
afcars_caseload<-afcars_caseload19 %>% 
  left_join(afcars_adopt_caseload_19 %>% 
  mutate(race_ethn = ifelse(race_ethn=="AIAN", 
                            race_ethn,
                            "other")) %>% 
  mutate(n_non_preferred = ifelse(race_ethn=="AIAN",
                                  n - n_kin_or_aian,
                                  n - n_kin),
         adopt_non_preferred = n_non_preferred / n) %>% 
    rename(st_fips = STATE,
           state = St,
           adopted = n) %>% 
  select(state, race_ethn, adopted, adopt_non_preferred)) %>% 
  mutate(state,  race_ethn, fc, adopted) %>% 
  mutate(period = "2019")

### state comparisons
icwa_delta<-icwa_fc %>% 
  select(-St) %>% 
  mutate(race_ethn = ifelse(race_ethn=="White",
                            "other",
                            race_ethn)) %>% 
  bind_rows(afcars_caseload %>% 
              select(state, period, race_ethn, fc, adopted)) %>% 
  filter(state%in%icwa_fc$state)

### make comparable national totals
### can't compare per cap rates
### pop denominators aren't comparable

ggplot(icwa_delta %>% 
         mutate(race_ethn = 
                  ifelse(race_ethn=="other",
                         "Non-AIAN",
                         race_ethn)),
       aes(y = state, x = fc, fill = period)) + 
  geom_col(position = position_dodge2(reverse = T)) + 
  facet_wrap(~race_ethn, scales = "free") + 
  scale_fill_brewer(palette = "Dark2") + 
  labs(y = "",
       x = "Children in foster care",
       fill = "") + 
  ggsave("./vis/cjlr/1.png", width = 8, height = 4)
  
  ggplot(icwa_delta %>% 
           mutate(race_ethn = 
                    ifelse(race_ethn=="other",
                           "Non-AIAN",
                           race_ethn)),
         aes(y = state, x = adopted, fill = period)) + 
    geom_col(position = position_dodge2(reverse = T)) + 
    facet_wrap(~race_ethn, scales = "free") + 
    scale_fill_brewer(palette = "Dark2") + 
    labs(y = "",
         x = "Children in adoption",
         fill = "") + 
  ggsave("./vis/cjlr/1_2.png", width = 8, height = 4)

#### make state-level proportional change over time 
### for both outcomes
  
icwa_delta_pct<-
  icwa_delta %>% 
  pivot_wider(id_cols = c(state, race_ethn),
              names_from = period, 
              values_from = c(fc, adopted)) %>% 
  mutate(delta_fc = (fc_2019 / fc_1976 -1) * 100,
         delta_adopt = (adopted_2019/adopted_1976 -1)*100) %>% 
  select(state, race_ethn, delta_fc, delta_adopt) %>% 
  pivot_longer(cols = delta_fc:delta_adopt) %>% 
  mutate(name = ifelse(
    name=="delta_fc",
    "Foster care",
    "Adoption"
  ))

### remove adoption states where we don't have AIAN ICWA numbers
index<-icwa_delta_pct %>% 
  filter(is.na(value))

icwa_delta_pct <- icwa_delta_pct %>% 
  mutate(value = ifelse(state%in%index$state & name == "Adoption",
         NA,
         value))

ggplot(icwa_delta_pct %>% 
         mutate(race_ethn =
                  ifelse(race_ethn == "AIAN",
                         race_ethn,
                         "Non-AIAN")),
       aes(x = value,
           y = state,
           fill = name)) + 
  geom_col(position = position_dodge2(reverse = T)) + 
  labs(y = "") +
  geom_vline(xintercept = 0, lty = 2) +
  scale_fill_brewer(palette = "Dark2") + 
  facet_wrap(~race_ethn, scales = "free") + 
  labs(fill = "", 
       x = "Percent increase, 1976 to 2019") +
  ggsave("./vis/cjlr/1_3.png", width = 8, height = 4)
    
  
icwa_nat<-icwa_delta %>% 
  filter(!state%in%index$state) %>% 
  group_by(period, race_ethn) %>% 
  summarise(fc = sum(fc),
            adopted = sum(adopted))


### plus boarding
total_inst<-icwa_delta %>% 
  group_by(period, race_ethn) %>% 
  summarise(fc = sum(fc),
            adopted = sum(adopted, na.rm=T)) %>% 
  mutate(total = fc + adopted)


##############################################################
###### National age-spec risk
##############################################################

# make 2 x 2 age-spec plots for each outcome

## stack tables for faceting
plot_dat<-inv_nat_tab %>% 
  group_by(race_ethn, age) %>% 
  summarise(qmax = max(q) , qmin=min(q) , q = mean(q),
            c = mean(c), cmax = max(c), cmin = min(c)) %>% 
  ungroup() %>% 
  mutate(var = "Investigation") %>% 
  bind_rows(
    sub_nat_tab %>% 
      group_by(race_ethn, age) %>% 
      summarise(qmax = max(q) , qmin=min(q) , q = mean(q),
                c = mean(c), cmax = max(c), cmin = min(c)) %>% 
      ungroup() %>% 
      mutate(var = "Substantiation")
  ) %>% 
  bind_rows(
    fc_nat_tab %>% 
      group_by(race_ethn, age) %>% 
      summarise(qmax = max(q) , qmin=min(q) , q = mean(q),
                c = mean(c), cmax = max(c), cmin = min(c)) %>% 
      ungroup() %>% 
      mutate(var = "Foster Care")
  ) %>% 
  bind_rows(
    tpr_nat_tab %>% 
      group_by(race_ethn, age) %>% 
      summarise(qmax = max(q) , qmin=min(q) , q = mean(q),
                c = mean(c), cmax = max(c), cmin = min(c)) %>% 
      ungroup() %>% 
      mutate(var = "Termination")
  ) %>% 
  mutate(var = factor(var,
                      levels = c("Investigation",
                                 "Substantiation",
                                 "Foster Care",
                                 "Termination")))


ggplot(plot_dat,
       aes(x = age, y = q, ymin = qmin, ymax = qmax,
           color = race_ethn, fill = race_ethn)) + 
  geom_line() + 
  geom_ribbon(alpha = 0.5, color = NA) +
  labs(color = "", fill = "") + 
  labs(x = "Age", y = "Probability") + 
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2") + 
  facet_wrap(~var) + 
  ggsave("./vis/cjlr/2.png")


##############################################################
###### National conditionals
##############################################################

# make 2 x 2 age-spec plots for each conditional

plot_dat<-cond_sub_inv %>% 
  select(.imp, age, race_ethn, cond, var) %>% 
  bind_rows(
    cond_fc_inv %>% 
      select(.imp, age, race_ethn, cond, var)) %>% 
  bind_rows(cond_fc_sub %>% 
              select(.imp, age, race_ethn, cond, var)) %>% 
  bind_rows(
    cond_tpr_fc %>% 
      select(.imp, age, race_ethn, cond, var)
  ) %>% 
  group_by(age, race_ethn, var) %>% 
  summarise(q = mean(cond),
            qmin = min(cond),
            qmax = max(cond))

plot_dat<-plot_dat %>% 
  mutate(var = case_when(
    var == "P(FC|inv)" ~ "2. Foster care after investigation",
    var == "P(FC|sub)" ~ "3. Foster care after substantiation",
    var == "P(sub|inv)" ~ "1. Substantiation after investigation",
    var == "P(TPR|FC)"  ~ "4. Termination after foster care"
  )) 

ggplot(plot_dat,
       aes(x = age, y = q, ymin = qmin, ymax = qmax,
           fill = race_ethn, color = race_ethn)) + 
  geom_line() + 
  geom_ribbon(alpha = 0.5, color = NA) + 
  facet_wrap(~factor(var)) + 
  labs(color = "", fill = "", x = "Age", y = "Conditional probability") + 
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2") + 
  ggsave("./vis/cjlr/3.png", width = 6.5)

#################################################################
############# MAPS
#################################################################
### Lifetime risk

map_dat<-investigation_tables_c %>% 
  filter(race_ethn=="AIAN") %>% 
  group_by(state) %>% 
  summarize(c = mean(c_c)) 

### xwalk state maps onto abbreviations
us_map<-us_map()

us_map<-us_map %>% 
  left_join(map_dat %>% 
              rename(abbr = state)) 
                      


inv_map<-ggplot(us_map %>% 
         filter(!(is.na(var))),
       aes(x = x, y = y, group = group,
           fill = c)) + 
  geom_polygon(color = "black") + 
  coord_fixed() +
  theme_void() + 
  scale_fill_gradient2() +
  theme(legend.position = "bottom") +
  labs(fill = "Investigation") +
  guides(fill = guide_colourbar(title.position = "top",
                             label.position = "bottom",
                             title.hjust = 0.5))

map_dat<-subst_tables_c %>% 
  filter(race_ethn=="AIAN") %>% 
  group_by(state) %>% 
  summarize(c = mean(c_c)) 

us_map<-us_map()

us_map<-us_map %>% 
  left_join(map_dat %>% 
              rename(abbr = state)) 

sub_map<-ggplot(us_map %>% 
                  filter(!(is.na(var))),
                aes(x = x, y = y, group = group,
                    fill = c)) + 
  geom_polygon(color = "black") + 
  coord_fixed() +
  theme_void() + 
  scale_fill_gradient2() +
  theme(legend.position = "bottom") +
  labs(fill = "Substantation") +
  guides(fill = guide_colourbar(title.position = "top",
                                label.position = "bottom",
                                title.hjust = 0.5))

map_dat<-fc_tables_c %>% 
  filter(race_ethn=="AIAN") %>% 
  group_by(state) %>% 
  summarize(c = mean(c_c)) 

us_map<-us_map()

us_map<-us_map %>% 
  left_join(map_dat %>% 
              rename(abbr = state)) 

fc_map<-ggplot(us_map %>% 
                  filter(!(is.na(var))),
                aes(x = x, y = y, group = group,
                    fill = c)) + 
  geom_polygon(color = "black") + 
  coord_fixed() +
  theme_void() + 
  scale_fill_gradient2() +
  theme(legend.position = "bottom") +
  labs(fill = "Foster Care") +
  guides(fill = guide_colourbar(title.position = "top",
                                label.position = "bottom",
                                title.hjust = 0.5))

map_dat<-tpr_tables_c %>% 
  filter(race_ethn=="AIAN") %>% 
  group_by(state) %>% 
  summarize(c = mean(c_c)) 

us_map<-us_map()

us_map<-us_map %>% 
  left_join(map_dat %>% 
              rename(abbr = state)) 

tpr_map<-ggplot(us_map %>% 
                 filter(!(is.na(var))),
               aes(x = x, y = y, group = group,
                   fill = c)) + 
  geom_polygon(color = "black") + 
  coord_fixed() +
  theme_void() + 
  scale_fill_gradient2() +
  theme(legend.position = "bottom") +
  labs(fill = "TPR") +
  guides(fill = guide_colourbar(title.position = "top",
                                label.position = "bottom",
                                title.hjust = 0.5))


c_map<-grid.arrange(inv_map, sub_map,
                    fc_map, tpr_map)

ggsave("./vis/cjlr/4.png", c_map)


##################
#### #### #### #### 
#### rate ratio for inequality
#### #### #### #### 
### xwalk state maps onto abbreviations
us_map<-us_map()

us_map<-us_map %>% 
  left_join(map_dat %>% 
              rename(abbr = state)) 


#### DO OTHER MAPS HERE

  


map_dat<-investigation_tables_c %>% 
  group_by(state, race_ethn) %>% 
  filter(c == min(c)) %>% 
  mutate(var = "Investigation") %>% 
  bind_rows(subst_tables_c %>% 
              group_by(state, race_ethn) %>% 
              filter(c == min(c)) %>% 
              mutate(var = "Substantiation")) %>% 
  bind_rows(fc_tables_c %>% 
              group_by(state, race_ethn) %>% 
              filter(c == min(c)) %>% 
              mutate(var = "Foster Care")) %>% 
  bind_rows(tpr_tables_c %>% 
              group_by(state, race_ethn) %>% 
              filter(c == min(c)) %>% 
              mutate(var = "TPR")) %>% 
  select(-.imp) %>% 
  distinct()

map_dat_wht<-map_dat %>% 
  ungroup() %>% 
  filter(race_ethn == "White") %>% 
  rename(c_white = c_c) %>% 
  select(-race_ethn, -c, -race_ethn)

map_dat<-map_dat %>% 
  filter(race_ethn=="AIAN") %>% 
  left_join(map_dat_wht) %>% 
  mutate(disp = c_c / c_white) 

### slice into quantiles
### xwalk state maps onto abbreviations
us_map<-us_map()

us_map<-us_map %>% 
  left_join(map_dat %>% 
              rename(abbr = state)) %>% 
  mutate(var = factor(var,
                      levels = c(
                        "Investigation", 
                        "Substantiation",
                        "Foster Care",
                        "TPR"
                      )))

### cut into an ordered categorical 

us_map<-us_map %>% 
  mutate(rate_ratio = 
           case_when(
             disp<0.5 ~ "0-0.5",
             disp<1 ~ "0.5-1",
             disp<1.5 ~ "1-1.5",
             disp<2 ~ "1.5-2",
             disp<3 ~ "2-3",
             disp<4 ~ "3-4",
             disp>=4 ~ "4+",
           ),
         rate_ratio = factor(rate_ratio,
                             levels = c("0-0.5",
                                        "0.5-1",
                                        "1-1.5",
                                        "1.5-2",
                                        "2-3",
                                        "3-4",
                                        "4+")))


ggplot(us_map,
       aes(x = x, y = y, group = group,
           fill = rate_ratio)) + 
  geom_polygon(color = "black") + 
  coord_fixed() +
  facet_wrap(~var) + 
  theme_void() + 
  scale_fill_brewer(palette = "Purples") +
  labs(fill = "AIAN / White\nRate Ratio") + 
  ggsave("./vis/cjlr/5.png", width = 8, height = 4)
