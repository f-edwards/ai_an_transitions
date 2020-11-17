# rm(list=ls())
# gc()
# library(tidyverse)
# library(geofacet)
# library(RColorBrewer)
# library(mapproj)
# library(usmap)
# source("main_state.R")
# 
# theme_set(theme_bw())


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
afcars_caseload<-read_csv("./data/state_first_fc.csv")%>%
  filter(.imp==1) %>% 
  rename(year = fy) %>%
  filter(state!=72) %>%
  rename(var = first_entry) %>%
  rename(st_fips = state) %>% 
  filter(year == 2018) %>% 
  mutate(race_ethn = ifelse(race_ethn=="AI/AN",
                            "AIAN",
                            "other")) %>% 
  group_by(st_fips, race_ethn) %>% 
  summarise(fc = sum(var)) %>% 
  ungroup() %>% 
  left_join(pop_seer_nat %>% 
              filter(age<=21) %>% 
              select(state, st_fips, age, race_ethn, pop) %>% 
              group_by(state, st_fips, race_ethn) %>% 
              summarize(pop = sum(pop)) %>% 
              ungroup()) %>% 
  filter(state%in%icwa_fc$state) %>% 
  ungroup()

### make comparable national totals

afcars_nat<-afcars_caseload %>% 
  group_by(race_ethn) %>% 
  summarize(fc = sum(fc),
            pop = sum(pop))

icwa_nat<-left_join(
  icwa_nat %>% 
    pivot_longer(cols = everything()) %>% 
    mutate(race_ethn = 
             case_when(name %in% c("fc_aian",
                                   "pop_aian21") ~ "AIAN",
                       name%in% c("fc_non",
                                  "pop_non") ~ "other")) %>% 
    filter(name %in% c("fc_aian", "fc_non")) %>% 
             rename(fc = value) %>% 
    select(-name),
  icwa_nat %>% 
    pivot_longer(cols = everything()) %>% 
    mutate(race_ethn = 
             case_when(name %in% c("fc_aian",
                                   "pop_aian21") ~ "AIAN",
                       name%in% c("fc_non",
                                  "pop_non") ~ "other")) %>% 
    filter(name %in% c("pop_aian21", "pop_non")) %>% 
    rename(pop = value) %>% 
    select(-name)
  ) %>% 
  mutate(year = 1976)

nat_delta<-icwa_nat %>% 
  mutate(prop_change =  fc/ afcars_nat$fc) %>% 
  bind_rows(afcars_nat %>% 
              mutate(year = 2018,
                     prop_change = 1)) %>% 
  mutate(race_ethn = ifelse(
    race_ethn == "AIAN", "AIAN",
    "Not AIAN"
  )) %>% 
  mutate(rate = fc / pop * 100)
  
ggplot(nat_delta,
       aes(x = factor(year), y = fc)) + 
  geom_col() + 
  facet_wrap(~race_ethn, scales = "free") +
  labs(x = "Year", y = "Children in foster care") 

icwa_delta<-icwa_fc %>% 
  mutate(race_ethn = 
                  ifelse(race_ethn == "White",
                         "other", 
                         race_ethn)) %>% 
  bind_rows(afcars_caseload %>% 
              mutate(period = "2018") %>% 
              select(state, race_ethn, fc, period)) %>% 
  filter(!is.na(state),
         state!="MS")

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


### totals for over/under change
icwa_delta %>% 
  mutate(fc = ifelse(period == "1976",
                     fc * -1,
                     fc)) %>% 
  group_by(state, race_ethn) %>% 
  summarise(delta = sum(fc)) %>% 
  ungroup() %>% 
  group_by(race_ethn) %>% 
  summarise(increased = sum(delta>0),
            decreased = sum(delta<0))

### National table

nat_tab<-list()
race_id<-unique(ncands_inv$race_ethn)
imps<-unique(ncands_inv$.imp)
index<-1
for(i in 1:length(imps)){
  for(r in 1:length(race_id)){
    temp<-ncands_inv 
    temp<-temp %>% 
      filter(race_ethn == race_id[r])
    temp<-temp %>% 
      filter(.imp==i)
    temp<-temp %>% 
      group_by(.imp, age, race_ethn) %>% 
      summarise(var = sum(var), 
                pop = sum(pop),
                pop_max = sum(pop_max)) %>% 
      ungroup()
    nat_tab[[index]]<-make_life_table(temp %>% 
                                    mutate(pop = pop_max))
    index<-index+1
  }
}
nat_tab<-bind_rows(nat_tab)

### Plot national estimates

ggplot(nat_tab %>% 
         filter(race_ethn=="AIAN") %>% 
         group_by(race_ethn, age) %>% 
         summarise(qmax = max(q), qmin=min(q), q = mean(q)), 
  aes(x = age, y = q, ymin = qmin, ymax = qmax,
      color = race_ethn, fill = race_ethn)) + 
  geom_line() + 
  labs(color = "", fill = "") + 
  labs(x = "Age", y = "Proportion of children") + 
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2") + 
  ylim(0, 0.06) +
  ggsave("./vis/data_leaders/1.png", width = 8, height = 4)

ggplot(nat_tab %>% 
         group_by(race_ethn, age) %>% 
         summarise(qmax = max(q), qmin=min(q), q = mean(q)), 
       aes(x = age, y = q, ymin = qmin, ymax = qmax,
           color = race_ethn, fill = race_ethn)) + 
  geom_line() + 
  labs(color = "", fill = "") + 
  labs(x = "Age", y = "Proportion of children") + 
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2") + 
  ylim(0, 0.06) +
  ggsave("./vis/data_leaders/2.png", width = 8, height = 4)


### STATE TABLES
ggplot(investigation_tables %>% 
         group_by(state, race_ethn, age) %>% 
         summarise(qmax = max(q_c), qmin=min(q_c), q = mean(q_c)),
       aes(x = age, y = q, ymin = qmin, ymax = qmax,
           color = race_ethn, fill = race_ethn)) + 
  geom_line() + 
  geom_ribbon(alpha = 0.5, color = NA) + 
  facet_geo(~state) + 
  labs(color = "", fill = "") + 
  theme_minimal() + 
  labs(x = "Age", y = "Proportion of children") + 
  theme(strip.text.x = element_text(size = 7),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 4)) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) + 
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2") + 
  ggsave("./vis/data_leaders/3.png", width = 8, height = 4)

### High pop states

high_pop<-data.frame(state = factor(c("CA", "OK", "AZ", "NM", "TX", 
                               "NC", "NY", "AK",
                               "WA", "SD", "FL", "MT")))

ggplot(investigation_tables %>% 
         filter(state %in% high_pop$state) %>% 
         mutate(state = factor(state, levels = high_pop$state)) %>% 
         group_by(state, race_ethn, age) %>% 
         summarise(qmax = max(q_c), qmin=min(q_c), q = mean(q_c)),
       aes(x = age, y = q, ymin = qmin, ymax = qmax,
           color = race_ethn, fill = race_ethn)) + 
  geom_ribbon(alpha = 0.5, color = NA) + 
  geom_line() + 
  labs(color = "", fill = "") + 
  facet_wrap(~state)+
  labs(x = "Age", y = "Proportion of children") + 
  # theme(strip.text.x = element_text(size = 7),
  #       axis.text.x = element_text(size = 6),
  #       axis.text.y = element_text(size = 6)) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) + 
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2") + 
  ggsave("./vis/data_leaders/4.png", width = 8, height = 4)

ggplot(investigation_tables %>% 
         filter(state %in% high_pop$state) %>% 
         mutate(state = factor(state, levels = high_pop$state)) %>% 
         group_by(state, race_ethn, age) %>% 
         summarise(qmax = max(q_c), qmin=min(q_c), q = mean(q_c)),
       aes(x = age, y = q, ymin = qmin, ymax = qmax,
           color = race_ethn, fill = race_ethn)) + 
  geom_ribbon(alpha = 0.5, color = NA) + 
  geom_line() + 
  labs(color = "", fill = "") + 
  geom_line(data = nat_tab %>% 
              filter(race_ethn == "AIAN") %>% 
              group_by(race_ethn, age) %>% 
              summarise(qmax = max(q), qmin=min(q), q = mean(q)),
            aes(y = q, x = age, color = race_ethn), lty = 2) + 
  facet_wrap(~state)+
  labs(x = "Age", y = "Proportion of children") + 
  # theme(strip.text.x = element_text(size = 7),
  #       axis.text.x = element_text(size = 6),
  #       axis.text.y = element_text(size = 6)) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) + 
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2") + 
  ggsave("./vis/data_leaders/5.png", width = 8, height = 4)

### high rate states
inv_high <- investigation_tables %>% 
  filter(age==0, .imp==1) %>% 
  filter(race_ethn=="AIAN") %>% 
  arrange(desc(q_c)) %>% 
  ungroup() %>% 
  slice(1:6)

ggplot(investigation_tables %>% 
         filter(state %in% inv_high$state) %>% 
         group_by(state, race_ethn, age) %>% 
         summarise(qmax = max(q_c), qmin=min(q_c), q = mean(q_c)),
       aes(x = age, y = q, ymin = qmin, ymax = qmax,
           color = race_ethn, fill = race_ethn)) + 
  geom_ribbon(alpha = 0.5, color = NA) + 
  geom_line() + 
  labs(color = "", fill = "") + 
  geom_line(data = nat_tab %>%
              filter(race_ethn == "AIAN") %>%
              group_by(race_ethn, age) %>%
              summarise(qmax = max(q), qmin=min(q), q = mean(q)),
            aes(y = q, x = age, color = race_ethn), lty = 2) +
  facet_wrap(~state)+
  labs(x = "Age", y = "Proportion of children") + 
  # theme(strip.text.x = element_text(size = 7),
  #       axis.text.x = element_text(size = 6),
  #       axis.text.y = element_text(size = 6)) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) + 
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2") + 
  ggsave("./vis/data_leaders/6.png", width = 8, height = 4)

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
  


############ Subtantiations

### National table

nat_tab_s<-list()
race_id<-unique(ncands_sub$race_ethn)
imps<-unique(ncands_sub$.imp)
index<-1
for(i in 1:length(imps)){
  for(r in 1:length(race_id)){
    temp<-ncands_sub
    temp<-temp %>% 
      filter(race_ethn == race_id[r])
    temp<-temp %>% 
      filter(.imp==i)
    temp<-temp %>% 
      group_by(.imp, age, race_ethn) %>% 
      summarise(var = sum(var), 
                pop = sum(pop),
                pop_max = sum(pop_max)) %>% 
      ungroup()
    nat_tab_s[[index]]<-make_life_table(temp %>% 
                                        mutate(pop = pop_max))
    index<-index+1
  }
}
nat_tab_s<-bind_rows(nat_tab_s)

ggplot(nat_tab_s %>% 
         group_by(race_ethn, age) %>% 
         summarise(qmax = max(q), qmin=min(q), q = mean(q)), 
       aes(x = age, y = q, ymin = qmin, ymax = qmax,
           color = race_ethn, fill = race_ethn)) + 
  geom_line() + 
  labs(color = "", fill = "") + 
  labs(x = "Age", y = "Proportion of children") + 
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2") + 
  ggsave("./vis/data_leaders/8.png", width = 8, height = 4)

### STATE TABLES
ggplot(subst_tables %>% 
         group_by(state, race_ethn, age) %>% 
         summarise(qmax = max(q_c), qmin=min(q_c), q = mean(q_c)),
       aes(x = age, y = q, ymin = qmin, ymax = qmax,
           color = race_ethn, fill = race_ethn)) + 
  geom_line() + 
  geom_ribbon(alpha = 0.5, color = NA) + 
  facet_geo(~state) + 
  labs(color = "", fill = "") + 
  theme_minimal() + 
  labs(x = "Age", y = "Proportion of children") + 
  theme(strip.text.x = element_text(size = 7),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 4)) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) + 
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2") + 
  ggsave("./vis/data_leaders/9.png", width = 8, height = 4)

### maps

map_dat<-subst_tables_c %>% 
  filter(race_ethn=="AIAN") %>% 
  group_by(state) %>% 
  summarize(c = mean(c_c)) 

### xwalk state maps onto abbreviations
us_map<-us_map()

us_map<-us_map %>% 
  left_join(map_dat %>% 
              rename(abbr = state)) 



subst_map<-ggplot(us_map %>% 
         filter(!(is.na(var))),
       aes(x = x, y = y, group = group,
           fill = c)) + 
  geom_polygon(color = "black") + 
  coord_fixed() +
  theme_void() + 
  scale_fill_gradient2() +
  theme(legend.position = "bottom") +
  labs(fill = "Substantiation") +
  guides(fill = guide_colourbar(title.position = "top",
                                label.position = "bottom",
                                title.hjust = 0.5))

### high subst states

### high rate states
sub_high <- subst_tables %>% 
  filter(age==0, .imp==1) %>% 
  filter(race_ethn=="AIAN") %>% 
  arrange(desc(q_c)) %>% 
  ungroup() %>% 
  slice(1:6)

ggplot(subst_tables %>% 
         filter(state %in% sub_high$state) %>% 
         group_by(state, race_ethn, age) %>% 
         summarise(qmax = max(q_c), qmin=min(q_c), q = mean(q_c)),
       aes(x = age, y = q, ymin = qmin, ymax = qmax,
           color = race_ethn, fill = race_ethn)) + 
  geom_ribbon(alpha = 0.5, color = NA) + 
  geom_line() + 
  labs(color = "", fill = "") + 
  facet_wrap(~state)+
  labs(x = "Age", y = "Proportion of children") + 
  # theme(strip.text.x = element_text(size = 7),
  #       axis.text.x = element_text(size = 6),
  #       axis.text.y = element_text(size = 6)) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) + 
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2") + 
  ggsave("./vis/data_leaders/11.png", width = 8, height = 4)


###### FOSTER CARE

nat_tab_fc<-list()
race_id<-unique(ncands_sub$race_ethn)
imps<-unique(ncands_sub$.imp)
index<-1
for(i in 1:length(imps)){
  for(r in 1:length(race_id)){
    temp<-afcars_fc
    temp<-temp %>% 
      filter(race_ethn == race_id[r])
    temp<-temp %>% 
      filter(.imp==i)
    temp<-temp %>% 
      group_by(.imp, age, race_ethn) %>% 
      summarise(var = sum(var), 
                pop = sum(pop),
                pop_max = sum(pop_max)) %>% 
      ungroup()
    nat_tab_fc[[index]]<-make_life_table(temp %>% 
                                          mutate(pop = pop_max))
    index<-index+1
  }
}
nat_tab_fc<-bind_rows(nat_tab_fc)

ggplot(nat_tab_fc %>% 
         group_by(race_ethn, age) %>% 
         summarise(qmax = max(q), qmin=min(q), q = mean(q)), 
       aes(x = age, y = q, ymin = qmin, ymax = qmax,
           color = race_ethn, fill = race_ethn)) + 
  geom_line() + 
  labs(color = "", fill = "") + 
  labs(x = "Age", y = "Proportion of children") + 
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2") + 
  ggsave("./vis/data_leaders/12.png", width = 8, height = 4)

### maps

map_dat<-fc_tables_c %>% 
  filter(race_ethn=="AIAN") %>% 
  group_by(state) %>% 
  summarize(c = mean(c_c)) 

### xwalk state maps onto abbreviations
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
  labs(fill = "Foster Care") +
  theme(legend.position = "bottom") +
  guides(fill = guide_colourbar(title.position = "top",
                                label.position = "bottom",
                                title.hjust = 0.5))

### high rate states
fc_high <- fc_tables %>% 
  filter(age==0, .imp==1) %>% 
  filter(race_ethn=="AIAN") %>% 
  arrange(desc(q_c)) %>% 
  ungroup() %>% 
  slice(1:6)

ggplot(fc_tables %>% 
         filter(state %in% fc_high$state) %>% 
         group_by(state, race_ethn, age) %>% 
         summarise(qmax = max(q_c), qmin=min(q_c), q = mean(q_c)),
       aes(x = age, y = q, ymin = qmin, ymax = qmax,
           color = race_ethn, fill = race_ethn)) + 
  geom_ribbon(alpha = 0.5, color = NA) + 
  geom_line() + 
  labs(color = "", fill = "") + 
  facet_wrap(~state)+
  labs(x = "Age", y = "Proportion of children") + 
  # theme(strip.text.x = element_text(size = 7),
  #       axis.text.x = element_text(size = 6),
  #       axis.text.y = element_text(size = 6)) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) + 
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2") + 
  ggsave("./vis/data_leaders/14.png", width = 8, height = 4)

### TPR

nat_tab_tpr<-list()
race_id<-unique(ncands_inv$race_ethn)
imps<-unique(ncands_inv$.imp)
index<-1
for(i in 1:length(imps)){
  for(r in 1:length(race_id)){
    temp<-tpr_tables 
    temp<-temp %>% 
      filter(race_ethn == race_id[r])
    temp<-temp %>% 
      filter(.imp==i)
    temp<-temp %>% 
      group_by(.imp, age, race_ethn) %>% 
      summarise(var = sum(var), 
                pop = sum(pop),
                pop_max = sum(pop_max)) %>% 
      ungroup()
    nat_tab_tpr[[index]]<-make_life_table(temp %>% 
                                        mutate(pop = pop_max))
    index<-index+1
  }
}
nat_tab_tpr<-bind_rows(nat_tab_tpr)

ggplot(nat_tab_tpr %>% 
         group_by(race_ethn, age) %>% 
         summarise(qmax = max(q), qmin=min(q), q = mean(q)), 
       aes(x = age, y = q, ymin = qmin, ymax = qmax,
           color = race_ethn, fill = race_ethn)) + 
  geom_line() + 
  labs(color = "", fill = "") + 
  labs(x = "Age", y = "Proportion of children") + 
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2") + 
  ggsave("./vis/data_leaders/15.png", width = 8, height = 4)

map_dat<-tpr_tables_c %>% 
  filter(race_ethn=="AIAN") %>% 
  group_by(state) %>% 
  summarize(c = mean(c_c)) 

### xwalk state maps onto abbreviations
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
  labs(fill = "TPR") +
  theme(legend.position = "bottom") +
  guides(fill = guide_colourbar(title.position = "top",
                                label.position = "bottom",
                                title.hjust = 0.5))

library(gridExtra)

grid.arrange(inv_map, subst_map, fc_map, tpr_map)

g<-arrangeGrob(inv_map, subst_map, fc_map, tpr_map,
               nrow = 2)

ggsave("./vis/cjlr/2.png", g, width = 6.5, height = 5)

#### summary map

map_dat<-investigation_tables_c %>% 
  filter(race_ethn=="AIAN") %>% 
  group_by(state) %>% 
  summarize(c = mean(c_c)) %>% 
  mutate(var = "Investigation") %>% 
  bind_rows(subst_tables_c %>% 
              filter(race_ethn=="AIAN") %>% 
              group_by(state) %>% 
              summarize(c = mean(c_c)) %>% 
              mutate(var = "Confirmed malt.")) %>% 
  bind_rows(fc_tables_c %>% 
              filter(race_ethn=="AIAN") %>% 
              group_by(state) %>% 
              summarize(c = mean(c_c)) %>% 
              mutate(var = "Foster Care")) %>% 
  bind_rows(tpr_tables_c %>% 
              filter(race_ethn=="AIAN") %>% 
              group_by(state) %>% 
              summarize(c = mean(c_c)) %>% 
              mutate(var = "TPR")) %>% 
  distinct()

### slice into quantiles
map_dat<-map_dat %>% 
  group_by(var) %>% 
  mutate(Percentile = percent_rank(c) * 100)
### xwalk state maps onto abbreviations
us_map<-us_map()

us_map<-us_map %>% 
  left_join(map_dat %>% 
              rename(abbr = state)) %>% 
  mutate(var = factor(var,
                      levels = c(
                        "Investigation", 
                        "Confirmed malt.",
                        "Foster Care",
                        "TPR"
                      )))


ggplot(us_map %>% 
         filter(!(is.na(var))),
       aes(x = x, y = y, group = group,
           fill = Percentile)) + 
  geom_polygon(color = "black") + 
  coord_fixed() +
  facet_wrap(~var) + 
  theme_void() + 
  scale_fill_gradient2() +
  ggsave("./vis/data_leaders/17.png", width = 8, height = 4)
  


#######################
## Conditionals

## MN conditionals

mn<-investigation_tables %>% 
  filter(.imp==1, age == 0, state == "MN") %>% 
  select(var, race_ethn, pop_max, q_c) %>% 
  bind_rows(
  subst_tables %>% 
  filter(.imp==1, age == 0,  state == "MN") %>% 
  select(var, race_ethn, pop_max, q_c)) %>% 
  bind_rows(fc_tables %>% 
              filter(.imp==1, age == 0,  state == "MN") %>% 
              select(var, race_ethn, pop_max, q_c))

mn_aian<-mn %>% 
  filter(race_ethn=="AIAN")

# mn1<-ncands_sub_inv %>% 
#   filter(.imp==1, age==0, state =="MN")


mn2<-fc_tables %>% 
  filter(.imp==1, age==0, state =="MN") %>% 
  select(race_ethn, q_c)


fc_cond_inv_tabs<-fc_post_inv_tables %>%
  rename(fc_inv = q_c) %>% 
  select(.imp,age,  race_ethn, state, fc_inv) %>% 
  left_join(investigation_tables %>% 
              rename(inv = q_c) %>% 
              select(.imp, age, race_ethn, state, inv)) %>% 
  mutate(fc_cond_inv = fc_inv / inv)


mn2<-fc_cond_inv_tabs%>% 
  filter(.imp==1, age==0, state =="MN") 

fc_cond_sub_tabs<-fc_sub_tables %>%
  rename(fc_sub = q_c) %>% 
  select(.imp,age,  race_ethn, state, fc_sub) %>% 
  left_join(subst_tables %>% 
              rename(sub = q_c) %>% 
              select(.imp, age, race_ethn, state, sub)) %>% 
  mutate(fc_cond_sub = fc_sub / sub)

mn3<-fc_cond_sub_tabs %>% 
  filter(.imp==1, age==0, state=="MN")



### zoom in on high states

hi_states<-c("MN", "ND", "OK", "SD")

### plot each outcome for the high states

ncands_sub_inv_tab<-subst_tables %>%
  rename(sub = q_c) %>% 
  select(.imp , age, race_ethn, state, sub) %>% 
  left_join(investigation_tables %>% 
              rename(inv = q_c) %>% 
              select(.imp, age, race_ethn, state, inv)) %>% 
  mutate(sub_inv = sub / inv) %>% 
  filter(sub_inv<1,
         !(is.na(sub_inv)))



ggplot(ncands_sub_inv_tab %>% 
  filter(state%in%hi_states) %>% 
  group_by(age, race_ethn, state) %>% 
    summarise(qmin = min(sub_inv),
              qmax = max(sub_inv),
              q = mean(sub_inv)),
  aes(x = age, y = q, color = race_ethn,
      ymin = qmin, ymax = qmax, fill = race_ethn)) +
  geom_line() + 
  geom_ribbon(alpha = 0.5,
              color = NA) + 
  labs(fill = "", color = "",
       y = "Probability", x = "Age") + 
  facet_wrap(~state) + 
  ggsave("./vis/data_leaders/18.png", width = 8, height = 4)


ggplot(fc_cond_inv_tabs %>% 
         filter(state%in%hi_states) %>% 
         group_by(age, race_ethn, state) %>% 
         summarise(qmin = min(fc_cond_inv),
                   qmax = max(fc_cond_inv),
                   q = mean(fc_cond_inv)) %>% 
         filter(!(is.na(q)),
                q<1),
       aes(x = age, y = q, color = race_ethn,
           ymin = qmin, ymax = qmax, fill = race_ethn)) +
  geom_line() + 
  geom_ribbon(alpha = 0.5,
              color = NA) + 
  labs(fill = "", color = "",
       y = "Probability", x = "Age") + 
  facet_wrap(~state) + 
  ggsave("./vis/data_leaders/19.png", width = 8, height = 4)

ggplot(fc_cond_sub_tabs %>% 
         filter(state%in%hi_states) %>% 
         group_by(age, race_ethn, state) %>% 
         summarise(qmin = min(fc_cond_sub),
                   qmax = max(fc_cond_sub),
                   q = mean(fc_cond_sub)) %>% 
         filter(!(is.na(q)),
                q<1),
       aes(x = age, y = q, color = race_ethn,
           ymin = qmin, ymax = qmax, fill = race_ethn)) +
  geom_line() + 
  geom_ribbon(alpha = 0.5,
              color = NA) + 
  labs(fill = "", color = "",
       y = "Probability", x = "Age") + 
  facet_wrap(~state) + 
  ggsave("./vis/data_leaders/20.png", width = 8, height = 4)

### ICWA preferred placements LAST

non_icwa_cond_fc<-non_icwa_tables %>%
  rename(non_icwa = q_c) %>% 
  select(.imp, age, race_ethn, state, non_icwa) %>% 
  left_join(fc_tables%>% 
              rename(fc = q_c) %>% 
              select(.imp, age, race_ethn, state, fc)) %>% 
  mutate(non_icwa_fc = non_icwa / fc)

ggplot(non_icwa_cond_fc %>% 
         filter(race_ethn=="AIAN",
                state%in%hi_states) %>% 
         group_by(age, state) %>% 
         summarise(qmin = min(non_icwa_fc),
                   qmax = max(non_icwa_fc),
                   q = mean(non_icwa_fc)) %>% 
         filter(!(is.na(q)),
                q<1),
       aes(x = age, y = q,
           ymin = qmin, ymax = qmax)) + 
  geom_line() +
  geom_ribbon(alpha = 0.5, color = NA) + 
  facet_wrap(~state) + 
  labs(x = "Age", y = "Proportion") + 
  ggsave("./vis/data_leaders/21.png")

### 2x2 map

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
library(mapproj)
library(usmap)

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
  ggsave("./vis/data_leaders/22.png", width = 8, height = 4)
