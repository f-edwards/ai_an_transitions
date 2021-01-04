### load packages and suppress warnings
library(tidyverse, warn.conflicts = FALSE)
library(bookdown)
library(geofacet)
library(RColorBrewer)
library(mapproj)
library(usmap)
library(gridExtra)
theme_set(theme_bw())

# options
options(scipen=999)
load("./cfp_final_report.RData")

### ICWA DATA NAT

icwa_nat<-icwa_fc %>%
  group_by(race_ethn) %>% 
  summarize(fc = sum(fc, na.rm=T),
            adopted = sum(adopted, na.rm=T),
            boarding = sum(boarding, na.rm=T))

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
  mutate(race_ethn = ifelse(race_ethn=="White",
                            "other",
                            race_ethn)) %>% 
  bind_rows(afcars_caseload %>% 
              select(state, period, race_ethn, fc, adopted)) %>% 
  filter(state%in%icwa_fc$state) %>% 
  mutate(boarding = ifelse(is.na(boarding), 0, boarding))

### make comparable national totals
### can't compare per cap rates
### pop denominators aren't comparable

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

icwa_nat_na<-icwa_delta %>% 
  group_by(period, race_ethn) %>% 
  summarise(fc = sum(fc, na.rm=T),
            adopted = sum(adopted, na.rm=T))

### change since ICWA
### step through figure

### start with table
icwa_delta %>% 
  filter(state == "AK") %>% 
  mutate(race_ethn = 
           ifelse(race_ethn=="other",
                  "Non-AIAN",
                  race_ethn))

ggplot(icwa_delta %>% 
         filter(state == "AK",
                period == 1976) %>% 
         mutate(race_ethn = 
                  ifelse(race_ethn=="other",
                         "Non-AIAN",
                         race_ethn)),
       aes(y = race_ethn, x = fc, fill = period)) + 
  geom_col(position = position_dodge2(reverse = T)) + 
  scale_fill_brewer(palette = "Dark2") + 
  labs(y = "",
       x = "Children in foster care",
       fill = "")  + 
  ggsave("./vis/ak_icwa_1.png", width = 8, height = 4)

ggplot(icwa_delta %>% 
         filter(state == "AK") %>% 
         mutate(race_ethn = 
                  ifelse(race_ethn=="other",
                         "Non-AIAN",
                         race_ethn)),
       aes(y = race_ethn, x = fc, fill = period)) + 
  geom_col(position = position_dodge2(reverse = T)) + 
  scale_fill_brewer(palette = "Dark2") + 
  labs(y = "",
       x = "Children in foster care",
       fill = "")  + 
  ggsave("./vis/ak_icwa_2.png", width = 8, height = 4)

ggplot(icwa_delta %>% 
         filter(state == "AK") %>% 
         mutate(race_ethn = 
                  ifelse(race_ethn=="other",
                         "Non-AIAN",
                         race_ethn)),
       aes(y = race_ethn, x = adopted, fill = period)) + 
  geom_col(position = position_dodge2(reverse = T)) + 
  scale_fill_brewer(palette = "Dark2") + 
  labs(y = "",
       x = "Children in Adoptions",
       fill = "") + 
  ggsave("./vis/ak_icwa_3.png", width = 8, height = 4)

ggplot(icwa_delta %>% 
         filter(state == "AK") %>% 
         mutate(race_ethn = 
                  ifelse(race_ethn=="other",
                         "Non-AIAN",
                         race_ethn)),
       aes(y = race_ethn, x = fc + adopted + boarding, fill = period)) + 
  geom_col(position = position_dodge2(reverse = T)) + 
  scale_fill_brewer(palette = "Dark2") + 
  labs(y = "",
       x = "Children in foster care, adoption, or BIA boarding school",
       fill = "") + 
  ggsave("./vis/ak_icwa_4.png", width = 8, height = 4)

icwa_delta_pct <- icwa_delta_pct %>% 
  mutate(value = ifelse(state%in%index$state & name == "Adoption",
                        NA,
                        value))

ggplot(icwa_delta_pct %>% 
         filter(state=="AK") %>% 
         mutate(race_ethn =
                  ifelse(race_ethn == "AIAN",
                         race_ethn,
                         "Non-AIAN")),
       aes(x = value,
           y = race_ethn,
           fill = name)) + 
  geom_col(position = position_dodge2(reverse = T)) + 
  labs(y = "") +
  geom_vline(xintercept = 0, lty = 2) +
  scale_fill_brewer(palette = "Dark2") + 
  labs(fill = "", 
       x = "Percent increase, 1976 to 2019")+ 
  ggsave("./vis/ak_icwa_5.png", width = 8, height = 4)

##### age specs

nat_plot_dat<-inv_nat_tab %>% 
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


ggplot(investigation_tables %>% 
         filter(state=="AK") %>% 
         group_by(state, race_ethn, age) %>% 
         summarise(qmax = max(q), qmin=min(q), q = mean(q)) %>% 
         ungroup() %>% 
         mutate(state = "Alaska"),
       aes(x = age, y = q, color = race_ethn)) + 
  geom_line() + 
  labs(color = "", fill = "", lty = "") + 
  labs(x = "Age", y = "Probability")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2") + 
  ggsave("./vis/ak_inv_5.png", width = 8, height = 4)

ggplot(investigation_tables %>% 
         filter(state=="AK") %>% 
         group_by(state, race_ethn, age) %>% 
         summarise(qmax = max(q), qmin=min(q), q = mean(q)) %>% 
         ungroup() %>% 
         mutate(state = "Alaska") %>% 
         bind_rows(
           nat_plot_dat %>% 
             mutate(state = "National") %>% 
             filter(var=="Investigation") %>% 
             select(-var)
         ),
       aes(x = age, y = q, color = race_ethn, lty = state)) + 
  geom_line() + 
  labs(color = "", fill = "", lty = "") + 
  labs(x = "Age", y = "Probability")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2") + 
  ggsave("./vis/ak_inv_6.png", width = 8, height = 4)

ggplot(fc_tables %>% 
         filter(state=="AK") %>% 
         group_by(state, race_ethn, age) %>% 
         summarise(qmax = max(q), qmin=min(q), q = mean(q)) %>% 
         ungroup() %>% 
         mutate(state = "Alaska") %>% 
         bind_rows(
           nat_plot_dat %>% 
             mutate(state = "National") %>% 
             filter(var=="Foster Care") %>% 
             select(-var)
         ),
       aes(x = age, y = q, color = race_ethn, lty = state)) + 
  geom_line() + 
  labs(color = "", fill = "", lty = "") + 
  labs(x = "Age", y = "Probability")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2") + 
  ggsave("./vis/ak_fc_7.png", width = 8, height = 4)



map_dat<-investigation_tables_c %>% 
  filter(race_ethn=="AIAN") %>% 
  group_by(state) %>% 
  summarize(c = mean(c)) 

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
  summarize(c = mean(c)) 

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
  summarize(c = mean(c)) 

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
  summarize(c = mean(c)) 

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

ggsave("./vis/ak_map_8.png", c_map, width = 8, height = 4)


####### CONDITIONALS

ncands_sub_inv_tab<-subst_tables %>%
  rename(sub = q) %>% 
  select(.imp , age, race_ethn, state, sub) %>% 
  left_join(investigation_tables %>% 
              rename(inv = q) %>% 
              select(.imp, age, race_ethn, state, inv)) %>% 
  mutate(sub_inv = sub / inv) %>% 
  filter(sub_inv<1,
         !(is.na(sub_inv))) %>% 
  rename(value = sub_inv) %>% 
  mutate(var = "1. Substantiation after investigation") %>% 
  select(.imp, age, race_ethn, state, value, var)

fc_cond_inv_tabs<-fc_post_inv_tables %>%
  rename(fc_inv = q) %>% 
  select(.imp,age,  race_ethn, state, fc_inv) %>% 
  left_join(investigation_tables %>% 
              rename(inv = q) %>% 
              select(.imp, age, race_ethn, state, inv)) %>% 
  mutate(fc_cond_inv = fc_inv / inv) %>% 
  filter(!is.infinite(fc_cond_inv))%>% 
  rename(value = fc_cond_inv) %>% 
  mutate(var = "2. Foster care after investigation")%>% 
  select(.imp, age, race_ethn, state, value, var)

fc_cond_sub_tabs<-fc_sub_tables %>%
  rename(fc_sub = q) %>% 
  select(.imp,age,  race_ethn, state, fc_sub) %>% 
  left_join(subst_tables %>% 
              rename(sub = q) %>% 
              select(.imp, age, race_ethn, state, sub)) %>% 
  mutate(fc_cond_sub = fc_sub / sub)%>% 
  filter(!is.infinite(fc_cond_sub)) %>% 
  filter(fc_cond_sub<1)%>% 
  rename(value = fc_cond_sub) %>% 
  mutate(var = "3. Foster care after substantiation")%>% 
  select(.imp, age, race_ethn, state, value, var)

plot_dat<-bind_rows(ncands_sub_inv_tab,
                    fc_cond_inv_tabs,
                    fc_cond_sub_tabs)

ggplot(plot_dat %>% 
         filter(state=="AK") %>% 
         group_by(state, race_ethn, age, var) %>% 
         summarise(q = mean(value)) %>% 
         ungroup() %>% 
         mutate(state = "Alaska"),
       aes(x = age, y = q, color = race_ethn)) + 
  geom_line() + 
  labs(color = "", fill = "", lty = "") + 
  labs(x = "Age", y = "Probability")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2") + 
  facet_wrap(~var) + 
  ggsave("./vis/ak_cond_9.png", width = 8, height = 4)

non_icwa_cond_fc<-non_icwa_tables %>%
  rename(non_icwa = q) %>% 
  select(.imp, age, race_ethn, state, non_icwa) %>% 
  left_join(fc_tables%>% 
              rename(fc = q) %>% 
              select(.imp, age, race_ethn, state, fc)) %>% 
  mutate(non_icwa_fc = non_icwa / fc)

ggplot(non_icwa_cond_fc %>% 
         filter(race_ethn=="AIAN",
                state == "AK") %>% 
         group_by(age, state) %>% 
         summarise(qmin = min(non_icwa_fc),
                   qmax = max(non_icwa_fc),
                   q = mean(non_icwa_fc)) %>% 
         filter(!(is.na(q)),
                q<=1, qmax<=1),
       aes(x = age, y = q,
           ymin = qmin, ymax = qmax)) + 
  geom_line() +
  geom_ribbon(alpha = 0.5, color = NA) + 
  labs(x = "Age", y = "Proportion") + 
  ggsave("./vis/ak_non_icwa_10.png", width = 8, height = 4)

