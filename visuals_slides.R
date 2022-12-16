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
  mutate(boarding = ifelse(is.na(boarding), 0, boarding)) %>% 
  filter(state!="MS")

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



ggplot(icwa_delta %>% 
         filter(period == 1976,
                race_ethn == "AIAN"),
       aes(y = state, x = fc, fill = period)) + 
  geom_col(position = position_dodge2(reverse = T)) + 
  scale_fill_brewer(palette = "Dark2") + 
  labs(y = "",
       x = "Children in foster care",
       fill = "")  
  ggsave("./vis/slides_1.png", width = 8, height = 4)

ggplot(icwa_delta %>% 
         filter(race_ethn == "AIAN"),
       aes(y = state, x = fc, fill = period)) + 
  geom_col(position = position_dodge2(reverse = T)) + 
  scale_fill_brewer(palette = "Dark2") + 
  labs(y = "",
       x = "Children in foster care",
       fill = "")  
  ggsave("./vis/slides_2.png", width = 8, height = 4)

ggplot(icwa_delta,
      aes(y = state, x = fc, fill = period)) + 
  geom_col(position = position_dodge2(reverse = T)) + 
  scale_fill_brewer(palette = "Dark2") + 
  labs(y = "",
       x = "Children in foster care",
       fill = "")  + 
  facet_wrap(~race_ethn, ncol = 2, scales = "free") 
  ggsave("./vis/slides_3.png", width = 8, height = 4)

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

### fig 4, nat investigation step through

ggplot(nat_plot_dat %>% 
         filter(var=="Investigation",
                race_ethn=="AIAN"),
       aes(x = age, y = q, color = race_ethn)) + 
  geom_line() + 
  labs(color = "", fill = "", lty = "") + 
  labs(x = "Age", y = "Probability")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2")  
  ggsave("./vis/slides_4.png", width = 8, height = 4)

ggplot(nat_plot_dat %>% 
         filter(var=="Investigation"),
       aes(x = age, y = q, color = race_ethn)) + 
  geom_line() + 
  labs(color = "", fill = "", lty = "") + 
  labs(x = "Age", y = "Probability")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2")  
  ggsave("./vis/slides_5.png", width = 8, height = 4)

### fig 6 facet_geo for inv

ggplot(investigation_tables %>% 
         group_by(state, race_ethn, age) %>% 
         summarise(qmax = max(q), qmin=min(q), q = mean(q)) %>% 
         ungroup(),
  aes(x = age, y = q, color = race_ethn,
      ymin = qmin, ymax = qmax)) + 
  geom_line() + 
  geom_ribbon(aes(color = NULL, fill = race_ethn), alpha = 0.5)+
  labs(color = "", fill = "", lty = "") + 
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2") + 
  facet_geo(~state) + 
  theme_minimal() +
  labs(x = "Age", y = "Probability") + 
  theme(strip.text.x = element_text(size = 7),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 4)) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) + 
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2") 
  ggsave("./vis/slides_6.png", width = 8, height = 4)

### SD compared to national
ggplot(investigation_tables %>% 
         filter(state=="MN") %>% 
         group_by(state, race_ethn, age) %>% 
         summarise(qmax = max(q), qmin=min(q), q = mean(q)) %>% 
         ungroup() %>% 
         mutate(state = "Minnesota") %>% 
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
  scale_color_brewer(palette = "Dark2") 
  ggsave("./vis/slides_7.png", width = 8, height = 4)

### investigation c map

map_dat<-investigation_tables_c %>% 
  filter(race_ethn=="AIAN") %>% 
  group_by(state) %>% 
  summarize(c = mean(c)) 

### xwalk state maps onto abbreviations
us_map<-us_map()

us_map<-us_map %>% 
  left_join(map_dat %>% 
              rename(abbr = state)) 

ggplot(us_map,
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
                                title.hjust = 0.5)) + 
  ggsave("./vis/slides_8.png",  width = 8, height = 4)

### substantiation

ggplot(nat_plot_dat %>% 
         filter(var=="Substantiation"),
       aes(x = age, y = q, color = race_ethn)) + 
  geom_line() + 
  labs(color = "", fill = "", lty = "") + 
  labs(x = "Age", y = "Probability")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2") 
  ggsave("./vis/slides_9.png", width = 8, height = 4)

###  facet_geo for sub

ggplot(subst_tables %>% 
         group_by(state, race_ethn, age) %>% 
         summarise(qmax = max(q), qmin=min(q), q = mean(q)) %>% 
         ungroup(),
       aes(x = age, y = q, color = race_ethn,
           ymin = qmin, ymax = qmax)) + 
  geom_line() + 
  geom_ribbon(aes(color = NULL, fill = race_ethn), alpha = 0.5)+
  labs(color = "", fill = "", lty = "") + 
  labs(x = "Age", y = "Probability")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2") + 
  facet_geo(~state) + 
  theme_minimal() +
  labs(x = "Age", y = "Probability") + 
  theme(strip.text.x = element_text(size = 7),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 4)) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) 
  ggsave("./vis/slides_10.png", width = 8, height = 4)

### SD compared to national
ggplot(subst_tables %>% 
         filter(state=="MN") %>% 
         group_by(state, race_ethn, age) %>% 
         summarise(qmax = max(q), qmin=min(q), q = mean(q)) %>% 
         ungroup() %>% 
         mutate(state = "Minnesota") %>% 
         bind_rows(
           nat_plot_dat %>% 
             mutate(state = "National") %>% 
             filter(var=="Substantiation") %>% 
             select(-var)
         ),
       aes(x = age, y = q, color = race_ethn, lty = state)) + 
  geom_line() + 
  labs(color = "", fill = "", lty = "") + 
  labs(x = "Age", y = "Probability")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2") + 
  ggsave("./vis/slides_11.png", width = 8, height = 4)

### cumulative map for sub
map_dat<-subst_tables_c %>% 
  filter(race_ethn=="AIAN") %>% 
  group_by(state) %>% 
  summarize(c = mean(c)) 

### xwalk state maps onto abbreviations
us_map<-us_map()

us_map<-us_map %>% 
  left_join(map_dat %>% 
              rename(abbr = state)) 

ggplot(us_map,
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
  ggsave("./vis/slides_12.png",  width = 8, height = 4)

### foster care

ggplot(nat_plot_dat %>% 
         filter(var=="Foster Care"),
       aes(x = age, y = q, color = race_ethn)) + 
  geom_line() + 
  labs(color = "", fill = "", lty = "") + 
  labs(x = "Age", y = "Probability")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2") + 
  ggsave("./vis/slides_13.png", width = 8, height = 4)

###  facet_geo for FC

ggplot(fc_tables %>% 
         group_by(state, race_ethn, age) %>% 
         summarise(qmax = max(q), qmin=min(q), q = mean(q)) %>% 
         ungroup(),
       aes(x = age, y = q, color = race_ethn,
           ymin = qmin, ymax = qmax)) + 
  geom_line() + 
  geom_ribbon(aes(color = NULL, fill = race_ethn), alpha = 0.5)+
  labs(color = "", fill = "", lty = "") + 
  labs(x = "Age", y = "Probability")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2") + 
  facet_geo(~state) + 
  theme_minimal() +
  labs(x = "Age", y = "Probability") + 
  theme(strip.text.x = element_text(size = 7),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 4)) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) + 
    ggsave("./vis/slides_14.png", width = 8, height = 4)

### SD compared to national
ggplot(fc_tables %>% 
         filter(state=="MN") %>% 
         group_by(state, race_ethn, age) %>% 
         summarise(qmax = max(q), qmin=min(q), q = mean(q)) %>% 
         ungroup() %>% 
         mutate(state = "Minnesota") %>% 
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
  scale_color_brewer(palette = "Dark2") 
  ggsave("./vis/slides_15.png", width = 8, height = 4)

### cumulative map for fc
map_dat<-fc_tables_c %>% 
  filter(race_ethn=="AIAN") %>% 
  group_by(state) %>% 
  summarize(c = mean(c)) 

### xwalk state maps onto abbreviations
us_map<-us_map()

us_map<-us_map %>% 
  left_join(map_dat %>% 
              rename(abbr = state)) 

ggplot(us_map,
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
  ggsave("./vis/slides_16.png",  width = 8, height = 4)


### tpr

ggplot(nat_plot_dat %>% 
         filter(var=="Termination"),
       aes(x = age, y = q, color = race_ethn)) + 
  geom_line() + 
  labs(color = "", fill = "", lty = "") + 
  labs(x = "Age", y = "Probability")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2") + 
  ggsave("./vis/slides_17.png", width = 8, height = 4)

###  facet_geo for FC

ggplot(tpr_tables %>% 
         group_by(state, race_ethn, age) %>% 
         summarise(qmax = max(q), qmin=min(q), q = mean(q)) %>% 
         ungroup(),
       aes(x = age, y = q, color = race_ethn,
           ymin = qmin, ymax = qmax)) + 
  geom_line() + 
  geom_ribbon(aes(color = NULL, fill = race_ethn), alpha = 0.5)+
  labs(color = "", fill = "", lty = "") + 
  labs(x = "Age", y = "Probability")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2") + 
  facet_geo(~state) + 
  theme_minimal() +
  labs(x = "Age", y = "Probability") + 
  theme(strip.text.x = element_text(size = 7),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 4)) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) + 
    ggsave("./vis/slides_18.png", width = 8, height = 4)

### SD compared to national
ggplot(tpr_tables %>% 
         filter(state=="MN") %>% 
         group_by(state, race_ethn, age) %>% 
         summarise(qmax = max(q), qmin=min(q), q = mean(q)) %>% 
         ungroup() %>% 
         mutate(state = "Minnesota") %>% 
         bind_rows(
           nat_plot_dat %>% 
             mutate(state = "National") %>% 
             filter(var=="Termination") %>% 
             select(-var)
         ),
       aes(x = age, y = q, color = race_ethn, lty = state)) + 
  geom_line() + 
  labs(color = "", fill = "", lty = "") + 
  labs(x = "Age", y = "Probability")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2") 
  ggsave("./vis/slides_19.png", width = 8, height = 4)

### cumulative map for fc
map_dat<-tpr_tables_c %>% 
  filter(race_ethn=="AIAN") %>% 
  group_by(state) %>% 
  summarize(c = mean(c)) 

### xwalk state maps onto abbreviations
us_map<-us_map()

us_map<-us_map %>% 
  left_join(map_dat %>% 
              rename(abbr = state)) 

ggplot(us_map,
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
  ggsave("./vis/slides_20.png",  width = 8, height = 4)


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

labs<-unique(plot_dat$var)

ggplot(plot_dat %>% 
         filter(
           state %in% c("SD", "ND", "MN", "NE"),
           var == labs[1]) %>% 
         group_by(state, race_ethn, age, var) %>% 
         summarise(q = mean(value)) %>% 
         ungroup() %>% 
         filter(q<=1),
       aes(x = age, y = q, color = race_ethn)) + 
  geom_line() + 
  labs(color = "", fill = "", lty = "") + 
  labs(x = "Age", y = "Probability")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2") + 
  facet_wrap(~state, ncol = 4) 
  ggsave("./vis/slides_21.png", width = 8, height = 4)

ggplot(plot_dat %>% 
         filter(
           state %in% c("SD", "ND", "MN", "NE"),
           var == labs[2]) %>% 
         group_by(state, race_ethn, age, var) %>% 
         summarise(q = mean(value)) %>% 
         ungroup() %>% 
         filter(q<=1),
       aes(x = age, y = q, color = race_ethn)) + 
  geom_line() + 
  labs(color = "", fill = "", lty = "") + 
  labs(x = "Age", y = "Probability")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2") + 
  facet_wrap(~state, ncol = 4) + 
  ggsave("./vis/slides_22.png", width = 8, height = 4)

ggplot(plot_dat %>% 
         filter(
           state %in% c("SD", "ND", "MN", "NE"),
           var == labs[3]) %>% 
         group_by(state, race_ethn, age, var) %>% 
         summarise(q = mean(value)) %>% 
         ungroup() %>% 
         filter(q<=1),
       aes(x = age, y = q, color = race_ethn)) + 
  geom_line() + 
  labs(color = "", fill = "", lty = "") + 
  labs(x = "Age", y = "Probability")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2") + 
  facet_wrap(~state, ncol = 4) + 
  ggsave("./vis/slides_23.png", width = 8, height = 4)

non_icwa_cond_fc<-non_icwa_tables %>%
  rename(non_icwa = q) %>% 
  select(.imp, age, race_ethn, state, non_icwa) %>% 
  left_join(fc_tables%>% 
              rename(fc = q) %>% 
              select(.imp, age, race_ethn, state, fc)) %>% 
  mutate(non_icwa_fc = non_icwa / fc)

ggplot(non_icwa_cond_fc %>% 
         filter(race_ethn=="AIAN",
                state == "SD") %>% 
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
  ggsave("./vis/slides_24.png", width = 8, height = 4)


cumulative_slide<-fc_tables_c %>% 
  filter(.imp==1,
         state %in% c("SD", "ND", "MN", "NE")) %>% 
  mutate(outcome = "2. Foster care") %>% 
  bind_rows(investigation_tables_c %>% 
              filter(.imp==1,
                     state %in% c("SD", "ND", "MN", "NE")) %>% 
              mutate(outcome = "1. Investigation")) 
  
ggplot(cumulative_slide,
       aes(x = c * 100, y = state, fill = race_ethn)) + 
  geom_col(position = position_dodge()) + 
  facet_wrap(~outcome) +
  scale_fill_brewer(palette = "Dark2") + 
  labs(x = "Percent of children",
       y = "",
       fill = "") 
  ggsave("./vis/slides_25.png", width = 8, height = 4)
  
