library(tidyverse)
library(RColorBrewer)

### read data
ncands<-read_csv("./data/ncands_subset_imputed.csv")

pop<-read_fwf("~/Projects/data/us.1990_2019.singleages.adjusted.txt",
              fwf_widths(c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8),
                         c("year", "state", "st_fips",
                           "cnty_fips", "reg", "race",
                           "hisp", "sex", "age", "pop")))
pop<-pop%>%
  filter(year>=2015) %>% 
  mutate(pop = as.integer(pop))%>%
  mutate(race_ethn =
           case_when(
             race==1 & hisp ==0 ~ "White",
             race==2 ~ "Black",
             race==3 ~ "AIAN",
             race==4 ~ "API",
             hisp==1 ~ "Latinx")) %>%
  mutate(age = as.integer(age)) %>% 
  group_by(year, state, st_fips, age, race_ethn) %>% 
  summarise(pop = sum(pop))

### make 5-year avg pop
pop_5yr<-pop %>% 
  group_by(state, st_fips, age, race_ethn) %>% 
  summarize(pop = mean(pop)) %>% 
  rename(staterr = state,
         chage = age)
#### ncands age spec reporting (unconditional)
### with rubin SE
m<-max(ncands$.imp)
  
ncands_age<-ncands %>% 
  filter(race_ethn=="AIAN" | race_ethn == "White") %>% 
  group_by(.imp, staterr, subyr, race_ethn, chage) %>% 
  summarize(investigations = n())

ncands_age_hat<-ncands_age %>% 
  group_by(.imp, staterr, race_ethn, chage) %>% 
  summarize(i_hat = mean(investigations))

ncands_age_bar<-ncands_age %>% 
  group_by(staterr, race_ethn, chage) %>% 
  summarize(i_bar = mean(investigations))

ncands_age_v_bar<-ncands_age %>% 
  group_by(staterr, race_ethn, chage, .imp) %>% 
  summarize(v = var(investigations)) %>% 
  summarize(v_bar = mean(v))

ncands_age_b<-ncands_age_hat %>% 
  left_join(ncands_age_bar) %>% 
  group_by(staterr, race_ethn, chage) %>% 
  summarize(b = 1/(m-1) * sum((i_hat - i_bar)^2))

ncands_age_se<-ncands_age_v_bar %>% 
  left_join(ncands_age_b) %>% 
  mutate(v_total = v_bar + (1 + 1/m) * b) %>% 
  mutate(se = sqrt(v_total)) %>% 
  select(staterr, race_ethn, chage, se)

ncands_age_out<-ncands_age_bar %>% 
  left_join(ncands_age_se) %>% 
  left_join(pop_5yr) %>% 
  mutate(i_rate = i_bar / pop * 1e2,
         se_rate = se / pop * 1e2)

#### plot



ggplot(ncands_age_out %>% 
         filter(chage<=17),
       aes(x = chage, 
           y = i_rate, 
           ymin = i_rate - se_rate,
           ymax = i_rate + se_rate,
           color = race_ethn,
           fill = race_ethn)) + 
  geom_line() + 
  geom_ribbon(aes(color = NULL), alpha = 0.5) + 
  facet_wrap(~staterr) + 
  scale_fill_brewer(palette = "Dark2") + 
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Age", y = "Percent of children", 
       color = "", fill = "")

ggsave("./vis/revised_fig3_wide.png", width = 11, height = 5)


#### rptsrc x age

ncands_rptsrc<-ncands %>% 
  mutate(age_under_5 = chage<=5) %>% 
  mutate(rptsrc = case_when(
    rptsrc == 1 ~ "4. Social services",
    rptsrc == 2 | rptsrc == 3 ~ "1. Medical",
    rptsrc == 4 ~ "2. Law enforcement",
    rptsrc == 5 | rptsrc == 6 ~ "3. Education or daycare",
    rptsrc > 6 ~ "5. Non-professional"
  )) %>% 
  filter(race_ethn=="AIAN" | race_ethn == "White") %>% 
  group_by(.imp, staterr, subyr, race_ethn, rptsrc, age_under_5) %>% 
  summarize(investigations = n())

ncands_rptsrc_hat<-ncands_rptsrc %>% 
  group_by(.imp, staterr, race_ethn, rptsrc, age_under_5) %>% 
  summarize(i_hat = mean(investigations))

ncands_rptsrc_bar<-ncands_rptsrc %>% 
  group_by(staterr, race_ethn, rptsrc, age_under_5) %>% 
  summarize(i_bar = mean(investigations))

ncands_rptsrc_v_bar<-ncands_rptsrc %>% 
  group_by(staterr, race_ethn, age_under_5, rptsrc, .imp) %>% 
  summarize(v = var(investigations)) %>% 
  summarize(v_bar = mean(v))

ncands_rptsrc_b<-ncands_rptsrc_hat %>% 
  left_join(ncands_rptsrc_bar) %>% 
  group_by(staterr, race_ethn, age_under_5, rptsrc) %>% 
  summarize(b = 1/(m-1) * sum((i_hat - i_bar)^2))

ncands_rptsrc_se<-ncands_rptsrc_v_bar %>% 
  left_join(ncands_rptsrc_b) %>% 
  mutate(v_total = v_bar + (1 + 1/m) * b) %>% 
  mutate(se = sqrt(v_total)) %>% 
  select(staterr, race_ethn, age_under_5, rptsrc, se)

ncands_rptsrc_out<-ncands_rptsrc_bar %>% 
  left_join(ncands_rptsrc_se) %>% 
  left_join(pop_5yr %>% 
              filter(chage<18) %>% 
              mutate(age_under_5 = chage<=5) %>% 
              group_by(staterr, st_fips, age_under_5, race_ethn) %>% 
              summarize(pop = sum(pop))) %>% 
  mutate(i_rate = i_bar / pop * 1e2,
         se_rate = se / pop * 1e2) 

### plot
ncands_rptsrc_out<-ncands_rptsrc_out %>% 
  mutate(age_under_5 = 
           case_when(
             age_under_5 == T ~ "0-4",
             age_under_5 == F ~ "5-17"
           ))
         


ggplot(ncands_rptsrc_out,
       aes(y = age_under_5,
           x = i_rate,
           xmin = i_rate - se_rate,
           xmax = i_rate + se_rate,
           fill = race_ethn,
           color = race_ethn)) + 
  geom_col(position="dodge") + 
  geom_errorbar(position=position_dodge(.9),
                color = "black", 
                width = 0.01) +
  facet_wrap(staterr~rptsrc, ncol = 5) + 
  scale_fill_brewer(palette = "Dark2") + 
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Percent of children",
       y = "Age",
       fill = "",
       color = "") 

ggsave("./vis/revised_fig4_wide.png", width = 11, height = 5)

### slide version
ggplot(ncands_rptsrc_out %>% 
         filter(staterr == "AK"),
       aes(x = age_under_5,
           y = i_rate,
           ymin = i_rate - se_rate,
           ymax = i_rate + se_rate,
           fill = rptsrc,
           color = rptsrc)) + 
  geom_col(position="dodge") + 
  geom_errorbar(position=position_dodge(.9),
                color = "black", 
                width = 0.01) +
  facet_wrap(~race_ethn, ncol = 5) + 
  scale_fill_brewer(palette = "Dark2") + 
  scale_color_brewer(palette = "Dark2") +
  labs(y = "Percent of children",
       x = "Age",
       fill = "",
       color = "") 

ggsave("vis/fig4_1.png", width = 11, height = 6)

ggplot(ncands_rptsrc_out %>% 
         filter(staterr == "MN"),
       aes(x = age_under_5,
           y = i_rate,
           ymin = i_rate - se_rate,
           ymax = i_rate + se_rate,
           fill = rptsrc,
           color = rptsrc)) + 
  geom_col(position="dodge") + 
  geom_errorbar(position=position_dodge(.9),
                color = "black", 
                width = 0.01) +
  facet_wrap(~race_ethn, ncol = 5) + 
  scale_fill_brewer(palette = "Dark2") + 
  scale_color_brewer(palette = "Dark2") +
  labs(y = "Percent of children",
       x = "Age",
       fill = "",
       color = "") 
ggsave("vis/fig4_2.png", width = 11, height = 6)

ggplot(ncands_rptsrc_out %>% 
         filter(staterr == "MT"),
       aes(x = age_under_5,
           y = i_rate,
           ymin = i_rate - se_rate,
           ymax = i_rate + se_rate,
           fill = rptsrc,
           color = rptsrc)) + 
  geom_col(position="dodge") + 
  geom_errorbar(position=position_dodge(.9),
                color = "black", 
                width = 0.01) +
  facet_wrap(~race_ethn, ncol = 5) + 
  scale_fill_brewer(palette = "Dark2") + 
  scale_color_brewer(palette = "Dark2") +
  labs(y = "Percent of children",
       x = "Age",
       fill = "",
       color = "") 
ggsave("vis/fig4_3.png", width = 11, height = 6)


#### for total prof vs non-prof
ncands_rptsrc_out %>% 
  filter(staterr=="MT") %>% 
  group_by(rptsrc=="5. Non-professional", race_ethn) %>% 
  summarize(i_rate = sum(i_rate))

### maltype x rptsrc
#### rptsrc x age

ncands_maltype<-ncands %>% 
  mutate(rptsrc = case_when(
    rptsrc == 1 ~ "4. Social services",
    rptsrc == 2 | rptsrc == 3 ~ "1. Medical",
    rptsrc == 4 ~ "2. Law enforcement",
    rptsrc == 5 | rptsrc == 6 ~ "3. Education or daycare",
    rptsrc > 6 ~ "5. Non-professional"
  )) %>% 
  filter(race_ethn=="AIAN" | race_ethn == "White") %>% 
  group_by(.imp, staterr, subyr, race_ethn, rptsrc) %>% 
  summarize(across(neglect:other, sum)) %>% 
  ungroup() %>% 
  pivot_longer(neglect:other, 
               values_to = "investigations",
               names_to = "maltype")

ncands_maltype_hat<-ncands_maltype %>% 
  group_by(.imp, staterr, race_ethn, rptsrc, maltype) %>% 
  summarize(i_hat = mean(investigations))

ncands_maltype_bar<-ncands_maltype %>% 
  group_by(staterr, race_ethn, rptsrc, maltype) %>% 
  summarize(i_bar = mean(investigations))

ncands_maltype_v_bar<-ncands_maltype %>% 
  group_by(staterr, race_ethn, maltype, rptsrc, .imp) %>% 
  summarize(v = var(investigations)) %>% 
  summarize(v_bar = mean(v))

ncands_maltype_b<-ncands_maltype_hat %>% 
  left_join(ncands_maltype_bar) %>% 
  group_by(staterr, race_ethn, maltype, rptsrc) %>% 
  summarize(b = 1/(m-1) * sum((i_hat - i_bar)^2))

ncands_maltype_se<-ncands_maltype_v_bar %>% 
  left_join(ncands_maltype_b) %>% 
  mutate(v_total = v_bar + (1 + 1/m) * b) %>% 
  mutate(se = sqrt(v_total)) %>% 
  select(staterr, race_ethn, maltype, rptsrc, se)

ncands_maltype_out<-ncands_maltype_bar %>% 
  left_join(ncands_maltype_se) %>% 
  left_join(pop_5yr %>% 
              filter(chage<18) %>% 
              group_by(staterr, st_fips, race_ethn) %>% 
              summarize(pop = sum(pop))) %>% 
  mutate(i_rate = i_bar / pop * 1e2,
         se_rate = se / pop * 1e2) 

#### plot fig 5
ncands_maltype_out<-ncands_maltype_out %>% 
  mutate(maltype = case_when(
    maltype == "neglect" ~ "Neglect",
    maltype == "phy_abuse" ~ "Physical abuse",
    maltype == "sex_abuse" ~ "Sexual abuse",
    maltype == "other" ~ "Other"
  )) %>% 
  mutate(maltype = factor(maltype,
                          levels = c("Neglect",
                                     "Physical abuse",
                                     "Sexual abuse",
                                     "Other")))



ggplot(ncands_maltype_out %>% 
         filter(staterr=="AK"),
       aes(x = race_ethn,
           y = i_rate,
           ymin = i_rate - se_rate,
           ymax = i_rate + se_rate,
           fill = rptsrc,
           color = rptsrc)) + 
  geom_col(position = "dodge") + 
  facet_wrap(staterr~rptsrc, ncol = 5) +  geom_col(position="dodge") + 
  geom_errorbar(position=position_dodge(.9),
                color = "black", 
                width = 0.01) +
  facet_wrap(~maltype, ncol = 5) + 
  scale_fill_brewer(palette = "Dark2") + 
  scale_color_brewer(palette = "Dark2") +
  labs(y = "Percent of children",
       x = "",
       fill = "",
       color = "") 

ggsave("./vis/revised_fig5_wide_1.png", width = 11, height = 5)

ggplot(ncands_maltype_out %>% 
         filter(staterr=="MN"),
       aes(x = race_ethn,
           y = i_rate,
           ymin = i_rate - se_rate,
           ymax = i_rate + se_rate,
           fill = rptsrc,
           color = rptsrc)) + 
  geom_col(position = "dodge") + 
  facet_wrap(staterr~rptsrc, ncol = 5) +  geom_col(position="dodge") + 
  geom_errorbar(position=position_dodge(.9),
                color = "black", 
                width = 0.01) +
  facet_wrap(~maltype, ncol = 5) + 
  scale_fill_brewer(palette = "Dark2") + 
  scale_color_brewer(palette = "Dark2") +
  labs(y = "Percent of children",
       x = "",
       fill = "",
       color = "") 

ggsave("./vis/revised_fig5_wide_2.png", width = 11, height = 5)

ggplot(ncands_maltype_out %>% 
         filter(staterr=="MT"),
       aes(x = race_ethn,
           y = i_rate,
           ymin = i_rate - se_rate,
           ymax = i_rate + se_rate,
           fill = rptsrc,
           color = rptsrc)) + 
  geom_col(position = "dodge") + 
  facet_wrap(staterr~rptsrc, ncol = 5) +  geom_col(position="dodge") + 
  geom_errorbar(position=position_dodge(.9),
                color = "black", 
                width = 0.01) +
  facet_wrap(~maltype, ncol = 5) + 
  scale_fill_brewer(palette = "Dark2") + 
  scale_color_brewer(palette = "Dark2") +
  labs(y = "Percent of children",
       x = "",
       fill = "",
       color = "") 

ggsave("./vis/revised_fig5_wide_3.png", width = 11, height = 5)



### FIG 1

ggplot(pre_icwa %>% 
         filter(var == "FC"),
       aes(x = rate, y = state,
           color = var)) + 
  geom_point() + 
  labs(x = "Percent of children",
       y = "")+
  scale_color_brewer(palette = "Dark2") + 
  labs(color = "") + 
  coord_cartesian(xlim = c(0, 20)) + 
  theme(legend.position = "bottom")

ggsave("./vis/REVISION_icwa.png", 
       width = 11,
       height = 5)

### plot FC plus adoption
ggplot(pre_icwa,
       aes(x = rate, y = state,
           color = var)) + 
  geom_point() + 
  labs(x = "Percent of children",
       y = "") +
  scale_color_brewer(palette = "Dark2") + 
  labs(color = "") + 
  coord_cartesian(xlim = c(0, 20))+ 
  theme(legend.position = "bottom")

ggsave("./vis/REVISION_icwa2.png", 
       width = 11,
       height = 5)

fig1_dat<-bind_rows(rates, counts) %>% 
  mutate(state = factor(state, 
                        levels = st_levels$state))

#### big fig with contemp and historic

# plot_dat<-plot_dat %>% 
#   left_join(pre_icwa %>% 
#               rename(pre_icwa_var = var))

pre_icwa<-pre_icwa %>% 
  rename("Pre-ICWA" = var)


ggplot(plot_dat %>% 
         filter(state!="HI",
                varname == "1. Children in foster care (percent of population)"),
       aes(x = var, y = state)) + 
  geom_point() + 
  geom_linerange(aes(xmin = var - var_sd,
                     xmax = var + var_sd)) + 
  scale_fill_brewer(palette = "Dark2") + 
  scale_color_brewer(palette = "Dark2") +
  labs(y = "", x = "Percent of AIAN children in foster care") + 
  coord_cartesian(xlim=c(0,20))

ggsave("./vis/revised_fig1_1.png",
       height = 6, width = 11)

ggplot(plot_dat %>% 
         filter(state!="HI",
                varname == "1. Children in foster care (percent of population)"),
       aes(x = var, y = state)) + 
  geom_point() + 
  geom_linerange(aes(xmin = var - var_sd,
                     xmax = var + var_sd)) + 
  geom_point(data = pre_icwa,
             aes(y = state, x = rate, color = `Pre-ICWA`),
             alpha = 0.7)+
  scale_fill_brewer(palette = "Dark2") + 
  scale_color_brewer(palette = "Dark2") +
  labs(y = "", x = "Percent of AIAN children in foster care") + 
  coord_cartesian(xlim=c(0,20)) + 
  theme(legend.position = "bottom")

ggsave("./vis/revised_fig1_2.png",
       height = 6, width = 11)


ggplot(plot_dat %>% 
         filter(state!="HI",
                varname == "2. Children in foster care (count)"),
       aes(x = var, y = state)) + 
  geom_point() + 
  geom_linerange(aes(xmin = var - var_sd,
                     xmax = var + var_sd)) + 
  scale_fill_brewer(palette = "Dark2") + 
  scale_color_brewer(palette = "Dark2") +
  labs(y = "", x = "AIAN children in foster care")

ggsave("./vis/revised_fig1_3.png",
       height = 6, width = 11)

ggplot(plot_dat %>% 
         filter(state!="HI",
                varname == "2. Children in foster care (count)"),
       aes(x = var, y = state)) + 
  geom_point() + 
  geom_linerange(aes(xmin = var - var_sd,
                     xmax = var + var_sd)) + 
  geom_point(data = pre_icwa,
             aes(y = state, x = n, color = `Pre-ICWA`),
             alpha = 0.7)+
  scale_fill_brewer(palette = "Dark2") + 
  scale_color_brewer(palette = "Dark2") +
  labs(y = "", x = "AIAN children in foster care") + 
  theme(legend.position = "bottom")

ggsave("./vis/revised_fig1_4.png",
       height = 6, width = 11)

