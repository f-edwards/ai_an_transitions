###################################################
### age-specific state figure
###################################################

###### AGE IS WRONG FOR ALL EVENTS IN THAT FILE. RERUNNING, USE 15_19 IMPUTATIONS
### USING AGEATEND INSTEAD
#### INITIAL RUN USED AGE AT LAT REM, ONLY VALID AS AGE FOR FIRST REMOVALS

library(tidyverse)
library(geofacet)

afcars_st<-read_csv("~/Projects/ndacan_processing/data/afcars_all_events_state.csv")

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
             race==3 ~ "AI/AN",
             race==4 ~ "Asian/PI",
             hisp==1 ~ "Hispanic")) %>%
  mutate(age = as.integer(age))

pop<-pop %>% 
  group_by(year, state, st_fips, age, race_ethn) %>% 
  summarise(pop = sum(pop))

#### compute 5 year / 10 imputation mean contact count and SD of mean
age_spec<-afcars_st %>% 
  filter(year>=2015, race_ethn == "AI/AN" | race_ethn == "White", age<18) %>% 
  group_by(state, age, race_ethn) %>% 
  summarize(fc_contact_mean = mean(fc_total_contact)) 

### for MI SE, use rubin's rules
### within imputation variance
### within imputation variance:V_bar =  1/m \sum_1^m(var(x))

v_within<-afcars_st %>% 
  filter(year>=2015, race_ethn == "AI/AN" | race_ethn == "White", age<18) %>% 
  group_by(state, age, race_ethn, .imp) %>% 
  summarize(v = var(fc_total_contact)) %>% 
  summarize(v_bar = mean(v))

### between imputation variance is 1/(m-1) \sum_1^m (\hat{x} - \bar{x})^2
# \bar{x} = fc_contact_mean
# \hat{x} = 5-yr mean per imputation
m<-max(afcars_st$.imp)

v_between<-afcars_st %>% 
  filter(year>=2015, race_ethn == "AI/AN" | race_ethn == "White", age<18) %>% 
  group_by(state, age, race_ethn, .imp) %>% 
  summarize(xhat = mean(fc_total_contact)) %>% 
  left_join(age_spec) %>% 
  summarize(b = 1 / (m-1) * sum((xhat - fc_contact_mean)^2))

### total variance is v_bar + (1 + 1/m)b
v_total<-v_within %>% 
  left_join(v_between) %>% 
  mutate(v_total = v_bar + (1 + 1/m) * b,
         fc_contact_mean_se = sqrt(v_total)) %>% 
  select(state, age, race_ethn, fc_contact_mean_se)

### join MI standard errors to point estimates
age_spec<-age_spec %>% 
  left_join(v_total)

### join to pop
age_spec<-age_spec %>% 
  rename(st_fips = state) %>% 
  left_join(pop %>% 
              mutate(st_fips = as.numeric(st_fips)) %>% 
              filter(year>=2015, 
                     race_ethn == "AI/AN" | race_ethn == "White",
                     age<18) %>% 
              group_by(state, st_fips, age, race_ethn) %>% 
              summarize(pop_5yr = sum(pop)/5)) 

### compute event rates
age_spec<-age_spec %>% 
  mutate(fc_rate = fc_contact_mean / pop_5yr * 1e2,
         fc_rate_se = fc_contact_mean_se / pop_5yr * 1e2)

### subset to top 15 highest states by total contact rt
# highs<-totals_15_19 %>% 
#   arrange(desc(fc_contact_rt)) %>% 
#   select(state) %>% 
#   slice(1:15)
# 
### USE FACET_GEO

plot_dat<-age_spec 
plot_dat<-plot_dat %>% 
  mutate(race_ethn = ifelse(race_ethn=="AI/AN", "AIAN", race_ethn))
library(RColorBrewer)

ggplot(plot_dat,
       aes(x = age, y = fc_rate,
           color = race_ethn)) + 
  geom_line() + 
  geom_ribbon(aes(x = age, 
                  ymin = fc_rate - fc_rate_se, 
                  ymax = fc_rate + fc_rate_se, 
                  fill = race_ethn,
                  color = NULL),
              alpha = 0.5) + 
  facet_geo(~state) + 
  labs(x = "Child Age", y = "Percent of children in foster care", color = "", fill = "") + 
  scale_fill_brewer(palette = "Dark2") + 
  scale_color_brewer(palette = "Dark2") 

ggsave("./vis/fig2.png",
       height = 7, width = 11)

# for MT call out
ggplot(plot_dat %>% 
         filter(state == "MN"),
       aes(x = age, y = fc_rate,
           color = race_ethn)) + 
  geom_line() + 
  geom_ribbon(aes(x = age, 
                  ymin = fc_rate - fc_rate_se, 
                  ymax = fc_rate + fc_rate_se, 
                  fill = race_ethn,
                  color = NULL),
              alpha = 0.5) + 
  # facet_geo(~state) + 
  labs(x = "Child Age", y = "Percent of children in foster care", color = "", fill = "") + 
  scale_fill_brewer(palette = "Dark2") + 
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Montana 2015 - 2019 average levels")
ggsave("./vis/fig2_MT.png")



### ineq callout in text
t<-plot_dat %>% 
  ungroup() %>% 
  select(state, age, race_ethn, fc_rate) %>% 
  group_by(state, age) %>% 
  pivot_wider(names_from = race_ethn, values_from = fc_rate) %>% 
  mutate(disp = AIAN/White)
