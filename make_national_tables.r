###################################################
### age-specific National tables
###################################################

### make national tables for each outcome

## investigation

inv_nat_tab<-list()
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
                pop = sum(pop)) %>% 
      ungroup()
    inv_nat_tab[[index]]<-make_life_table(temp)
    index<-index+1
  }
}
inv_nat_tab<-bind_rows(inv_nat_tab)

## substantiation

sub_nat_tab<-list()
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
                pop = sum(pop)) %>% 
      ungroup()
    sub_nat_tab[[index]]<-make_life_table(temp)
    index<-index+1
  }
}
sub_nat_tab<-bind_rows(sub_nat_tab)

###### FOSTER CARE

fc_nat_tab<-list()
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
                pop = sum(pop)) %>% 
      ungroup()
    fc_nat_tab[[index]]<-make_life_table(temp)
    index<-index+1
  }
}
fc_nat_tab<-bind_rows(fc_nat_tab)

### TPR

tpr_nat_tab<-list()
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
                pop = sum(pop)) %>% 
      ungroup()
    tpr_nat_tab[[index]]<-make_life_table(temp)
    index<-index+1
  }
}
tpr_nat_tab<-bind_rows(tpr_nat_tab)


########################### JOINTS AND CONDITIONALS

#######################
## P(sub|inv)
########################

cond_sub_inv<-sub_nat_tab %>%
  rename(sub = q) %>% 
  select(.imp, age, race_ethn, sub) %>% 
  left_join(inv_nat_tab %>% 
              rename(inv = q) %>% 
              select(.imp, race_ethn, age, inv)) %>% 
  mutate(cond = sub / inv,
         var = "P(sub|inv)")



#######################
## P(FC,inv)
########################

fc_inv_nat_tab<-list()
race_id<-unique(afcars_fc_inv$race_ethn)
imps<-unique(afcars_fc_inv$.imp)
index<-1
for(i in 1:length(imps)){
  for(r in 1:length(race_id)){
    temp<-afcars_fc_inv
    temp<-temp %>% 
      filter(race_ethn == race_id[r])
    temp<-temp %>% 
      filter(.imp==i)
    temp<-temp %>% 
      group_by(.imp, age, race_ethn) %>% 
      summarise(var = sum(var), 
                pop = sum(pop)) %>% 
      ungroup()
      fc_inv_nat_tab[[index]]<-make_life_table(temp)
    index<-index+1
  }
}
fc_inv_nat_tab<-bind_rows(fc_inv_nat_tab)


###### P(FC|INV)

cond_fc_inv<-fc_inv_nat_tab %>% 
  select(.imp, age, race_ethn, q) %>% 
  rename(fc_inv = q) %>% 
  right_join(inv_nat_tab %>% 
               select(.imp, age, race_ethn, q) %>% 
               rename(inv = q)) %>% 
  mutate(cond = fc_inv / inv,
         var = "P(FC|inv)")

#######################
## P(FC,sub)
########################

fc_sub_nat_tab<-list()
race_id<-unique(afcars_fc_sub$race_ethn)
imps<-unique(afcars_fc_sub$.imp)
index<-1
for(i in 1:length(imps)){
  for(r in 1:length(race_id)){
    temp<-afcars_fc_sub
    temp<-temp %>% 
      filter(race_ethn == race_id[r])
    temp<-temp %>% 
      filter(.imp==i)
    temp<-temp %>% 
      group_by(.imp, age, race_ethn) %>% 
      summarise(var = sum(var), 
                pop = sum(pop)) %>% 
      ungroup()
    fc_sub_nat_tab[[index]]<-make_life_table(temp)
    index<-index+1
  }
}
fc_sub_nat_tab<-bind_rows(fc_sub_nat_tab)


###### P(FC|SUB)

cond_fc_sub<-fc_sub_nat_tab %>% 
  select(.imp, age, race_ethn, q) %>% 
  rename(fc_sub = q) %>% 
  right_join(sub_nat_tab %>% 
               select(.imp, age, race_ethn, q) %>% 
               rename(sub = q)) %>% 
  mutate(cond = fc_sub / sub,
         var = "P(FC|sub)")

###### P(TPR|FC)

cond_tpr_fc<-tpr_nat_tab %>% 
  select(.imp, age, race_ethn, q) %>% 
  rename(tpr = q) %>% 
  right_join(fc_nat_tab %>% 
               select(.imp, age, race_ethn, q) %>% 
               rename(fc = q)) %>% 
  mutate(cond = tpr / fc,
         var = "P(TPR|FC)")


