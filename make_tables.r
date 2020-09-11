library(tidyverse)
source("lifetable.r")

make_table<-function(x){
  tables<-list()
  years<-unique(x$year)
  race_id<-unique(x$race_ethn)
  imps<-unique(x$.imp)
  index<-1
  for(i in 1:length(imps)){
    for(j in 1:length(years)){
      for(r in 1:length(race_id)){
        temp<-x %>% 
          filter(year==years[j])
        temp<-temp %>% 
          filter(race_ethn == race_id[r])
        temp<-temp %>% 
          filter(.imp==i)
        tables[[index]]<-make_life_table(temp)
        index<-index+1
      }
    }
  }
  out<-bind_rows(tables)
  
  out_c<-out %>% 
    group_by(year, race_ethn) %>% 
    summarise(c_mn = mean(c), cmax = quantile(c, 0.95), cmin = quantile(c, 0.05),
              q_mn = mean(q), qmin = quantile(q, 0.95), qmax = quantile(q, 0.05))
  
  return(out_c)
}

