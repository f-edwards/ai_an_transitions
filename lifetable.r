
make_life_table<-function(dat){
  ### create proportions of pop with each outcome by age/year
  dat<-dat%>%
    mutate(m = var / pop)
  
  ### convert to probability 
  ### age_period (n) = 1 for all cases
  ### a = 0.5 (avg age of TPR for ppl in year, within-period survival)
  dat<-dat%>%
    mutate(q = 1 * m / (1 + (1 - 0.5) * m),
           p = 1 - q)
  ### make cumulative survival
  dat<-dat%>%
    mutate(lx = 1e5 * cumprod(c(1, p))[-nrow(dat)])
  ### deaths
  dat<-dat%>%
    mutate(d = -c(diff(lx),0))
  ## person-years in each group
  dat<-dat%>%
    mutate(L = (lx - d) * 1 + d * 0.5,
           t = sum(L)- cumsum(L) + L)
  ## life expectancy (time to TPR)
  dat<-dat%>%
    mutate(e = t/lx)
  ### cum prevalence
  dat<-dat%>%
    mutate(c = 1-lx/1e5)
  
  dat<-dat %>% 
    mutate(s = 1 - c) %>% 
    mutate(var = as.numeric(var),
           pop = as.numeric(pop)) %>% 
    mutate(se = s * 
             sqrt(
               sum(var / 
                     (pop * (pop - var)))
             ))
  
  dat<-dat %>% 
    mutate(se = ifelse(age==0, 0, se)) # set se to 0 when age == 0 for variance on s, c=1-s
  
  return(dat)
}
