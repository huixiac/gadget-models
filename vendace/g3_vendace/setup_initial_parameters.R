## weight length relationship (from Mikaela 20181206)
## lw.constants.ven <- data.frame(a=2e-06,
##                                b=3.2201)

lw.constants.ven <- 
  mfdb_dplyr_sample(mdb) %>% data.frame() %>% 
  filter(species == defaults.ven$species,
         data_source == 'individual_comm_vendace',
         !is.na(weight)) %>% 
  select(length,weight) %>%
  mutate(weight=weight/1000) %>% #g2kg
  collect(n=Inf) %>% 
  lm(log(weight)~log(length),.) %>% 
  broom::tidy() %>% 
  select(estimate)
## transport back to right dimension
lw.constants.ven$estimate[1] <- exp(lw.constants.ven$estimate[1])
lw.constants.ven <- data.frame(a=lw.constants.ven$estimate[1],
                               b=lw.constants.ven$estimate[2])

## tmp <- mfdb_dplyr_sample(mdb) %>% data.frame() %>%
##        filter(species == defaults.ven$species,
##               data_source == 'individual_comm_vendace',
##               !is.na(weight)) %>%
##        data.frame()
## ggplot(tmp,
##        aes(length,weight/1000)) +
##    geom_point() +
##    geom_line(data=data.frame(x=tmp$length,
##                              y=lw.constants.ven$a*tmp$length^lw.constants.ven$b),
##              aes(x,y), color=2, lwd=0.6) +
##    facet_wrap(~year)

## VB growth params
vb.constants.ven <- 
  mfdb_dplyr_sample(mdb) %>%  data.frame() %>%
  filter(species == defaults.ven$species,
         data_source == 'individual_comm_vendace') %>% 
  select(length,age,month) %>%
  mutate(age2 = as.numeric(age) + as.numeric(month/12-1/(12*2))) %>% 
  collect(n=Inf) %>%
  nls(length ~ Linf*(1-exp(-k*(age2-t0))),. , start=list(Linf=15,k=0.5,t0=0.1)) %>%
  broom::tidy() %>% 
  select(estimate)
vb.constants.ven <- data.frame(Linf=vb.constants.ven$estimate[1],
                               k=vb.constants.ven$estimate[2],
                               t0=vb.constants.ven$estimate[3])

## tmp <- 
##   mfdb_dplyr_sample(mdb) %>%  data.frame() %>%
##   filter(species == defaults.ven$species,
##          data_source == 'individual_comm_vendace') %>%
##   mutate(age2 = as.numeric(age) + as.numeric(month/12-1/(12*2))) %>%
##   data.frame()
## ggplot(tmp,
##        aes(age2,length)) +
##    geom_point() +
##     geom_line(data=data.frame(x=seq(min(tmp$age2,na.rm=T),max(tmp$age2,na.rm=T),length.out=100)) %>%
##                    mutate(y=vb.constants.ven$Linf*(1-exp(-vb.constants.ven$k*(x-vb.constants.ven$t0)))),
##               aes(x,y), color=2, lwd=0.6)

             
## initial conditions sigma
init.sigma.ven <- 
  mfdb_dplyr_sample(mdb) %>%  data.frame() %>%
  dplyr::filter(species == defaults.ven$species,
                data_source == 'individual_comm_vendace',
                ## areacell %in% defaults.spr$area[[1]],
                ## institute %in% 'SLU',
                ## age >0,
                !is.na(length),
                !is.na(age))  %>%
  dplyr::select(age,length) %>% 
  dplyr::collect(n=Inf) %>% 
  dplyr::group_by(age) %>% 
  dplyr::summarise(ml=mean(length,na.rm=TRUE),ms=sd(length,na.rm=TRUE))

# manually adjust sd age0 and age8+
## init.sigma.ven[init.sigma.ven$age == 0,"ms"] <- 0.9
init.sigma.ven[init.sigma.ven$age >= 8,"ms"] <- 2.00
# manually add age10
init.sigma.ven <- rbind(init.sigma.ven, c(10,18.5,2))


## initial guess for L50 of the maturity ogive:
## mat.constants.ven <- 
##   mfdb_dplyr_sample(mdb) %>%  data.frame() %>%
##   filter(species == defaults.ven$species,
##          data_source == 'individual_comm_vendace',
##          !is.na(maturity_stage)) %>% 
##   select(length,maturity_stage) %>% 
##   group_by(length,maturity_stage) %>% 
##   dplyr::summarise(n=n()) %>% 
##   group_by(length) %>% 
##   dplyr::mutate(p=n/sum(n, na.rm=T)) %>% 
##   ungroup() %>% 
##   filter(maturity_stage=='5',p>0.50) %>% 
##   dplyr::summarise(l50=min(length, na.rm=T)) %>% 
##   collect(n=Inf)
## mat.constants.ven <- data.frame(a=NA,
##                                 l50=mat.constants.ven[[1]])

## initial guess for the maturity ogive:
mat.constants.ven <- 
  mfdb_dplyr_sample(mdb) %>%  data.frame() %>%
  filter(species == defaults.ven$species,
         data_source == 'individual_comm_vendace',
         year %in% c(1998,2006:2008,2010:2018), # temporary until data get fixed***
         !is.na(maturity_stage)) %>% 
  select(length,maturity_stage) %>% 
  group_by(length,maturity_stage) %>% 
  dplyr::summarise(n=n()) %>% 
  data.frame() %>%
  complete(maturity_stage,length, fill=list(n=0)) %>%
  group_by(length) %>% 
  dplyr::mutate(p=n/sum(n, na.rm=T)) %>% 
  filter(maturity_stage=='5') %>%
  collect(n=Inf) %>% 
  nls(p~1/(1+exp(-a*(length-l50))),. , start=list(a=2,l50=11)) %>%
  broom::tidy() %>% 
  select(estimate)
mat.constants.ven <- data.frame(a=mat.constants.ven$estimate[1],
                                l50=mat.constants.ven$estimate[2])

## tmp <-   mfdb_dplyr_sample(mdb) %>%  data.frame() %>%
##   filter(species == defaults.ven$species,
##          data_source == 'individual_comm_vendace',
##          year %in% c(1998,2006:2008,2010:2018), # temporary until data get fixed***
##          !is.na(maturity_stage)) %>% 
##   select(length,maturity_stage) %>% 
##   group_by(length,maturity_stage) %>% 
##   dplyr::summarise(n=n()) %>% 
##   data.frame() %>%
##   complete(maturity_stage,length, fill=list(n=0)) %>%
##   group_by(length) %>% 
##   dplyr::mutate(p=n/sum(n, na.rm=T)) %>% 
##   filter(maturity_stage=='5') %>%
##   data.frame()
## ggplot(tmp,
##        aes(length,p)) +
##    geom_point() +
##    geom_line(data=data.frame(x=tmp$length,
##                              y=1/(1+exp(-mat.constants.ven$a*(tmp$length-mat.constants.ven$l50)))),
##              aes(x,y), color=2, lwd=0.6)
