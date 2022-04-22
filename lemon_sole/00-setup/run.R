# BEGINNING MODEL 45
setwd('~/')
#devtools::install_github('hafro/Rgadget', force = TRUE)
library(Rgadget)
library(ggmisc)
base_dir <- 'lemon gadget'
vers <- c('45-base')
gd <- gadget.variant.dir(sprintf(paste0("%s/",vers),base_dir))

#setwd('../../..')
old <- getwd()

setwd(paste0('gadget project/',gd))

Sys.setenv(GADGET_WORKING_DIR=normalizePath(getwd()))

tmp <- gadget.iterative(rew.sI=TRUE,
                        main='main',
                        grouping=list(sind1=c('si.10-20','si.20-25','si.25-30'),
                                      sind2=c('si.30-35','si.35-40','si.40-45',
                                              'si.45-60'),
                                      comm=c('ldist.dse','ldist.bmt','aldist.dse',
                                             'aldist.bmt')),
                        cv.floor = 0.05,
                        params.file = 'params.init',
                        wgts='WGTS')

setwd('~/')

# BEGINNING MODEL 46
library(Rgadget)
base_dir <- 'lemon gadget'
vers <- c('46-databefore2001')
gd <- gadget.variant.dir(sprintf(paste0("%s/",vers),base_dir))

#setwd('../../..')
old <- getwd()
setwd(paste0('gadget project/',gd))
Sys.setenv(GADGET_WORKING_DIR=normalizePath(getwd()))

tmp <- gadget.iterative(rew.sI=TRUE,
                        main='main',
                        grouping=list(sind1=c('si.10-20','si.20-25','si.25-30'),
                                      sind2=c('si.30-35','si.35-40','si.40-45',
                                              'si.45-60'),
                                      comm=c('ldist.dse','ldist.bmt','aldist.dse',
                                             'aldist.bmt')),
                        cv.floor = 0.05,
                        params.file = 'params.init',
                        wgts='WGTS')

setwd('~/')



# RUNNING FIT 45
library(Rgadget)
base_dir <- 'lemon gadget'
vers <- c('45
          -base')
gd <- gadget.variant.dir(sprintf(paste0("%s/",vers),base_dir))

#setwd('../../..')
old <- getwd()
setwd(paste0('gadget project/',gd))
fit_49 <- gadget.fit()


#print('Running analytical retro')
#try(gadget.retro(mainfile = 'WGTS/main.final',params.file = 'params.init'), silent = T)

callGadget(s = 1, i = 'params.init', p = 'params.test', log = 'init.log3', ignore.stderr = FALSE)


plot(fit_45)

plot(fit_45,data='summary')

plot(fit_45,data='summary',type = 'weighted')

plot(fit_45,data='summary',type='pie')

tmp <- plot(fit_45,data = 'catchdist.fleets')
names(tmp)

tmp$aldist.bmt
tmp$aldist.dse
tmp$aldist.igfs
tmp$ldist.bmt
tmp$ldist.dse
tmp$ldist.igfs

bubbles <- plot(fit_45,data = 'catchdist.fleets',type='bubble')

names(bubbles)

bubbles$aldist +  ggplot2::labs(y = "Age", x = "Year")

bubbles$ldist

grplot_45 <- plot(fit_45,data = 'catchdist.fleets',type='growth')
names(grplot_49)

grplot_45$aldist.bmt
grplot_45$aldist.dse
grplot_45$aldist.igfs

plot(fit_45,data='stockdist')

plot(fit_45,data="suitability")

plot(fit_45,data='stock.std') + ggmisc::scale_fill_crayola()

plot(fit_45,data='res.by.year',type='total') + theme(legend.position = 'none') +
  plot(fit_45,data='res.by.year',type='F') + theme(legend.position = 'none') +
  plot(fit_45,data = 'res.by.year',type='catch') + theme(legend.position = 'none') +
  plot(fit_45, data='res.by.year',type='rec')
tmp<- plot(fit_45,data = 'catchdist.fleets')



tmp$aldist.igfsfit$params###

tmp$aldist.igfsfit$params###

# retros
#gadget.retro()
# retros

gadget.retro.fit2 <- function (pre = "RETRO", ...) 
{
  tmp_func <- purrr::lift(bind.gadget.fit, .unnamed = TRUE)
  list.files(pre, pattern = "R[0-9]+") %>% purrr::set_names(., 
                                                            .) %>% purrr::map(function(x) {
                                                              print(x)
                                                              gadget.fit(main.file = sprintf("%s/%s/main", pre, x), 
                                                                         params.file = sprintf("%s/params.retro.%s", pre, 
                                                                                               gsub("R", "", x)), wgts = NULL, ...)
                                                            }) %>% tmp_func()
}



gadget.retro(main.file = 'WGTS/main.final',params.file = 'params.init', optinfofile = 'optinfofile')
retro.fit <- gadget.retro.fit()
#retro.fit <- gadget.retro.fit2(recruitment_step_age = tibble(stock = 'lemimm', age = 1, step = 1))  #age can change1-5
retro.igfs <- bind.gadget.fit(retro.fit,fit_45)
names(retro.igfs$res.by.year)[1] <- 'model1'
save(retro.fit, file = 'retroFit.Rdata')
rm(retro.fit)

#In the plots below you will need to adjust the scales (years included, xlim, ylim, /1000000).

retro_igfs_bio <-
  retro.igfs$res.by.year %>% 
  mutate(model = ifelse(is.na(model), 'original', model)) %>% 
  filter(stock=='lemmat') %>% 
  mutate(`Biomass 000s tonnes` = total.biomass/1000000, Year = year) %>% 
  ggplot()+
  geom_line(aes(x = Year, y = `Biomass 000s tonnes`, color = model)) + 
  ylim(0,15) +
  xlim(1990,2020)+
  theme_bw()

retro_igfs_F <-
  retro.igfs$res.by.year %>% 
  mutate(model = ifelse(is.na(model), 'original', model)) %>% 
  filter(stock=='lemmat') %>% 
  mutate( Year = year) %>% 
  ggplot()+
  geom_line(aes(x = Year, y = F, color = model)) + 
  ylim(0,1) +
  xlim(1990,2020)+
  theme_bw()


retro_igfs_rec <-
  retro.igfs$res.by.year %>% 
  mutate(model = ifelse(is.na(model), 'original', model)) %>% 
  filter(stock=='lemimm', year > 2000) %>% 
  mutate(`Recruitment 000000s` = recruitment/1000000, Year = year) %>% 
  ggplot()+
  geom_line(aes(x = Year, y = `Recruitment 000000s`, color = model)) + 
  #ylim(0,2.1) +
  xlim(2000,2020)+
  theme_bw()

gridExtra::grid.arrange(retro_igfs_bio, retro_igfs_F, retro_igfs_rec, nrow = 3)




#prediction

library(Rgadget)
library(tidyverse)
###old <- getwd()
###setwd(paste0('gadget project/',gd))
setwd('/home/unu/huixia/gadget project/lemon gadget/45-base')
fit <- gadget.fit()

Rgadget::gadget_project_time(variant_dir = 'PRE') %>%
  #Rgadget::gadget_project_time() %>%
  Rgadget::gadget_project_stocks(imm.file = 'lemimm',mat.file = 'lemmat') %>%
  Rgadget::gadget_project_fleets(pre_fleet = c('dse')) 

callGadget(s=1,main = 'PRE/main',i='WGTS/params.final',p='PRE/params.pre',log='tmp')



?gadget_project_time

hrs <- seq(0.02, 2, 0.02)  #0.2
trials_per_hr <- 100

fit <- gadget.fit() 
res <-  
  gadget_project_time() %>% 
  gadget_project_stocks(imm.file = 'lemimm',mat.file = 'lemmat') %>%  
  gadget_project_fleets(pre_fleet = 'dse') %>% 
  gadget_evaluate(params.out = paste(attr(.,'variant_dir'),'params.pre',sep='/'),   
                  params.in = 'WGTS/params.final') %>% 
  gadget_project_recruitment(stock = 'lemimm',   
                             recruitment = fit$stock.recruitment %>% 
                               filter(stock == 'lemimm', 
                                      year > 1990),
                             
                             params.file = paste(attr(.,'variant_dir'),'params.pre',sep='/'),
                             method = 'bootstrap') %>% 
  gadget_project_ref_point(ref_points = tibble(lemmat.blim = 3710000), 
                           params.file = paste(attr(.,'variant_dir'),'params.pre',sep='/')) %>%
  gadget_project_advice(pre_fleet = 'dse',
                        harvest_rate = hrs,   
                        advice_cv = 0.2,   
                        params.file =paste(attr(.,'variant_dir'),'params.pre',sep='/')) %>%  
  gadget_project_output(imm.file = 'lemimm',mat.file = 'lemmat', 
                        pre_fleet = 'dse') %>% 
  gadget_evaluate(params.in = paste(attr(.,'variant_dir'),'params.pre',sep='/'))  %>% 
  {read.printfiles(paste(attr(.,'variant_dir'),'out',sep='/'))} %>%
  map(mutate, trial=cut(1:length(year),c(0,which(diff(year)<0),1e9),labels = FALSE)) %>% 
  set_names(c("catch.F","catch.lw",'lemimm.rec','lemmat.ssb')) %>% 
  map(left_join,tibble(trial=1:(length(hrs)*trials_per_hr),harvest_rate = rep(hrs,trials_per_hr))) 

yield_curve <-   
  res$catch.lw %>% 
  filter(year>2050) %>% 
  group_by(trial,harvest_rate,year) %>%
  summarise(c=sum(biomass_consumed)/1e6) %>% 
  group_by(harvest_rate) %>%
  summarise(m=median(c),u=quantile(c,0.95),l=quantile(c,0.05))   


ssb_curve <-  
  res$lemmat.ssb %>% 
  filter(year>2050) %>%  
  group_by(trial,harvest_rate,year) %>% 
  summarise(c=sum(number*mean_weight)/1e6) %>%
  group_by(harvest_rate) %>%
  summarise(m=median(c),u=quantile(c,0.95),l=quantile(c,0.05))  

f.curve <- 
  res$catch.F %>%  
  filter(year>2050) %>%  
  group_by(trial,harvest_rate,year) %>%  
  summarise(c=median(mortality)) %>% 
  group_by(harvest_rate) %>% 
  summarise(m=median(c),u=quantile(c,0.95),l=quantile(c,0.05))   


blim <-   
  fit$res.by.year %>%   
  filter(grepl('mat',stock)) %>% 
  summarise(b=min(total.biomass)/1e6) %>%  
  .$b 

bpa <- 1.4*blim 

hr_msy <- 
  yield_curve %>%
  filter(m==max(m)) %>%
  .$harvest_rate 

hr_lim <- 
  ssb_curve %>%  
  filter(m>blim) %>%
  filter(harvest_rate == max(harvest_rate)) %>% 
  .$harvest_rate  

f.msy <- 
  f.curve %>%  
  filter(harvest_rate == hr_msy) %>% 
  .$m 


f.lim <-  
  f.curve %>% 
  filter(harvest_rate == hr_lim) %>% 
  .$m  

f.pa <- 
  f.lim/1.4


hr_pa <- 
  f.curve %>% 
  filter(m < f.pa) %>% 
  summarise(hr = max(harvest_rate)) %>% 
  .$hr  


library(patchwork) 

yield_curve %>% 
  left_join(f.curve %>%  
              select(harvest_rate,F=m)) %>% 
  ggplot(aes(F,m)) +  
  geom_ribbon(aes(ymin=l,ymax=u),fill = 'gold') + 
  geom_line() + 
  geom_vline(xintercept = min(c(f.msy,f.pa))) + 
  geom_vline(xintercept = f.pa,lty=2,col='red') +  
  geom_vline(xintercept = f.lim,lwd=1.1,col='red') +
  ylabel("") + 
  ssb_curve %>%  
  left_join(f.curve %>%  
              select(harvest_rate,F=m)) %>%  
  ggplot(aes(F,m)) + 
  geom_ribbon(aes(ymin=l,ymax=u),fill = 'gold') + 
  geom_line() +  
  geom_vline(xintercept = min(c(f.msy,f.pa))) +
  geom_vline(xintercept = f.pa,lty=2,col='red') + 
  geom_vline(xintercept = f.lim,lwd=1.1,col='red') + 
  geom_hline(yintercept = blim, col = 'red', lwd = 1.1) +
  geom_hline(yintercept = bpa,col = 'red',lty = 2)  +
  ylabel("") +
  xlabel("")




#bootstraps
setwd('~/')
library(Rgadget)
base_dir <- 'lemon gadget'

setwd('~/')
vers <- c('45-base')
gd <- gadget.variant.dir(sprintf(paste0("%s/",vers),base_dir))

#setwd('../../..')
old <- getwd()
setwd(paste0('gadget project/',gd))

#range <- 1:50 #hafdruna
#range <- 51:100 #hafbjarmi

range <- 1

source('bootrun.R')

setwd(old)







setwd('~/')
# RUNNING FIT 46
library(Rgadget)
base_dir <- 'lemon gadget'

setwd('~/')
vers <- c('46-databefore2001')
gd <- gadget.variant.dir(sprintf(paste0("%s/",vers),base_dir))

#setwd('../../..')
old <- getwd()
setwd(paste0('gadget project/',gd))
fit_46 <- gadget.fit()
#print('Running analytical retro')
#try(gadget.retro(mainfile = 'WGTS/main.final',params.file = 'params.init'), silent = T)

callGadget(s = 1, i = 'params.init', p = 'params.test', log = 'init.log3', ignore.stderr = FALSE)


plot(fit_46)

plot(fit_46,data='summary')

plot(fit_46,data='summary',type = 'weighted')

plot(fit_46,data='summary',type='pie')

tmp <- plot(fit_46,data = 'catchdist.fleets')
names(tmp)

tmp$aldist.bmt
tmp$aldist.dse
tmp$aldist.igfs
tmp$ldist.bmt
tmp$ldist.dse
tmp$ldist.igfs

bubbles <- plot(fit_46,data = 'catchdist.fleets',type='bubble')

names(bubbles)

bubbles$aldist

bubbles$ldist


grplot_46 <- plot(fit_46,data = 'catchdist.fleets',type='growth')
names(grplot_46)

grplot_46$aldist.bmt
grplot_46$aldist.dse
grplot_46$aldist.igfs

plot(fit_46,data='stockdist')

plot(fit_46,data="suitability")

plot(fit_46,data='stock.std') + ggmisc::scale_fill_crayola()

plot(fit_46,data='res.by.year',type='total') + theme(legend.position = 'none') +
  plot(fit_46,data='res.by.year',type='F') + theme(legend.position = 'none') +
  plot(fit_46,data = 'res.by.year',type='catch') + theme(legend.position = 'none') +
  plot(fit_46, data='res.by.year',type='rec')
tmp<- plot(fit_46,data = 'catchdist.fleets')



tmp$aldist.igfsfit$params###

tmp$aldist.igfsfit$params###