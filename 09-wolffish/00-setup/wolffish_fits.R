library(Rgadget)
library(gridExtra)

setwd('/home/pamela/Documents/Hafro/fishvice/gadget-models/09-wolffish/01-firsttry')
setwd("/net/hafkaldi.hafro.is/export/home/haf/pamela/gadget-models/09-wolffish/01-firsttry")
#setwd("/net/hafkaldi.hafro.is/export/home/haf/pamela/gadget-models/09-wolffish_first/01-firsttry")

Sys.setenv(GADGET_WORKING_DIR=getwd())
#fit2 <- gadget.fit(wgts=NULL, main.file = 'WGTS/main.final', params.file='WGTS/params.final')

figsavedir<-paste0('/home/pamela/Documents/Hafro/fishvice/gadget-models/09-wolffish/99-docs/fit_figs')

fit <- gadget.fit(wgts=NULL, main.file = 'WGTS/main.final', params.file='WGTS/params.final')

plot(fit)

plot(fit,data='res.by.year',type='total')
plot(fit,data='res.by.year',type='F')
plot(fit,data='res.by.year',type='rec')

plot(fit, data = "summary")

plot(fit,data='suitability')#should not work now - plot by hand

tmp <- plot(fit,data='catchdist.fleets')
tmp2 <- plot(fit2,data='catchdist.fleets')


plot(fit,data='stock.std')
matfig<-
  fit$stockdist %>% 
  filter(stock=='wolfmat',name == 'matp.igfs') %>% 
  ggplot(aes(length,obs.ratio, color = as.character(step))) + geom_point() + 
  geom_line(aes(y=pred.ratio))+
  facet_wrap(~year) + theme_minimal() + 
  labs(y='Prop. mature',x='Length')

growthfig<-
  fit$stock.std %>% 
  filter(age %in% c(2,3,4,5,6)) %>%
  ggplot(aes(year,mean.length, 
             color = as.character(age), 
             linetype = stock, 
             size = as.character(step))) + geom_line() 

for(x in c('Haust', 'Vetur', 'Trawl', 'Longline', 'Gillnet')){
pl<- 
  fit$catchdist.fleets %>%
  ungroup() %>% 
  mutate(name = ifelse(grepl(pattern = 'aut', name), 'Haust',
                       ifelse(grepl('igfs', name), 'Vor',
                              ifelse(grepl('bmt',name), 'Trawl',
                                     ifelse(grepl('lln', name), 'LongLine', 'Gillnet'))))) %>% 
  group_by(name, area) %>% 
  filter(name==x) %>% 
  ggplot(aes(avg.length, observed)) + 
  geom_col(color = 'lightgrey')+
  geom_line(aes(avg.length, predicted, color = area))+
  facet_wrap(~name + year, scales='free_y')
print(pl)
}

catch<-
  fit$fleet.info %>% 
  ungroup() %>% 
  mutate(fleet = ifelse(fleet=='aut', 'Haust',
                        ifelse(fleet=='igfs', 'Vor', 
                               ifelse(fleet=='gil', 'Gillnet',
                                      ifelse(fleet=='lln', 'Longline',
                                             ifelse(fleet=='bmt', 'Trawl', 'Foreign')))))) %>% 
  group_by(year, fleet) %>% 
  ggplot(aes(year, amount/1000, 
             fill = fleet)) + 
  geom_bar(stat='identity', position=position_dodge()) + 
  geom_bar(stat='identity') + 
  facet_wrap(~area, scales='free_y')
