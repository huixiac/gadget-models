library(tidyverse)
library(patchwork)
load('/net/hafkaldi/export/u2/reikn/R/SurveyWork/SMB/Allaggrsmbindex.rdata')
load('/net/hafkaldi/export/u2/reikn/R/SurveyWork/SMH/Allaggrsmhindex.rdata')

cutoffs_list<-NULL
cutoffs_list[[6]]<-c(10,40,40,80)
cutoffs_list[[8]]<-c(5,30,40,60)

cutoffs<-cutoffs_list[[marteg]]

smb.index <- 
  Allaggrsmbindex %>% 
  filter(svaedi == 'Heild', 
         species == marteg,
         fixed == 1) %>% 
  left_join(Allaggrsmbindex %>% 
              filter(svaedi %in% c('HeildAnFaereyjahryggs'), 
                     species == marteg, 
                     fixed == 1) %>% 
              select(ar,lengd,
                     bio.anF = bio.staerri,
                     fj.anF = fj)) %>% 
  left_join(Allaggrsmbindex %>% 
              filter(svaedi %in% c('HeildAnFaereyjahryggs','Heild'), 
                     species == marteg, 
                     fixed == 1,
                     ar %in% c(1994,1995,2004,2005)) %>% 
              group_by(svaedi,lengd) %>% 
              summarise(sc.bio = mean(bio.staerri),
                        sc.fj = mean(fj)) %>% 
              group_by(lengd) %>% 
              summarise(sc.bio = prod(ifelse(svaedi=='HeildAnFaereyjahryggs',1/sc.bio,sc.bio)),
                        sc.fj = prod(ifelse(svaedi=='HeildAnFaereyjahryggs',1/sc.fj,sc.fj)))) %>% 
  mutate(scaling = ifelse(ar %in% c(1996:2003),'scaled','unscaled'),
         fj.c = ifelse(ar %in% c(1996:2003),fj*sc.fj,fj),
         fj.u = fj.c*(1+cv.fj),
         fj.l = fj.c*(1-cv.fj),
         bio.c = ifelse(ar %in% c(1996:2003),bio.staerri*sc.bio,bio.staerri),
         bio.u = bio.c*(1+cv.bio.staerri),
         bio.l = bio.c*(1-cv.bio.staerri)) %>% 
  as_data_frame()

smh.index <- 
  Allaggrsmhindex %>% 
  filter(svaedi == 'Heild', 
         species == marteg,
         fixed == 0) %>% 
  mutate(fj.u = fj*(1+cv.fj),
         fj.l = fj*(1-cv.fj),
         bio.u = bio.staerri*(1+cv.bio.staerri),
         bio.l = bio.staerri*(1-cv.bio.staerri)) %>% 
  as_data_frame()


## total biomass plot

total_bio_plot <- 
  smb.index %>% 
  filter(lengd == cutoffs[1]) %>% 
  ggplot(aes(ar,bio.c)) + 
  geom_ribbon(aes(ymin=bio.l,ymax=bio.u),fill='grey') + 
  geom_ribbon(aes(ymin=bio.l,ymax=bio.u),fill='lightgreen',
              data = smb.index %>% 
                filter(lengd == cutoffs[[1]],scaling=='scaled')) + 
  geom_line() + 
  geom_line(aes(y=bio.anF),col='darkgreen') +
  theme_light() +
  labs(x='Year',y='Total biomass') +
  expand_limits(y=0) +
  #  geom_point(data=smh.index %>% 
  #               filter(lengd == cutoffs[1]),
  #             aes(ar,bio.staerri)) +
  geom_pointrange(data=smh.index %>% 
                    filter(lengd == cutoffs[1]),
                  aes(ar,bio.staerri,ymax=bio.u,ymin=bio.l)) +
  geom_label(data = smb.index %>% 
               filter(lengd == cutoffs[1]) %>% 
               select(sc.bio) %>% 
               distinct() %>% 
               mutate(x=1990,y=0,label=paste('Scaling factor =',round(sc.bio,4))),
             aes(x,y,label=label))

## biomass > 40 cm
b40_plot <- 
  smb.index %>% 
  filter(lengd == cutoffs[3]) %>% 
  ggplot(aes(ar,bio.c)) + 
  geom_ribbon(aes(ymin=bio.l,ymax=bio.u),fill='grey') + 
  geom_ribbon(aes(ymin=bio.l,ymax=bio.u),fill='lightgreen',
              data = smb.index %>% 
                filter(lengd == cutoffs[3],scaling=='scaled')) + 
  geom_line() + 
  geom_line(aes(y=bio.anF),col='darkgreen') +
  theme_light() +
  labs(x='Year',y=paste0('Biomass > ',cutoffs[3])) +
  expand_limits(y=0) +
  #  geom_point(data=smh.index %>% 
  #               filter(lengd == cutoffs[1]),
  #             aes(ar,bio.staerri)) +
  geom_pointrange(data=smh.index %>% 
                    filter(lengd == cutoffs[3]),
                  aes(ar,bio.staerri,ymax=bio.u,ymin=bio.l)) +
  geom_label(data = smb.index %>% 
               filter(lengd == cutoffs[3]) %>% 
               select(sc.bio) %>% 
               distinct() %>% 
               mutate(x=1990,y=0,label=paste('Scaling factor =',round(sc.bio,4))),
             aes(x,y,label=label))

## biomass > 60

b60_plot <- 
  smb.index %>% 
  filter(lengd == cutoffs[4]) %>% 
  ggplot(aes(ar,bio.c)) + 
  geom_ribbon(aes(ymin=bio.l,ymax=bio.u),fill='grey') + 
  geom_ribbon(aes(ymin=bio.l,ymax=bio.u),fill='lightgreen',
              data = smb.index %>% 
                filter(lengd == cutoffs[4],scaling=='scaled')) + 
  geom_line() + 
  geom_line(aes(y=bio.anF),col='darkgreen') +
  theme_light() +
  labs(x='Year',y=paste0('Biomass > ',cutoffs[4])) +
  expand_limits(y=0) +
  #  geom_point(data=smh.index %>% 
  #               filter(lengd == cutoffs[1]),
  #             aes(ar,bio.staerri)) +
  geom_pointrange(data=smh.index %>% 
                    filter(lengd == cutoffs[4]),
                  aes(ar,bio.staerri,ymax=bio.u,ymin=bio.l)) +
  geom_label(data = smb.index %>% 
               filter(lengd == cutoffs[4]) %>% 
               select(sc.bio) %>% 
               distinct() %>% 
               mutate(x=1990,y=0,label=paste('Scaling factor =',round(sc.bio,4))),
             aes(x,y,label=label))

## abundance < 30

a30_plot <- 
  smb.index %>% 
  filter(lengd == cutoffs[2]) %>% 
  ggplot(aes(ar,fj.c)) + 
  geom_ribbon(aes(ymin=fj.l,ymax=fj.u),fill='grey') + 
  geom_ribbon(aes(ymin=fj.l,ymax=fj.u),fill='lightgreen',
              data = smb.index %>% 
                filter(lengd == cutoffs[2],scaling=='scaled')) + 
  geom_line() + 
  geom_line(aes(y=fj.anF),col='darkgreen') +
  theme_light() +
  labs(x='Year',y=paste0('Abundance < ',cutoffs[2])) +
  expand_limits(y=0) +
  #  geom_point(data=smh.index %>% 
  #               filter(lengd == cutoffs[1]),
  #             aes(ar,bio.staerri)) +
  geom_pointrange(data=smh.index %>% 
                    filter(lengd == cutoffs[3]), #was 40
                  aes(ar,fj,ymax=fj.u,ymin=fj.l)) +
  geom_label(data = smb.index %>% 
               filter(lengd == cutoffs[3]) %>% #was40
               select(sc.fj) %>% 
               distinct() %>% 
               mutate(x=1990,y=0,label=paste('Scaling factor =',round(sc.fj,4))),
             aes(x,y,label=label))

## 4-plot

four_plot <- 
  total_bio_plot + 
  b40_plot + 
  b60_plot + 
  a30_plot + 
  plot_layout(ncol=2)
