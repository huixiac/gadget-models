library(tidyverse)
library(patchwork)
load('/net/hafkaldi/export/u2/reikn/R/SurveyWork/SMB/Allaggrsmbindex.rdata')
load('/net/hafkaldi/export/u2/reikn/R/SurveyWork/SMH/Allaggrsmhindex.rdata')


smb.index <- 
  Allaggrsmbindex %>% 
  filter(svaedi == 'Heild', 
         species == 8,
         fixed == 1) %>% 
  left_join(Allaggrsmbindex %>% 
              filter(svaedi %in% c('HeildAnFaereyjahryggs'), 
                     species == 8, 
                     fixed == 1) %>% 
              select(ar,lengd,
                     bio.anF = bio.staerri,
                     fj.minni.anF = fj.minni)) %>% 
  left_join(Allaggrsmbindex %>% 
              filter(svaedi %in% c('HeildAnFaereyjahryggs','Heild'), 
                     species == 8, 
                     fixed == 1,
                     ar %in% c(1994,1995,2004,2005)) %>% 
              group_by(svaedi,lengd) %>% 
              summarise(sc.bio = mean(bio.staerri),
                        sc.fj.minni = mean(fj.minni)) %>% 
              group_by(lengd) %>% 
              summarise(sc.bio = prod(ifelse(svaedi=='HeildAnFaereyjahryggs',1/sc.bio,sc.bio)),
                        sc.fj.minni = prod(ifelse(svaedi=='HeildAnFaereyjahryggs',1/sc.fj.minni,sc.fj.minni)))) %>% 
  mutate(scaling = ifelse(ar %in% c(1996:2003),'scaled','unscaled'),
         fj.minni.c = ifelse(ar %in% c(1996:2003),fj.minni*sc.fj.minni,fj.minni),
         fj.minni.u = fj.minni.c*(1+cv.fj.minni),
         fj.minni.l = fj.minni.c*(1-cv.fj.minni),
         bio.c = ifelse(ar %in% c(1996:2003),bio.staerri*sc.bio,bio.staerri),
         bio.u = bio.c*(1+cv.bio.staerri),
         bio.l = bio.c*(1-cv.bio.staerri)) %>% 
  as_data_frame()

smh.index <- 
  Allaggrsmhindex %>% 
  filter(svaedi == 'Heild', 
         species == 8,
         fixed == 0) %>% 
  mutate(fj.minni.u = fj.minni*(1+cv.fj.minni),
         fj.minni.l = fj.minni*(1-cv.fj.minni),
         bio.u = bio.staerri*(1+cv.bio.staerri),
         bio.l = bio.staerri*(1-cv.bio.staerri)) %>% 
  as_data_frame()


## total biomass plot

total_bio_plot <- 
  smb.index %>% 
  filter(lengd == 5) %>% 
  ggplot(aes(ar,bio.c)) + 
  geom_ribbon(aes(ymin=bio.l,ymax=bio.u),fill='grey') + 
  geom_ribbon(aes(ymin=bio.l,ymax=bio.u),fill='lightgreen',
              data = smb.index %>% 
                filter(lengd == 5,scaling=='scaled')) + 
  geom_line() + 
  geom_line(aes(y=bio.anF),col='darkgreen') +
  theme_light() +
  labs(x='Year',y='Total biomass') +
  expand_limits(y=0) +
  #  geom_point(data=smh.index %>% 
  #               filter(lengd == 5),
  #             aes(ar,bio.staerri)) +
  geom_pointrange(data=smh.index %>% 
                    filter(lengd == 5),
                  aes(ar,bio.staerri,ymax=bio.u,ymin=bio.l)) +
  geom_label(data = smb.index %>% 
               filter(lengd == 5) %>% 
               select(sc.bio) %>% 
               distinct() %>% 
               mutate(x=1990,y=0,label=paste('Scaling factor =',round(sc.bio,4))),
             aes(x,y,label=label))

## biomass > 40 cm
b40_plot <- 
  smb.index %>% 
  filter(lengd == 40) %>% 
  ggplot(aes(ar,bio.c)) + 
  geom_ribbon(aes(ymin=bio.l,ymax=bio.u),fill='grey') + 
  geom_ribbon(aes(ymin=bio.l,ymax=bio.u),fill='lightgreen',
              data = smb.index %>% 
                filter(lengd == 40,scaling=='scaled')) + 
  geom_line() + 
  geom_line(aes(y=bio.anF),col='darkgreen') +
  theme_light() +
  labs(x='Year',y='Biomass > 40') +
  expand_limits(y=0) +
  #  geom_point(data=smh.index %>% 
  #               filter(lengd == 5),
  #             aes(ar,bio.staerri)) +
  geom_pointrange(data=smh.index %>% 
                    filter(lengd == 40),
                  aes(ar,bio.staerri,ymax=bio.u,ymin=bio.l)) +
  geom_label(data = smb.index %>% 
               filter(lengd == 40) %>% 
               select(sc.bio) %>% 
               distinct() %>% 
               mutate(x=1990,y=0,label=paste('Scaling factor =',round(sc.bio,4))),
             aes(x,y,label=label))

## biomass > 60

b60_plot <- 
  smb.index %>% 
  filter(lengd == 60) %>% 
  ggplot(aes(ar,bio.c)) + 
  geom_ribbon(aes(ymin=bio.l,ymax=bio.u),fill='grey') + 
  geom_ribbon(aes(ymin=bio.l,ymax=bio.u),fill='lightgreen',
              data = smb.index %>% 
                filter(lengd == 60,scaling=='scaled')) + 
  geom_line() + 
  geom_line(aes(y=bio.anF),col='darkgreen') +
  theme_light() +
  labs(x='Year',y='Biomass > 60') +
  expand_limits(y=0) +
  #  geom_point(data=smh.index %>% 
  #               filter(lengd == 5),
  #             aes(ar,bio.staerri)) +
  geom_pointrange(data=smh.index %>% 
                    filter(lengd == 60),
                  aes(ar,bio.staerri,ymax=bio.u,ymin=bio.l)) +
  geom_label(data = smb.index %>% 
               filter(lengd == 60) %>% 
               select(sc.bio) %>% 
               distinct() %>% 
               mutate(x=1990,y=0,label=paste('Scaling factor =',round(sc.bio,4))),
             aes(x,y,label=label))

## abundance < 30

a30_plot <- 
  smb.index %>% 
  filter(lengd == 30) %>% 
  ggplot(aes(ar,fj.minni.c)) + 
  geom_ribbon(aes(ymin=fj.minni.l,ymax=fj.minni.u),fill='grey') + 
  geom_ribbon(aes(ymin=fj.minni.l,ymax=fj.minni.u),fill='lightgreen',
              data = smb.index %>% 
                filter(lengd == 30,scaling=='scaled')) + 
  geom_line() + 
  geom_line(aes(y=fj.minni.anF),col='darkgreen') +
  theme_light() +
  labs(x='Year',y='Abundance < 30') +
  expand_limits(y=0) +
  #  geom_point(data=smh.index %>% 
  #               filter(lengd == 5),
  #             aes(ar,bio.staerri)) +
  geom_pointrange(data=smh.index %>% 
                    filter(lengd == 40),
                  aes(ar,fj.minni,ymax=fj.minni.u,ymin=fj.minni.l)) +
  geom_label(data = smb.index %>% 
               filter(lengd == 40) %>% 
               select(sc.fj.minni) %>% 
               distinct() %>% 
               mutate(x=1990,y=0,label=paste('Scaling factor =',round(sc.fj.minni,4))),
             aes(x,y,label=label))

## 4-plot

four_plot <- 
  total_bio_plot + 
  b40_plot + 
  b60_plot + 
  a30_plot + 
  plot_layout(ncol=2)
