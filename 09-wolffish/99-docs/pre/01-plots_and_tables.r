library(mar)
library(mfdb)
library(knitr)
library(kableExtra)
library(gridExtra)
#devtools::install_github('einarhjorleifsson/gisland')
library(gisland)
#devtools::install_github('thomasp85/patchwork')
#devtools::install_github('Hafro/geo')
library(patchwork)
#library(forcats)
#this year and marteg defined in main .Rnw

foreign_missing<-
  read.csv('../../../06-ling/99-docs/pre/lin.27.5a_foreignlandings.csv') %>% 
  mutate(species = 6, Year = as.numeric(substring(Year,1,4))) %>%
  gather(key = Country, value = Catch, -c(Year,species)) %>% 
  full_join(read.csv('../../../08-tusk/99-docs/pre/usk.27.5a14_foreignlandings.csv') %>%
              mutate(species = 8, Year = as.numeric(substring(Year,1,4))) %>%
              gather(key = Country, value = Catch,  -c(Year,species)) %>% 
              full_join(read.csv('../../../06-ling/99-docs/pre/bli.27.5a14_foreignlandings.csv') %>%
                          mutate(species = 7, Year = as.numeric(substring(Year,1,4))) %>%
                          gather(key = Country, value = Catch,  -c(Year,species)) 
              )) %>% 
  filter(Year %in% c(2011,2012,2013), species==marteg) %>% 
  select(year = Year, country = Country, c = Catch) %>% 
  mutate(country = ifelse(country=='Faroes', 'Faroe Islands', country)) 

tusk14<-    
  read.csv('../../../08-tusk/99-docs/pre/usk.27.5a14_section14_foreignlandings.csv') %>% 
              gather(key = country, value = c, -Year) %>% 
              mutate(section = '14') %>% 
              rename(year = Year) %>% 
  bind_rows(
        read.csv('../../../08-tusk/99-docs/pre/ICES_preliminarycatch_tusk2017.csv') %>% 
        filter(grepl('27_14', Area)) %>% 
        group_by(Year) %>% 
        #BELOW NEEDS TO BE CHECKED EVERY YEAR - CORRECTING FOR INCORRECT UNITS
        mutate(corr = ifelse(Country %in% c('DE','NO'), 1, 1000)) %>% 
        summarise(c = sum(AMS.Catch.TLW./corr)) %>% 
        rename(year = 'Year') %>% 
        mutate(country = 'all', section = '14')
          )
              

mar <- connect_mar()
tyr <- thisyear
sp <-
  tbl(mar,'species_key') %>% 
  filter(tegund==marteg) %>% 
  select(species) %>%
  rename(mfdb_sp=species) %>% 
  collect()

load('/net/hafkaldi/export/home/haf/einarhj/r/Pakkar/landr/data/lices.rda')

sp_names<-data_frame(mfdb_sp = c('HAD', 'LIN', 'USK', 'GSS', 'BLI'), 
                     pretty_name = c('Haddock', 'Ling', 'Tusk', 'Greater silver smelt', 'Blue ling'),
                     lices_name = c('Haddock',
                                    'Ling',
                                      lices %>% filter(as.numeric(sare)==5,tolower(div)=='a', grepl('usk',species)) %>% 
                                                        select(species) %>% 
                                                        distinct() %>% as.character(.) , 
                                    'Greater silver smelt',
                                    'Blue ling')
                                    )
                     
left_join(sp,sp_names) %>% attach(.)

gearlist<-NULL
gearlist[[2]]<-c('BMT','LLN','DSE')
gearlist[[6]]<-c('BMT','LLN','GIL')
gearlist[[7]]<-c('BMT','LLN','GIL')
gearlist[[8]]<-c('BMT','LLN','GIL')
gearlist[[19]]<-c('BMT','LLN','GIL')

imp_gears<-gearlist[[marteg]]

depthlist<-NULL
depthlist[[2]]<-c(101,201,301)
depthlist[[6]]<-c(101,201,301)
depthlist[[8]]<-c(101,201,301)
depthlist[[7]]<-c(401,501,601)
depthlist[[19]]<-c(401,501,601)

depths<-depthlist[[marteg]]

pathlist<-NULL
pathlist[[19]]<-'../../../../19-GSS/tech report/figs/'
pathlist[[8]]<-'../../../08-tusk/99-docs/tech report/figs/'
pathlist[[7]]<-'../../../'
pathlist[[6]]<-'../../../06-ling/99-docs/tech report/figs/'


if(FALSE){
  
  ## stuff to load for the first time..
  
  load('/net/hafkaldi/export/home/haf/einarhj/r/Pakkar/landr/data/lices.rda')
  dbWriteTable(mar,'lices',lices)
  
  bind_rows(
    list.files('/net/hafkaldi/export/u2/reikn/R/Pakkar/Logbooks/Olddata',pattern = '^[0-9]+',full.names = TRUE) %>% 
      map(~read.table(.,skip=2,stringsAsFactors = FALSE,sep='\t')) %>% 
      bind_rows() %>% 
      rename_(.dots=stats::setNames(colnames(.),c('vf',	'skip',	'teg',	'ar',	'man',	'hofn',	'magn'))) %>% 
      mutate(magn=as.numeric(magn)),
    list.files('/net/hafkaldi/export/u2/reikn/R/Pakkar/Logbooks/Olddata',pattern = 'ready',full.names = TRUE) %>% 
      map(~read.table(.,skip=2,stringsAsFactors = FALSE,sep='\t')) %>% 
      bind_rows() %>% 
      rename_(.dots=stats::setNames(colnames(.),c(	'ar','hofn',	'man',	'vf',	'teg', 'magn'))),
    list.files('/net/hafkaldi/export/u2/reikn/R/Pakkar/Logbooks/Olddata',pattern = 'afli.[0-9]+$',full.names = TRUE) %>% 
      map(~read.table(.,skip=2,stringsAsFactors = FALSE,sep=';')) %>% 
      bind_rows()%>% 
      rename_(.dots=stats::setNames(colnames(.),c(	'ar','hofn',	'man',	'vf',	'teg', 'magn')))) %>%
    filter(!(ar==1991&is.na(skip))) %>% 
    mutate(veidisvaedi='I') %>% 
    rename(veidarfaeri=vf,skip_nr=skip,magn_oslaegt=magn,fteg=teg) %>% 
    dbWriteTable(mar,'landed_catch_pre94',.)
  
  dbRemoveTable(mar,paste0(sp,'_catch'))
  mar::afli_afli(mar) %>% 
    dplyr::filter(tegund == marteg) %>%
    dplyr::left_join(afli_afli(mar) %>% 
                       dplyr::group_by(visir) %>% 
                       dplyr::summarise(total=sum(afli))) %>% 
    dplyr::inner_join(mar::afli_stofn(mar) %>% mutate(gridcell = reitur*10+smareitur)) %>% 
    dplyr::left_join(mar::afli_toga(mar) %>% 
                       dplyr::select(visir,togtimi)) %>% 
    dplyr::left_join(tbl(mar,'gear_mapping'),by=c('veidarf'='veidarfaeri')) %>% 
    dplyr::select(id=visir,towtime=togtimi,gear,vessel_nr=skipnr,year=ar,month=man,
                  lat=breidd,lon=lengd,gridcell,depth=dypi,catch=afli,total) %>% 
    dplyr::mutate(depth = 1.83*depth) %>% 
    compute(name=paste0(sp,'_catch'),temporary=FALSE)
  
}



## landings data by country  
landings <- 
  tbl(mar,'lices') %>% 
  filter(#descr %in%  c('Va','5_A'),
    trim(sare) == '5',
    div == 'a',
    species == lices_name) %>%  
  rename(country = country2) %>% 
  filter(!(country == 'Iceland' & year>1981)) %>% 
  group_by(year,country) %>% 
  summarise(c = sum(landings,na.rm = TRUE)) %>% 
  collect(n=Inf) %>% 
  bind_rows(tbl(mar,'landed_catch_pre94') %>% 
              filter(fteg == marteg) %>% 
              group_by(ar) %>% 
              summarise(c=sum(magn_oslaegt,na.rm = TRUE)/1e3) %>% 
              mutate(country='Iceland') %>% 
              collect(n=Inf) %>% 
              rename(year=ar)) %>%  
  bind_rows(lods_oslaegt(mar) %>% 
              filter(fteg == marteg,veidisvaedi == 'I',ar>1993) %>% 
              left_join(lesa_skipaskra(mar)) %>% 
              mutate(country = ifelse(nvl(flokkur,0) != -4,'Iceland',einkst)) %>% 
              group_by(ar,country) %>% 
              summarise(c=sum(magn_oslaegt,na.rm = TRUE)/1e3)%>% 
              collect(n=Inf) %>% 
              rename(year=ar)) %>% 
  arrange(desc(country)) %>%
  mutate(country = forcats::fct_recode(country,Norway = 'NO',
                                       `Faroe Islands` = 'FO',
                                       Belgium = 'BE', 
                                       Greenland = "GR",
                                       Greenland = 'GL',
                                       Germany = 'DE',
                                       Iceland = 'IS',
                                       Russia = 'RU'),
         country = ifelse(grepl('UK',country)|country=='GB','UK',country)) %>% 
  filter(!(year %in% c(2011, 2012, 2013) & country != 'Iceland')) %>% 
  bind_rows( foreign_missing %>% 
            filter(!(country %in% c('Total', 'Iceland')))) %>% 
  filter(year < tyr,c>0) %>% 
  group_by(year,country) %>% 
  summarise(c=sum(c)) %>% 
  mutate(section = '5a')

if(marteg==8){
  landings <-
    landings %>%
    bind_rows(tusk14) 
} else {
  landings <-
    landings %>% 
    bind_rows(data.frame(year = 2000, country = 'Iceland', c = 0, section = '14'))
}

## catch history plot
landings.plot <- 
  landings %>%
  mutate(country = ifelse(ifelse(is.na(country),' ',country)=='Iceland','Iceland','Other nations')) %>%
  group_by(year,country) %>% 
  summarise(c=sum(c)) %>% 
  arrange(desc(country)) %>%
  ggplot(aes(year,c/1e3,fill=country)) + 
  geom_bar(stat='identity') + 
  theme_bw() + 
  labs(y = 'Landings (in kt)', x = 'Year', fill = '') + 
  theme(legend.background = element_blank(),
        legend.position = c(0.15,0.75)) + 
  scale_fill_manual(values=c('lightblue','darkblue'))

## catch history plot
landings.plot.tusk <- 
  landings %>%
  mutate(country = ifelse(section == 14, '14', country), country = ifelse(ifelse(is.na(country),' ',country)=='Iceland','Iceland in 5a',
                                                                          ifelse(country=='14', 'All nations in 14', 'Other nations in 5a'))) %>%
  mutate(country = ifelse(is.na(country), '', country)) %>% 
  group_by(year,country) %>% 
  summarise(c=sum(c)) %>% 
  arrange(desc(country)) %>%
  ggplot(aes(year,c/1e3,fill=country)) + 
  geom_bar(stat='identity') + 
  theme_bw() + 
  labs(y = 'Landings (in kt)', x = 'Year', fill = '') + 
  theme(legend.background = element_blank(),
        legend.position = c(0.15,0.75)) + 
  scale_fill_manual(values=c('white','orange', 'lightblue','darkblue'))



## landings by nation
landings_by_country <- 
  landings %>% 
#  filter(year>1978 & year < tyr) %>% 
  filter(year>tyr-19 & year < tyr) %>% 
  mutate(c=round(c)) %>% 
  spread(country,c,fill = '')

## number of boats and landings by gear 
nb_lnd_by_yr <- 
  lods_oslaegt(mar) %>% 
  filter(fteg == marteg,veidisvaedi == 'I',ar>1992) %>% 
  left_join(tbl(mar,'gear_mapping')) %>% 
  mutate(gear = ifelse(nvl(gear,' ') %in% imp_gears,gear, 'Other')) %>% 
  group_by(ar,gear) %>% 
  summarise(c = sum(magn_oslaegt)/1e3,
            nb = n_distinct(skip_nr)) %>% 
  collect(n=Inf) %>% 
  mutate(gear = forcats::fct_recode(gear,
                                    Longlines='LLN',
                                    `Bottom trawl`='BMT',
                                    `Danish seine`='DSE',
                                    `Gill nets`='GIL')) %>% 
  gather(col,val,-c(ar,gear)) %>% 
  unite(col,c(col,gear),sep = '_') %>%
  spread(col,val) %>% 
  ungroup() %>% 
  mutate(`Total catch` = rowSums(.[grep('c_',names(.))],na.rm = TRUE)) %>% 
  select(Year=ar,starts_with('nb_'),starts_with('c_'),`Total catch`) %>% 
  select(-nb_Other) %>% 
  set_names(.,gsub('c_|nb_','',names(.))) 

nb_land_gear_country <-
  lods_oslaegt(mar) %>% 
  filter(fteg == marteg,veidisvaedi == 'I',ar>1993) %>% 
  left_join(lesa_skipaskra(mar)) %>% 
  mutate(country = ifelse(nvl(flokkur,0) != -4,'Iceland',einkst)) %>% 
  left_join(tbl(mar,'gear_mapping')) %>% 
  mutate(gear = ifelse(country == 'Iceland', 
                       ifelse(nvl(gear,' ') %in% imp_gears,gear, 'Other'),'Foreign')) %>% 
  select(year = ar, gear, magn_oslaegt) %>% 
  group_by(year,gear) %>% 
  summarise(c=sum(magn_oslaegt,na.rm = TRUE)/1e3)%>% 
  collect(n=Inf) %>%
  mutate(gear = forcats::fct_recode(gear,
                                    lina='LLN',
                                    botnv='BMT',
                                    annad='Other',
                                    erl='Foreign',
                                    net='GIL')) %>% 
  spread(key = gear, value = c)


## plot landings in tons by gear and year
lnd_by_gear <- 
  lods_oslaegt(mar) %>% 
  filter(fteg == marteg,veidisvaedi == 'I',ar>1992,ar < tyr) %>% 
  left_join(tbl(mar,'gear_mapping')) %>% 
  mutate(gear = ifelse(nvl(gear,' ') %in% imp_gears,gear, 'Other')) %>% 
  group_by(ar,gear) %>% 
  summarise(c = sum(magn_oslaegt,na.rm=TRUE)/1e3) %>% 
  collect(n=Inf) %>% 
  mutate(gear = forcats::fct_recode(gear,
                                    Longlines='LLN',
                                    `Bottom trawl`='BMT',
                                    `Danish seine`='DSE',
                                    `Gill nets`='GIL')) %>% 
  arrange(gear) 


lnd_by_gear_top <- 
  lnd_by_gear %>% 
  ggplot(aes(ar,c,fill=gear)) + 
  geom_bar(stat = 'identity') + 
  theme_light()+
  theme(axis.title.x=element_blank(),
        axis.text.y=element_text(angle=90),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="top") +
  labs(y="Catches (kt)",fill='') +
  scale_fill_brewer(palette='YlOrRd',direction = -1)

lnd_by_gear_bottom <- 
  lnd_by_gear %>% 
  ggplot(aes(ar,c,fill=gear)) + 
  geom_bar(stat = 'identity',position = "fill") + 
  theme_light()+
  theme(axis.text.y=element_text(angle=90),
        legend.position=" ")+
  labs(y="Proportion (of catches)",x="Year") +
  scale_fill_brewer(palette='YlOrRd',direction = -1)

## depth by gear and year
depth_dat <- 
  tbl(mar,paste0(sp,'_catch')) %>% 
  filter(year > 2000, year < tyr) %>% 
  mutate(depth_class = ifelse(depth < depths[1],"4",
                              ifelse(depth < depths[2],"3",
                                     ifelse(depth < depths[3], "2","1")))) %>% 
  mutate(depth_class = nvl(depth_class,"1")) %>% 
  filter(gear %in% imp_gears) %>% 
  group_by(year,depth_class) %>% 
  summarise(c=sum(catch, na.rm = TRUE)/1e6) %>% 
  collect(n=Inf) %>% 
  arrange(depth_class)

depth_plot <- 
  depth_dat %>%  
  ggplot(aes(year,c,fill=depth_class)) + geom_bar(stat='identity') +
  theme_light()+
  theme(axis.title.x=element_blank(),
        axis.text.y=element_text(angle=90),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="top") +
  labs(y="Catches (kt)") + 
  scale_fill_manual(values = c("#225EA8","#41B6C4","#7FCDBB","#C7E9B4"),
                    name="Total catch \n by depth (m)",
                    breaks=c("4", "3", "2", "1"),
                    labels=c(paste0("1-",depths[1]),  
                             paste0(depths[1],"-",c(depths[2]-1)), 
                             paste0(depths[2],"-",c(depths[3]-1)), 
                             paste0(">",c(depths[3]-1)))
                    )

depth_fill_plot <- 
  depth_dat %>% 
  ggplot(aes(year,c,fill=depth_class)) + 
  geom_bar(stat='identity', position = 'fill') +
  theme_light()+
  theme(axis.text.y=element_text(angle=90),
        legend.position=" ")+
  labs(y="Proportion (of catches)",x="Year") +
  scale_fill_manual(values = c("#225EA8","#41B6C4","#7FCDBB","#C7E9B4"),
                    name="Total catch \n by depth (m)",
                    breaks=c("4", "3", "2", "1"),
                    labels=c("1-100",  "101-200", "201-300", ">300"))


## Catches by gadget division
reitmapping <- 
  read.table(
    system.file("demo-data", "reitmapping.tsv", package="mfdb"),
    header=TRUE,
    as.is=TRUE)

region.map <- function(d){
    d %>% 
      mutate(DIVISION = ifelse(is.na(DIVISION),0,DIVISION)) %>% 
      mutate(region = ifelse(DIVISION %in% c(101),'W',
                             ifelse(DIVISION %in% c(102),'NW',
                                    ifelse(DIVISION %in% c(103,104,105),'NE',
                                           ifelse(DIVISION %in% c(107,106),'SE',
                                                  ifelse(DIVISION %in% c(108),'SW','Other'))))))
  }

region_labels <- 
  list(data_frame(x=-25,y=64,label='W'),
       data_frame(x=-25,y=66,label='NW'),
       data_frame(x=-14,y=66,label='NE'),
       data_frame(x=-22.5,y=63.5,label='SW'),
       data_frame(x=-15,y=63.7,label='SE')) %>% 
  bind_rows()

region.plot <- 
  tbl(mar,'reitmapping') %>% 
  collect(n=Inf) %>% 
  inner_join(reitmapping %>% 
               mutate(SUBDIVISION=as.character(SUBDIVISION),
                      GRIDCELL=as.character(GRIDCELL))) %>% 
  mutate(SUBDIVISION=as.numeric(SUBDIVISION),
         GRIDCELL=as.numeric(GRIDCELL)) %>% 
  region.map() %>% 
  mutate(region = forcats::fct_relevel(region,'W','NW','NE','SE','SW','Other')) %>% 
  #mutate(region = as.factor(region)) %>% 
  filter(region!='Other') %>% 
  ggplot() + 
  geom_raster(aes(lon,lat,fill=region)) + 
  coord_quickmap() + 
  geom_polygon(data=gisland::biceland,aes(long,lat,group=group),col='black',fill='khaki') + 
  theme_light() +
  theme(legend.position = 'none', 
        axis.text = element_blank(),
        plot.margin = margin(0,0,0,0,'cm'),
        panel.spacing = unit(0,'lines')) + 
  geom_label(data=region_labels,aes(x,y,label=label),col='black',size=3) +
  labs(y='',x='') +
  scale_fill_brewer(palette='YlOrRd', drop=F) 

catch_by_area <- 
  tbl(mar,paste0(sp,'_catch')) %>%
  filter(year>1992,year<tyr) %>% 
  collect(n=Inf) %>% 
  left_join(tbl(mar,'reitmapping') %>% 
              collect(n=Inf) %>% 
              inner_join(reitmapping %>% 
                           mutate(SUBDIVISION=as.character(SUBDIVISION),
                                  GRIDCELL=as.character(GRIDCELL))) %>% 
              mutate(SUBDIVISION=as.numeric(SUBDIVISION),
                     GRIDCELL=as.numeric(GRIDCELL)) %>% 
              rename(gridcell = GRIDCELL) %>% 
              select(-c(id,lat,lon,size))) %>%
  region.map() %>% 
  group_by(year,region) %>% 
  summarise(c=sum(catch)/1e6) %>% 
  mutate(region = forcats::fct_relevel(region,'W','NW','NE','SE','SW','Other')) %>% 
  arrange(region) %>% 
  ggplot(aes(year,c,fill=region)) + 
  geom_bar(stat='identity') + 
  theme_light()+
  theme(axis.title.x=element_blank(),
        axis.text.y=element_text(angle=90),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = 'top') +
  labs(y="Catches (kt)",fill='') +
  scale_fill_brewer(palette='YlOrRd') +
  
  tbl(mar,paste0(sp,'_catch')) %>%
  filter(year>1992,year<tyr) %>% 
  collect(n=Inf) %>% 
  left_join(tbl(mar,'reitmapping') %>% 
              collect(n=Inf) %>% 
              inner_join(reitmapping %>% 
                           mutate(SUBDIVISION=as.character(SUBDIVISION),
                                  GRIDCELL=as.character(GRIDCELL))) %>% 
              mutate(SUBDIVISION=as.numeric(SUBDIVISION),
                     GRIDCELL=as.numeric(GRIDCELL)) %>% 
              rename(gridcell = GRIDCELL) %>% 
              select(-c(id,lat,lon,size))) %>%
  region.map() %>% 
  group_by(year,region) %>% 
  summarise(c=sum(catch)) %>% 
  mutate(region = forcats::fct_relevel(region,'W','NW','NE','SE','SW','Other')) %>% 
  arrange(region) %>% 
  ggplot(aes(year,c,fill=region)) + 
  geom_bar(stat='identity',position = 'fill') + 
  theme_light() +
  theme(axis.text.y=element_text(angle=90),
        legend.position=" ")+
  labs(y="Proportion (of catches)",x="Year") +
  scale_fill_brewer(palette='YlOrRd') +
  plot_layout(ncol=1)


## spatial distribution of catches
catch_dist_plot <- 
  tbl(mar,paste0(sp,'_catch')) %>% 
  filter(year >= tyr-16, year < tyr) %>% 
  #filter(year %in% c(1990,1995,2000,2005,2010,2016)) %>% 
  mar:::encode_zchords(dx=0.125,dy=0.0625) %>% 
  #  select(-sq) %>% 
  #  unite(sq,x,y,sep=':') %>% 
  group_by(year,sq) %>% 
  dplyr::summarise(catch=sum(catch),
                   effort=sum(towtime)) %>% 
  collect(n=Inf) %>% 
  separate(sq,c('lon','lat'),sep=':') %>% 
  mutate(lon = gsub(',','.',lon) %>% as.numeric(),
         lat = gsub(',','.',lat)%>% as.numeric()) %>% 
  filter(!is.na(catch)) %>% 
  dplyr::mutate(catch = cut(catch/1e3, 
                            breaks = c(-1,0.25,1,15,40,60,10000000),
                            labels = c('< 0.25 kt','< 1 kt',
                                       '< 15 kt','< 40 kt','< 60 kt','< 1000 kt'))) %>% 
  ungroup() %>% 
  ggplot() +
  geom_tile(aes(lon, lat, fill = catch),alpha=0.9) + 
  geom_polygon(data = geo::bisland, aes(lon, lat), fill = 'gray70',col='black') +
  #  geom_polygon(data= fortify(greenland),  aes(long, lat,group=group), fill = 'gray70',col='black') +
  #  geom_polygon(data= fortify(faroes),  aes(long, lat,group=group), fill = 'gray70',col='black') +
  geom_label(x=-18,y=65,aes(label=year),data= data_frame(year = (tyr-16):(tyr-1))) +
  
  #  geom_polygon(data=geo::eyjar, aes(lon, lat),col = 'black', fill = 'gray70',size = 0.3) +
  #  geom_polygon(data=geo::faeroes, aes(lon, lat),col = 'black', fill = 'gray70')+
  
  geom_path(aes(lon, lat),data=geo::gbdypi.100,col='grey',size = 0.3) +
  #  geom_path(data=gbdypi.800,lty=2,size = 0.3) +
  #  geom_polygon(alpha=0.5,data=gbdypi.100,fill='gray90') +
  #  geom_path(data=gbdypi.200,lty=2,size = 0.3) + #alpha=0.5,fill='gray90',
  geom_path(aes(lon, lat),data=geo::gbdypi.500,col='grey',size = 0.3) +
  geom_path(aes(lon, lat),data=geo::gbdypi.1000,col='grey',size = 0.3) +
  
  #  coord_quickmap( xlim=c(-75,-0),ylim=c(60,85)) +
  #  scale_x_continuous(name = NULL, breaks = NULL) +
  #  scale_y_continuous(name = NULL, breaks = NULL) +
  labs(fill = "Catch") +
  scale_fill_brewer(palette='YlOrRd') +
  #  scale_fill_gradient2(low = "yellow", mid = "red",
  #                       high = "black", midpoint = )+
  theme_bw()+
  coord_quickmap(xlim = c(-30, -10),ylim = c(63, 68))+
  
  #geom_path(data=fortify(ego),aes(long,lat,group=group))+
  facet_wrap(~year,ncol=4)+ 
  theme(#axis.text = element_text(size = 10),
    plot.title = element_text(size = 14),
    legend.background = element_rect(fill = "white"),
    legend.position = 'none',
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.key.size= unit(0.5, 'cm'),
    #legend.key = element_rect(colour = NA),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.margin = unit(0,'cm'),
    plot.margin = unit(c(0,0,0,0),'cm'),
    strip.background = element_blank(),
    strip.text.x = element_blank())

## position of samples from the catches
sampling_pos <- 
  lesa_stodvar(mar) %>% 
  filter(ar == (tyr -1),synaflokkur %in% c(1,2,8)) %>% 
  inner_join(lesa_kvarnir(mar) %>% filter(tegund == marteg)) %>%
  left_join(tbl(mar,'gear_mapping')) %>% 
  filter(gear %in% imp_gears) %>% 
  select(lat=kastad_n_breidd,lon=kastad_v_lengd,year = ar,gear) %>% 
  distinct() %>% 
  collect(n=Inf)

sampled_gears <- sampling_pos %>% select(gear) %>% distinct() %>% unlist()

sampling_pos_plot <- 
  tbl(mar,paste0(sp,'_catch')) %>% 
  filter(year == (tyr -1), gear %in% sampled_gears) %>% 
  #filter(year %in% c(1990,1995,2000,2005,2010,2016)) %>% 
  mar:::encode_zchords(dx=0.125,dy=0.0625) %>% 
  #  select(-sq) %>% 
  #  unite(sq,x,y,sep=':') %>% 
  group_by(gear,sq) %>% 
  dplyr::summarise(catch=sum(catch),
                   effort=sum(towtime)) %>% 
  collect(n=Inf) %>% 
  separate(sq,c('lon','lat'),sep=':') %>% 
  mutate(lon = gsub(',','.',lon) %>% as.numeric(),
         lat = gsub(',','.',lat)%>% as.numeric()) %>% 
  filter(!is.na(catch)) %>% 
  dplyr::mutate(catch = cut(catch/1e3, 
                            breaks = c(-1,0.25,1,15,40,60,10000000),
                            labels = c('< 0.25 kt','< 1 kt',
                                       '< 15 kt','< 40 kt','< 60 kt','< 1000 kt'))) %>% 
  ungroup() %>% 
  ggplot() +
  geom_tile(aes(lon, lat, fill = catch),alpha=0.9) + 
  geom_polygon(data = geo::bisland, aes(lon, lat), fill = 'gray70',col='black') +
  #  geom_polygon(data= fortify(greenland),  aes(long, lat,group=group), fill = 'gray70',col='black') +
  #  geom_polygon(data= fortify(faroes),  aes(long, lat,group=group), fill = 'gray70',col='black') +
  geom_label(x=-18,y=65,aes(label=gear),data=sampling_pos %>% select(gear) %>% distinct()) +
  
  #  geom_polygon(data=geo::eyjar, aes(lon, lat),col = 'black', fill = 'gray70',size = 0.3) +
  #  geom_polygon(data=geo::faeroes, aes(lon, lat),col = 'black', fill = 'gray70')+
  
  geom_path(aes(lon, lat),data=geo::gbdypi.100,col='grey',size = 0.3) +
  #  geom_path(data=gbdypi.800,lty=2,size = 0.3) +
  #  geom_polygon(alpha=0.5,data=gbdypi.100,fill='gray90') +
  #  geom_path(data=gbdypi.200,lty=2,size = 0.3) + #alpha=0.5,fill='gray90',
  geom_path(aes(lon, lat),data=geo::gbdypi.500,col='grey',size = 0.3) +
  geom_path(aes(lon, lat),data=geo::gbdypi.1000,col='grey',size = 0.3) +
  
  geom_point(aes(lon,lat), data= sampling_pos, pch = 8) +
  
  #  coord_quickmap( xlim=c(-75,-0),ylim=c(60,85)) +
  #  scale_x_continuous(name = NULL, breaks = NULL) +
  #  scale_y_continuous(name = NULL, breaks = NULL) +
  labs(fill = "Catch") +
  scale_fill_brewer(palette='YlOrRd') +
  #  scale_fill_gradient2(low = "yellow", mid = "red",
  #                       high = "black", midpoint = )+
  theme_bw()+
  coord_quickmap(xlim = c(-30, -10),ylim = c(63, 68))+
  
  #geom_path(data=fortify(ego),aes(long,lat,group=group))+
  facet_wrap(~gear,ncol=2)+ 
  theme(#axis.text = element_text(size = 10),
    plot.title = element_text(size = 14),
    legend.background = element_rect(fill = "white"),
    legend.position = 'none',
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.key.size= unit(0.5, 'cm'),
    #legend.key = element_rect(colour = NA),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.margin = unit(0,'cm'),
    plot.margin = unit(c(0,0,0,0),'cm'),
    strip.background = element_blank(),
    strip.text.x = element_blank())

## length samples from commercial samples
sampling_lengths <- 
  lesa_stodvar(mar) %>% 
  filter(ar < tyr, ar>1999,synaflokkur %in% c(1,2,8)) %>% 
  inner_join(lesa_lengdir(mar) %>% filter(tegund == marteg)) %>% 
  left_join(tbl(mar,'gear_mapping')) %>% 
  mutate(gear = ifelse(nvl(gear,' ') %in% 
                         c(' ','TRP','HLN','VAR','NPT','PGT','SHT','PSE'),'Other',gear)) %>% 
  group_by(ar,gear) %>% 
  summarise(n = sum(fjoldi,na.rm=TRUE)) %>% 
  #filter(gear %in% c('BMT','LLN','DSE')) %>% 
  select(year = ar,gear,n) %>% 
  collect(n=Inf) %>% 
  left_join(mfdb::gear %>% rename(gear=name) %>% 
              mutate(gear = as.character(gear),
                     description = as.character(description)) %>% 
              select(gear,description)) %>% 
  mutate(gear = ifelse(is.na(description),'Other',description)) %>% 
  select(-description) %>% 
  spread(gear,n, fill= 0) 


## age samples from commercial samples
sampling_ages <- 
  lesa_stodvar(mar) %>% 
  filter(ar < tyr, ar>1999,synaflokkur %in% c(1,2,8)) %>% 
  inner_join(lesa_kvarnir(mar) %>% filter(tegund == marteg)) %>% 
  left_join(tbl(mar,'gear_mapping')) %>% 
  mutate(gear = ifelse(nvl(gear,' ') %in% 
                         c(' ','TRP','HLN','VAR','NPT','PGT','SHT','PSE'),'Other',gear)) %>% 
  group_by(ar,gear) %>% 
  summarise(n = n()) %>% 
  #filter(gear %in% c('BMT','LLN','DSE')) %>% 
  select(year = ar,gear,n) %>% 
  collect(n=Inf) %>% 
  left_join(mfdb::gear %>% rename(gear=name) %>% 
              mutate(gear = as.character(gear),
                     description = as.character(description)) %>% 
              select(gear,description)) %>% 
  mutate(gear = ifelse(is.na(description),'Other',description)) %>% 
  select(-description) %>% 
  spread(gear,n, fill= 0) 

## transfers
transfer_table <- 
  mar:::kvoti_stada_summarised(mar) %>%
  filter(fteg==marteg) %>% 
  collect(n=Inf) %>% 
  mutate(timabil = ifelse(str_sub(timabil,1,1) %in% "9",
                          paste0(1900+as.integer(str_sub(timabil,1,2)),"/",str_sub(timabil,3)),
                          paste0(2000+as.integer(str_sub(timabil,1,2)),"/",str_sub(timabil,3)))) %>% 
  arrange(fteg, timabil) %>% 
  ungroup() %>% 
  mutate(diff=varanlegt - afli,
         diffp=diff/varanlegt,
         diff =sprintf("%s (%.1f %%)",diff,100*diffp),
         stada = sprintf("%s (%.1f %%)",stada,100*stada/kvoti)) %>% 
  select(Period=timabil,TAC=varanlegt, Catch=afli,Diff=diff,TACtrans = kvoti,Diff_trans=stada) 


transfer_plot <- 
  mar:::kvoti_stada_summarised(mar) %>%
  filter(fteg==marteg) %>% 
  collect(n=Inf) %>% 
  mutate(timabil = ifelse(str_sub(timabil,1,1) %in% "9",
                          paste0(1900+as.integer(str_sub(timabil,1,2)),"/",str_sub(timabil,3)),
                          paste0(2000+as.integer(str_sub(timabil,1,2)),"/",str_sub(timabil,3)))) %>% 
  arrange(fteg, timabil) %>% 
  head(-1) %>% 
  ungroup() %>% 
  mutate(m_ara = m_ara - onotad,
         m_p = 100*m_ara/varanlegt,
         til_p = 100*tilf/varanlegt,
         m_ara = m_ara/1e3,
         tilf = tilf/1e3) %>% 
  select(timabil,tilf,m_ara,
         m_p,til_p) %>% 
  gather(col,value,-timabil) %>%
  mutate(col=ifelse(col=='m_ara','Between years',
                    ifelse(col=='m_p','Between years (%)',
                           ifelse(col=='tilf','Between species',
                                  'Between species (%)')))) %>% 
  filter(!(timabil %in% c('2001/02', '2016/17'))) %>% 
#  filter(!(timabil %in% c('2001/02'))) %>% 
  ggplot(aes(timabil,value)) + geom_bar(stat='identity') + 
  facet_wrap(~col,ncol=2,scale='free_y') + 
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x='Quota period',y='Transfers (in kt)')


#creates four_plot
if(marteg==6){source('../../../06-ling/99-docs/pre/01-indices4plot.R')} 
if(marteg==8){source('../../../06-ling/99-docs/pre/01-indices4plot_orig.R')}

#simple length and age distribution plots - need to be by maturity and have means added to figure
survey_age_dist <-
  fit$catchdist.fleets %>% 
  dplyr::filter(name %in% c('aldist.igfs','aldist.smb')) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(age=as.numeric(gsub('age','',age)), 
                length=as.numeric(gsub('len','',length))) %>% 
#  dplyr::group_by(year,age,step,total.catch) %>% 
  dplyr::group_by(year,age,total.catch) %>% 
  dplyr::summarise(o=sum(observed,na.rm=TRUE)) %>% #,
  #                 p=sum(predicted)) %>% 
  dplyr::mutate(o=ifelse(o==0,NA,o)) %>% 
  ungroup() %>% 
  ggplot(aes(age,o)) + geom_point(col='black') + 
  geom_segment(aes(xend=age,yend=0),col='black') + 
  #facet_wrap(~year+step) + 
  facet_wrap(~year) + 
  theme_bw() + #geom_line(aes(y=p)) + 
  geom_label(x=5,y=0.2,aes(label=year),size=2) +
  #geom_text(x=2,y=0.22,aes(label=paste0('n = ',total.catch)),size=3) +
  theme(strip.background = element_blank(),strip.text=element_blank()) + 
  xlab('Age') + ylab('Proportion') + 
  theme(axis.text.y = element_blank())

survey_length_dist_dat <-
  fit$catchdist.fleets %>% 
  dplyr::filter(name %in% c('ldist.igfs','ldist.smb')) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
                #age=as.numeric(gsub('age','',age)),
                length=floor(as.numeric(gsub('len','',length))/4)*4
                ) %>% 
  select(year, name, length,  number.x) %>%
  dplyr::group_by(year, length) %>% 
  dplyr::summarise(num=sum(number.x,na.rm=TRUE)) %>% #,
  #                 p=sum(predicted)) %>%
  dplyr::left_join(  fit$catchdist.fleets %>% 
                       dplyr::filter(name %in% c('ldist.igfs','ldist.smb')) %>%
                       dplyr::ungroup() %>% 
                       dplyr::mutate(length=as.numeric(gsub('len','',length))) %>% 
                       dplyr::group_by(year) %>% 
                       dplyr::summarise(ml = round(sum(length*number.y, na.rm = T)/sum(number.y, na.rm = T))) 
                    ) %>% 
  #  dplyr::group_by(year,age,step,total.catch) %>% 
  full_join( fit$stockdist %>% 
               dplyr::filter(name %in% c('matp.igfs')) %>%
               dplyr::mutate(length=lower) %>%
               select(year, length, pred.ratio, stock) %>% 
               bind_rows(data.frame(year = rep(rep(c(1986,1987,1988,1992,1993), each = length(seq(4,108,4))),2), 
                                    length = rep(rep(seq(4,108,4), length(c(1986,1987,1988,1992,1993))),2), 
                                    pred.ratio = rep(NA,2*length(c(1986,1987,1988,1992,1993))*length(seq(4,108,4))),
                                    stock = rep(c('tuskimm','tuskmat'),
                                                each = length(c(1986,1987,1988,1992,1993))*length(seq(4,108,4)))
                                    )
                        )
            ) %>% 
  left_join(fit$stockdist %>% 
              dplyr::filter(name %in% c('matp.igfs')) %>%
              dplyr::mutate(length = lower) %>% 
              dplyr::group_by(length, stock) %>% 
              dplyr::summarise(mean.pred.ratio = mean(pred.ratio, na.rm = T)) %>% 
              dplyr::select(length, mean.pred.ratio, stock)) %>% 
  mutate(pred.ratio = ifelse(is.na(pred.ratio), mean.pred.ratio, pred.ratio)) %>% 
  dplyr::mutate(o_num=ifelse(num==0,NA,num)*pred.ratio)

survey_length_dist<-
  survey_length_dist_dat %>% 
  filter(!is.na(stock)) %>% 
  dplyr::group_by(year, length, stock) %>% 
  left_join(survey_length_dist_dat %>% 
              dplyr::group_by(year, stock) %>%
              dplyr::summarise(sumnum = sum(o_num, na.rm = T))
            ) %>% 
  mutate(o = o_num/sumnum) %>%  
  ggplot(aes(length,o, color = stock)) + geom_point(aes(color = stock)) + 
  geom_segment(aes(xend=length,yend=0)) + 
  facet_wrap(~year) + 
  theme_bw() + #geom_line(aes(y=p)) + 
  geom_label(x=90,y=0.2,aes(label=year),size=2, col = 'black') +
  geom_label(x=90,y=0.15,aes(label=paste0('ML = ',ml)),size=2, col = 'black') +
  theme(strip.background = element_blank(),strip.text=element_blank()) + 
  xlab('Length') + ylab('Survey index proportion') + 
  theme(axis.text.y = element_blank())

# ggplot(aes(length,o)) + geom_point(col='black') + 
#   geom_segment(aes(xend=length,yend=0),col='black') + 
#   #facet_wrap(~year+step) + 
#   facet_wrap(~year) + 
#   theme_bw() + #geom_line(aes(y=p)) + 
#   geom_label(x=90,y=0.05,aes(label=year),size=3) +
#   #geom_text(x=2,y=0.22,aes(label=paste0('n = ',total.catch)),size=3) +
#   theme(strip.background = element_blank(),strip.text=element_blank()) + 
#   xlab('Length') + ylab('Proportion') + 
#   theme(axis.text.y = element_blank())
# 
# survey_length_dist <-
#   fit$stockdist %>% 
#   dplyr::filter(name %in% c('matp.igfs')) %>%
#   dplyr::ungroup() %>%
#   left_join( fit$stockdist %>% 
#                dplyr::filter(name %in% c('matp.igfs')) %>%
#                group_by(year) %>% 
#                dplyr::summarise(ml =round(sum(length*number.y, na.rm = T)/sum(number.y, na.rm = T)))
#              ) %>% 
#   ggplot(aes(length,number.y, color = stock)) + geom_point(aes(color = stock)) + 
#   geom_segment(aes(xend=length,yend=0)) + 
#   facet_wrap(~year) + 
#   theme_bw() + #geom_line(aes(y=p)) + 
#   geom_label(x=90,y=200,aes(label=year),size=3, col = 'black') +
#   geom_label(x=90,y=130,aes(label=paste0('ML = ',ml)),size=2, col = 'black') +
#   theme(strip.background = element_blank(),strip.text=element_blank()) + 
#   xlab('Length') + ylab('Number') + 
#   theme(axis.text.y = element_blank())

survey_age_dist <-
  fit$stockdist %>% 
#  dplyr::filter(name %in% c('matp.igfs')) %>%
  dplyr::ungroup() %>%
  left_join( fit$stock.std %>% 
               #dplyr::filter(name %in% c('matp.igfs')) %>%
               group_by(year) %>% 
               dplyr::summarise(ma =round(sum(age*number, na.rm = T)/sum(number, na.rm = T)))
  ) %>% 
  ggplot(aes(age,number, color = stock)) + geom_point(aes(color = stock)) + 
  geom_segment(aes(xend=age,yend=0)) + 
  facet_wrap(~year) + 
  theme_bw() + #geom_line(aes(y=p)) + 
  geom_label(x=9,y=200,aes(label=year),size=3, col = 'black') +
  geom_label(x=9,y=130,aes(label=paste0('MA = ',ma)),size=2, col = 'black') +
  theme(strip.background = element_blank(),strip.text=element_blank()) + 
  xlab('Age') + ylab('Number') + 
  theme(axis.text.y = element_blank())

#simple length and age distribution plots
comm_age_dist_dat <-
  fit$catchdist.fleets %>% 
  dplyr::filter(name %in% c('aldist.bmt','aldist.lln', 'aldist.comm')) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(age=as.numeric(gsub('age','',age)), 
                length=floor(as.numeric(gsub('len','',length))/4)*4,
                comm = ifelse(name=='aldist.bmt', 'Bottom trawl', 'Longline'),
                num = observed*total.catch
                ) %>% 
  #  dplyr::group_by(year,age,step,total.catch) %>% 
  dplyr::group_by(year,age,comm) %>% 
  dplyr::summarise(sumnum=sum(num,na.rm=TRUE)) %>% #,
  #                 p=sum(predicted)) %>%
  dplyr::left_join(fit$catchdist.fleets %>% 
                     dplyr::filter(name %in% c('aldist.bmt','aldist.lln', 'aldist.comm')) %>% 
                     dplyr::ungroup() %>% 
                     dplyr::mutate(comm = ifelse(name=='aldist.bmt', 'Bottom trawl', 'Longline'),
                                   num = observed*total.catch
                                  ) %>% 
                    dplyr::group_by(year, comm) %>% 
                    dplyr::summarise(annual.total.catch=sum(num, na.rm=T))
                   ) %>% 
  dplyr::mutate(o=ifelse(sumnum==0,NA,sumnum)/annual.total.catch) %>% 
  ungroup()

comm_age_dist_dat_bmt<-
  comm_age_dist_dat %>% filter(comm == 'Bottom trawl')

comm_age_dist <-
  comm_age_dist_dat  %>% 
  filter(comm == 'Longline') %>% 
  ggplot(aes(age,o)) + geom_point(col='black') + 
  geom_segment(aes(xend=age,yend=0),col='black') + 
# include below for ling
#  geom_line(aes(age, o, data = comm_age_dist_dat %>% filter(comm=='Bottom trawls')) +
  #facet_wrap(~year+step) + 
  facet_wrap(~year) + 
  theme_bw() + #geom_line(aes(y=p)) + 
  geom_label(x=8,y=0.6,aes(label=year),size=4) +
  #geom_text(x=2,y=0.22,aes(label=paste0('n = ',total.catch)),size=3) +
  theme(strip.background = element_blank(),strip.text=element_blank()) + 
  xlab('Age') + ylab('Proportion') + 
  theme(axis.text.y = element_blank())

comm_length_dist <-
  fit$catchdist.fleets %>% 
  dplyr::filter(name %in% c('ldist.bmt','ldist.lln', 'ldist.comm')) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(
    #age=as.numeric(gsub('age','',age)), 
    length=floor(as.numeric(gsub('len','',length))/4)*4,
    comm = ifelse(name=='aldist.bmt', 'Bottom trawl', 'Longline'),
    num = observed*total.catch) %>% 
  #  dplyr::group_by(year,age,step,total.catch) %>% 
  dplyr::group_by(year,length) %>% 
  dplyr::summarise(sumnum=sum(num,na.rm=TRUE)) %>% #,
  #                 p=sum(predicted)) %>% 
  left_join(fit$catchdist.fleets %>% 
              dplyr::filter(name %in% c('ldist.bmt','ldist.lln', 'ldist.comm')) %>%
              dplyr::mutate(num = observed*total.catch) %>% 
              dplyr::group_by(year) %>% 
              dplyr::summarise(annual.total.catch=sum(num, na.rm = T))
  ) %>% 
  dplyr::mutate(o=ifelse(sumnum==0,NA,sumnum)/annual.total.catch) %>% 
  ungroup() %>% 
  ggplot(aes(length,o)) + geom_point(col='black') + 
  geom_segment(aes(xend=length,yend=0),col='black') + 
  # include below for ling
  #  geom_line(aes(age, o, data = comm_age_dist_dat %>% filter(comm=='Bottom trawls')) +
  #facet_wrap(~year+step) + 
  facet_wrap(~year) + 
  theme_bw() + #geom_line(aes(y=p)) + 
  geom_label(x=90,y=0.18, aes(label=year),size=4) +
  #geom_text(x=2,y=0.22,aes(label=paste0('n = ',total.catch)),size=3) +
  theme(strip.background = element_blank(),strip.text=element_blank()) + 
  xlab('Length') + ylab('Proportion') + 
  theme(axis.text.y = element_blank())

if(marteg==8){ # I believe these were not used in the end 
#number of samples 
 fit$catchdist.fleets %>% 
   filter(!(name %in% c('aldist.igfs','ldist.igfs','matp.igfs'))) %>% 
   group_by(name,year) %>% 
   dplyr::summarise(n=sum(number.x,na.rm=TRUE))

 fit$catchdist.fleets %>% 
   filter(!(name %in% c('aldist.comm','ldist.comm'))) %>% 
   group_by(name,year) %>% 
   dplyr::summarise(n=sum(number.x,na.rm=TRUE))
}

if(marteg %in% c(6,8)){ #Gadget assessments
  
  ####some diagnostic plots from presentation, prettier for report:
  comm_length_dist_settings<-NULL;
  comm_length_dist_settings[[6]]<-list(c('ldist.lln', 'green'), c(1999, 122, 0.18, 2))
  comm_length_dist_settings[[8]]<-list(c('ldist.comm', 'grey'), c(1999, 82, 0.18, 2))
  
  comm_length_dist_diag <-
    fit$catchdist.fleets %>% 
    filter(name==comm_length_dist_settings[[marteg]][[1]][1], year>comm_length_dist_settings[[marteg]][[2]][1]) %>% 
    #unite(year,year,step,sep = ', q') %>%
    ungroup() %>% 
    ggplot(aes(lower,observed)) + geom_point(col=comm_length_dist_settings[[marteg]][[1]][2], size=0.5) + 
    geom_segment(aes(xend=lower,yend=0),col=comm_length_dist_settings[[marteg]][[1]][2]) + 
    facet_wrap(~year+step, drop = F, ncol = 6) + theme_bw() + 
    geom_line(aes(y=predicted)) + #xlim(c(25,55)) + 
    geom_label(data=expand.grid(year=(comm_length_dist_settings[[marteg]][[2]][1]+1):thisyear,step=1:4) %>%
                 dplyr::mutate(label=paste(year,step,sep=', q')),
               aes(label=label),x=comm_length_dist_settings[[marteg]][[2]][2],
               y=comm_length_dist_settings[[marteg]][[2]][3],
               size=comm_length_dist_settings[[marteg]][[2]][4]) + 
    #geom_label(x=90,y=0.15,aes(label=sprintf('%s, q%s',year,step)), size=1.5) + 
    #geom_text(x=140,y=0.07,angle=90,aes(label=paste0('n = ',round(total.catch)))) +
    theme(strip.background = element_blank(),strip.text=element_blank()) + 
    xlab('Length') + ylab('Proportion')
  
  
#begin assessment results

assess_result_settings<-NULL
assess_result_settings[[6]]<-c(15, 0.2,0.8, 0.56,0.85, 0.2,0.8, 1)
assess_result_settings[[8]]<-c(10, 0.2,0.25, 0.45,0.15, 0.5,0.85, 0.5)

bio.plot <-
  finres %>% #was bootres
  rename(yea=year) %>%
  select(-matches('cv|r|ssb')) %>%
  rename(year=yea) %>%
  gather(col,value,-year) %>%
  dplyr::mutate(stat=gsub('(^u+|^l+|^m|^f).*',("\\1"),col),
                col = str_sub(col,-2),
                value=value/1e6) %>%
  filter((col %in% c('hb','tb'))) %>%
  spread(stat,value) %>%
  dplyr::mutate(col=ifelse(col=='hb','Ref. biomass','Total biomass')) %>%
  ggplot(aes(year,f,group=col, color = col)) + #changed from m
  #geom_ribbon(aes(ymax=u,ymin=l,fill=col),alpha=0.5) +
  #geom_ribbon(aes(ymax=uu,ymin=ll,fill=col),alpha=0.5) +
  geom_line() +
  #geom_line(aes(y=f),col='red')+
  theme_light() +
  expand_limits(y=0) +
  scale_color_manual(name=NULL,values=c('lightblue','gold')) +
  labs(y='Biomass (in kt)',x='Year') +
  theme(legend.position = c(assess_result_settings[[marteg]][2],assess_result_settings[[marteg]][3]))

ssb.plot <-
  finres %>%
  select(c(year,matches('ssb'))) %>%
  gather(col,value,-year) %>%
  dplyr::mutate(stat=gsub('(^u+|^l+|^m|^f).*',("\\1"),col),
                col = "ssb",
                value=value/1e6) %>%
  spread(stat,value) %>%
  #filter(year > 1990) %>% 
  ggplot(aes(year,f,group=col)) +
  #geom_ribbon(aes(ymax=u,ymin=l,fill=col),alpha=0.5) +
  #geom_ribbon(aes(ymax=uu,ymin=ll,fill=col),alpha=0.5) +
  geom_line() +
  #geom_line(aes(y=f),col='red')+
  theme_light() +
  scale_fill_manual(name=NULL,values=c('gold','lightblue')) +
  expand_limits(y=0) +
  labs(y='SSB (in kt)',x='Year') +
  theme(legend.position = 'none')


F.plot <-
  finres %>%
  select(year,matches('[a-z]F|[a-z]hr')) %>%
  select(-matches('cv')) %>%
  gather(col,value,-year) %>%
  dplyr::mutate(stat=gsub('(^u+|^l+|^m|^f).*',("\\1"),col),
                col = ifelse(grepl('hr',col),'Harvest rate','Fishing mortality')) %>%
  spread(stat,value) %>%
  filter(year < thisyear) %>% 
  ggplot(aes(year,f,group=col, color = col)) +
  #geom_ribbon(aes(ymax=u,ymin=l,fill=col),alpha=0.5) +
  #geom_ribbon(aes(ymax=uu,ymin=ll,fill=col),alpha=0.5) +
  geom_line() +
  #geom_line(aes(y=f),col='red')+
  theme_light() +
  #expand_limits(y=0) +
  ylim(0,assess_result_settings[[marteg]][8]) +
  scale_color_manual(name=NULL,values=c('gold','lightblue','lightblue')) +
  labs(y='Fishing mortality & Harvest rate',x="Year")+
  theme(legend.position = c(assess_result_settings[[marteg]][4],assess_result_settings[[marteg]][5])) #(0.25,0.15)

rec.plot <-
  finres %>%
  select(year,matches('[a-z]r')) %>%
  select(-matches('hr')) %>%
  gather(col,value,-year) %>%
  dplyr::mutate(stat=gsub('(^u+|^l+|^m|^f).*',("\\1"),col),
                col = str_sub(col,-1),
                value = value/1e6) %>%
  filter(stat!='c') %>%
  spread(stat,value) %>%
  ggplot(aes(year,f,group=col)) +
  #geom_ribbon(aes(ymax=u,ymin=l),alpha=0.5,fill='gold') +
  #geom_ribbon(aes(ymax=uu,ymin=ll),alpha=0.5,fill='gold') +
  geom_line() +
  #geom_line(aes(y=f),col='red')+
  theme_light() +
  expand_limits(y=0) +
  labs(y='Recruitment (in millions)',x="Year")

catch.plot <-
  fit$res.by.year %>%
  dplyr::mutate(stock=ifelse(grepl('mat',stock),'Mature','Immature')) %>%
  filter(year < thisyear) %>% 
  ggplot(aes(year,catch/1e6,fill=stock)) +
  geom_bar(stat='identity') +
  theme_light() +
  ylim(0,assess_result_settings[[marteg]][1]) +
  scale_fill_manual(name = NULL, values=c('gold','lightblue')) +
  labs(y='Landings (in kt)',x="Year") +
  theme(legend.position = c(assess_result_settings[[marteg]][6],assess_result_settings[[marteg]][7]))

# cv.plot <-
#   finres %>%
#   select(year,matches('cv')) %>%
#   gather(col,value,-year) %>%
#   dplyr::mutate(stat=str_sub(col,0,2),
#          col = str_sub(col,3,5)) %>%
#   filter(col %in% c('hb','ssb','r','F')) %>%
#   spread(stat,value) %>%
#   dplyr::mutate(col=case_when(.$col=='hb'~'Harv. biomass',
#                        .$col=='ssb'~'SSB',
#                        .$col=='r'~'Recruitment',
#                        .$col=='F'~'F')) %>%
#   ggplot(aes(year,cv,lty=col)) +
#   geom_line() +
#   scale_linetype_manual(name=NULL,values=1:4) +
#   theme_light() +
#   expand_limits(y=0) +
#   labs(y='CV',x='Year') +
#   theme(legend.position = c(0.2,0.8))
}

#########Begin exports for reports###########

###GSS section###
#First two items for GSS but don't work when within if statements

library(Logbooks)
ass.yr <- thisyear
gss.M50<-NULL
for(i in 1997:(ass.yr-1)){
  tmp<-botnv[botnv$ar==i,]
  tmp$hl<-tmp$gulllax/tmp$total
  tmp<-tmp[tmp$gulllax>0,]
  #k<-tmp[tmp$hl>=0.75,]
  #k<-tmp[tmp$hl<0.5 & tmp$hl>=0.1,]
  k<-tmp[tmp$hl<0.75 & tmp$hl>0.5,]
  j<-k[,c("gulllax","karfi","djupkarfi","graluda","langa","blalanga","total")]
  summa<-apply(j,2,sum, na.rm=T)
  Summa<-c(yr=i,summa, annad=summa[7]-sum(summa[1:6]))
  gss.M50<-rbind(gss.M50,Summa)
}
k<-gss.M50[,c("yr","karfi","djupkarfi","graluda","langa","blalanga","annad.total")]
kk<-cbind(yr=1997:(ass.yr-1),
          round((k[,2:7]/matrix(rep(apply(k[,2:7],1,sum),6), ncol=6,byrow=F)*100),1))
#write.table(kk, paste0(pathlist[[marteg]], "OtherCatch50-75.tex", sep=""), row.names=FALSE,
#            col.names=FALSE, quote=FALSE, sep='&\t',eol='\\\\\n')

write.csv(kk, paste0(pathlist[[19]], "OtherCatch50-75.csv"), row.names=FALSE)

###GSS Biomass indices ###
load('/net/hafkaldi/export/u2/reikn/Tac/2018/19-GSS/Rwork/gulllaxind.RData')
dat <- subset(gulllax.NewStr,sv %in% c('d400','total','shallow') & lengd == 10)
dat <- mutate(dat,bio.st=bio.st/1e3,xpos=ifelse(winsor,ar+0.1,ar),
              sv=ifelse(sv=='total','Total',
                        ifelse(sv=='d400','>400','<400')),
              winsor=ifelse(winsor,'Winsorised','Un-altered'))
dat.gullax <- dat
#postscript("tmp.eps", horizontal = TRUE)
#print(
pdf(paste0(pathlist[[19]],'GSS_Index.pdf'), width = 6, height = 5) 
  ggplot(dat,
       aes(xpos,bio.st,col=winsor)) +
  geom_line() + facet_wrap(~sv,ncol=2,scale='free_y') + theme_bw() +
  theme(legend.position=c(0.6,0.2),legend.title=element_blank()) + geom_point(size=2) +
  geom_errorbar(aes(ymin=bio.st*(1-cv.bio.st),ymax=bio.st*(1+cv.bio.st))) +
  ylab("Index (in '000)") + xlab('Year') +
  geom_line(data=subset(dat,ar %in% c(2010,2012) & winsor == 'Winsorised'),col='white',lty=2) +
  geom_line(data=subset(dat,ar %in% c(2010,2012) & winsor == 'Un-altered'),col='white',lty=2) %>% 
    print(.)

#)
dev.off()

#system(paste("cp tmp.eps ", path,"SSmeltIndex.eps",sep=""))
#system(paste("convert -verbose -density 300 tmp.eps -quality 92 -rotate 90  tmp.jpg", sep=""))
#system(paste("cp tmp.jpg ", path,"SSmeltIndex.jpg",sep=""))

#### End GSS section ####

#Write tables that go into report

  write.csv(landings, paste0(pathlist[[marteg]], 'landings_full.csv'), row.names = F)
  write.csv(landings_by_country, paste0(pathlist[[marteg]], 'landings_by_country.csv'), row.names = F)
  write.csv(nb_lnd_by_yr, paste0(pathlist[[marteg]], 'nb_lnd_by_yr.csv'), row.names = F)
  write.csv(lnd_by_gear, paste0(pathlist[[marteg]], 'lnd_by_gear.csv'), row.names = F)
  write.csv(sampling_lengths, paste0(pathlist[[marteg]], 'sampling_lengths.csv'), row.names = F)
  write.csv(sampling_ages, paste0(pathlist[[marteg]], 'sampling_ages.csv'), row.names = F)

#These are for updating the gogn_f_myndir and gogn_f_toflur files in TAC/Astand folders

if(marteg==8){
  
  t_progn<-
    read.csv2("../../../tusk_progn.csv") %>% 
    select(-X)
    write.csv(t_progn, paste0(pathlist[[marteg]],"tusk_progn_conv.csv"), row.names = F)
  
    read.csv2("../../../tusk_progn_by_adyear.csv") %>% 
      select(-X) %>% 
      write.csv(., paste0(pathlist[[marteg]],"tusk_progn_by_adyear_conv.csv"), row.names = F)
    
  t_stofnmat <-
    t_progn %>% 
    filter(trial==1) %>% 
    select(Year = year, rec, catch, ssb, hr, refbio) %>% 
    left_join(read.delim(paste0("/net/hafkaldi/export/u2/reikn/Tac/Astand",substring(lastyear,3),"/gogn_f_myndir/08-Keila/Keila_Stofnmat.txt"))
    ) %>%
    filter(Year <= thisyear) %>% 
    select(Year, rec, ssb, Catch, catch, hr, refbio) %>% 
    mutate(Year, Rec = round(rec/1000), SSB = round(ssb/1000), Catch = ifelse(is.na(Catch), round(catch/1000), Catch), HR = round(hr,3), B40 = round(refbio/1000)) %>% 
    select(Year, Rec, SSB, Catch, HR, B40)    
  
  t_stofnmat$Catch[t_stofnmat$Year==thisyear] <- NA
  t_stofnmat$HR[t_stofnmat$Year==thisyear] <- NA
  
  t_stofnmat %>% 
    select(Year, Rec, SSB, B40, Catch, HR) %>% 
    rename('B40+' = B40) %>% 
    write.csv(paste0(pathlist[[marteg]],"summary.csv"), row.names = F)
  
  t_stofnmat$Catch[t_stofnmat$Year==thisyear] <- ''
  t_stofnmat$HR[t_stofnmat$Year==thisyear] <- ''
  
  t_stofnmat %>% 
    write_tsv(paste0(pathlist[[marteg]],"Keila_Stofnmat.txt"))    
  
  t_retro <-
    read.csv(paste0("/net/hafkaldi/export/u2/reikn/Tac/Astand",substring(lastyear,3),"/gogn_f_myndir/08-Keila/KeilaRetro.csv")) %>% 
    full_join(read.delim(paste0("/net/hafkaldi/export/u2/reikn/Tac/Astand",substring(lastyear,3),"/gogn_f_myndir/08-Keila/Keila_Stofnmat.txt")) %>% 
                mutate(year = Year, biomass = B40./1000, harv = B40./1000, ssb = SSB/1000, recr = Rec/1000, catch = Catch/1000, Fbar = HR, a.yr = lastyear) %>% 
                select(year, biomass, harv, ssb, recr, catch, Fbar, a.yr)
    ) %>% 
    arrange(desc(a.yr), year) %>% 
    filter(a.yr != min(a.yr))
  
  t_retro$catch[t_retro$a.yr==lastyear & t_retro$year==lastyear] <-
    t_progn$catch[t_progn$trial==1 & t_progn$year==lastyear]/1000000

  if(thisyear==2018){t_retro$Fbar <-ifelse(t_retro$Fbar > 1, t_retro$Fbar/1000, t_retro$Fbar)}
  
  t_retro %>% 
    write.csv(paste0(pathlist[[marteg]],"KeilaRetro.csv"), row.names = F)    
  
  t_land <-
    read.delim(paste0("/net/hafkaldi/export/u2/reikn/Tac/Astand",substring(lastyear,3),"/gogn_f_myndir/08-Keila/keiluafli.txt")) %>% 
    bind_rows(
      landings %>% 
        filter(year==lastyear, section == '5a') %>%
        mutate(foreign = ifelse(country=='Iceland', 'isl', 'oth')) %>% 
        group_by(year, foreign) %>% 
        summarise(c = round(sum(c, na.rm = T))/1000) %>% 
        bind_rows(landings %>% 
                    filter(year==lastyear, section == '5a') %>%
                    group_by(year) %>% 
                    summarise(c = round(sum(c, na.rm = T))/1000) %>% 
                    mutate(foreign = 'tot')
        ) %>% 
        spread(key = foreign, value = c) %>% 
        rename(Year = year)
    )  
  
  t_land %>% 
    write_tsv(paste0(pathlist[[marteg]],"keiluafli.txt")) 
  
  read.csv(paste0("/net/hafkaldi/export/u2/reikn/Tac/Astand",substring(lastyear,3),"/gogn_f_toflur/tusk/landings.csv")) %>% 
    bind_rows(t_land %>% 
      mutate(Iceland = isl*1000, Others = oth*1000, Total = tot*1000) %>% 
      filter(Year==lastyear) %>% 
      select(Year, Iceland, Others, Total)) %>% 
    write.csv(paste0(pathlist[[marteg]],"landings.csv"), row.names = F)

  #for putting into Veiðar last Hafro advice sheet table:

  lods_oslaegt(mar) %>% 
    filter(fteg == marteg,veidisvaedi == 'I',ar==lastyear) %>% 
    left_join(tbl(mar,'gear_mapping')) %>% 
    mutate(gear = ifelse(nvl(gear,' ') %in% imp_gears,gear, 'Other')) %>% 
    group_by(ar,gear) %>% 
    summarise(c = sum(magn_oslaegt)/1e3) %>% 
    collect(n=Inf) %>% 
    mutate(gear = forcats::fct_recode(gear,
                                      Longlines='LLN',
                                      `Bottom trawl`='BMT',
                                      `Danish seine`='DSE',
                                      `Gill nets`='GIL'), 
           total =  lods_oslaegt(mar) %>% 
             filter(fteg == marteg,veidisvaedi == 'I',ar==lastyear) %>% 
             left_join(tbl(mar,'gear_mapping')) %>% 
             mutate(gear = ifelse(nvl(gear,' ') %in% imp_gears,gear, 'Other')) %>% 
             group_by(ar) %>% 
             summarise(total = sum(magn_oslaegt)/1e3) %>% 
             collect(n=Inf) %>% 
             select(total) %>% 
             unlist,
           percentage = c/total)  %>% 
    write.csv(paste0(pathlist[[marteg]],"gear_percentages.csv"), row.names = F) 
  
fishingyear<-paste0(lastyear-1,thisyear-1)
lods_oslaegt(mar) %>% 
    filter(fteg==marteg, timabil==fishingyear, veidisvaedi=='I') %>% 
    left_join(lesa_skipaskra(mar)) %>% 
    mutate(country = ifelse(nvl(flokkur,0) != -4,'Iceland',einkst)) %>% 
    mutate(country = ifelse(country == 'Iceland',country,'Other')) %>% 
    group_by(timabil,country) %>% 
    summarise(sum = sum(magn_oslaegt, na.rm=T)) %>% 
    collect(n=Inf) %>% 
    write.csv(paste0(pathlist[[marteg]],"fishingyearlandings.csv"), row.names = F)

}  

if(marteg==6){
  l_progn<-
    read.csv2("../../../ling_progn.csv") %>% 
    select(-X)
    write.csv(l_progn, paste0(pathlist[[marteg]],"ling_progn_conv.csv"), row.names = F)
    
  read.csv2("../../../ling_progn_by_adyear.csv") %>% 
      select(-X) %>% 
      write.csv(., paste0(pathlist[[marteg]],"/ling_progn_by_adyear_conv.csv"), row.names = F)
    
  l_stofnmat <-
    l_progn %>% 
    filter(trial==1) %>% 
    select(Year = year, rec, catch, ssb, hr, refbio) %>% 
    left_join(read.delim(paste0("/net/hafkaldi/export/u2/reikn/Tac/Astand",substring(lastyear,3),"/gogn_f_myndir/06-Langa/langaStofnmat.txt"))
      ) %>%
    filter(Year <= thisyear) %>%
    select(Year, rec, ssb, Catch, catch, hr, refbio) %>% 
    mutate(Year, Rec = round(rec/1000), SSB = round(ssb/1000), Catch = ifelse(is.na(Catch), round(catch/1000), Catch), HR = round(hr,3), B75 = round(refbio/1000)) %>% 
    select(Year, Rec, SSB, Catch, HR, B75)    
    
  l_stofnmat$Catch[l_stofnmat$Year==thisyear] <- NA
  l_stofnmat$HR[l_stofnmat$Year==thisyear] <- NA

  l_stofnmat %>% 
    select(Year, Rec, SSB, B75, Catch, HR) %>% 
    rename('B75+' = B75) %>% 
    write.csv(paste0(pathlist[[marteg]],"summary.csv"), row.names = F)

  l_stofnmat$Catch[l_stofnmat$Year==thisyear] <- ''
  l_stofnmat$HR[l_stofnmat$Year==thisyear] <- ''
  
  l_stofnmat %>%
    write_tsv(paste0(pathlist[[marteg]],"langaStofnmat.txt"))

  
  l_retro <-
    read.csv(paste0("/net/hafkaldi/export/u2/reikn/Tac/Astand",substring(lastyear,3),"/gogn_f_myndir/06-Langa/LangaRetro.csv")) %>% 
    full_join(read.delim(paste0("/net/hafkaldi/export/u2/reikn/Tac/Astand",substring(lastyear,3),"/gogn_f_myndir/06-Langa/langaStofnmat.txt")) %>% 
                mutate(year = Year, biomass = B75/1000, harvBio = B75/1000, SSB = SSB/1000, Fbar = HR, Catch = Catch/1000,  Rec = Rec/1000, a.yr = lastyear) %>% 
                select(year, biomass, harvBio, SSB, Fbar, Catch, Rec, a.yr)
    ) %>% 
    arrange(desc(a.yr), year) %>% 
    filter(a.yr != min(a.yr))
  
  l_retro$Catch[l_retro$a.yr==lastyear & l_retro$year==lastyear] <-
    l_progn$catch[l_progn$trial==1 & l_progn$year==lastyear]/1000000
  
  l_retro %>% 
    write.csv(paste0(pathlist[[marteg]],"LangaRetro.csv"), row.names = F)    
  
  
  l_land <-
    read.delim(paste0("/net/hafkaldi/export/u2/reikn/Tac/Astand",substring(lastyear,3),"/gogn_f_myndir/06-Langa/LangaAfli.txt")) %>% 
    full_join(nb_land_gear_country %>% filter(year==lastyear) %>% round(.))
  

  l_land %>% 
    write_tsv(paste0(pathlist[[marteg]],"LangaAfli.txt"))    

  l_land %>% 
    rename(Year = year, 'Long line' = lina, 'Gill net' = net, 'Bottom trawl' = botnv, 'Other gear' = annad, 'Other nations' = erl) %>% 
    write.csv(paste0(pathlist[[marteg]],"landings.csv"), row.names = F)    
 
  #for putting into Veiðar last Hafro advice sheet table:

  
  lods_oslaegt(mar) %>% 
    filter(fteg == marteg,veidisvaedi == 'I',ar==lastyear) %>% 
    left_join(tbl(mar,'gear_mapping')) %>% 
    mutate(gear = ifelse(nvl(gear,' ') %in% imp_gears,gear, 'Other')) %>% 
    group_by(ar,gear) %>% 
    summarise(c = sum(magn_oslaegt)/1e3) %>% 
    collect(n=Inf) %>% 
    mutate(gear = forcats::fct_recode(gear,
                                      Longlines='LLN',
                                      `Bottom trawl`='BMT',
                                      `Danish seine`='DSE',
                                      `Gill nets`='GIL'), 
           total =  lods_oslaegt(mar) %>% 
             filter(fteg == marteg,veidisvaedi == 'I',ar==lastyear) %>% 
             left_join(tbl(mar,'gear_mapping')) %>% 
             mutate(gear = ifelse(nvl(gear,' ') %in% imp_gears,gear, 'Other')) %>% 
             group_by(ar) %>% 
             summarise(total = sum(magn_oslaegt)/1e3) %>% 
             collect(n=Inf) %>% 
             select(total) %>% 
             unlist,
           percentage = c/total)  %>% 
    write.csv(paste0(pathlist[[marteg]],"gear_percentages.csv"), row.names = F)  
  
  
  
fishingyear<-paste0(lastyear-1,thisyear-1)
lods_oslaegt(mar) %>% 
    filter(fteg==marteg, timabil==fishingyear, veidisvaedi=='I') %>% 
    left_join(lesa_skipaskra(mar)) %>% 
    mutate(country = ifelse(nvl(flokkur,0) != -4,'Iceland',einkst)) %>% 
    mutate(country = ifelse(country == 'Iceland',country,'Other')) %>% 
    group_by(timabil,country) %>% 
    summarise(sum = sum(magn_oslaegt, na.rm=T)) %>% 
    collect(n=Inf) %>% 
    write.csv(paste0(pathlist[[marteg]],"fishingyearlandings.csv"), row.names = F)
  
}
  
  closeAllConnections()
  
  
if(marteg %in% c(6,8)){ #Gadget assessments

  pdf(paste0(pathlist[[marteg]], 'assess_results.pdf'), width = 12, height = 6)

    gridExtra::grid.arrange(bio.plot,
                        ssb.plot + 
                          geom_hline(data=data_frame(y=bloss),aes(yintercept=y),col='black',lty=2),
                        F.plot +   
                          geom_hline(data=data_frame(y=c(Hmp_low,Hmp_high)),aes(yintercept=y),col='black',lty=2)+
                          geom_hline(data=data_frame(y=Hmp),aes(yintercept=y),col='black'),
                        rec.plot,
                        catch.plot,ncol=3)

  dev.off()

  pdf(paste0(pathlist[[marteg]], 'comm_length_dist_diag.pdf'), width = 6, height = 8)
      comm_length_dist_diag
  dev.off()
  
}
  
  pdf(paste0(pathlist[[marteg]], 'landings_plot.pdf'), width = 6, height = 6)
    print(landings.plot)
  dev.off()
  
  pdf(paste0(pathlist[[marteg]], 'lnd_by_gear.pdf'), width = 8, height = 6)
  print(lnd_by_gear_top +
    lnd_by_gear_bottom +
    plot_layout(ncol = 1))
  dev.off()

  pdf(paste0(pathlist[[marteg]], 'depth_plot.pdf'), width = 8, height = 6)
  print(depth_plot +
    depth_fill_plot +
    plot_layout(ncol = 1))
  dev.off()

  pdf(paste0(pathlist[[marteg]], 'catch_dist_plot.pdf'), width = 6, height = 6)
    print(catch_dist_plot)
  dev.off()

  pdf(paste0(pathlist[[marteg]], 'sampling_pos.pdf'), width = 6, height = 6)
    print(sampling_pos_plot)
  dev.off()

  pdf(paste0(pathlist[[marteg]], 'transfer_plot.pdf'), width = 8, height = 6)
    print(transfer_plot)
  dev.off()

  pdf(paste0(pathlist[[marteg]], 'four_plot.pdf'), width = 8, height = 6)
    print(four_plot)
  dev.off()

catch_by_area_pos<-NULL; 
catch_by_area_pos[[6]]<-c(0.4, 0.32, 0.23, 0.77);
catch_by_area_pos[[8]]<-c(0.4, 0.24, 0.2, 0.88);
pdf(paste0(pathlist[[marteg]], 'catch_by_area.pdf'), width = 8, height = 6)
  catch_by_area
  vp <- grid::viewport(width = catch_by_area_pos[[marteg]][1], height = catch_by_area_pos[[marteg]][2], x = catch_by_area_pos[[marteg]][3], y = catch_by_area_pos[[marteg]][4])
  print(region.plot,vp = vp)
dev.off()

pdf(paste0(pathlist[[marteg]], 'survey_age_dist.pdf'), width = 8, height =6)
  print(survey_age_dist)
dev.off()

pdf(paste0(pathlist[[marteg]], 'survey_length_dist.pdf'), width = 8, height =6)
  print(survey_length_dist)
dev.off()

pdf(paste0(pathlist[[marteg]], 'comm_age_dist.pdf'), width = 8, height =6)
  print(comm_age_dist)
dev.off()

pdf(paste0(pathlist[[marteg]], 'comm_length_dist.pdf'), width = 8, height =6)
  print(comm_length_dist)
dev.off()

if(marteg==8){
pdf(paste0(pathlist[[marteg]], 'landings_plot.tusk.pdf'), width = 8, height = 6)
  print(landings.plot.tusk)
dev.off()
}

