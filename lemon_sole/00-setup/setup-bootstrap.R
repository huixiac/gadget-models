

#check if enough data for spatial bootstrap
# mfdb_dplyr_sample(mdb = mdb) %>% 
#   filter(species=='GSS') %>% 
#   collect(n=Inf) %>% 
#   left_join(tbl(mar, 'reitmapping') %>% 
#               select(GRIDCELL, DIVISION) %>% 
#               rename(areacell=GRIDCELL) %>% 
#               collect(n=Inf)) %>% 
#   group_by(year, DIVISION, data_source, sampling_type) %>% 
#   filter(year > 1984, data_source != 'commercial.landings', !(sampling_type=='AUT' & year==2011)) %>%  
#   count %>% 
#   spread(key = DIVISION, value = n) %>% 
#   arrange(data_source, sampling_type, year) %>% 
#   View



## setting up a bootstrap run



defaults <- 
  within(defaults,
         {area = mfdb_bootstrap_group(100,defaults$area,seed=9284)})
  
source(sprintf('%s/00-setup/setup-catchdistribution.R',base_dir))
  source(sprintf('%s/00-setup/setup-indices.R',base_dir))
save.image(file=sprintf('%s/00-setup/bootstrap-data.Rdata',base_dir))

#if(grepl('no_igfs', vers)){print('Warning - igfs data removed')}
#if(!grepl('s6', vers)){print('Warning - only 6 indices included')}

boot_setup <- function(i){
  var_dir <- gadget.variant.dir(gd, variant_dir = paste0('BS.WGTS/BS.', i))
  
  aldist.igfs[[i]]$step <- 2
  ldist.igfs[[i]]$step <- 2
  matp.igfs[[i]]$step <- 2
  
  tmp <-
    gadgetlikelihood('likelihood',gd,missingOkay = TRUE) %>% 
    ## Write a penalty component to the likelihood file
    gadget_update("penalty",
                  name = "bounds",
                  weight = "0.5",
                  data = data.frame(
                    switch = c("default"),
                    power = c(2),
                    upperW=10000,
                    lowerW=10000,
                    stringsAsFactors = FALSE)) %>%
    gadget_update("understocking",
                  name = "understocking",
                  weight = "100") %>% #
    gadget_update("catchdistribution",
                  name = "ldist.igfs",
                  weight = 1,
                  data = ldist.igfs[[i]],
                  fleetnames = c("igfs"),
                  stocknames =stock_names) %>% 
    gadget_update("catchdistribution",#         no age data from the autumn survey
                  name = "aldist.igfs",
                  weight = 1,
                  data = aldist.igfs[[i]],
                  #                 #filter(year!=1989),
                  #                 filter(year<1990),
                  fleetnames = c("igfs"),
                  stocknames =stock_names) %>% 
    gadget_update("catchdistribution",
                  name = "ldist.dse",
                  weight = 1,
                  data = ldist.dse[[i]], 
                  fleetnames = c("dse"),
                  stocknames = stock_names) %>% 
    gadget_update("catchdistribution",  #      no data...
                  name = "aldist.dse",
                  weight = 1,
                  data = aldist.dse[[i]]%>% 
                    filter(year > minyear_cutoff)%>% 
                    filter(age > minage_cutoff), 
                  fleetnames = c("dse"),
                  stocknames = stock_names) %>%
    # gadget_update("catchdistribution", #                no data...
    #               name = "aldist.gil",
    #               weight = 1,
    #               data = aldist.gil[[i]] %>% 
    #                 filter(year<1990),
    #               fleetnames = c("gil"),
    #               stocknames = stock_names) %>% 
    gadget_update("catchdistribution",
                  name = "ldist.bmt",
                  weight = 1,
                  data = ldist.bmt[[i]],
                  # !(year==1982&step==4),
                  #      !(year==1984&step==1),
                  #      !(year==1992&step==4),
                  #      !(year==1994&step==1),
                  #      !(year==1998&step==3),
                  #      !(year==1989&step==3)),
                  fleetnames = c("bmt"),
                  stocknames = stock_names) %>% 
    gadget_update("catchdistribution",
                  name = "aldist.bmt",
                  weight = 1,
                  data = aldist.bmt[[i]]%>% 
                    filter(year>minyear_cutoff)%>% 
                    filter(age>minage_cutoff), 
                  fleetnames = c("bmt"),
                  stocknames = stock_names) %>% 
    gadget_update("stockdistribution",
                  name = "matp.igfs",
                  weight = 1,
                  data = matp.igfs[[i]],
                  fleetnames = c("igfs"),
                  stocknames =stock_names) %>% 
    gadget_update("surveyindices",
                  name = "si.10-20",
                  weight = 1,
                  data = igfs.SI1[[i]],
                  #fittype = 'loglinearfit',
                  fittype = 'fixedslopeloglinearfit',
                  slope=1,
                  stocknames = stock_names) %>% 
    gadget_update("surveyindices",
                  name = "si.20-25",
                  weight = 1,
                  data = igfs.SI2a[[i]],
                  #fittype = 'loglinearfit',
                  fittype = 'fixedslopeloglinearfit',
                  slope=1,
                  stocknames = stock_names) %>% 
    gadget_update("surveyindices",
                  name = "si.25-30",
                  weight = 1,
                  data = igfs.SI2b[[i]],
                  #fittype = 'loglinearfit',
                  fittype = 'fixedslopeloglinearfit',
                  slope=1,
                  stocknames = stock_names) %>% 
    gadget_update("surveyindices",
                  name = "si.30-35",
                  weight = 1,
                  data = igfs.SI3a[[i]],
                  fittype = 'fixedslopeloglinearfit',
                  slope=1,
                  stocknames = stock_names) %>% 
    gadget_update("surveyindices",
                  name = "si.35-40",
                  weight = 1,
                  data = igfs.SI3b[[i]],
                  fittype = 'fixedslopeloglinearfit',
                  slope=1,
                  stocknames = stock_names) %>% 
    gadget_update("surveyindices",
                  name = "si.40-45",
                  weight = 1,
                  data = igfs.SI3c[[i]],
                  fittype = 'fixedslopeloglinearfit',
                  slope=1,
                  stocknames = stock_names) %>% 
    gadget_update("surveyindices",
                  name = "si.45-60",
                  weight = 1,
                  data = igfs.SI3d[[i]],
                  fittype = 'fixedslopeloglinearfit',
                  slope=1,
                  stocknames = stock_names)

 
  # if(grepl('no_igfs', vers)){
  #   tmp <- 
  #     tmp  %>% 
  #     gadget_discard(., c('si.10-25.igfs', 
  #                         'si.25-30.igfs', 
  #                         'si.30-35.igfs', 
  #                         'si.35-40.igfs', 
  #                         'si.40-45.igfs', 
  #                         'si.45-50.igfs', 
  #                         'si.50-60.igfs',
  #                         'ldist.igfs',
  #                         'matp.igfs',
  #                         'aldist.igfs'))
  #   tmp %>% 
  #     write.gadget.file(gd)
  # }
  # 
  # if(grepl('s5', vers)){
  #   tmp <-
  #     tmp  %>% 
  #     gadget_discard(., c('si.50-60.igfs',
  #                         'si.50-60.aut')) %>% 
  #     gadget_discard(., c('si.45-50.igfs',
  #                         'si.45-50.aut'))
  #   
  #   tmp %>% 
  #     write.gadget.file(gd)
  # }
  # 
  # if(grepl('s6', vers)){
  #   tmp <-
  #     tmp  %>% 
  #     gadget_discard(., c('si.50-60.igfs',
  #                         'si.50-60.aut'))
  #   tmp %>% 
  #     write.gadget.file(gd)
  # }
  
  

  attr(tmp,'file_config')$mainfile_overwrite = TRUE
  write.gadget.file(tmp,var_dir)
}

tmp2 <- 
  parallel::mclapply(seq_along(defaults$area),
           boot_setup,
           mc.cores = parallel::detectCores(logical = TRUE))
    

