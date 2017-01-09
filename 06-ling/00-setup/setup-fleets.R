## Collect catches by fleet:
comm.landings <- mfdb_sample_totalweight(mdb, NULL, c(list(
  #gear=c('HLN','LLN'),
  sampling_type = 'LND'),defaults)) 

foreign.landings <-
  mfdb_sample_totalweight(mdb, NULL,
                          c(list(
                            sampling_type = 'FLND',
                            species = defaults$species),
                            defaults))

igfs.landings <- 
  structure(data.frame(year=defaults$year,step=1,area=1,number=1),
            area_group = mfdb_group(`1` = 1))


## write to file
gadget_fleet_component('totalfleet',
                       name = 'igfs',
                       suitability = paste0('\n',
                                            paste(c('lingimm','lingmat'),
                                                  'function','exponentiall50',
                                                  '#ling.igfs.alpha','#ling.igfs.l50',
                                                  collapse='\n')),
                       data = igfs.landings) %>% 
  gadget_dir_write(gd,.)


gadget_fleet_component('totalfleet',
                       name = 'comm',
                       suitability = paste0('\n',
                                           paste(c('lingimm','lingmat'),
                                                 'function','exponentiall50',
                                                 '#ling.comm.alpha','#ling.comm.l50',
                                                 collapse='\n')),
                       data = comm.landings[[1]]) %>% 
  gadget_dir_write(gd,.)


gadget_fleet_component('totalfleet',
                       name = 'foreigh',
                       suitability = paste0('\n',
                                            paste(c('lingimm','lingmat'),
                                                  'function','exponentiall50',
                                                  '#ling.comm.alpha','#ling.comm.l50',
                                                  collapse='\n')),
                       data = foreign.landings[[1]]) %>% 
  gadget_dir_write(gd,.)
