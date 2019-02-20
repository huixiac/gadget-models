## Collect catches by fleet:
comm.landings <- mfdb_sample_totalweight(mdb, NULL, c(list(
  #gear=c('HLN','LLN'),
  sampling_type = 'LND'),defaults)) 

foreign.landings <-
  mfdb_sample_totalweight(mdb, NULL,
                          c(list(
                            sampling_type = 'FLND',
                            data_source = c('lods.foreign.landings','statlant.foreign.landings'),
                            species = defaults$species),
                            defaults))

igfs.landings <- 
  structure(data.frame(year=defaults$year,step=1,area=1,number=1),
            area_group = mfdb_group(`1` = 1))


## write to file
gadgetfleet('Modelfiles/fleet',gd$dir,missingOkay = TRUE) %>% 
  gadget_update('totalfleet',
                name = 'igfs',
                suitability = paste0('\n',
                                     paste(c('lingimm','lingmat'),
                                           'function','exponentiall50',
                                           '#ling.igfs.alpha','#ling.igfs.l50',
                                           collapse='\n')),
                data = igfs.landings) %>%
  gadget_update('totalfleet',
                name = 'comm',
                suitability = paste0('\n',
                                     paste(c('lingimm','lingmat'),
                                           'function','exponentiall50',
                                           '#ling.comm.alpha','#ling.comm.l50',
                                           collapse='\n')),
                data = comm.landings[[1]]) %>% 
  gadget_update('totalfleet',
                name = 'foreign',
                suitability = paste0('\n',
                                     paste(c('lingimm','lingmat'),
                                           'function','exponentiall50',
                                           '#ling.comm.alpha','#ling.comm.l50',
                                           collapse='\n')),
                data = foreign.landings[[1]]) %>% 
  write.gadget.file(gd$dir)








ldist.comm <- 
  mfdb_sample_count(mdb, c('age', 'length'), 
                    c(list(
                      sampling_type = 'SEA',
                      #    gear = c('LLN','HLN'),
                      length = mfdb_interval("len", 
                                             c(0,seq(minlength+dl, maxlength, by = dl)),
                                             open_ended = TRUE)),
                      defaults))

for(i in seq_along(ldist.comm)){
  attributes(ldist.comm[[i]])$age$all <- minage:maxage
  attr(attributes(ldist.comm[[i]])$length$len0,'min') <- minlength
}

aldist.comm <-
  mfdb_sample_count(mdb, c('age', 'length'),
                    c(list(sampling_type = 'SEA',
                           #                             gear = c('LLN','HLN'),
                           age = mfdb_step_interval('age',by=1,from=minage,to=maxage+1,open_ended = TRUE),
                           length = mfdb_interval("len", c(0,seq(minlength+dl, maxlength, by = dl)),
                                                  open_ended = TRUE)),
                      defaults))
for(i in seq_along(aldist.comm)){
  attr(attributes(aldist.comm[[i]])$length$len0,'min') <- minlength
}

