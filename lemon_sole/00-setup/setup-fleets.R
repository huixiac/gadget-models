## Collect catches by fleet:
#bli.landings <- mfdb_sample_totalweight(mdb, NULL, c(list(
  #gear=c('HLN','LLN'),
  #sampling_type = 'LND',
  #species = defaults$species),
  #defaults))

dse.landings <- mfdb_sample_totalweight(mdb, NULL, c(list(
  gear=c('DSE'),
  sampling_type = 'LND',
  species = defaults$species),
  defaults))

bmt.landings <- mfdb_sample_totalweight(mdb, NULL, c(list(
  gear=c('BMT','NPT','PSE','PGT','SHT','GIL','LLN','HLN'),
  sampling_type = 'LND',
  species = defaults$species),
  defaults))

# comm.landings <- mfdb_sample_totalweight(mdb, NULL, c(list(
#   gear=c('DSE', 'BMT','NPT','PSE','PGT','SHT','GIL','LLN','HLN'),
#   sampling_type = 'LND',
#   species = defaults$species),
#   defaults))

# gil.landings <- mfdb_sample_totalweight(mdb, NULL, c(list(
#   gear='GIL',
#   sampling_type = 'LND',
#   species = defaults$species),
#   defaults))

#ASK GUdjon
# foreign.landings <-
#   mfdb_sample_totalweight(mdb, NULL,
#                           c(list(
#                             sampling_type = 'FLND',
#                             data_source = c('lods.foreign.landings','statlant.foreign.landings'),
#                             species = defaults$species),
#                             defaults))

igfs.landings <- 
  structure(data.frame(year=defaults$year,step=2,area=1,number=1),
            area_group = mfdb_group(`1` = 1))

## write to file
gadgetfleet('Modelfiles/fleet',gd,missingOkay = TRUE) %>% 
  gadget_update('totalfleet',
                name = 'igfs',
                suitability = paste0('\n',
                                     paste(c('lemimm','lemmat'),
                                           'function','exponentiall50',
                                           '#lemon.igfs.alpha','#lemon.igfs.l50',
                                           collapse='\n')),
                data = igfs.landings) %>%
   # gadget_update('totalfleet',
   #               name = 'comm',
   #               suitability = 
   #                 paste0('\n',
   #                        paste(c('lemimm','lemmat'),
   #                              'function','exponentiall50',
   #                              '#lemon.comm.alpha','#lemon.comm.l50',
   #                              collapse='\n')),
   #               data = comm.landings[[1]]) %>% 
   gadget_update('totalfleet',
                 name = 'dse',
                 suitability = 
                   paste0('\n',
                          paste(c('lemimm','lemmat'),
                                'function','exponentiall50',
                                '#lemon.dse.alpha','#lemon.dse.l50',
                                collapse='\n')),
                 data = dse.landings[[1]]) %>% 
   gadget_update('totalfleet',
                 name = 'bmt',
                 suitability = paste0('\n',
                                      paste(c('lemimm','lemmat'),
                                            'function','exponentiall50',
                                            '#lemon.bmt.alpha','#lemon.bmt.l50',
                                            collapse='\n')),
                data = bmt.landings[[1]]) %>% 
  # gadget_update('totalfleet',
  #               name = 'gil',
  #               suitability = paste0('\n',
  #                                    paste(c('lemimm','lemmat'),
  #                                          'function','exponentiall50',
  #                                          '#lemon.gil.alpha','#lemon.gil.l50',
  #                                          collapse='\n')),
  #               data = gil.landings[[1]]) %>% 
  #' gadget_update('totalfleet',
  #'               name = 'foreign',
  #'               suitability = 
  #'                 paste0('\n',
  #'                        paste(c('lemimm','lemmat'),
  #'                              'function','exponentiall50',
  #'                              '#lemon.bmt.alpha','#lemon.bmt.l50',
  #'                              #'function','andersenfleet',
  #'                              #'#ling.lln.p0',to.gadget.formulae(quote(log(180/ling.lln.lmode))),'#ling.lln.p2',
  #'                              #'#ling.lln.p3','#ling.lln.p4','180',
  #'                              collapse='\n')),
  #'               data = foreign.landings[[1]]) %>% 
  write.gadget.file(gd)


