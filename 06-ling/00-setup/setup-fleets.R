## Collect catches by fleet:
lln.landings <- mfdb_sample_totalweight(mdb, NULL, c(list(
  gear=c('HLN','LLN'),
  sampling_type = 'LND',
  species = defaults$species),
  defaults))


bmt.landings <- mfdb_sample_totalweight(mdb, NULL, c(list(
  gear=c('BMT','NPT','DSE','PSE','PGT','SHT'),
  sampling_type = 'LND',
  species = defaults$species),
  defaults))


gil.landings <- mfdb_sample_totalweight(mdb, NULL, c(list(
  gear='GIL',
  sampling_type = 'LND',
  species = defaults$species),
  defaults))

foreign.landings <-
  mfdb_sample_totalweight(mdb, NULL,
                          c(list(
                            sampling_type = 'FLND',
                            data_source = c('lods.foreign.landings','statlant.foreign.landings'),
                            species = defaults$species),
                            defaults))

igfs.landings <- 
  structure(data.frame(year=defaults$year,step=2,area=1,number=1),
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
                name = 'lln',
                suitability = 
                  paste0('\n',
                         paste(c('lingimm','lingmat'),
                               'function','exponentiall50',
                               '#ling.lln.alpha','#ling.lln.l50',
                               collapse='\n')),
                data = lln.landings[[1]]) %>% 
  gadget_update('totalfleet',
                name = 'bmt',
                suitability = paste0('\n',
                                     paste(c('lingimm','lingmat'),
                                           'function','exponentiall50',
                                           '#ling.bmt.alpha','#ling.bmt.l50',
                                           collapse='\n')),
                data = bmt.landings[[1]]) %>% 
  gadget_update('totalfleet',
                name = 'gil',
                suitability = paste0('\n',
                                     paste(c('lingimm','lingmat'),
                                           'function','exponentiall50',
                                           '#ling.gil.alpha','#ling.gil.l50',
                                           collapse='\n')),
                data = gil.landings[[1]]) %>% 
  gadget_update('totalfleet',
                name = 'foreign',
                suitability = 
                  paste0('\n',
                         paste(c('lingimm','lingmat'),
                               'function','exponentiall50',
                               '#ling.lln.alpha','#ling.lln.l50',
                               #'function','andersenfleet',
                               #'#ling.lln.p0',to.gadget.formulae(quote(log(180/ling.lln.lmode))),'#ling.lln.p2',
                               #'#ling.lln.p3','#ling.lln.p4','180',
                               collapse='\n')),
                data = foreign.landings[[1]]) %>% 
  write.gadget.file(gd$dir)