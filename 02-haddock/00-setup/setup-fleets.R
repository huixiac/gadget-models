## Collect catches by fleet:
lln.landings_e <- mfdb_sample_totalweight(mdb, NULL, c(list(
  gear=c('HLN','LLN'),
  sampling_type = 'LND',
  species = defaults$species),
  defaults_early))

lln.landings_l <- mfdb_sample_totalweight(mdb, NULL, c(list(
  gear=c('HLN','LLN'),
  sampling_type = 'LND',
  species = defaults$species),
  defaults_late))

bmt.landings_e <- mfdb_sample_totalweight(mdb, NULL, c(list(
  gear=c('BMT'),
  sampling_type = 'LND',
  species = defaults$species),
  defaults_early))

bmt.landings_l <- mfdb_sample_totalweight(mdb, NULL, c(list(
  gear=c('BMT'),
  sampling_type = 'LND',
  species = defaults$species),
  defaults_late))

ott.landings_e <- mfdb_sample_totalweight(mdb, NULL, c(list(
  gear=c('DRD','NPT','SHT','PGT'),
  sampling_type = 'LND',
  species = defaults$species),
  defaults_early))

ott.landings_l <- mfdb_sample_totalweight(mdb, NULL, c(list(
  gear=c('DRD','NPT','SHT','PGT'),
  sampling_type = 'LND',
  species = defaults$species),
  defaults_late))

dse.landings_e <- mfdb_sample_totalweight(mdb, NULL, c(list(
  gear=c('DSE','PSE'),
  sampling_type = 'LND',
  species = defaults$species),
  defaults_early))

dse.landings_l <- mfdb_sample_totalweight(mdb, NULL, c(list(
  gear=c('DSE','PSE'),
  sampling_type = 'LND',
  species = defaults$species),
  defaults_late))

gil.landings_e <- mfdb_sample_totalweight(mdb, NULL, c(list(
  gear='GIL',
  sampling_type = 'LND',
  species = defaults$species),
  defaults_early))

gil.landings_l <- mfdb_sample_totalweight(mdb, NULL, c(list(
  gear='GIL',
  sampling_type = 'LND',
  species = defaults$species),
  defaults_late))

foreign.landings <-
  mfdb_sample_totalweight(mdb, NULL,
                          c(list(
                            sampling_type = 'FLND',
                            data_source = c('lods.foreign.landings','statlant.foreign.landings'),
                            species = defaults$species),
                            defaults))
old.landings <-
  mfdb_sample_totalweight(mdb, NULL,
                          c(list(
                            data_source = c('old.landings'),
                            species = defaults$species),
                            defaults))


igfs.landings_e <- 
  structure(data.frame(year=defaults_early$year,step=2,area=1,number=1),
            area_group = mfdb_group(`1` = 1)) #shows 1 kg being taken

igfs.landings_l <- 
  structure(data.frame(year=defaults_late$year,step=2,area=1,number=1),
            area_group = mfdb_group(`1` = 1)) #shows 1 kg being taken

aut.landings <- 
  structure(data.frame(year=defaults$year,step=5,area=1,number=1),
            area_group = mfdb_group(`1` = 1)) #shows 1 kg being taken



## write to file
gadgetfleet('Modelfiles/fleet',gd$dir,missingOkay = TRUE) %>% 
  gadget_update('totalfleet',
                name = 'aut',
                suitability = paste0('\n',
                                     paste(c('hadimm','hadmat'),
                                           #'function','exponentiall50',
                                           #'#had.aut.alpha','#had.aut.l50',
                                           'function','andersenfleet',
                                           '#had.aut.p0',to.gadget.formulae(quote(log(100/had.aut.lmode))),'#had.aut.p2',
                                           '#had.aut.p3','#had.aut.p4','100',
                                           collapse='\n')),
                data = aut.landings) %>%
  gadget_update('totalfleet',
                name = 'igfs_e',
                suitability = paste0('\n',
                                     paste(c('hadimm','hadmat'),
                                           #'function','exponentiall50',
                                           #'#had.igfs.alpha','#had.igfs.l50',
                                           'function','andersenfleet',
                                           '#had.igfs.p0',to.gadget.formulae(quote(log(100/had.igfs.lmode))),'#had.igfs.p2',
                                           '#had.igfs.p3','#had.igfs.p4','100',
                                           collapse='\n')),
                data = igfs.landings_e) %>%
  gadget_update('totalfleet',
                name = 'igfs_l',
                suitability = paste0('\n',
                                     paste(c('hadimm','hadmat'),
                                           #'function','exponentiall50',
                                           #'#had.igfs.alpha','#had.igfs.l50',
                                           'function','andersenfleet',
                                           '#had.igfs.p0',to.gadget.formulae(quote(log(100/had.igfs.lmode))),'#had.igfs.p2',
                                           '#had.igfs.p3','#had.igfs.p4','100',
                                           collapse='\n')),
                data = igfs.landings_l) %>%
  gadget_update('totalfleet',
                name = 'lln_e',
                suitability = 
                  paste0('\n',
                         paste(c('hadimm','hadmat'),
                               'function','exponentiall50',
                               'Modelfiles/timevariablealpha.lln','Modelfiles/timevariablel50.lln',
                               collapse='\n')),
                data = lln.landings_e[[1]]) %>% 
  gadget_update('totalfleet',
                name = 'lln_l',
                suitability = 
                  paste0('\n',
                         paste(c('hadimm','hadmat'),
                               'function','exponentiall50',
                               'Modelfiles/timevariablealpha.lln','Modelfiles/timevariablel50.lln',
                               collapse='\n')),
                data = lln.landings_l[[1]]) %>% 
  gadget_update('totalfleet',
                name = 'bmt_e',
                suitability = paste0('\n',
                                     paste(c('hadimm','hadmat'),
                                           #'function','exponentiall50',
                                           #'#had.bmt.alpha','#had.bmt.l50',
                                           'function','andersenfleet',
                                           'Modelfiles/timevariablep0.bmt','Modelfiles/timevariablelmode.bmt','Modelfiles/timevariablep2.bmt',
                                           'Modelfiles/timevariablep3.bmt','Modelfiles/timevariablep4.bmt','100',
                                           collapse='\n')),
                data = bmt.landings_e[[1]]) %>% 
  gadget_update('totalfleet',
                name = 'bmt_l',
                suitability = paste0('\n',
                                     paste(c('hadimm','hadmat'),
                                           #'function','exponentiall50',
                                           #'#had.bmt.alpha','#had.bmt.l50',
                                           'function','andersenfleet',
                                           'Modelfiles/timevariablep0.bmt','Modelfiles/timevariablelmode.bmt','Modelfiles/timevariablep2.bmt',
                                           'Modelfiles/timevariablep3.bmt','Modelfiles/timevariablep4.bmt','100',
                                           collapse='\n')),
                data = bmt.landings_l[[1]]) %>% 
  gadget_update('totalfleet',
                name = 'ott_e',
                suitability = paste0('\n',
                                     paste(c('hadimm','hadmat'),
                                           #'function','exponentiall50',
                                           #'#had.bmt.alpha','#had.bmt.l50',
                                           'function','andersenfleet',
                                           'Modelfiles/timevariablep0.ott','Modelfiles/timevariablelmode.ott','Modelfiles/timevariablep2.ott',
                                           'Modelfiles/timevariablep3.ott','Modelfiles/timevariablep4.ott','100',
                                           collapse='\n')),
                data = ott.landings_e[[1]]) %>% 
  gadget_update('totalfleet',
                name = 'ott_l',
                suitability = paste0('\n',
                                     paste(c('hadimm','hadmat'),
                                           #'function','exponentiall50',
                                           #'#had.bmt.alpha','#had.bmt.l50',
                                           'function','andersenfleet',
                                           'Modelfiles/timevariablep0.ott','Modelfiles/timevariablelmode.ott','Modelfiles/timevariablep2.ott',
                                           'Modelfiles/timevariablep3.ott','Modelfiles/timevariablep4.ott','100',
                                           collapse='\n')),
                data = ott.landings_l[[1]]) %>% 
  gadget_update('totalfleet',
                name = 'dse_e',
                suitability = paste0('\n',
                                     paste(c('hadimm','hadmat'),
                                           'function','exponentiall50',
                                           'Modelfiles/timevariablealpha.dse','Modelfiles/timevariablel50.dse',
                                           collapse='\n')),
                data = dse.landings_e[[1]]) %>% 
  gadget_update('totalfleet',
                name = 'dse_l',
                suitability = paste0('\n',
                                     paste(c('hadimm','hadmat'),
                                           'function','exponentiall50',
                                           'Modelfiles/timevariablealpha.dse','Modelfiles/timevariablel50.dse',
                                           collapse='\n')),
                data = dse.landings_l[[1]]) %>% 
  gadget_update('totalfleet',
                name = 'gil_e',
                suitability = paste0('\n',
                                     paste(c('hadimm','hadmat'),
                                           'function','exponentiall50',
                                           'Modelfiles/timevariablealpha.gil','Modelfiles/timevariablel50.gil',
                                           collapse='\n')),
                data = gil.landings_e[[1]]) %>% 
  gadget_update('totalfleet',
                name = 'gil_l',
                suitability = paste0('\n',
                                     paste(c('hadimm','hadmat'),
                                           'function','exponentiall50',
                                           'Modelfiles/timevariablealpha.gil','Modelfiles/timevariablel50.gil',
                                           collapse='\n')),
                data = gil.landings_l[[1]]) %>% 
  gadget_update('totalfleet',
                name = 'foreign',
                suitability = 
                  paste0('\n',
                         paste(c('hadimm','hadmat'),
                               'function','exponentiall50',
                               'Modelfiles/timevariablealpha.lln','Modelfiles/timevariablel50.lln',
                               collapse='\n')),
                data = foreign.landings[[1]]) %>%
  gadget_update('totalfleet',
                name = 'old',
                suitability = 
                  paste0('\n',
                         paste(c('hadimm','hadmat'),
                               #'function','exponentiall50',
                               #'#had.bmt.alpha','#had.bmt.l50',
                               'function','andersenfleet',
                               'Modelfiles/timevariablep0.bmt','Modelfiles/timevariablelmode.bmt','Modelfiles/timevariablep2.bmt',
                               'Modelfiles/timevariablep3.bmt','Modelfiles/timevariablep4.bmt','100',
                               collapse='\n')),
                data = old.landings[[1]]) %>% 
  write.gadget.file(gd$dir)
