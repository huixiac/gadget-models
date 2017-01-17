three_fleets <- gadget.variant.dir(gd$dir,'three_fleets')

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


gadgetfleet('Modelfiles/fleet',three_fleets,missingOkay = TRUE) %>%
  gadget_discard('comm') %>% 
  gadget_update('totalfleet',
                name = 'lln',
                suitability = paste0('\n',
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
                data = lln.landings[[1]]) %>% 
  gadget_update('totalfleet',
                name = 'gil',
                suitability = paste0('\n',
                                     paste(c('lingimm','lingmat'),
                                           'function','exponentiall50',
                                           '#ling.gil.alpha','#ling.gil.l50',
                                           collapse='\n')),
                data = lln.landings[[1]]) %>% 
  write.gadget.file(three_fleets)

## setup new catchdistribution likelihoodsldist.comm <- 
ldist.lln <- 
  mfdb_sample_count(mdb, c('age', 'length'), 
                    c(list(
                      sampling_type = 'SEA',
                      gear = c('LLN','HLN'),
                      length = mfdb_interval("len", 
                                             c(0,seq(minlength+dl, maxlength, by = dl)),
                                             open_ended = TRUE)),
                      defaults))

for(i in seq_along(ldist.lln)){
  attributes(ldist.lln[[i]])$age$all <- minage:maxage
  attr(attributes(ldist.lln[[i]])$length$len0,'min') <- minlength
}

aldist.lln <-
  mfdb_sample_count(mdb, c('age', 'length'),
                    c(list(sampling_type = 'SEA',
                           gear = c('LLN','HLN'),
                           age = mfdb_step_interval('age',by=1,from=minage,to=maxage+1,open_ended = TRUE),
                           length = mfdb_interval("len", c(0,seq(minlength+dl, maxlength, by = dl)),
                                                  open_ended = TRUE)),
                      defaults))
for(i in seq_along(aldist.lln)){
  attr(attributes(aldist.lln[[i]])$length$len0,'min') <- minlength
}


ldist.bmt <- 
  mfdb_sample_count(mdb, c('age', 'length'), 
                    c(list(
                      sampling_type = 'SEA',
                      gear=c('BMT','NPT','DSE','PSE','PGT','SHT'),
                      length = mfdb_interval("len", 
                                             c(0,seq(minlength+dl, maxlength, by = dl)),
                                             open_ended = TRUE)),
                      defaults))

for(i in seq_along(ldist.bmt)){
  attributes(ldist.bmt[[i]])$age$all <- minage:maxage
  attr(attributes(ldist.bmt[[i]])$length$len0,'min') <- minlength
}

aldist.bmt <-
  mfdb_sample_count(mdb, c('age', 'length'),
                    c(list(sampling_type = 'SEA',
                           gear=c('BMT','NPT','DSE','PSE','PGT','SHT'),
                           age = mfdb_step_interval('age',by=1,from=minage,to=maxage+1,open_ended = TRUE),
                           length = mfdb_interval("len", c(0,seq(minlength+dl, maxlength, by = dl)),
                                                  open_ended = TRUE)),
                      defaults))
for(i in seq_along(aldist.bmt)){
  attr(attributes(aldist.bmt[[i]])$length$len0,'min') <- minlength
}

ldist.gil <- 
  mfdb_sample_count(mdb, c('age', 'length'), 
                    c(list(
                      sampling_type = 'SEA',
                      gear='GIL',
                      length = mfdb_interval("len", 
                                             c(0,seq(minlength+dl, maxlength, by = dl)),
                                             open_ended = TRUE)),
                      defaults))

for(i in seq_along(ldist.gil)){
  attributes(ldist.gil[[i]])$age$all <- minage:maxage
  attr(attributes(ldist.gil[[i]])$length$len0,'min') <- minlength
}

aldist.gil <-
  mfdb_sample_count(mdb, c('age', 'length'),
                    c(list(sampling_type = 'SEA',
                           gear='GIL',
                           age = mfdb_step_interval('age',by=1,from=minage,to=maxage+1,open_ended = TRUE),
                           length = mfdb_interval("len", c(0,seq(minlength+dl, maxlength, by = dl)),
                                                  open_ended = TRUE)),
                      defaults))
for(i in seq_along(aldist.gil)){
  attr(attributes(aldist.gil[[i]])$length$len0,'min') <- minlength
}





gadgetlikelihood('likelihood',three_fleets,missingOkay = TRUE) %>% 
  gadget_discard(c('aldist.comm','ldist.comm')) %>% 
  gadget_update("catchdistribution",
                name = "ldist.lln",
                weight = 1,
                data = ldist.lln[[1]],
                fleetnames = c("lln"),
                stocknames = c("lingimm", "lingmat")) %>% 
  gadget_update("catchdistribution",
                name = "aldist.lln",
                weight = 1,
                data = aldist.lln[[1]],
                fleetnames = c("lln"),
                stocknames = c("lingimm", "lingmat")) %>% 
  gadget_update("catchdistribution",
                name = "ldist.gil",
                weight = 1,
                data = ldist.gil[[1]],
                fleetnames = c("gil"),
                stocknames = c("lingimm", "lingmat")) %>% 
  gadget_update("catchdistribution",
                name = "aldist.gil",
                weight = 1,
                data = aldist.gil[[1]],
                fleetnames = c("gil"),
                stocknames = c("lingimm", "lingmat")) %>% 
  gadget_update("catchdistribution",
                name = "ldist.bmt",
                weight = 1,
                data = ldist.bmt[[1]],
                fleetnames = c("bmt"),
                stocknames = c("lingimm", "lingmat")) %>% 
  gadget_update("catchdistribution",
                name = "aldist.bmt",
                weight = 1,
                data = aldist.bmt[[1]],
                fleetnames = c("bmt"),
                stocknames = c("lingimm", "lingmat")) %>% 
  write.gadget.file(three_fleets)



  