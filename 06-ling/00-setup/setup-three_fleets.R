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


gadgetfleet('Modelfiles/fleet',three_fleets,missingOkay = TRUE) -> tmp%>%
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
                data = bmt.landings[[1]]) %>% 
  gadget_update('totalfleet',
                name = 'gil',
                suitability = paste0('\n',
                                     paste(c('lingimm','lingmat'),
                                           'function','exponentiall50',
                                           '#ling.gil.alpha','#ling.gil.l50',
                                           collapse='\n')),
                data = gil.landings[[1]]) -> tmp 
attr(tmp,'file_config')$mainfile_overwrite <- TRUE
## fixes due to read.gadget.file issues
#tmp[[1]][4] <- NULL
tmp[[1]][4]$suitability$suitability <- NULL
#tmp[[2]][4] <- NULL
tmp[[2]][4]$suitability$suitability <- NULL
tmp[[2]][4]$suitability$lingimm <- gsub('comm','lln',tmp[[2]][4]$suitability$lingimm)
tmp[[2]][4]$suitability$lingmat <- gsub('comm','lln',tmp[[2]][4]$suitability$lingmat)
write.gadget.file(tmp,three_fleets)

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
                stocknames = c("lingimm", "lingmat")) -> tmp
attr(tmp,'file_config')$mainfile_overwrite <- TRUE

  write.gadget.file(tmp,three_fleets)


Sys.setenv(GADGET_WORKING_DIR=normalizePath(gd$dir))
callGadget(s=1,log = 'init.log',main='three_fleets/main',p='three_fleets/params.out') #ignore.stderr = FALSE,

## update the input parameters with sane initial guesses
read.gadget.parameters(sprintf('%s/three_fleets/params.out',gd$dir)) %>% 
  init_guess('rec.[0-9]|init.[0-9]',1,0.001,100,1) %>%
  init_guess('recl',12,4,20,1) %>% 
  init_guess('rec.sd',5, 4, 20,1) %>% 
  init_guess('Linf',160, 100, 160,0) %>% 
  init_guess('k$',90, 40, 100,1) %>% 
  init_guess('bbin',6, 1e-08, 100, 1) %>% 
  init_guess('alpha', 0.5,  0.01, 3, 1) %>% 
  init_guess('l50',50,10,100,1) %>% 
  init_guess('walpha',lw.constants$estimate[1], 1e-10, 1,0) %>% 
  init_guess('wbeta',lw.constants$estimate[2], 2, 4,0) %>% 
  init_guess('M$',0.15,0.001,1,0) %>% 
  init_guess('rec.scalar',400,1,500,1) %>% 
  init_guess('init.scalar',200,1,300,1) %>% 
  init_guess('mat2',mat.l50$l50,0.75*mat.l50$l50,1.25*mat.l50$l50,1) %>% 
  init_guess('mat1',70,  10, 200, 1) %>% 
  init_guess('init.F',0.4,0.1,1,0) %>% 
  write.gadget.parameters(.,file=sprintf('%s/%s/params.in',gd$dir,'three_fleets'))

file.copy(sprintf('%s/run-three_fleets.R','06-ling/00-setup'),gd$dir)


  