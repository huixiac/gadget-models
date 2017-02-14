minage <- tusk.imm[[1]]$minage
maxage <- tusk.mat[[1]]$maxage
maxlength <- tusk.mat[[1]]$maxlength 
minlength <- tusk.imm[[1]]$minlength
dl <- tusk.imm[[1]]$dl

## Query length data to create IGFS catchdistribution components
ldist.igfs <-
  mfdb_sample_count(mdb, 
                    c('age', 'length'), 
                    c(list(
                      data_source = 'iceland-ldist',
                      sampling_type = 'IGFS',
                      age = mfdb_interval("all",c(minage,maxage),
                                          open_ended = c("upper","lower")),
                      length = mfdb_interval("len", 
                                             seq(minlength, maxlength, by = dl),
                                             open_ended = c("upper","lower"))),
                      defaults))


## Age IGFS

aldist.igfs <-
  mfdb_sample_count(mdb, 
                    c('age', 'length'),
                    c(list(sampling_type = 'IGFS',
                           data_source = 'iceland-aldist',
                           age = mfdb_interval('age',c(minage:10,maxage),open_ended = c('lower','upper')),
                           length = mfdb_interval("len", 
                                                  seq(minlength, maxlength, by = dl),
                                                  open_ended = c("upper","lower"))),
                      defaults))

matp.igfs <- 
  mfdb_sample_count(mdb, c('maturity_stage','age','length'),
                    append(defaults,
                           list(sampling_type='IGFS',
                                age=mfdb_group(mat_ages=minage:maxage),
                                length = mfdb_interval('len',
                                                       seq(minlength, maxlength, by = 2*dl),
                                                       open_ended = 'upper'),              
                                maturity_stage = mfdb_group(tuskimm = 1, tuskmat = 2:5))))


## setup new catchdistribution likelihoodsldist.comm <- 
ldist.comm <- 
  mfdb_sample_count(mdb, 
                    c('age', 'length'), 
                    c(list(
                      data_source = 'iceland-ldist',
                      sampling_type = 'SEA',
                      age = mfdb_interval("all",c(minage,maxage),
                                          open_ended = c("upper","lower")),
                      length = mfdb_interval("len", 
                                             seq(minlength, maxlength, by = dl),
                                             open_ended = c("upper","lower"))),
                      defaults))


aldist.comm <-
  mfdb_sample_count(mdb, 
                    c('age', 'length'), 
                    c(list(
                      data_source = 'iceland-aldist',
                      sampling_type = 'SEA',
                      age = mfdb_interval('age',c(minage:10,maxage),
                                          open_ended = c('lower','upper')),
                      length = mfdb_interval("len", 
                                             seq(minlength, maxlength, by = dl),
                                             open_ended = c("upper","lower"))),
                      defaults))
  
  
