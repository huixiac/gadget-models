minage <- lem.imm[[1]]$minage
maxage <- lem.mat[[1]]$maxage
maxlength <- lem.mat[[1]]$maxlength 
minlength <- lem.imm[[1]]$minlength
dl <- lem.imm[[1]]$dl

## Query length data to create aut catchdistribution components
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

# for(i in seq_along(ldist.aut)){
#   attributes(ldist.aut[[i]])$age$all <- minage:maxage
#   attr(attributes(ldist.aut[[i]])$length$len0,'min') <- minlength
# }


## Age aut
aldist.igfs <-
  mfdb_sample_count(mdb, 
                    c('age', 'length'),
                    c(list(sampling_type = 'IGFS',
                           data_source = 'iceland-aldist',
                           age = mfdb_interval('age',c(minage:maxage),open_ended = c('upper')),
                           length = mfdb_interval("len", 
                                                  seq(minlength, maxlength, by = dl),
                                                  open_ended = c("upper","lower"))),
                      defaults))
# for(i in seq_along(aldist.aut)){
#   attr(attributes(aldist.aut[[i]])$length$len0,'min') <- minlength
# }

matp.igfs <- 
  mfdb_sample_count(mdb, c('maturity_stage','age','length'),
                    append(defaults,
                           list(sampling_type='IGFS',
                                age=mfdb_group(mat_ages=minage:maxage),
                                length = mfdb_interval('len',
                                                       seq(minlength, maxlength, by = 2*dl),
                                                       open_ended = c('lower','upper')),              
                                maturity_stage = mfdb_group(lemimm = 1, lemmat = 2:5))))




ldist.dse <- 
  mfdb_sample_count(mdb, 
                    c('age', 'length'), 
                    c(list(
                      sampling_type = 'SEA',
                      data_source = 'iceland-ldist',
                      gear = c('DSE'),
                      age = mfdb_interval("all",c(minage,maxage),
                                          open_ended = c("upper","lower")),
                      length = mfdb_interval("len", 
                                             seq(minlength, maxlength, by = dl),
                                             open_ended = c("upper","lower"))),
                      defaults))


aldist.dse <-
  mfdb_sample_count(mdb, 
                    c('age', 'length'),
                    c(list(sampling_type = 'SEA',
                           data_source = 'iceland-aldist',
                           gear = c('DSE'),
                           age =  mfdb_interval('age',c(minage:maxage),open_ended = c('upper')),
                           length = mfdb_interval("len", 
                                                  seq(minlength, maxlength, by = dl),
                                                  open_ended = c("upper","lower"))),
                      defaults))

ldist.bmt <- 
  mfdb_sample_count(mdb,
                    c('age', 'length'), 
                    c(list(
                      sampling_type = 'SEA',
                      data_source = 'iceland-ldist',
                      gear=c('BMT','NPT','LLN','PSE','PGT','SHT','HLN','GLN'),
                      age = mfdb_interval("all",c(minage,maxage),
                                          open_ended = c("upper","lower")),
                      length = mfdb_interval("len", 
                                             seq(minlength, maxlength, by = dl),
                                             open_ended = c("upper","lower"))),
                      defaults))

aldist.bmt <-
  mfdb_sample_count(mdb, 
                    c('age', 'length'),
                    c(list(sampling_type = 'SEA',
                           data_source = 'iceland-aldist',
                           gear=c('BMT','NPT','PSE','PGT','SHT','LLN','HLN','GLN'),
                           age = mfdb_interval('age',c(minage:maxage),open_ended = c('upper')),
                           length = mfdb_interval("len", 
                                                  seq(minlength, maxlength, by = dl),
                                                  open_ended = c("upper","lower"))),
                      defaults))

#ldist.gil <- 
  #mfdb_sample_count(mdb, c('age', 'length'), 
                    #c(list(
                      #data_source = 'iceland-ldist',
                      #sampling_type = 'SEA',
                      #gear='GIL',
                      #age = mfdb_interval("all",c(minage,maxage),
                      #                    open_ended = c("lower")),
                      #length = mfdb_interval("len", 
                      #                       seq(minlength, maxlength, by = dl),
                      #                       open_ended = c("upper","lower"))),
                      #defaults))


# aldist.gil <-
#   mfdb_sample_count(mdb, c('age', 'length'),
#                     c(list(sampling_type = 'SEA',
#                            data_source = 'iceland-aldist',
#                            gear='GIL',
#                            age =  mfdb_interval('age',c(minage:maxage),open_ended = c('upper')),
#                            length = mfdb_interval("len", 
#                                                   seq(minlength, maxlength, by = dl),
#                                                   open_ended = c("upper","lower"))),
#                       defaults))