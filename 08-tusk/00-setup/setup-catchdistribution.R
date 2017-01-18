minage <- tusk.imm[[1]]$minage
maxage <- tusk.mat[[1]]$maxage
maxlength <- tusk.mat[[1]]$maxlength 
minlength <- tusk.imm[[1]]$minlength
dl <- tusk.imm[[1]]$dl

## Query length data to create IGFS catchdistribution components
ldist.igfs <- mfdb_sample_count(mdb, c('age', 'length'), 
                             c(list(
                               sampling_type = 'IGFS',
                               length = mfdb_interval("len", 
                                                      c(0,seq(minlength+dl, maxlength, by = dl)),
                                                      open_ended = TRUE)),
                               defaults))

for(i in seq_along(ldist.igfs)){
  attributes(ldist.igfs[[i]])$age$all <- minage:maxage
  attr(attributes(ldist.igfs[[i]])$length$len0,'min') <- minlength
}


## Age IGFS
aldist.igfs <-
  mfdb_sample_count(mdb, c('age', 'length'),
                    c(list(sampling_type = 'IGFS',
                           age = mfdb_step_interval('age',by=1,from=minage,to=maxage+1,
                                                    open_ended = TRUE),
                           length = mfdb_interval("len", 
                                                  c(0,seq(minlength+dl, maxlength, by = dl)),
                                                  open_ended = TRUE)),
                      defaults))
for(i in seq_along(aldist.igfs)){
  attr(attributes(aldist.igfs[[i]])$length$len0,'min') <- minlength
}

matp.igfs <- 
  mfdb_sample_count(mdb, c('maturity_stage','age','length'),
                    append(defaults,
                           list(sampling_type='IGFS',
                                age=mfdb_group(mat_ages=minage:maxage),
                                length = mfdb_interval('len',
                                                       seq(minlength+dl, maxlength, by = 2*dl),
                                                       open_ended = TRUE),              
                                maturity_stage = mfdb_group(tuskimm = 1, tuskmat = 2:5))))


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

