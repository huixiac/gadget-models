## IGFS survey indices

igfs.SI1 <- mfdb_sample_count(mdb, c('length'), c(list(
  sampling_type = 'IGFS',
  length = mfdb_interval("len", c(0,20,30,40))),
  defaults))

for(i in seq_along(igfs.SI1)){
  attr(attributes(igfs.SI1[[i]])$length$len0,'min') <- minlength
}

igfs.SI2 <- mfdb_sample_count(mdb, c('length'), c(list(
  sampling_type = 'IGFS',
  length = mfdb_interval("len", c(40,50,60,70))),
  defaults))

igfs.SI3 <- mfdb_sample_count(mdb, c( 'length'), c(list(
  sampling_type = 'IGFS',
  length = mfdb_interval("len", c(70,80,110),open_ended = TRUE)),
  defaults))

