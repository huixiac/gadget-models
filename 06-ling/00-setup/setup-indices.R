## IGFS survey indices

igfs.SI1 <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type = 'IGFS',
    length = mfdb_interval("len", c(0,52))),
    defaults))
for(i in seq_along(igfs.SI1)){
  attr(attributes(igfs.SI1[[i]])$length$len0,'min') <- minlength
}

igfs.SI2 <- mfdb_sample_count(mdb, c('length'), c(list(
    sampling_type = 'IGFS',
    length = mfdb_interval("len", c(52,60,72))),
    defaults))

igfs.SI3 <- mfdb_sample_count(mdb, c( 'length'), c(list(
    sampling_type = 'IGFS',
    length = mfdb_interval("len", c(72,80,92,100,160),open_ended = TRUE)),
    defaults))


