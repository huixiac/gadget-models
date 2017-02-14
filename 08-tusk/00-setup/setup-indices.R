## IGFS survey indices

igfs.SI1a <- mfdb_sample_count(mdb, c('length'), c(list(
  sampling_type = 'IGFS',
  length = mfdb_interval("len", c(10,20),open_ended='lower')),
  defaults))

igfs.SI1b <- mfdb_sample_count(mdb, c('length'), c(list(
  sampling_type = 'IGFS',
  length = mfdb_interval("len", c(20,30))),
  defaults))


igfs.SI1c <- mfdb_sample_count(mdb, c('length'), c(list(
  sampling_type = 'IGFS',
  length = mfdb_interval("len", c(30,40))),
  defaults))

igfs.SI2a <- mfdb_sample_count(mdb, c('length'), c(list(
  sampling_type = 'IGFS',
  length = mfdb_interval("len", c(40,50))),
  defaults))

igfs.SI2b <- mfdb_sample_count(mdb, c('length'), c(list(
  sampling_type = 'IGFS',
  length = mfdb_interval("len", c(50,60))),
  defaults))

igfs.SI2c <- mfdb_sample_count(mdb, c('length'), c(list(
  sampling_type = 'IGFS',
  length = mfdb_interval("len", c(60,70))),
  defaults))

igfs.SI3a <- mfdb_sample_count(mdb, c('length'), c(list(
  sampling_type = 'IGFS',
  length = mfdb_interval("len", c(70,80))),
  defaults))

igfs.SI3b <- mfdb_sample_count(mdb, c( 'length'), c(list(
  sampling_type = 'IGFS',
  length = mfdb_interval("len", c(80,110),open_ended = 'upper')),
  defaults))
