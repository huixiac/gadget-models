## IGFS survey indices

igfs.SI1 <- 
  mfdb_sample_count(mdb, c('length'), c(list(
    data_source = 'iceland-ldist',
    sampling_type = 'IGFS',
    length = mfdb_interval("len", c(13,18),open_ended = 'lower')),
    defaults))

igfs.SI2 <- 
  mfdb_sample_count(mdb, c('length'), c(list(
    data_source = 'iceland-ldist',
    sampling_type = 'IGFS',
    length = mfdb_interval("len", c(18,24))),
    defaults))


igfs.SI3 <- 
  mfdb_sample_count(mdb, 
                    c('length'), 
                    c(list(
                      data_source = 'iceland-ldist',
                      sampling_type = 'IGFS',
                      length = mfdb_interval("len", c(24,31))),
                      defaults))


igfs.SI4 <- 
  mfdb_sample_count(mdb, 
                    c('length'), 
                    c(list(
                      data_source = 'iceland-ldist',
                      sampling_type = 'IGFS',
                      length = mfdb_interval("len", c(31,39))),
                      defaults))


igfs.SI5 <- 
  mfdb_sample_count(mdb, 
                    c('length'), 
                    c(list(
                      data_source = 'iceland-ldist',
                      sampling_type = 'IGFS',
                      length = mfdb_interval("len", c(39,48))),
                      defaults))

igfs.SI6 <- 
  mfdb_sample_count(mdb, 
                    c('length'), 
                    c(list(
                      data_source = 'iceland-ldist',
                      sampling_type = 'IGFS',
                      length = mfdb_interval("len", c(48,58))),
                      defaults))


igfs.SI7 <- 
  mfdb_sample_count(mdb, 
                    c('length'),
                    c(list(
                      data_source = 'iceland-ldist',
                      sampling_type = 'IGFS',
                      length = mfdb_interval("len", c(58,100),open_ended = 'upper')),
                      defaults))


## AUT survey indices

off<-3

aut.SI1 <- 
  mfdb_sample_count(mdb, c('length'), c(list(
    data_source = 'iceland-ldist',
    sampling_type = 'AUT',
    length = mfdb_interval("len", c(13,18)-off,open_ended = 'lower')),
    defaults))

aut.SI2 <- 
  mfdb_sample_count(mdb, c('length'), c(list(
    data_source = 'iceland-ldist',
    sampling_type = 'AUT',
    length = mfdb_interval("len", c(18,24)-off)),
    defaults))


aut.SI3 <- 
  mfdb_sample_count(mdb, 
                    c('length'), 
                    c(list(
                      data_source = 'iceland-ldist',
                      sampling_type = 'AUT',
                      length = mfdb_interval("len", c(24,31)-off)),
                      defaults))


aut.SI4 <- 
  mfdb_sample_count(mdb, 
                    c('length'), 
                    c(list(
                      data_source = 'iceland-ldist',
                      sampling_type = 'AUT',
                      length = mfdb_interval("len", c(31,39)-off)),
                      defaults))


aut.SI5 <- 
  mfdb_sample_count(mdb, 
                    c('length'), 
                    c(list(
                      data_source = 'iceland-ldist',
                      sampling_type = 'AUT',
                      length = mfdb_interval("len", c(39,48)-off)),
                      defaults))

aut.SI6 <- 
  mfdb_sample_count(mdb, 
                    c('length'), 
                    c(list(
                      data_source = 'iceland-ldist',
                      sampling_type = 'AUT',
                      length = mfdb_interval("len", c(48,58)-off)),
                      defaults))


aut.SI7 <- 
  mfdb_sample_count(mdb, 
                    c('length'),
                    c(list(
                      data_source = 'iceland-ldist',
                      sampling_type = 'AUT',
                      length = mfdb_interval("len", c(58,100)-off,open_ended = 'upper')),
                      defaults))


