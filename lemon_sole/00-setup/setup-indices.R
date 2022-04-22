## AUT survey indices

igfs.SI1 <- 
  mfdb_sample_count(mdb, c('length'), c(list(
    data_source = 'iceland-ldist',
    sampling_type = 'IGFS',
    length = mfdb_interval("len", c(10,20),open_ended = 'lower')),
    defaults))

igfs.SI2a <- 
  mfdb_sample_count(mdb, c('length'), c(list(
    data_source = 'iceland-ldist',
    sampling_type = 'IGFS',
    length = mfdb_interval("len", c(20,25))),
    defaults))


igfs.SI2b <- 
  mfdb_sample_count(mdb, 
                    c('length'), 
                    c(list(
                      data_source = 'iceland-ldist',
                      sampling_type = 'IGFS',
                      length = mfdb_interval("len", c(25,30))),
                      defaults))


igfs.SI3a <- 
  mfdb_sample_count(mdb, 
                    c('length'), 
                    c(list(
                      data_source = 'iceland-ldist',
                      sampling_type = 'IGFS',
                      length = mfdb_interval("len", c(30,35))),
                      defaults))


igfs.SI3b <- 
  mfdb_sample_count(mdb, 
                    c('length'), 
                    c(list(
                      data_source = 'iceland-ldist',
                      sampling_type = 'IGFS',
                      length = mfdb_interval("len", c(35,40))),
                      defaults))

igfs.SI3c <- 
  mfdb_sample_count(mdb, 
                    c('length'), 
                    c(list(
                      data_source = 'iceland-ldist',
                      sampling_type = 'IGFS',
                      length =
                        mfdb_interval("len", c(40,45))),
                      defaults))


igfs.SI3d <- 
  mfdb_sample_count(mdb, 
                  c('length'),
                    c(list(
                      data_source = 'iceland-ldist',
                      sampling_type = 'IGFS',
                      length = mfdb_interval("len", c(45,60),open_ended = 'upper')),
                      defaults))

