## IGFS survey indices
#from report: indices in groups 5-13, 14-19,20-29,30-55,56-74,75-109.
#from 4-plot: <40, >60 (harvestable), >79 (large fish),total
aut.SI1 <- 
  mfdb_sample_count(mdb, c('length'), c(list(
    data_source = 'iceland-ldist',
    sampling_type = 'AUT',
    length = mfdb_interval("len", c(5,19),open_ended = 'lower')),
    defaults))

# aut.SI2 <- 
#   mfdb_sample_count(mdb, c('length'), c(list(
#     data_source = 'iceland-ldist',
#     sampling_type = 'AUT',
#     length = mfdb_interval("len", c(14,19))),
#     defaults))


aut.SI3 <- 
  mfdb_sample_count(mdb, 
                    c('length'), 
                    c(list(
                      data_source = 'iceland-ldist',
                      sampling_type = 'AUT',
                      length = mfdb_interval("len", c(20,29))),
                      defaults))


aut.SI4 <- 
  mfdb_sample_count(mdb, 
                    c('length'), 
                    c(list(
                      data_source = 'iceland-ldist',
                      sampling_type = 'AUT',
                      length = mfdb_interval("len", c(30,55))),
                      defaults))


aut.SI5 <- 
  mfdb_sample_count(mdb, 
                    c('length'), 
                    c(list(
                      data_source = 'iceland-ldist',
                      sampling_type = 'AUT',
                      length = mfdb_interval("len", c(56,74))),
                      defaults))

aut.SI6 <- 
  mfdb_sample_count(mdb, 
                    c('length'), 
                    c(list(
                      data_source = 'iceland-ldist',
                      sampling_type = 'AUT',
                      length = mfdb_interval("len", c(75,110), open_ended = 'upper')),
                      defaults))


# aut.SI3d <- 
#   mfdb_sample_count(mdb, 
#                     c('length'),
#                     c(list(
#                       data_source = 'iceland-ldist',
#                       sampling_type = 'AUT',
#                       length = mfdb_interval("len", c(100,160),open_ended = 'upper')),
#                       defaults))

igfs.SI1 <- 
  mfdb_sample_count(mdb, c('length'), c(list(
    data_source = 'iceland-ldist',
    sampling_type = 'IGFS',
    length = mfdb_interval("len", c(5,19),open_ended = 'lower')),
    defaults))

# igfs.SI2 <- 
#   mfdb_sample_count(mdb, c('length'), c(list(
#     data_source = 'iceland-ldist',
#     sampling_type = 'IGFS',
#     length = mfdb_interval("len", c(14,19))),
#     defaults))
# 

igfs.SI3 <- 
  mfdb_sample_count(mdb, 
                    c('length'), 
                    c(list(
                      data_source = 'iceland-ldist',
                      sampling_type = 'IGFS',
                      length = mfdb_interval("len", c(20,29))),
                      defaults))


igfs.SI4 <- 
  mfdb_sample_count(mdb, 
                    c('length'), 
                    c(list(
                      data_source = 'iceland-ldist',
                      sampling_type = 'IGFS',
                      length = mfdb_interval("len", c(30,55))),
                      defaults))


igfs.SI5 <- 
  mfdb_sample_count(mdb, 
                    c('length'), 
                    c(list(
                      data_source = 'iceland-ldist',
                      sampling_type = 'IGFS',
                      length = mfdb_interval("len", c(56,74))),
                      defaults))

igfs.SI6 <- 
  mfdb_sample_count(mdb, 
                    c('length'), 
                    c(list(
                      data_source = 'iceland-ldist',
                      sampling_type = 'IGFS',
                      length = mfdb_interval("len", c(75,110), open_ended = 'upper')),
                      defaults))


# igfs.SI3d <- 
#   mfdb_sample_count(mdb, 
#                     c('length'),
#                     c(list(
#                       data_source = 'iceland-ldist',
#                       sampling_type = 'IGFS',
#                       length = mfdb_interval("len", c(100,160),open_ended = 'upper')),
#                       defaults))

