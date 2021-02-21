si.ven.cpue <- mfdb_sample_count(mdb, c('length'), list(
    area          = "SD3031",
    timestep      = mfdb_timestep_quarterly,
    year          = 1996:2018,
    sampling_type = 'LND',
    data_source   = 'cpue_allfishery_vendace'))[[1]]
si.ven.cpue$area <- '1'
names(attributes(si.ven.cpue)$area) <- '1'
attributes(si.ven.cpue)$area$'1' <- '1'
attributes(si.ven.cpue)$length$all <- c(minlength,maxlength)

si.ven.aco <- mfdb_sample_count(mdb, c('age'), list(
    area          = "SD3031",
    timestep      = mfdb_timestep_quarterly,
    year          = 2009:2018,
    sampling_type = 'RES',
    data_source   = 'acoustic_agedistribution_vendace',
    age           = mfdb_group('all'=0:10)))[[1]]
si.ven.aco$area <- '1'
names(attributes(si.ven.aco)$area) <- '1'
attributes(si.ven.aco)$area$'1' <- '1'

## ggplot() +
##     geom_point(data=si.ven.cpue, aes(year,number/max(number))) + geom_line(data=si.ven.cpue, aes(year,number/max(number))) +
##     geom_point(data=si.ven.aco, aes(year,number/max(number)),col=2) + geom_line(data=si.ven.aco, aes(year,number/max(number)),col=2) +
##     geom_point(data=si.ven.reff, aes(year,number/max(number)),col=3) + geom_line(data=si.ven.reff, aes(year,number/max(number)),col=3)
        
## # ---------------------------------------------------------------------
## # Query Acoustic and make index abundance ageXX
## # age0:
## si.ven.aco0 <- mfdb_sample_count(mdb, c('age'), list(
##     area          = "SD3031",
##     timestep      = mfdb_timestep_quarterly,
##     year          = 2009:2018,
##     sampling_type = 'RES',
##     data_source   = 'acoustic_agedistribution_vendace',
##     age           = mfdb_group('age0'=0)))[[1]]
## si.ven.aco0$area <- '1'
## # age1:
## si.ven.aco1 <- mfdb_sample_count(mdb, c('age'), list(
##     area          = "SD3031",
##     timestep      = mfdb_timestep_quarterly,
##     year          = 2009:2018,
##     sampling_type = 'RES',
##     data_source   = 'acoustic_agedistribution_vendace',
##     age           = mfdb_group('age1'=1)))[[1]]
## si.ven.aco1$area <- '1'
## # age2:
## si.ven.aco2 <- mfdb_sample_count(mdb, c('age'), list(
##     area          = "SD3031",
##     timestep      = mfdb_timestep_quarterly,
##     year          = 2009:2018,
##     sampling_type = 'RES',
##     data_source   = 'acoustic_agedistribution_vendace',
##     age           = mfdb_group('age2'=2)))[[1]]
## si.ven.aco2$area <- '1'
## # age3:
## si.ven.aco3 <- mfdb_sample_count(mdb, c('age'), list(
##     area          = "SD3031",
##     timestep      = mfdb_timestep_quarterly,
##     year          = 2009:2018,
##     sampling_type = 'RES',
##     data_source   = 'acoustic_agedistribution_vendace',
##     age           = mfdb_group('age3'=3)))[[1]]
## si.ven.aco3$area <- '1'
## # age4:
## si.ven.aco4 <- mfdb_sample_count(mdb, c('age'), list(
##     area          = "SD3031",
##     timestep      = mfdb_timestep_quarterly,
##     year          = 2009:2018,
##     sampling_type = 'RES',
##     data_source   = 'acoustic_agedistribution_vendace',
##     age           = mfdb_group('age4'=4)))[[1]]
## si.ven.aco4$area <- '1'
## # age5:
## si.ven.aco5 <- mfdb_sample_count(mdb, c('age'), list(
##     area          = "SD3031",
##     timestep      = mfdb_timestep_quarterly,
##     year          = 2009:2018,
##     sampling_type = 'RES',
##     data_source   = 'acoustic_agedistribution_vendace',
##     age           = mfdb_group('age5'=5)))[[1]]
## si.ven.aco5$area <- '1'
## # age6:
## si.ven.aco6 <- mfdb_sample_count(mdb, c('age'), list(
##     area          = "SD3031",
##     timestep      = mfdb_timestep_quarterly,
##     year          = 2009:2018,
##     sampling_type = 'RES',
##     data_source   = 'acoustic_agedistribution_vendace',
##     age           = mfdb_group('age6'=6)))[[1]]
## si.ven.aco6$area <- '1'
## # age7:
## si.ven.aco7 <- mfdb_sample_count(mdb, c('age'), list(
##     area          = "SD3031",
##     timestep      = mfdb_timestep_quarterly,
##     year          = 2009:2018,
##     sampling_type = 'RES',
##     data_source   = 'acoustic_agedistribution_vendace',
##     age           = mfdb_group('age7'=7)))[[1]]
## si.ven.aco7$area <- '1'
## # age8:
## si.ven.aco8 <- mfdb_sample_count(mdb, c('age'), list(
##     area          = "SD3031",
##     timestep      = mfdb_timestep_quarterly,
##     year          = 2009:2018,
##     sampling_type = 'RES',
##     data_source   = 'acoustic_agedistribution_vendace',
##     age           = mfdb_group('age8'=8:10)))[[1]]
## si.ven.aco8$area <- '1'
