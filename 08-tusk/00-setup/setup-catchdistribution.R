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
if(vers=='04-2017noage'|vers=='05-2017noage_growth_rest'){defaults$year<-1982:2016}
aldist.igfs <-
  mfdb_sample_count(mdb, 
                    c('age', 'length'),
                    c(list(sampling_type = 'IGFS',
                           data_source = 'iceland-aldist',
                           age = mfdb_interval('age',c(minage:10,maxage),open_ended = c('upper')),
                           length = mfdb_interval("len", 
                                                  seq(minlength, maxlength, by = dl),
                                                  open_ended = c("upper","lower"))),
                      defaults))
if(vers=='04-2017noage'|vers=='05-2017noage_growth_rest'){defaults$year<-1982:2017}

matp.igfs <- 
  mfdb_sample_count(mdb, c('maturity_stage','age','length'),
                    append(defaults,
                    list(sampling_type='IGFS',
                         length = mfdb_interval('len',
                                                seq(minlength, maxlength, by = 2*dl),
                                                open_ended = 'upper'),              
                         maturity_stage = mfdb_group(tuskimm = 1, tuskmat = 2:5))))

for(i in seq_along(matp.igfs)){
  attributes(matp.igfs[[i]])$age$all <- minage:maxage
}

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

if(vers=='04-2017noage'|vers=='05-2017noage_growth_rest'){defaults$year<-1982:2016}
aldist.comm <-
  mfdb_sample_count(mdb, 
                    c('age', 'length'), 
                    c(list(
                      data_source = 'iceland-aldist',
                      sampling_type = 'SEA',
                      age = mfdb_interval('age',c(minage:10,maxage),open_ended = c('upper')),
                      length = mfdb_interval("len", 
                                             seq(minlength, maxlength, by = dl),
                                             open_ended = c("upper","lower"))),
                      defaults))
if(vers=='04-2017noage'|vers=='05-2017noage_growth_rest'){defaults$year<-1982:2017}

  
