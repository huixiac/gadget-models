minage <- ling.imm[[1]]$minage
maxage <- ling.mat[[1]]$maxage
maxlength <- ling.mat[[1]]$maxlength #max(Rgadget:::getLengthgroups(gm))
minlength <- ling.imm[[1]]$minlength

## Query length data to create IGFS catchdistribution components
aggdata <- mfdb_sample_count(mdb, c('age', 'length'), 
                             c(list(
                               sampling_type = 'IGFS',
                               length = mfdb_interval("len", 
                                                      c(0,seq(minlength+4, maxlength, by = 4)),
                                                      open_ended = TRUE)),
                               defaults))

attributes(aggdata[['0.0.0.0.0']])$age$all <- minage:maxage
attr(attributes(aggdata[['0.0.0.0.0']])$length$len0,'min') <- minlength

gadget_likelihood_component("catchdistribution",
                            name = "ldist.igfs",
                            weight = 1,
                            data = aggdata[[1]],
                            fleetnames = c("igfs"),
                            stocknames = c("lingimm", "lingmat")) %>% 
gadget_dir_write(gd,.)


## Age IGFS
aggdata <-
    mfdb_sample_count(mdb, c('age', 'length'),
                      c(list(sampling_type = 'IGFS',
                             age = mfdb_step_interval('age',by=1,from=3,to=12,open_ended = TRUE),
                             length = mfdb_interval("len", 
                                                    c(0,seq(minlength+4, maxlength, by = 4)),
                                                    open_ended = TRUE)),
                        defaults))
attr(attributes(aggdata[[1]])$length$len0,'min') <- minlength

#attributes(aggdata[[1]])$age <-
#    llply(attributes(aggdata[[1]])$age,function(x) x[1])
gadget_likelihood_component("catchdistribution",
                            name = "aldist.igfs",
                            weight = 1,
                            data = aggdata[[1]],
                            fleetnames = c("igfs"),
                            stocknames = c("lingimm", "lingmat")) %>% 
gadget_dir_write(gd,.)

## Maturity @3 from IGFS
aggdata <- 
  mfdb_sample_count(mdb, c('maturity_stage','age','length'),
                    append(defaults,
                           list(sampling_type='IGFS',
                                age=mfdb_group(mat_ages=minage:maxage),
                                length = mfdb_interval('len',
                                                       seq(minlength+4, maxlength, by = 8),
                                                       open_ended = TRUE),              
                                maturity_stage = mfdb_group(lingimm = 1, lingmat = 2:5))))

gadget_likelihood_component("stockdistribution",
                            name = "matp.igfs",
                            weight = 1,
                            data = aggdata[[1]],
                            fleetnames = c("igfs"),
                            stocknames = c("lingimm", "lingmat")) %>% 
gadget_dir_write(gd,.)



## Query length data to create lln catchdistribution components
aggdata <- 
  mfdb_sample_count(mdb, c('age', 'length'), 
                    c(list(
                      sampling_type = 'SEA',
                      #    gear = c('LLN','HLN'),
                      length = mfdb_interval("len", 
                                             c(0,seq(minlength+4, maxlength, by = 4)),
                                             open_ended = TRUE)),
                      defaults))
attributes(aggdata[[1]])$age$all <- minage:maxage
attr(attributes(aggdata[[1]])$length$len0,'min') <- minlength

gadget_likelihood_component("catchdistribution",
                            name = "ldist.comm",
                            weight = 1,
                            data = aggdata[[1]],
                            fleetnames = c("comm"),
                            stocknames = c("lingimm", "lingmat")) %>% 
gadget_dir_write(gd,.)
rm(aggdata)
## Age lln
aggdata <-
    mfdb_sample_count(mdb, c('age', 'length'),
                      c(list(sampling_type = 'SEA',
#                             gear = c('LLN','HLN'),
                             age = mfdb_step_interval('age',by=1,from=3,to=12,open_ended = TRUE),
                             length = mfdb_interval("len", c(0,seq(minlength+4, maxlength, by = 4)),
                                                    open_ended = TRUE)),
                        defaults))
attr(attributes(aggdata[[1]])$length$len0,'min') <- minlength
#attributes(aggdata[[1]])$age <-
#    llply(attributes(aggdata[[1]])$age,function(x) x[1])

gadget_likelihood_component("catchdistribution",
                            name = "aldist.comm",
                            weight = 1,
                            data = aggdata[[1]],
                            fleetnames = c("comm"),
                            stocknames = c("lingimm", "lingmat")) %>% 
gadget_dir_write(gd,.)
rm(aggdata)


