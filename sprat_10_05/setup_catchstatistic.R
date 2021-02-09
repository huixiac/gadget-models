minage <- spr.imm[[1]]$minage
maxage <- spr.mat[[1]]$maxage
maxlength <- spr.mat[[1]]$maxlength 
minlength <- spr.imm[[1]]$minlength
dl <- 1

# ---------------------------------------------------------------------
# Query commercial weight at age (8+) WECA:

# https://github.com/Hafro/gadget2/issues/35
# it seems a bug so use only 1 stock at a time (assume age2 all sprmat)


wecaSpr1 <- mfdb_sample_meanweight(mdb, c('age'), list(
    area            = "SD2232",
    timestep        = mfdb_timestep_quarterly,
    year            = 1974:1994,
    sampling_type   = 'LND',
    age             = mfdb_group('age2'=2,'age3'=3,'age4'=4,'age5'=5,'age6'=6,'age7'=7,'age8'=8:10),
    data_source     = 'canumANDweca_spr_byQ_1974-2010'))[[1]]

wecaSpr2 <- mfdb_sample_meanweight(mdb, c('age'), list(
    area            = "SD2232",
    timestep        = mfdb_timestep_quarterly,
    year            = 1995:2018,
    sampling_type   = 'LND',
    age             = mfdb_group('age2'=2,'age3'=3,'age4'=4,'age5'=5,'age6'=6,'age7'=7,'age8'=8:10),
    data_source     = 'canumANDweca_spr_byQ_1995-2018'))[[1]]

waa2pl.spr.com <- rbind(wecaSpr1,wecaSpr2)
waa2pl.spr.com[1:3,]

waa2pl.spr.com$stddev <- 0.001 # indications from run mfdb_05_7
waa2pl.spr.com$area <- "area1"
attributes(waa2pl.spr.com)$area <- "area1"
names(attributes(waa2pl.spr.com)$area) <- "area1"

## ggplot(filter(waa2pl.spr.com,year %in% 1993:2002)) +
##     geom_point(aes(age,mean), stat="identity") +
##     facet_grid(step~year)

# ---------------------------------------------------------------------
