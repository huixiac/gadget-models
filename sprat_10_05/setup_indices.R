# Query index abundance BIAS
si1.spr.bias <- mfdb_sample_count(mdb, c('age'), list(
    area            = "SD2232",
    timestep        = mfdb_timestep_quarterly,
    year            = c(1991,1992,1994,1996,1998:2018), # from WGBFAS
    species         = 'SPR', 
    age             = mfdb_group('age1'=1),
    sampling_type   = 'RES',
    data_source     = 'acoustic_bias_age_stock'))[[1]]

si1.spr.bias$area <- "area1"
names(attributes(si1.spr.bias)$area) <- "area1"
attributes(si1.spr.bias)$area$area1 <- "area1"

si2.spr.bias <- mfdb_sample_count(mdb, c('age'), list(
    area            = "SD2232",
    timestep        = mfdb_timestep_quarterly,
    year            = c(1991,1992,1994,1996,1998:2018), # from WGBFAS
    species         = 'SPR', 
    age             = mfdb_group('age2'=2),
    sampling_type   = 'RES',
    data_source     = 'acoustic_bias_age_stock'))[[1]]

si2.spr.bias$area <- "area1"
names(attributes(si2.spr.bias)$area) <- "area1"
attributes(si2.spr.bias)$area$area1 <- "area1"

si3.spr.bias <- mfdb_sample_count(mdb, c('age'), list(
    area            = "SD2232",
    timestep        = mfdb_timestep_quarterly,
    year            = c(1991,1992,1994,1996,1998:2018), # from WGBFAS
    species         = 'SPR', 
    age             = mfdb_group('age3'=3),
    sampling_type   = 'RES',
    data_source     = 'acoustic_bias_age_stock'))[[1]]

si3.spr.bias$area <- "area1"
names(attributes(si3.spr.bias)$area) <- "area1"
attributes(si3.spr.bias)$area$area1 <- "area1"

si4.spr.bias <- mfdb_sample_count(mdb, c('age'), list(
    area            = "SD2232",
    timestep        = mfdb_timestep_quarterly,
    year            = c(1991,1992,1994,1996,1998:2018), # from WGBFAS
    species         = 'SPR', 
    age             = mfdb_group('age4'=4),
    sampling_type   = 'RES',
    data_source     = 'acoustic_bias_age_stock'))[[1]]

si4.spr.bias$area <- "area1"
names(attributes(si4.spr.bias)$area) <- "area1"
attributes(si4.spr.bias)$area$area1 <- "area1"

si5.spr.bias <- mfdb_sample_count(mdb, c('age'), list(
    area            = "SD2232",
    timestep        = mfdb_timestep_quarterly,
    year            = c(1991,1992,1994,1996,1998:2018), # from WGBFAS
    species         = 'SPR', 
    age             = mfdb_group('age5'=5),
    sampling_type   = 'RES',
    data_source     = 'acoustic_bias_age_stock'))[[1]]

si5.spr.bias$area <- "area1"
names(attributes(si5.spr.bias)$area) <- "area1"
attributes(si5.spr.bias)$area$area1 <- "area1"

si6.spr.bias <- mfdb_sample_count(mdb, c('age'), list(
    area            = "SD2232",
    timestep        = mfdb_timestep_quarterly,
    year            = c(1991,1992,1994,1996,1998:2018), # from WGBFAS
    species         = 'SPR', 
    age             = mfdb_group('age6'=6),
    sampling_type   = 'RES',
    data_source     = 'acoustic_bias_age_stock'))[[1]]

si6.spr.bias$area <- "area1"
names(attributes(si6.spr.bias)$area) <- "area1"
attributes(si6.spr.bias)$area$area1 <- "area1"

si7.spr.bias <- mfdb_sample_count(mdb, c('age'), list(
    area            = "SD2232",
    timestep        = mfdb_timestep_quarterly,
    year            = c(1991,1992,1994,1996,1998:2018), # from WGBFAS
    species         = 'SPR', 
    age             = mfdb_group('age7'=7),
    sampling_type   = 'RES',
    data_source     = 'acoustic_bias_age_stock'))[[1]]

si7.spr.bias$area <- "area1"
names(attributes(si7.spr.bias)$area) <- "area1"
attributes(si7.spr.bias)$area$area1 <- "area1"

si8.spr.bias <- mfdb_sample_count(mdb, c('age'), list(
    area            = "SD2232",
    timestep        = mfdb_timestep_quarterly,
    year            = c(1991,1992,1994,1996,1998:2018), # from WGBFAS
    species         = 'SPR', 
    age             = mfdb_group('age8'=8),
    sampling_type   = 'RES',
    data_source     = 'acoustic_bias_age_stock'))[[1]]

si8.spr.bias$area <- "area1"
names(attributes(si8.spr.bias)$area) <- "area1"
attributes(si8.spr.bias)$area$area1 <- "area1"

## ggplot(indexNatAgeSpr) +
##     geom_point(aes(year,number)) +
##     geom_line(aes(year,number)) +
##     facet_wrap(~age, scale="free_y")

# Retrieve sprat recruitment index RCT3 (BIAS)
si0.spr.rct3 <- mfdb_sample_count(mdb, c('age'), list(
    area            = "SD2232",
    timestep        = mfdb_timestep_quarterly,
    year            = c(1991,1992,1994,1996,1998:2017), # from WGBFAS
    species         = 'SPR', 
    sampling_type   = 'RES',
    age             =  mfdb_group('age0'=0),
    data_source     = 'rct3_sprat'))[[1]]
si0.spr.rct3[1:4,]
si0.spr.rct3$area <- "area1"
names(attributes(si0.spr.rct3)$area) <- "area1"
attributes(si0.spr.rct3)$area$area1 <- "area1"

## ggplot(indexRecSpr) +
##     geom_point(aes(year,number)) +
##     ## geom_line(aes(year,number)) +
##     facet_wrap(~age, scale="free_y")

