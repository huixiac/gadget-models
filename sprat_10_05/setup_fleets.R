# ---------------------------------------------------------------------
# Retrieve catches SPR for the period 1974-2018
catonYr <- mfdb_sample_meanweight(mdb, 'age', list(
    area          = "SD2232",
    timestep      = mfdb_timestep_quarterly,
    year          = 1974:2018,
    sampling_type = 'LND',
    data_source   = 'canumANDweca_spr_byYr_1974-2018',
    age           = mfdb_group('age1'=1,'age2'=2,'age3'=3,'age4'=4,'age5'=5,'age6'=6,'age7'=7,'age8'=8:10)))[[1]] %>%
    mutate(age=as.numeric(substring(age,4,6)),
           wgt=number*mean*1000) %>%
    group_by(year) %>%
    summarise(wgtYr=sum(wgt))
catonYr[1:3,]

ggplot(catonYr) +
    geom_point(aes(year,wgtYr)) +
    geom_line(aes(year,wgtYr)) +
    theme_bw()

# retrieve caton by Q from SMS and WGBFAS
catonQ1 <- mfdb_sample_meanweight(mdb, 'age', list(
    area            = "SD2232",
    timestep        = mfdb_timestep_quarterly,
    year            = 1974:1994,
    sampling_type = 'LND',
    data_source   = 'canumANDweca_spr_byQ_1974-2010',
    age           = mfdb_group('age1'=1,'age2'=2,'age3'=3,'age4'=4,'age5'=5,'age6'=6,'age7'=7,'age8'=8:10)))[[1]] %>%
    mutate(age=as.numeric(substring(age,4,6)),
           wgt=number*mean,
           wgt=ifelse(number==0,0,wgt)) %>%
    group_by(year,step) %>%
    summarise(wgt=sum(wgt)) %>%
    mutate(source="sms")

catonQ2 <- mfdb_sample_meanweight(mdb, 'age', list(
    area            = "SD2232",
    timestep        = mfdb_timestep_quarterly,
    year            = 1995:2018,
    sampling_type = 'LND',
    data_source   = 'canumANDweca_spr_byQ_1995-2018',
    age           = mfdb_group('age1'=1,'age2'=2,'age3'=3,'age4'=4,'age5'=5,'age6'=6,'age7'=7,'age8'=8:10)))[[1]] %>%
    mutate(age=as.numeric(substring(age,4,6)),
           wgt=number*mean,
           wgt=ifelse(number==0,0,wgt)) %>%
    group_by(year,step) %>%
    summarise(wgt=sum(wgt)) %>%
    mutate(source="wgbfas")

catonQ <- bind_rows(catonQ1,catonQ2) %>%
          mutate(wgt=wgt*1000)
catonQ[1:3,]

## ggplot(catonQ) +
##     geom_point(aes(year,wgt,col=source)) +
##     geom_line(aes(year,wgt)) +
##     theme_bw() +
##     facet_wrap(~step)

# merge yearly and quaterly catches
com.catch.spr <- catonQ %>%
          group_by(year) %>%
          summarise(wgtSum=sum(wgt)) %>%
          right_join(catonQ) %>%
          mutate(prop=wgt/wgtSum) %>%
          left_join(catonYr) %>%
          mutate(biomass=prop*wgtYr) %>%
          mutate(area=1, fleet='comspr') %>%
          select(year,step,area,fleet,biomass)
com.catch.spr[1:3,]

com.catch.spr <- structure(rbind(com.catch.spr),
            area_group = mfdb_group(`1` = 1))

# ---------------------------------------------------------------------
# make survey
bias.catch.spr <- 
  structure(rbind(data.frame(year=1991:2018,step=4,area=1,fleet="biasspr",number=1)),
            area_group = mfdb_group(`1` = 1))

# ---------------------------------------------------------------------
## write to file
tmp <- gadgetfleet('Modelfiles/fleet',gd$dir,missingOkay = TRUE) %>% 
  gadget_update('totalfleet',
                name = 'biasspr',
                suitability =
                paste0('\n',
                         paste(c('sprimm','sprmat'),
                               'function','exponentiall50',
                               '#spr.aco.alpha','#spr.aco.l50',
                               collapse='\n')),
                data = bias.catch.spr) %>%
  gadget_update('totalfleet',
                name = 'comspr',
                ## livesonareas = 1,
                suitability = 
                  paste0('\n',
                         paste(c('sprimm','sprmat'),
                               'function','exponentiall50',
                               '#spr.com.alpha','#spr.com.l50',
                               collapse='\n')),
                data = com.catch.spr)

write.gadget.file(tmp, gd$dir)
