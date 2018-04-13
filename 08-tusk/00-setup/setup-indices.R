## IGFS survey indices

# tmp <- unique(reitmapping$SUBDIVISION)
# rosegarden <- list(
#   area = mfdb_group("1" = tmp[tmp!=1061]),
#   timestep = mfdb_timestep_quarterly,
#   year = year_range,
#   species = 'USK')
# 
# 
# igfs.SI <- mfdb_sample_count(mdb,c('length'),c(list(
#   sampling_type = 'IGFS',
#   length = mfdb_interval("len", c(10,20,30,40,50,60,70,110),open_ended=c('lower','upper'))),
#   defaults))
# 
# rose.SI <- mfdb_sample_count(mdb,c('length'),c(list(
#   sampling_type = 'IGFS',
#   length = mfdb_interval("len", c(10,20,30,40,50,60,70,110),open_ended=c('lower','upper'))),
#   rosegarden))
# 
# igfs.SI[[i]] %>% 
#   left_join(rose.SI[[i]],by=c('year','length')) %>% 
#   mutate(ratio=number.x/number.y, 
#          no_rose = ifelse(year %in% 1996:2004,'No Rose','With Rose')) %>% 
#   group_by(length,no_rose) %>% dplyr::summarise(m=median(ratio),l=quantile(ratio,0.05),u=quantile(ratio,0.95))
#   ggplot(aes(ratio,fill=no_rose))+geom_histogram() + facet_wrap(~length)
# 

igfs.SI1a <- mfdb_sample_count(mdb, c('length'), c(list(
  sampling_type = 'IGFS',
  length = mfdb_interval("len", c(10,20),open_ended='lower')),
  defaults))

for(i in seq_along(defaults$area))  {
  igfs.SI1a[[i]]$number[igfs.SI1a[[i]]$year %in% 1996:2004] <- 
    1.05*igfs.SI1a[[i]]$number[igfs.SI1a[[i]]$year %in% 1996:2004]
}

igfs.SI1b <- mfdb_sample_count(mdb, c('length'), c(list(
  sampling_type = 'IGFS',
  length = mfdb_interval("len", c(20,30))),
  defaults))

for(i in seq_along(defaults$area))  {
  igfs.SI1b[[i]]$number[igfs.SI1b[[i]]$year %in% 1996:2004] <- 
    1.25*igfs.SI1b[[i]]$number[igfs.SI1b[[i]]$year %in% 1996:2004]
}

igfs.SI1c <- mfdb_sample_count(mdb, c('length'), c(list(
  sampling_type = 'IGFS',
  length = mfdb_interval("len", c(30,40))),
  defaults))

for(i in seq_along(defaults$area))  {
  igfs.SI1c[[i]]$number[igfs.SI1c[[i]]$year %in% 1996:2004] <- 
    1.25*igfs.SI1c[[i]]$number[igfs.SI1c[[i]]$year %in% 1996:2004]
}

igfs.SI2a <- mfdb_sample_count(mdb, c('length'), c(list(
  sampling_type = 'IGFS',
  length = mfdb_interval("len", c(40,50))),
  defaults))

for(i in seq_along(defaults$area))  {
  igfs.SI2a[[i]]$number[igfs.SI2a[[i]]$year %in% 1996:2004] <- 
    1.17*igfs.SI2a[[i]]$number[igfs.SI2a[[i]]$year %in% 1996:2004]
}

igfs.SI2b <- mfdb_sample_count(mdb, c('length'), c(list(
  sampling_type = 'IGFS',
  length = mfdb_interval("len", c(50,60))),
  defaults))

for(i in seq_along(defaults$area))  {
  igfs.SI2b[[i]]$number[igfs.SI2b[[i]]$year %in% 1996:2004] <- 
    1.16*igfs.SI2b[[i]]$number[igfs.SI2b[[i]]$year %in% 1996:2004]
}

igfs.SI2c <- mfdb_sample_count(mdb, c('length'), c(list(
  sampling_type = 'IGFS',
  length = mfdb_interval("len", c(60,70))),
  defaults))

for(i in seq_along(defaults$area))  {
  igfs.SI2c[[i]]$number[igfs.SI2c[[i]]$year %in% 1996:2004] <- 
    1.14*igfs.SI2c[[i]]$number[igfs.SI2c[[i]]$year %in% 1996:2004]
}


igfs.SI3a <- mfdb_sample_count(mdb, c('length'), c(list(
  sampling_type = 'IGFS',
  length = mfdb_interval("len", c(70,110),open_ended = 'upper')),
  defaults))

igfs.SI3a[[i]]$number[igfs.SI3a[[i]]$year %in% 1996:2004] <- 
  1.05*igfs.SI3a[[i]]$number[igfs.SI3a[[i]]$year %in% 1996:2004]

