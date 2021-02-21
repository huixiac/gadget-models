# ---------------------------------------------------------------------
# Retrieve catches VEN for the period 1991-2018
com.catch.ven <- read.table("~/../valerio/Share/Gadget/vendace/SS3_input/data/commercial_catch.dat", sep="\t", header=T)
tmp <- expand.grid(year=1991:2018,step=1:4,area=1,fleet="comven")
com.catch.ven <- left_join(tmp,com.catch.ven,by="year") %>%
                 arrange(year,step) %>%
                 mutate(biomass=biomass * 1000, # convert in kg
                        biomass=biomass * # all catches assigned to quarter4
                                rep(c(0,0,0,1), length(1991:2018))) %>%
                 mutate(fleet=ifelse(year<2008,"comven1","comven2")) # introduction of grid in ~2008

# ---------------------------------------------------------------------
# Retrieve biomass removed by SEAL for the period 1991-2018
seal.cons.ven <- read.table("~/../valerio/Share/Gadget/vendace/SS3_input/data/seal_consumption_vendace_high.dat", sep="\t", header=T)
tmp <- expand.grid(year=1991:2018,step=1:4,area=1,fleet="sealven")
seal.cons.ven <- left_join(tmp,seal.cons.ven,by="year") %>%
                 arrange(year,step) %>%
                 mutate(biomass=biomass * 1000, # convert in kg
                        biomass=biomass * # partition consumption by seal 25% on each quarter
                                rep(c(0.25,0.25,0.25,0.25), length(1991:2018)))


# ---------------------------------------------------------------------
# make survey
aco.catch <- 
  structure(rbind(data.frame(year=2009:2012,step=3,area=1,fleet="aco",number=1), # month 9.3
                  data.frame(year=2013:2014,step=4,area=1,fleet="aco",number=1), # month 11.0
                  data.frame(year=2015:2018,step=4,area=1,fleet="aco",number=1)), # month 10.4
            area_group = mfdb_group(`1` = 1))
