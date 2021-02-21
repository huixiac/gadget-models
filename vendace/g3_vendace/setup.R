library(mfdb)
library(tidyverse)
library(gadget3)

rm(list=ls())

# connect to the database and load ICES rect and subd for convenience
mdb <- mfdb('Baltic', db_params=list(dbname="bothnia"))
source("area_units.R")

year_range <- 1991:2018

# define 'default' spatial and temporal aggregation
defaults.ven <- list(
    area = mfdb_group('1'=as.character(squares$ICES_Rectangle[squares$SD %in% 30:31])),
    timestep = mfdb_timestep_quarterly,
    year = year_range,
    species = 'FVE')

time_actions <-  list(
    g3a_time(start_year = min(defaults.ven$year), 
             end_year = max(defaults.ven$year), 
             defaults.ven$timestep),
    list())

areas <- structure(
    seq_along(defaults.ven$area),
    names = names(defaults.ven$area))

# -------------------------
# setup the model blue print

## modelled stocks
ven_imm <- 
  g3_stock('ven_imm', seq(3.5, 17.5, 0.5)) %>%
  g3s_livesonareas(areas[c('1')]) %>%
  g3s_age(0, 1)

ven_mat <- 
  g3_stock('ven_mat', seq(5.5, 20.5, 0.5)) %>%
  g3s_livesonareas(areas[c('1')]) %>%
  g3s_age(0, 10)

## fleets

acoven <- 
  g3_fleet('acoven') %>% 
  g3s_livesonareas(areas[c('1')])

comven1 <- 
  g3_fleet('comven1') %>% 
  g3s_livesonareas(areas[c('1')])

comven2 <- 
  g3_fleet('comven2') %>% 
  g3s_livesonareas(areas[c('1')])

sealven <- 
  g3_fleet('sealven') %>% 
  g3s_livesonareas(areas[c('1')])


source("setup_fleets_data.R")
source("setup_catchdistribution.R")
source("setup_indices.R")
source("setup_initial_parameters.R")

source("setup_fleets.R")
source("setup_model.R")
source("setup_likelihood.R")


## set up reporting:

imm_report <- g3s_clone(ven_imm, 'imm_report') %>%
  g3s_time(year = local(year_range), step = 1:4)

mat_report <- g3s_clone(ven_mat, 'mat_report') %>%
  g3s_time(year = local(year_range), step = 1:4)


report_actions <- list(
    # Report numbers
    g3a_report_stock(imm_report,ven_imm, ~stock_ss(ven_imm__num)),
    g3a_report_stock(mat_report,ven_mat, ~stock_ss(ven_mat__num)),
    # Report mean weight
    g3a_report_stock(imm_report,ven_imm, ~stock_ss(ven_imm__wgt)),
    g3a_report_stock(mat_report,ven_mat, ~stock_ss(ven_mat__wgt)),
    # Report biomass caught by acoven
    g3a_report_stock(mat_report,ven_mat, ~stock_ss(ven_mat__acoven)),
    g3a_report_stock(imm_report,ven_imm, ~stock_ss(ven_imm__acoven)),
    # Report biomass caught by comven1
    g3a_report_stock(imm_report,ven_imm, ~stock_ss(ven_imm__comven1)),
    g3a_report_stock(mat_report,ven_mat, ~stock_ss(ven_mat__comven1)),
    # Report biomass caught by comven2
    g3a_report_stock(imm_report,ven_imm, ~stock_ss(ven_imm__comven2)),
    g3a_report_stock(mat_report,ven_mat, ~stock_ss(ven_mat__comven2)),
    # Report biomass consumed by seal
    g3a_report_stock(imm_report,ven_imm, ~stock_ss(ven_imm__sealven)),
    g3a_report_stock(mat_report,ven_mat, ~stock_ss(ven_mat__sealven)))

ven_model <- g3_to_r(c(
  ven_mat_actions,
  ven_imm_actions,
  fleet_actions,
  ven_likelihood_actions,
  report_actions,  # unactivate if problems with memory
  time_actions), strict = TRUE)

## update initial parameters with better guesses
ven_param <- attr(ven_model, 'parameter_template')
ven_param[["ven.Linf"]] <- vb.constants.ven$Linf
ven_param[["ven.k"]]    <- 1e3 * vb.constants.ven$k
ven_param[["ven.bbin"]] <- 0.9
ven_param[["ven.recl"]] <- init.sigma.ven$ml[1]
ven_param[["venimm.init.scalar"]] <- 1e3
ven_param[["venmat.init.scalar"]] <- 1e3
ven_param[["ven.init.sd"]] <- init.sigma.ven$ms
ven_param[["ven.rec.scalar"]] <- 1e3
ven_param[["ven.mat1"]] <- mat.constants.ven$a
ven_param[["ven.mat2"]] <- mat.constants.ven$l50
ven_param[["ven.init.F"]] <- 0.3
ven_param[["venimm.walpha"]] <- 6.4227e-06
ven_param[["venimm.wbeta"]] <- lw.constants.ven$b
ven_param[["venmat.walpha"]] <- 6.4227e-06
ven_param[["venmat.wbeta"]] <- lw.constants.ven$b
ven_param[["ven.aco.alpha"]]  <- 0.7
ven_param[["ven.aco.l50"]]    <- 11.5 
ven_param[["ven.com1.alpha"]] <- 0.7
ven_param[["ven.com1.l50"]]   <- 11.5
ven_param[["ven.com2.alpha"]] <- 0.7
ven_param[["ven.com2.l50"]]   <- 11.5
ven_param[["ven.sea.alpha"]] <- 0.7
ven_param[["ven.sea.l50"]]   <- 11.5
ven_param[["ven_si_alpha1"]] <- 2.7e-5
ven_param[["ven_si_alpha2"]] <- 2.7e-5
ven_param[grepl('^venimm\\.M\\.', names(ven_param))] <- 0.2
ven_param[grepl('^venimm\\.init\\.', names(ven_param))] <- 1
ven_param[grepl('^venmat\\.M\\.', names(ven_param))] <- 0.2
ven_param[grepl('^venmat\\.init\\.', names(ven_param))] <- 1
ven_param[grepl('^ven\\.rec\\.', names(ven_param))] <- 1

# You can edit the model code with:
#ling_model <- edit(ling_model)

# Run model with params above
result <- ven_model(ven_param)
result[[1]]
# List all available reports
print(names(attributes(result)))


##### Run TMB-based model #####################################################

# Turn actions into C++ objective function code
tmb_ven <- g3_to_tmb(c(
  ven_mat_actions,
  ven_imm_actions,
  fleet_actions,
  ven_likelihood_actions,
  time_actions))


## update initial parameters with better guesses
tmb_param <- attr(tmb_ven, 'parameter_template')

tmb_param$value <- I(ven_param[rownames(tmb_param)])

## pp <- Rgadget::read.gadget.parameters("~/../valerio/Share/Gadget/vendace/vendace_13.3/vendace01/WGTS/params.final")
## ven_param <- as.list(pp$value)
## names(ven_param) <- pp$switch

tmb_param$value[which(!is.na(match(rownames(tmb_param), pp$switch)))] <- I(pp$value[na.omit(match(rownames(tmb_param), pp$switch))])
tmb_param$lower <- vapply(tmb_param$value, function (x) 0.5 * x[[1]], numeric(1))
tmb_param$upper <- vapply(tmb_param$value, function (x) 2 * x[[1]], numeric(1))

# Disable optimisation for some parameters, to make life easier
tmb_param[c('venimm.walpha',
            'venimm.wbeta',
            'venmat.walpha',
            'venmat.wbeta',
            'ven.Linf'),]$optimise <- FALSE
tmb_param[grepl('^venimm\\.M\\.', rownames(tmb_param)),]$optimise <- FALSE
tmb_param[grepl('^venmat\\.M\\.', rownames(tmb_param)),]$optimise <- FALSE

# Compile and generate TMB ADFun (see ?TMB::MakeADFun)
ven_model_tmb <- g3_tmb_adfun(tmb_ven,tmb_param)
# writeLines(TMB::gdbsource(g3_tmb_adfun(tmb_ling, tmb_param, compile_flags = "-g", output_script = TRUE)))
# Run model once, using g3_tmb_par to reshape tmb_param into param vector.
# Will return nll
ven_model_tmb$fn(g3_tmb_par(tmb_param))

# Run model once, returning model report
ling_model_tmb$report(g3_tmb_par(tmb_param))




# set initial params and bounds
param_names <- rownames(tmb_param)

ven_param <- tmb_param$value ## "~/../valerio/Share/Gadget/vendace/vendace_13.3/"


environment(ven_model)$model_report -> tmp


# set upper and lower bounds, both default and ad hoc 
tmb_param$lower <- vapply(tmb_param$value, function (x) 0.5 * x[[1]], numeric(1))
tmb_param$upper <- vapply(tmb_param$value, function (x) 2 * x[[1]], numeric(1))

tmb_param['ven.rec', c("lower","upper")] <-
    tmb_param['venimm.init', c("lower","upper")] <-
        tmb_param['venmat.init', c("lower","upper")] <- c(0.001,100)

tmb_param['venimm.init.scalar', c("lower","upper")] <-
    tmb_param['venmat.init.scalar', c("lower","upper")] <-
        tmb_param['ven.rec.scalar', c("lower","upper")] <- c(1,1e8)

# set which parameters to optimse
tmb_param[c("ven.Linf",
            "ven.init.sd",
            "venimm.M",
            "venmat.M",
            "ven.init.F",
            "venimm.walpha",
            "venimm.wbeta",
            "venmat.walpha",
            "venmat.wbeta",
            "ven.sea.alpha",
            "venimm.init.scalar",
            "venmat.init.scalar",
            "ven.rec.scalar"),]$optimise <- FALSE
     
ven_model_tmb <- g3_tmb_adfun(tmb_ven,tmb_param)

ven_model_tmb$fn(g3_tmb_par(tmb_param))


## # alternative
## selCol <- c("value","lower","upper")
## tmb_param["ven.Linf",selCol] <- c(vb.constants.ven$Linf, 12, 20)
## tmb_param["ven.k",selCol]    <- c(1e3 * vb.constants.ven$k, 0.1, 100)
## tmb_param["ven.bbin",selCol] <- c(0.9, 0.001, 50)

## tmb_param["ven.recl",selCol] <- c(init.sigma.ven$ml[1], 5, 15)

## tmb_param["venimm.init.scalar",selCol] <- c(1e3, 1, 1e8)
## tmb_param["venmat.init.scalar",selCol] <- c(1e3, 1, 1e8)
## tmb_param["ven.init.sd","value"] <- init.sigma.ven$ms

## tmb_param["ven.rec.scalar",selCol] <- c(1e3, 1, 1e8)
## tmb_param['ven.rec', "value"] <- rep(1, length(year_range))

## tmb_param["venimm.M", "value"] <- rep(0.2,2)
## tmb_param["venmat.M", "value"] <- rep(0.2,11)

## tmb_param["ven.mat1", selCol] <- c(mat.constants.ven$a, 0.1, 10)
## tmb_param["ven.mat2", selCol] <- c(mat.constants.ven$l50, 1, 30)

## tmb_param["ven.init.F",selCol] <- c(0.3, 0.1, 1)

## tmb_param["venimm.init", "value"] <- rep(1,1) 
## tmb_param["venmat.init", "value"] <- rep(1,9) # 0:8 used in initPop

## tmb_param["venimm.walpha",selCol] <- c(lw.constants.ven$a, 1e-10, 1)
## tmb_param["venimm.wbeta",selCol] <- c(lw.constants.ven$b, 2, 4)
## tmb_param["venmat.walpha",selCol] <- c(lw.constants.ven$a, 1e-10, 1)
## tmb_param["venmat.wbeta",selCol] <- c(lw.constants.ven$b, 2, 4)

## tmb_param["ven.aco.alpha",selCol] <- c(0.7,  0.01, 3)
## tmb_param["ven.aco.l50",selCol] <- c(11.5, 2, 20)
## tmb_param["ven.com1.alpha",selCol] <- c(0.7,  0.01, 3)
## tmb_param["ven.com1.l50",selCol] <- c(11.5, 2, 20)
## tmb_param["ven.com2.alpha",selCol] <- c(0.7,  0.01, 3)
## tmb_param["ven.com2.l50",selCol] <- c(11.5, 2, 20)
## tmb_param["ven.sea.alpha",selCol] <- c(0.7,  0.01, 3)
## tmb_param["ven.sea.l50",selCol] <- c(11.5, 2, 20)

## tmb_param["ven_si_alpha1",selCol] <- c(2.7e-5, 1e-8, 1)
## tmb_param["ven_si_alpha2",selCol] <- c(2.7e-5, 1e-8, 1)
