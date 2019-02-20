library(mfdb)
library(tidyverse)
library(Rgadget)
source('R/utils.R')
bootstrap <- FALSE

setwd('/home/pamela/Documents/Hafro/fishvice/gadget-models/')
Sys.setenv(GADGET_WORKING_DIR=getwd())

## Create a gadget directory, define some defaults to use with our queries below
#gd <- gadget_directory("09-wolffish/12-new_ass")
#mdb<-mfdb('Iceland',db_params=list(host='hafgeimur.hafro.is'))
mdb<-mfdb('Iceland')
vers <- c('01-firsttry', '02-growth_rest')[1]
year_range <- 1982:2018
base_dir <- '09-wolffish'
mat_stock <- 'wolfmat'
imm_stock <- 'wolfimm'
stock_names <- c(imm_stock,mat_stock)
species_name <- 'wolffish'
## Create a gadget directory, define some defaults to use with our queries below
gd <- gadget_directory(sprintf(paste0("%s/",vers),base_dir))
#mdb<-mfdb('Iceland')#,db_params=list(host='hafgeimur.hafro.is'))

reitmapping <- 
  read.table(
        system.file("demo-data", "reitmapping.tsv", package="mfdb"),
        header=TRUE,
        as.is=TRUE)

defaults <- list(
    area = mfdb_group("1" = unique(reitmapping$SUBDIVISION)),
    timestep = mfdb_timestep_quarterly,
    year = year_range,
    species = 'CAA')


gadgetfile('Modelfiles/time',
           file_type = 'time',
           components = list(list(firstyear = min(defaults$year),
                                  firststep=1,
                                  lastyear=max(defaults$year),
                                  laststep=4,
                                  notimesteps=c(4,3,3,3,3)))) %>% 
  write.gadget.file(gd$dir)

## Write out areafile and update mainfile with areafile location
gadget_areafile(
  size = mfdb_area_size(mdb, defaults)[[1]],
  temperature = expand.grid(year=defaults$year,
                            step = as.numeric(names(defaults$timestep))) %>%
                as.data.frame() %>% 
                mutate(area = 1, mean = 3) %>% 
                arrange(year, step)
    ) %>% 
  #temperature = mfdb_temperature(mdb, defaults)[[1]]) %>% #mfdb_temperature seems to not be working the same
  gadget_dir_write(gd,.)

gadgetfile('Modelfiles/timevariableK.mat',
           file_type = 'timevariable',
           components = list(list('annualgrowth',
                                  data= data_frame(year = rep(year_range, each=4), 
                                                   step = rep(1:4, length(year_range)), 
                                                   value = parse(text=sprintf(rep('0.001*wolfmat.k.%s', length(year_range)*4),rep(1:4, 
                                                                                                          length(year_range)))) %>%
                                                     map(to.gadget.formulae)%>% 
                                                     unlist()))
           )) %>% 
  write.gadget.file(gd$dir)

source('R/utils.R')
source('09-wolffish/00-setup/setup-fleets.R')
if(vers=='02-growth_rest'|vers=='05-2017noage_growth_rest'){
  source(sprintf('%s/00-setup/setup-model_growth_rest.R',base_dir))
} else {
  source(sprintf('%s/00-setup/setup-model.R',base_dir))}
source('09-wolffish/00-setup/setup-catchdistribution.R')
source('09-wolffish/00-setup/setup-indices.R')
source('09-wolffish/00-setup/setup-likelihood.R')

Sys.setenv(GADGET_WORKING_DIR=normalizePath(gd$dir))
callGadget(s=1,i='params.in',p='params.init', log = 'init.log2')
callGadget(l=1,i='params.in',p='params.init')

if(FALSE){
  source('09-wolffish/00-setup/setup-fixed_slope.R')
  ## setting up model variants
  source('09-wolffish/00-setup/setup-est_slope.R')
  #source('09-wolffish/00-setup/setup-three_fleets.R')
  source('09-wolffish/00-setup/setup-single_fleet.R')
}


if(bootstrap){
  source('09-wolffish/00-setup/setup-bootstrap.R')
  file.copy(sprintf('%s/bootrun.R','09-wolffish/00-setup'),gd$dir)
}

file.copy(sprintf('%s/itterfitter.sh','09-wolffish/00-setup'),gd$dir)
file.copy(sprintf('%s/run.R','09-wolffish/00-setup'),gd$dir)
file.copy(sprintf('%s/optinfofile','09-wolffish/00-setup'),gd$dir)
#file.copy(sprintf('%s/run-fixed_slope.R','09-wolffish/00-setup'),gd$dir)
