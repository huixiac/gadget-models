library(mfdb)
library(tidyverse)
#install.packages("devtools")
#devtools::install_github("hafro/rgadget")
library(Rgadget)
library(broom)
library(infuser)
bootstrap <- FALSE
## Create a gadget directory, define some defaults to use with our queries below
gd <- gadget_directory("01-firsttry")
mdb<-mfdb('Iceland')#,db_params=list(host='hafgeimur.hafro.is'))

year_range <- 1979:2016
base_dir <- '02-haddock'
mat_stock <- 'hadmat'
imm_stock <- 'hadimm'
stock_names <- c(imm_stock,mat_stock)
species_name <- 'haddock'

reitmapping <- 
  read.table(
        system.file("demo-data", "reitmapping.tsv", package="mfdb"),
        header=TRUE,
        as.is=TRUE)

mfdb_timestep_by6<-mfdb_timestep_quarterly
mfdb_timestep_by6$'1'<-c(1,2); mfdb_timestep_by6$'2'<-c(3,4); mfdb_timestep_by6$'3'<-c(5,6); 
mfdb_timestep_by6$'4'<-c(7,8); mfdb_timestep_by6$'5'<-c(9,10); mfdb_timestep_by6$'6'<-c(11,12)
#maybe table name needs to be changed

defaults <- list(
    area = mfdb_group("1" = unique(reitmapping$SUBDIVISION)),
    timestep = mfdb_timestep_by6,
    year = year_range,
    species = 'HAD')

early<-c(1:7)

defaults_early <- list(
  area = mfdb_group("1" = unique(reitmapping$SUBDIVISION)),
  timestep = mfdb_timestep_by6,
  year = year_range[early],
  species = 'HAD')

defaults_late <- list(
  area = mfdb_group("1" = unique(reitmapping$SUBDIVISION)),
  timestep = mfdb_timestep_by6,
  year = year_range[-early],
  species = 'HAD')

gadgetfile('Modelfiles/time',
           file_type = 'time',
           components = list(list(firstyear = min(defaults$year),
                                  firststep=1,
                                  lastyear=max(defaults$year),
                                  laststep=6,
                                  notimesteps=c(6,2,2,2,2,2,2)))) %>% 
  write.gadget.file(gd$dir)

source('00-setup/setup-timevariablefiles.R')


## Write out areafile and update mainfile with areafile location
gadget_areafile(
  size = mfdb_area_size(mdb, defaults)[[1]],
  temperature = mfdb_temperature(mdb, defaults)[[1]]) %>% 
gadget_dir_write(gd,.)

source('../R/utils.R')
source('00-setup/setup-fleets.R')
source('00-setup/setup-model.R')
#fix M parameters in stock files here
source('00-setup/setup-catchdistribution.R')
source('00-setup/setup-indices.R')
source('00-setup/setup-likelihood.R')

Sys.setenv(GADGET_WORKING_DIR=normalizePath(gd$dir))
callGadget(l=1,i='params.in',p='params.init')

#fix M values in params.init file

if(FALSE){
  source('00-setup/setup-fixed_slope.R')
  ## setting up model variants
  source('00-setup/setup-est_slope.R')
  #source('00-setup/setup-three_fleets.R')
  source('00-setup/setup-single_fleet.R')
}


if(bootstrap){
  source('00-setup/setup-bootstrap.R')
  file.copy(sprintf('%s/bootrun.R','00-setup'),gd$dir)
}

file.copy(sprintf('%s/itterfitter.sh','00-setup'),gd$dir)
file.copy(sprintf('%s/run.R','/00-setup'),gd$dir)
file.copy(sprintf('%s/optinfofile','00-setup'),gd$dir)
#file.copy(sprintf('%s/run-fixed_slope.R','00-setup'),gd$dir)
