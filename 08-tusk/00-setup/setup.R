library(mfdb)
library(tidyverse)
library(Rgadget)
source('R/utils.R')
bootstrap <- FALSE
## Create a gadget directory, define some defaults to use with our queries below

base_dir <- '08-tusk'
mat_stock <- 'tuskmat'
imm_stock <- 'tuskimm'
stock_names <- c(imm_stock,mat_stock)
species_name <- 'tusk'


gd <- gadget_directory(sprintf("%s/02-mod",base_dir))
mdb<-mfdb('Iceland')#,db_params=list(host='hafgeimur.hafro.is'))
year_range <- 1982:2016

reitmapping <- 
  read.table(
        system.file("demo-data", "reitmapping.tsv", package="mfdb"),
        header=TRUE,
        as.is=TRUE)

defaults <- list(
    area = mfdb_group("1" = unique(reitmapping$SUBDIVISION)),
    timestep = mfdb_timestep_quarterly,
    year = year_range,
    species = 'USK')


gadgetfile('Modelfiles/time',
           file_type = 'time',
           components = list(list(firstyear = min(year_range),
                                  firststep= 1,
                                  lastyear= max(year_range),
                                  laststep= 4,
                                  notimesteps=c(4,3,3,3,3)))) %>% 
  write.gadget.file(gd$dir)

## Write out areafile and update mainfile with areafile location
gadget_areafile(
  size = mfdb_area_size(mdb, defaults)[[1]],
  temperature = mfdb_temperature(mdb, defaults)[[1]]) %>% 
gadget_dir_write(gd,.)

source(sprintf('%s/00-setup/setup-fleets.R',base_dir))
source(sprintf('%s/00-setup/setup-model.R',base_dir))
source(sprintf('%s/00-setup/setup-catchdistribution.R',base_dir))
source(sprintf('%s/00-setup/setup-indices.R',base_dir))
source(sprintf('%s/00-setup/setup-likelihood.R',base_dir))

Sys.setenv(GADGET_WORKING_DIR=normalizePath(gd$dir))
callGadget(l=1,i='params.in',p='params.init')

## setting up model variants

if(bootstrap){
  source(sprintf('%s/00-setup/setup-bootstrap.R',base_dir))
  file.copy(sprintf('%s/%s/bootrun.R',base_dir,'00-setup'),gd$dir)
}

file.copy(sprintf('%s/%s/itterfitter.sh',base_dir,'00-setup'),gd$dir)
file.copy(sprintf('%s/%s/run.R',base_dir,'00-setup'),gd$dir)
file.copy(sprintf('%s/%s/optinfofile',base_dir,'00-setup'),gd$dir)
