library(mfdb)
library(tidyverse)
library(Rgadget)

rm(list=ls())

## Create a gadget directory, define some defaults to use with our queries below
dirName <- "vendace02"
## system(paste("rm -r", dirName))
if(sum(match(list.files(),dirName), na.rm=T)==1){
    print(paste("folder",dirName,"exists"))
} else {gd <- gadget_directory(dirName)}

mdb <- mfdb('Baltic', db_params=list(dbname="bothnia"))

# load ICES rect and subd for convenience
source("area_units.R")

year_range <- 1991:2018
base_dir <- 'vendace'
mat_stock <- 'venmat'
imm_stock <- 'venimm'
stock_names <- c(imm_stock,mat_stock)
species_name <- 'vendace'

# define 'default' spatial and temporal aggregation
defaults.ven <- list(
    area = mfdb_group('area1'=as.character(squares$ICES_Rectangle[squares$SD %in% 30:31])),
    timestep = mfdb_timestep_quarterly,
    year = year_range,
    species = 'FVE')

gadgetfile('Modelfiles/time',
           file_type = 'time',
           components = list(list(firstyear = min(defaults.ven$year),
                                  firststep=1,
                                  lastyear=max(defaults.ven$year),
                                  laststep=4,
                                  notimesteps=c(4,3,3,3,3)))) %>% 
write.gadget.file(gd$dir)

## Write out areafile and update mainfile with areafile location
area <- expand.grid(year=min(defaults.ven$year):max(defaults.ven$year),
                    step=1:4,
                    area="area1",
                    mean=5)
area <- arrange(area,year,step)

gadget_areafile(
  size = mfdb_area_size(mdb, defaults.ven)[[1]],
  ## temperature = mfdb_temperature(mdb, defaults.ven)[[1]]) %>% 
  temperature = area) %>% 
gadget_dir_write(gd,.)

source('utils.R')
source('setup_fleets.R')
source('setup_model.R')
source('setup_catchdistribution.R')
source('setup_indices.R')
source('setup_likelihood.R')

Sys.setenv(GADGET_WORKING_DIR=normalizePath(gd$dir))
file.copy(sprintf('optinfofile','./'),gd$dir)


## callGadget(l=1,i='params.in',p='params.init', log='init.log')

## file.copy(sprintf('%s/itterfitter.sh','06-ling/00-setup'),gd$dir)
file.copy(sprintf('%s/run.R','./'),gd$dir)
## file.copy(sprintf('%s/optinfofile','06-ling/00-setup'),gd$dir)
## file.copy(sprintf('%s/run-fixed_slope.R','06-ling/00-setup'),gd$dir)
