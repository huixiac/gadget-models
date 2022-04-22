#devtools::install_github('mareframe/mfdb', ref='6.x', force = TRUE)
#devtools::install_github('fishvice/mar', force = T)
#devtools::install_github('Hafro/rgadget', force = T)
#install.packages('stringi')
#devtools::install_github()
#library(remotes)
#install_version("infuser", "0.2.8")
library(stringi)
library(tidyverse)
library(Rgadget)
library(mfdb)
library(infuser)

bootstrap <- TRUE

## Create a gadget directory, define some defaults to use with our queries below
#gd <- gadget_directory("~/lemon gadget")
mdb<-mfdb('iceland')

base_dir <- 'lemon gadget'

for(i in 1:2){
vers <- c('45-base', '46-databefore2001')[i]
gd <- gadget.variant.dir(sprintf(paste0("%s/",vers),base_dir))

year_range <- 1990:2018
mat_stock <- 'lemmat'
imm_stock <- 'lemimm'
stock_names <- c(imm_stock,mat_stock)
species_name <- 'lemonsole'

reitmapping <- 
  read.table(
        system.file("demo-data", "reitmapping.tsv", package="mfdb"),
        header=TRUE,
        as.is=TRUE)

defaults <- list(
    area = mfdb_group("1" = unique(reitmapping$SUBDIVISION)),
    timestep = mfdb_timestep_quarterly,
    year = year_range,
    species = 'LEM')

schedule <- 
  expand.grid(year = year_range, step = 1:4) %>% 
  arrange(year)

gadgetfile('Modelfiles/time',
           file_type = 'time',
           components = list(list(firstyear = min(schedule$year),
                                  firststep=min(schedule$step),
                                  lastyear=max(schedule$year),
                                  laststep=max(schedule$step),
                                  notimesteps=c(4,3,3,3,3)))) %>% 
  write.gadget.file(gd)


## Write out areafile and update mainfile with areafile location

gadgetfile('Modelfiles/area',
           file_type = 'area',
           components = list(list(areas = 1,
                                  size = 1,
                                  temperature= schedule %>% mutate(area = 1, temperature = 5)))) %>% 
  write.gadget.file(gd)

source(paste0(base_dir,'/00-setup/setup-fleets.R'))
source(paste0(base_dir,'/00-setup/setup-model.R'))
source(paste0(base_dir,'/00-setup/setup-catchdistribution.R'))
source(paste0(base_dir,'/00-setup/setup-indices.R'))
source(paste0(base_dir,'/00-setup/setup-likelihood.R'))

Sys.setenv(GADGET_WORKING_DIR=normalizePath(gd))
#callGadget(s=1,i='params.in',p='params.init', log = 'init.log2') #just for testing
callGadget(l=1,i='params.in',p='params.init') # this for setup

# if(FALSE){
#   source('00-setup/setup-fixed_slope.R')
#   ## setting up model variants
#   source('00-setup/setup-est_slope.R')
#   #source('00-setup/setup-three_fleets.R')
#   source('00-setup/setup-single_fleet.R')
# }


if(bootstrap){
  source(paste0(base_dir,'/00-setup/setup-bootstrap.R'))
  file.copy(sprintf('%s/bootrun.R','00-setup'),gd)
}

#file.copy(sprintf('%s/itterfitter.sh','lemon gadget/00-setup'),gd)
file.copy(sprintf('%s/run.R','lemon gadget/00-setup'),gd)
file.copy(sprintf('%s/optinfofile','lemon gadget/00-setup'),gd)

}




