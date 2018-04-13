## setting up a bootstrap run

defaults <- 
  within(defaults,
         {area = mfdb_bootstrap_group(100,defaults$area,seed=1337)})

source(sprintf('%s/00-setup/setup-catchdistribution.R',base_dir))
source(sprintf('%s/00-setup/setup-indices.R',base_dir))
save.image(file=sprintf('%s/00-setup/bootstrap-data.Rdata',base_dir))

boot_setup <- function(i){
  var_dir <- gadget.variant.dir(gd$dir, variant_dir = paste0('BS.WGTS/BS.', i))
  
  aldist.igfs[[i]]$step <- 2
  ldist.igfs[[i]]$step <- 2
  matp.igfs[[i]]$step <- 2
  
  gadgetlikelihood('likelihood',var_dir,missingOkay = TRUE) %>% 
    gadget_update("catchdistribution",
                  name = "ldist.igfs",
                  weight = 1,
                  data = ldist.igfs[[i]],
                  fleetnames = c("igfs"),
                  stocknames =stock_names) %>% #  
    gadget_update("catchdistribution",
                  name = "aldist.igfs",
                  weight = 1,
                  data = aldist.igfs[[i]] ,
                  fleetnames = c("igfs"),
                  stocknames =stock_names) %>% 
    gadget_update("stockdistribution",
                  name = "matp.igfs",
                  weight = 1,
                  data = matp.igfs[[i]] ,
                  fleetnames = c("igfs"),
                  stocknames =stock_names) %>% 
    gadget_update("catchdistribution",
                  name = "ldist.comm",
                  weight = 1,
                  data = ldist.comm[[i]],
                  fleetnames = c("comm"),
                  stocknames =stock_names) %>% 
    gadget_update("catchdistribution",
                  name = "aldist.comm",
                  weight = 1,
                  data = aldist.comm[[i]],
                  fleetnames = c("comm"),
                  stocknames =stock_names) %>% 
    gadget_update("surveyindices",
                  name = "si.10-20",
                  weight = 1,
                  data = igfs.SI1a[[i]],
                  fittype = 'fixedslopeloglinearfit',
                  slope=1,
                  stocknames = imm_stock) %>%  
    gadget_update("surveyindices",
                  name = "si.20-30",
                  weight = 1,
                  data = igfs.SI1b[[i]],
                  fittype = 'fixedslopeloglinearfit',
                  slope=1,
                  stocknames = stock_names) %>%  
    gadget_update("surveyindices",
                  name = "si.30-40",
                  weight = 1,
                  data = igfs.SI1c[[i]],
                  fittype = 'fixedslopeloglinearfit',
                  slope=1,
                  stocknames = stock_names) %>% 
    gadget_update("surveyindices",
                  name = "si.40-50",
                  weight = 1,
                  data = igfs.SI2a[[i]],
                  fittype = 'fixedslopeloglinearfit',
                  slope=1,
                  stocknames = stock_names) %>% 
    gadget_update("surveyindices",
                  name = "si.50-60",
                  weight = 1,
                  data = igfs.SI2b[[i]],
                  fittype = 'fixedslopeloglinearfit',
                  slope=1,
                  stocknames = stock_names) %>% 
    gadget_update("surveyindices",
                  name = "si.60-70",
                  weight = 1,
                  data = igfs.SI2c[[i]],
                  fittype = 'fixedslopeloglinearfit',
                  slope=1,
                  stocknames = stock_names) %>% 
    gadget_update("surveyindices",
                  name = "si.70-110",
                  weight = 1,
                  data = igfs.SI3a[[i]],
                  fittype = 'fixedslopeloglinearfit',
                  slope=1,
                  stocknames = stock_names) -> tmp
  attr(tmp,'file_config')$mainfile_overwrite = TRUE
  write.gadget.file(tmp,var_dir)
}

tmp <- 
  mclapply(seq_along(defaults$area),
           boot_setup,
           mc.cores = detectCores(logical = TRUE))
    

