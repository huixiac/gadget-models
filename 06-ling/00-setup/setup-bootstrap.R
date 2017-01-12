## setting up a bootstrap run

defaults <- 
  within(defaults,
         {area = mfdb_bootstrap_group(100,defaults$area,seed=1337)})

source('06-ling/00-setup/setup-catchdistribution.R')
source('06-ling/00-setup/setup-indices.R')
save.image(file='06-ling/00-setup/bootdata.R')

for(i in seq_along(defaults$area))  {
  var_dir <- gadget.variant.dir(gd$dir, variant_dir = paste0('BS.WGTS/BS.', i))
  gadgetlikelihood('likelihood',var_dir,missingOkay = TRUE) %>% 
    gadget_update("catchdistribution",
                  name = "ldist.igfs",
                  weight = 1,
                  data = igfs.ldist[[i]],
                  fleetnames = c("igfs"),
                  stocknames = c("lingimm", "lingmat")) %>% 
    gadget_update("catchdistribution",
                  name = "aldist.igfs",
                  weight = 1,
                  data = aldist.igfs[[i]],
                  fleetnames = c("igfs"),
                  stocknames = c("lingimm", "lingmat")) %>% 
    gadget_update("stockdistribution",
                  name = "matp.igfs",
                  weight = 1,
                  data = matp.igfs[[i]],
                  fleetnames = c("igfs"),
                  stocknames = c("lingimm", "lingmat")) %>% 
    gadget_update("catchdistribution",
                  name = "ldist.comm",
                  weight = 1,
                  data = ldist.comm[[i]],
                  fleetnames = c("comm"),
                  stocknames = c("lingimm", "lingmat")) %>% 
    gadget_update("catchdistribution",
                  name = "aldist.comm",
                  weight = 1,
                  data = aldist.comm[[i]],
                  fleetnames = c("comm"),
                  stocknames = c("lingimm", "lingmat")) %>% 
    gadget_update("surveyindices",
                  name = "si.20-50",
                  weight = 1,
                  data = igfs.SI1[[i]],
                  fittype = 'fixedslopeloglinearfit',
                  slope=1,
                  stocknames = c("lingimm","lingmat")) %>% 
    gadget_update("surveyindices",
                  name = "si.50-70",
                  weight = 1,
                  data = igfs.SI2[[i]],
                  fittype = 'fixedslopeloglinearfit',
                  slope=1,
                  stocknames = c("lingimm","lingmat")) %>% 
    gadget_update("surveyindices",
                  name = "si.70-180",
                  weight = 1,
                  data = igfs.SI3[[i]],
                  fittype = 'fixedslopeloglinearfit',
                  slope=1,
                  stocknames = c("lingimm","lingmat")) %>% 
    write.gadget.file(var_dir)
}

