gadgetlikelihood('likelihood',gd$dir,missingOkay = TRUE) %>% 
  ## Write a penalty component to the likelihood file
  gadget_update("penalty",
                name = "bounds",
                weight = "0.5",
                data = data.frame(
                  switch = c("default"),
                  power = c(2),
                  upperW=10000,
                  lowerW=10000,
                  stringsAsFactors = FALSE)) %>%
  gadget_update("understocking",
                name = "understocking",
                weight = "100") %>% #
  gadget_update("catchdistribution",
                name = "ldist.igfs",
                weight = 1,
                data = ldist.igfs[[1]],
                fleetnames = c("igfs"),
                stocknames =stock_names) %>% #  
  gadget_update("catchdistribution",
                name = "aldist.igfs",
                weight = 1,
                data = aldist.igfs[[1]],
                fleetnames = c("igfs"),
                stocknames =stock_names) %>% 
  gadget_update("stockdistribution",
                name = "matp.igfs",
                weight = 1,
                data = matp.igfs[[1]],
                fleetnames = c("igfs"),
                stocknames =stock_names) %>% 
  gadget_update("catchdistribution",
                name = "ldist.comm",
                weight = 1,
                data = ldist.comm[[1]],
                fleetnames = c("comm"),
                stocknames =stock_names) %>% 
  gadget_update("catchdistribution",
                name = "aldist.comm",
                weight = 1,
                data = aldist.comm[[1]],
                fleetnames = c("comm"),
                stocknames =stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.10-40",
                weight = 1,
                data = igfs.SI1[[1]],
                fittype = 'fixedslopeloglinearfit',
                slope=1,
                stocknames = stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.40-70",
                weight = 1,
                data = igfs.SI2[[1]],
                fittype = 'fixedslopeloglinearfit',
                slope=1,
                stocknames = stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.70-110",
                weight = 1,
                data = igfs.SI3[[1]],
                fittype = 'fixedslopeloglinearfit',
                slope=1,
                stocknames = stock_names) %>% 
  write.gadget.file(gd$dir)
