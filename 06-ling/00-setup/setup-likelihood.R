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
                stocknames =stock_names) %>% 
  gadget_update("catchdistribution",
                name = "aldist.igfs",
                weight = 1,
                data = aldist.igfs[[1]] %>% ## only two age samples in 1989
                  filter(year!=1989),
                fleetnames = c("igfs"),
                stocknames =stock_names) %>% 
  gadget_update("catchdistribution",
                name = "ldist.lln",
                weight = 1,
                data = ldist.lln[[1]] %>% ## tow == 60228 was wrongly assigned, omit samples from that quarter
                  filter(year!=1993&step!=4),
                fleetnames = c("lln"),
                stocknames = stock_names) %>% 
  gadget_update("catchdistribution",
                name = "aldist.lln",
                weight = 1,
                data = aldist.lln[[1]],
                fleetnames = c("lln"),
                stocknames = stock_names) %>% 
  gadget_update("catchdistribution",
                name = "ldist.gil",
                weight = 1,
                data = ldist.gil[[1]],
                fleetnames = c("gil"),
                stocknames = stock_names) %>% 
  gadget_update("catchdistribution",
                name = "aldist.gil",
                weight = 1,
                data = aldist.gil[[1]],
                fleetnames = c("gil"),
                stocknames = stock_names) %>% 
  gadget_update("catchdistribution",
                name = "ldist.bmt",
                weight = 1,
                data = ldist.bmt[[1]],
                fleetnames = c("bmt"),
                stocknames = stock_names) %>% 
  gadget_update("catchdistribution",
                name = "aldist.bmt",
                weight = 1,
                data = aldist.bmt[[1]],
                fleetnames = c("bmt"),
                stocknames = stock_names) %>% 
  gadget_update("stockdistribution",
                name = "matp.igfs",
                weight = 1,
                data = matp.igfs[[1]],
                fleetnames = c("igfs"),
                stocknames =stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.20-50",
                weight = 1,
                data = igfs.SI1[[1]],
                fittype = 'loglinearfit',
                stocknames = stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.50-60",
                weight = 1,
                data = igfs.SI2a[[1]],
                fittype = 'loglinearfit',
                stocknames = stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.60-70",
                weight = 1,
                data = igfs.SI2b[[1]],
                fittype = 'loglinearfit',
                stocknames = stock_names) %>% 
    gadget_update("surveyindices",
                name = "si.70-80",
                weight = 1,
                data = igfs.SI3a[[1]],
                fittype = 'loglinearfit',
                stocknames = stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.80-90",
                weight = 1,
                data = igfs.SI3b[[1]],
                fittype = 'loglinearfit',
                stocknames = stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.90-100",
                weight = 1,
                data = igfs.SI3c[[1]],
                fittype = 'loglinearfit',
                stocknames = stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.100-160",
                weight = 1,
                data = igfs.SI3d[[1]],
                fittype = 'loglinearfit',
                stocknames = stock_names) %>% 
  write.gadget.file(gd$dir)
