## all age reading before 1999 are omitted

## weird inconsistencies in Gadget
aldist.igfs[[1]]$step <- 2
ldist.igfs[[1]]$step <- 2
matp.igfs[[1]]$step <- 2


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
                  #filter(year!=1989),
                  filter(year>1998),
                fleetnames = c("igfs"),
                stocknames =stock_names) %>% 
  gadget_update("catchdistribution",
                name = "ldist.lln",
                weight = 1,
                data = ldist.lln[[1]] %>% ## tow == 60228 was wrongly assigned, omit samples from that quarter
                  filter(!(year==1993&step==4)),
                fleetnames = c("lln"),
                stocknames = stock_names) %>% 
  gadget_update("catchdistribution",
                name = "aldist.lln",
                weight = 1,
                data = aldist.lln[[1]] %>%  ## only 20 fish aged taken in those quarters
                  filter(year>1998,!((year==2002|year==2003)&step==2)),
                fleetnames = c("lln"),
                stocknames = stock_names) %>% 
  gadget_update("catchdistribution",
                name = "ldist.gil",
                weight = 1,
                data = ldist.gil[[1]] %>% ## only one fish lengthmeasured
                  filter(!(year==2005&step==2)),
                fleetnames = c("gil"),
                stocknames = stock_names) %>% 
  gadget_update("catchdistribution",
                name = "aldist.gil",
                weight = 1,
                data = aldist.gil[[1]] %>% 
                  filter(year>1998),
                fleetnames = c("gil"),
                stocknames = stock_names) %>% 
  gadget_update("catchdistribution",
                name = "ldist.bmt",
                weight = 1,
                data = ldist.bmt[[1]] %>% ## to few samples (<=20 fish lengths)
                  filter(!(year==1982&step==4),
                         !(year==1984&step==1),
                         !(year==1992&step==4),
                         !(year==1994&step==1),
                         !(year==1998&step==3),
                         !(year==1989&step==3)),
                fleetnames = c("bmt"),
                stocknames = stock_names) %>% 
  gadget_update("catchdistribution",
                name = "aldist.bmt",
                weight = 1,
                data = aldist.bmt[[1]] %>% 
                  filter(year>1998),
                fleetnames = c("bmt"),
                stocknames = stock_names) %>% 
  gadget_update("stockdistribution",
                name = "matp.igfs",
                weight = 1,
                data = matp.igfs[[1]] %>% ## maturity @ length in 1985 appears to be silly and only one sample in 1989
                  filter(year>1989),
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
                fittype = 'fixedslopeloglinearfit',
                slope=1,
                stocknames = stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.70-80",
                weight = 1,
                data = igfs.SI3a[[1]],
                fittype = 'fixedslopeloglinearfit',
                slope=1,
                stocknames = stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.80-90",
                weight = 1,
                data = igfs.SI3b[[1]],
                fittype = 'fixedslopeloglinearfit',
                slope=1,
                stocknames = stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.90-100",
                weight = 1,
                data = igfs.SI3c[[1]],
                fittype = 'fixedslopeloglinearfit',
                slope=1,
                stocknames = stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.100-160",
                weight = 1,
                data = igfs.SI3d[[1]],
                fittype = 'fixedslopeloglinearfit',
                slope=1,
                stocknames = stock_names) %>% 
  write.gadget.file(gd$dir)
