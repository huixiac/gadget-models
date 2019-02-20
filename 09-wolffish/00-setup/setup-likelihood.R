

## weird inconsistencies in Gadget
aldist.igfs[[1]]$step <- 2
ldist.igfs[[1]]$step <- 2
aldist.aut[[1]]$step <- 4
ldist.aut[[1]]$step <- 4
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
                data = aldist.igfs[[1]] %>% filter(!(year %in% c(2015:2017))), #temporary fix due to ageing problems
                fleetnames = c("igfs"),
                stocknames =stock_names) %>% 
  gadget_update("catchdistribution",
                name = "ldist.aut",
                weight = 1,
                data = ldist.aut[[1]],
                fleetnames = c("aut"),
                stocknames =stock_names) %>% 
  gadget_update("catchdistribution",
                name = "aldist.aut",
                weight = 1,
                data = aldist.aut[[1]] %>% filter(!(year %in% c(2015:2017))), #temporary fix due to ageing problems,
                fleetnames = c("aut"),
                stocknames =stock_names) %>% 
  gadget_update("catchdistribution",
                name = "ldist.lln",
                weight = 1,
                data = ldist.lln[[1]],
                fleetnames = c("lln"),
                stocknames = stock_names) %>% 
  gadget_update("catchdistribution",
                name = "aldist.lln",
                weight = 1,
                data = aldist.lln[[1]] %>% filter(!(year %in% c(2015:2017))), #temporary fix due to ageing problems,
                fleetnames = c("lln"),
                stocknames = stock_names) %>% 
  gadget_update("catchdistribution",
                name = "ldist.gil",
                weight = 1,
                data = ldist.gil[[1]] %>% ## only one year of data
                  filter(year==2017),
                fleetnames = c("gil"),
                stocknames = stock_names) %>%
  # gadget_update("catchdistribution",
  #               name = "aldist.gil",
  #               weight = 1,
  #               data = aldist.gil[[1]] %>% ## only one year of data during bad ageing years
  #                 filter(year==2017),
  #               fleetnames = c("gil"),
  #               stocknames = stock_names) %>%
  gadget_update("catchdistribution",
                name = "ldist.bmt",
                weight = 1,
                data = ldist.bmt[[1]], 
                fleetnames = c("bmt"),
                stocknames = stock_names) %>% 
  gadget_update("catchdistribution",
                name = "aldist.bmt",
                weight = 1,
                data = aldist.bmt[[1]] %>% filter(!(year %in% c(2015:2017))), #temporary fix due to ageing problems ,
                fleetnames = c("bmt"),
                stocknames = stock_names) %>% 
  gadget_update("stockdistribution",
                name = "matp.igfs",
                weight = 1,
                data = matp.igfs[[1]] %>% filter(year > 2002), # per Ásgeir's judgement, but 2002 looks weird too
                fleetnames = c("igfs"),
                stocknames =stock_names) %>% 
  #from report: indices in groups 5-13, 14-19,20-29,30-55,56-74,75-110.
    gadget_update("surveyindices",
                name = "si.05-19",
                weight = 1,
                data = igfs.SI1[[1]],
                fittype = 'fixedslopeloglinearfit',
                slope=1,
                stocknames = stock_names) %>% 
  # gadget_update("surveyindices",
  #               name = "si.14-19",
  #               weight = 1,
  #               data = igfs.SI2[[1]],
  #               fittype = 'fixedslopeloglinearfit',
  #               slope=1,
  #               stocknames = stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.20-29",
                weight = 1,
                data = igfs.SI3[[1]],
                fittype = 'fixedslopeloglinearfit',
                slope=1,
                stocknames = stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.30-55",
                weight = 1,
                data = igfs.SI4[[1]],
                fittype = 'loglinearfit',
                #slope=1,
                stocknames = stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.56-74",
                weight = 1,
                data = igfs.SI5[[1]],
                fittype = 'loglinearfit',
                #slope=1,
                stocknames = stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.75-110",
                weight = 1,
                data = igfs.SI6[[1]],
                fittype = 'fixedslopeloglinearfit',
                slope=1,
                stocknames = stock_names) %>% 
  # gadget_update("surveyindices",
  #               name = "si.100-160",
  #               weight = 1,
  #               data = igfs.SI3d[[1]],
  #               fittype = 'fixedslopeloglinearfit',
  #               slope=1,
  #               stocknames = stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.05-19.aut",
                weight = 1,
                data = aut.SI1[[1]],
                fittype = 'fixedslopeloglinearfit',
                slope=1,
                stocknames = stock_names) %>% 
  # gadget_update("surveyindices",
  #               name = "si.14-19.aut",
  #               weight = 1,
  #               data = aut.SI2[[1]],
  #               fittype = 'fixedslopeloglinearfit',
  #               slope=1,
  #               stocknames = stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.20-29.aut",
                weight = 1,
                data = aut.SI3[[1]],
                fittype = 'fixedslopeloglinearfit',
                slope=1,
                stocknames = stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.30-55.aut",
                weight = 1,
                data = aut.SI4[[1]],
                fittype = 'fixedslopeloglinearfit',
                slope=1,
                stocknames = stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.56-74.aut",
                weight = 1,
                data = aut.SI5[[1]],
                fittype = 'loglinearfit',
                #slope=1,
                stocknames = stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.75-110.aut",
                weight = 1,
                data = aut.SI6[[1]],
                fittype = 'fixedslopeloglinearfit',
                slope=1,
                stocknames = stock_names) %>% 
  # gadget_update("surveyindices",
  #               name = "si.100-160.aut",
  #               weight = 1,
  #               data = aut.SI3d[[1]],
  #               fittype = 'fixedslopeloglinearfit',
  #               slope=1,
  #               stocknames = stock_names) %>% 
  write.gadget.file(gd$dir)
