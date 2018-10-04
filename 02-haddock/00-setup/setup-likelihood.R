## 

## weird inconsistencies in Gadget
## time step 2 is used instead of time step 1 for catch distribution data rather than survey indices, because survey indices are compared in first quarter but consumption compared in second quarter
aldist.igfs_e[[1]]$step <- 2
ldist.igfs_e[[1]]$step <- 2
matp.igfs_e[[1]]$step <- 2
aldist.igfs_l[[1]]$step <- 2
ldist.igfs_l[[1]]$step <- 2
matp.igfs_l[[1]]$step <- 2

aut.SI1[[1]]$step <- 5
aut.SI2[[1]]$step <- 5
aut.SI3[[1]]$step <- 5
aut.SI4[[1]]$step <- 5
aut.SI5[[1]]$step <- 5
aut.SI6[[1]]$step <- 5
aut.SI7[[1]]$step <- 5

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
                weight = "100") %>% 
  ### Catch distributions
  # Autumn
  gadget_update("catchdistribution",
                name = "ldist.aut",
                weight = 1,
                data = ldist.aut[[1]],
                fleetnames = c("aut"),
                stocknames =stock_names) %>% 
  gadget_update("catchdistribution",
                name = "aldist.aut",
                weight = 1,
                data = aldist.aut[[1]], 
                fleetnames = c("aut"),
                stocknames =stock_names) %>% 
  #Spring
  gadget_update("catchdistribution",
                name = "ldist.igfs_e",
                weight = 1,
                data = ldist.igfs_e[[1]],
                fleetnames = c("igfs_e"),
                stocknames =stock_names) %>% 
  gadget_update("catchdistribution",
                name = "ldist.igfs_l",
                weight = 1,
                data = ldist.igfs_l[[1]],
                fleetnames = c("igfs_l"),
                stocknames =stock_names) %>% 
  gadget_update("catchdistribution",
                name = "aldist.igfs_e",
                weight = 1,
                data = aldist.igfs_e[[1]],
                fleetnames = c("igfs_e"),
                stocknames =stock_names) %>% 
  gadget_update("catchdistribution",
                name = "aldist.igfs_l",
                weight = 1,
                data = aldist.igfs_l[[1]],
                fleetnames = c("igfs_l"),
                stocknames =stock_names) %>% 
  #Long line
  gadget_update("catchdistribution",
                name = "ldist.lln_e",
                weight = 1,
                data = ldist.lln_e[[1]], #%>% ## tow == 60228 was wrongly assigned, omit samples from that quarter
                  #filter(!(year==1993&step==4)),
                fleetnames = c("lln_e"),
                stocknames = stock_names) %>% 
  gadget_update("catchdistribution",
                name = "ldist.lln_l",
                weight = 1,
                data = ldist.lln_l[[1]], #%>% ## tow == 60228 was wrongly assigned, omit samples from that quarter
                #filter(!(year==1993&step==4)),
                fleetnames = c("lln_l"),
                stocknames = stock_names) %>% 
  gadget_update("catchdistribution",
                name = "aldist.lln_e",
                weight = 1,
                data = aldist.lln_e[[1]], #%>%  ## only 20 fish aged taken in those quarters
                  #filter(year>1998,!((year==2002|year==2003)&step==2)),
                fleetnames = c("lln_e"),
                stocknames = stock_names) %>% 
  gadget_update("catchdistribution",
                name = "aldist.lln_l",
                weight = 1,
                data = aldist.lln_l[[1]], #%>%  ## only 20 fish aged taken in those quarters
                #filter(year>1998,!((year==2002|year==2003)&step==2)),
                fleetnames = c("lln_l"),
                stocknames = stock_names) %>% 
  #Gill nets
  gadget_update("catchdistribution",
                name = "ldist.gil_e",
                weight = 1,
                data = ldist.gil_e[[1]], #%>% ## only one fish lengthmeasured
                  #filter(!(year==2005&step==2)),
                fleetnames = c("gil_e"),
                stocknames = stock_names) %>% 
  gadget_update("catchdistribution",
                name = "ldist.gil_l",
                weight = 1,
                data = ldist.gil_l[[1]], #%>% ## only one fish lengthmeasured
                #filter(!(year==2005&step==2)),
                fleetnames = c("gil_l"),
                stocknames = stock_names) %>% 
  gadget_update("catchdistribution",
                name = "aldist.gil_e",
                weight = 1,
                data = aldist.gil_e[[1]], #%>% 
                  #filter(year>1998),
                fleetnames = c("gil_e"),
                stocknames = stock_names) %>% 
  gadget_update("catchdistribution",
                name = "aldist.gil_l",
                weight = 1,
                data = aldist.gil_l[[1]], #%>% 
                #filter(year>1998),
                fleetnames = c("gil_l"),
                stocknames = stock_names) %>%
  #Bottom trawls
  gadget_update("catchdistribution",
                name = "ldist.bmt_e",
                weight = 1,
                data = ldist.bmt_e[[1]], #%>% ## to few samples (<=20 fish lengths)
                  # filter(!(year==1982&step==4),
                  #        !(year==1984&step==1),
                  #        !(year==1992&step==4),
                  #        !(year==1994&step==1),
                  #        !(year==1998&step==3),
                  #        !(year==1989&step==3)),
                fleetnames = c("bmt_e"),
                stocknames = stock_names) %>% 
  gadget_update("catchdistribution",
                name = "ldist.bmt_l",
                weight = 1,
                data = ldist.bmt_l[[1]], #%>% ## to few samples (<=20 fish lengths)
                # filter(!(year==1982&step==4),
                #        !(year==1984&step==1),
                #        !(year==1992&step==4),
                #        !(year==1994&step==1),
                #        !(year==1998&step==3),
                #        !(year==1989&step==3)),
                fleetnames = c("bmt_l"),
                stocknames = stock_names) %>% 
  gadget_update("catchdistribution",
                name = "aldist.bmt_e",
                weight = 1,
                data = aldist.bmt_e[[1]], #%>% 
                  #filter(year>1998),
                fleetnames = c("bmt_e"),
                stocknames = stock_names) %>% 
  gadget_update("catchdistribution",
                name = "aldist.bmt_l",
                weight = 1,
                data = aldist.bmt_l[[1]], #%>% 
                #filter(year>1998),
                fleetnames = c("bmt_l"),
                stocknames = stock_names) %>% 
  #Other trawls
  gadget_update("catchdistribution",
                name = "ldist.ott_e",
                weight = 1,
                data = ldist.ott_e[[1]], #%>% ## to few samples (<=20 fish lengths)
                # filter(!(year==1982&step==4),
                #        !(year==1984&step==1),
                #        !(year==1992&step==4),
                #        !(year==1994&step==1),
                #        !(year==1998&step==3),
                #        !(year==1989&step==3)),
                fleetnames = c("ott_e"),
                stocknames = stock_names) %>% 
  gadget_update("catchdistribution",
                name = "ldist.ott_l",
                weight = 1,
                data = ldist.ott_l[[1]], #%>% ## to few samples (<=20 fish lengths)
                # filter(!(year==1982&step==4),
                #        !(year==1984&step==1),
                #        !(year==1992&step==4),
                #        !(year==1994&step==1),
                #        !(year==1998&step==3),
                #        !(year==1989&step==3)),
                fleetnames = c("ott_l"),
                stocknames = stock_names) %>% 
  gadget_update("catchdistribution",
                name = "aldist.ott_e",
                weight = 1,
                data = aldist.ott_e[[1]], #%>% 
                #filter(year>1998),
                fleetnames = c("ott_e"),
                stocknames = stock_names) %>% 
  gadget_update("catchdistribution",
                name = "aldist.ott_l",
                weight = 1,
                data = aldist.ott_l[[1]], #%>% 
                #filter(year>1998),
                fleetnames = c("ott_l"),
                stocknames = stock_names) %>% 
  #Danish and other seines
  gadget_update("catchdistribution",
                name = "ldist.dse_e",
                weight = 1,
                data = ldist.dse_e[[1]], 
                fleetnames = c("dse_e"),
                stocknames = stock_names) %>% 
  gadget_update("catchdistribution",
                name = "ldist.dse_l",
                weight = 1,
                data = ldist.dse_l[[1]], 
                fleetnames = c("dse_l"),
                stocknames = stock_names) %>% 
  gadget_update("catchdistribution",
                name = "aldist.dse_e",
                weight = 1,
                data = aldist.dse_e[[1]], 
                fleetnames = c("dse_e"),
                stocknames = stock_names) %>% 
  gadget_update("catchdistribution",
                name = "aldist.dse_l",
                weight = 1,
                data = aldist.dse_l[[1]], 
                fleetnames = c("dse_l"),
                stocknames = stock_names) %>% 
  #Maturity data
  gadget_update("stockdistribution",
                name = "matp.igfs_e",
                weight = 1,
                data = matp.igfs_e[[1]], # %>% ## maturity @ length in 1985 appears to be silly and only one sample in 1989
                  #filter(year>1989),
                fleetnames = c("igfs_e"),
                stocknames =stock_names) %>% 
  gadget_update("stockdistribution",
                name = "matp.igfs_l",
                weight = 1,
                data = matp.igfs_l[[1]], # %>% ## maturity @ length in 1985 appears to be silly and only one sample in 1989
                #filter(year>1989),
                fleetnames = c("igfs_l"),
                stocknames =stock_names) %>% 
  gadget_update("stockdistribution",
                name = "matp.aut",
                weight = 1,
                data = matp.aut[[1]], 
                fleetnames = c("aut"),
                stocknames =stock_names) %>% 
  ### Survey Indices ###
  #survey indices are renamed according to AUTUMN intervals just because numbers were rounder. 
  #For example, si.i.10_15 actually has an interval 13 - 18 because its from the IGFS survey, 
  #whereas si.a.10_15 actually has interval 10-15.
  gadget_update("surveyindices",
                name = "si.i.10_15",
                weight = 1,
                data = igfs.SI1[[1]],
                fittype = 'loglinearfit',
                stocknames = stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.i.15_21",
                weight = 1,
                data = igfs.SI2[[1]],
                fittype = 'loglinearfit',
                stocknames = stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.i.21_28",
                weight = 1,
                data = igfs.SI3[[1]],
                fittype = 'loglinearfit', #'fixedslopeloglinearfit', #why changed fittype here previously?
                #slope=1,
                stocknames = stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.i.28_36",
                weight = 1,
                data = igfs.SI4[[1]],
                fittype = 'loglinearfit', #'fixedslopeloglinearfit',
                #slope=1,
                stocknames = stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.i.36_45",
                weight = 1,
                data = igfs.SI5[[1]],
                fittype = 'fixedslopeloglinearfit',
                slope=1,
                stocknames = stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.i.45_55",
                weight = 1,
                data = igfs.SI6[[1]],
                fittype = 'fixedslopeloglinearfit',
                slope=1,
                stocknames = stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.i.55_97",
                weight = 1,
                data = igfs.SI7[[1]],
                fittype = 'fixedslopeloglinearfit',
                slope=1,
                stocknames = stock_names) %>% 
 
  gadget_update("surveyindices",
                name = "si.a.10_15",
                weight = 1,
                data = aut.SI1[[1]],
                fittype = 'loglinearfit',
                stocknames = stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.a.15_21",
                weight = 1,
                data = aut.SI2[[1]],
                fittype = 'loglinearfit',
                stocknames = stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.a.21_28",
                weight = 1,
                data = aut.SI3[[1]],
                fittype = 'loglinearfit', #'fixedslopeloglinearfit', #why changed fittype here previously?
                #slope=1,
                stocknames = stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.a.28_36",
                weight = 1,
                data = aut.SI4[[1]],
                fittype = 'loglinearfit', #'fixedslopeloglinearfit',
                #slope=1,
                stocknames = stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.a.36_45",
                weight = 1,
                data = aut.SI5[[1]],
                fittype = 'fixedslopeloglinearfit',
                slope=1,
                stocknames = stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.a.45_55",
                weight = 1,
                data = aut.SI6[[1]],
                fittype = 'fixedslopeloglinearfit',
                slope=1,
                stocknames = stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.a.55_97",
                weight = 1,
                data = aut.SI7[[1]],
                fittype = 'fixedslopeloglinearfit',
                slope=1,
                stocknames = stock_names) %>% 
  
  write.gadget.file(gd$dir)
