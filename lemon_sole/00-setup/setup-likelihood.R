## all age reading before 1999 are omitted

## weird inconsistencies in Gadget
aldist.igfs[[1]]$step <- 2
ldist.igfs[[1]]$step <- 2
matp.igfs[[1]]$step <- 2

minage_cutoff <- 0
minyear_cutoff <- 1900

if(grepl('46-databefore2001', vers)){
  minage_cutoff <- 0
  minyear_cutoff <- 2000
}


gadgetlikelihood('likelihood',gd,missingOkay = TRUE) %>% 
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
   gadget_update("catchdistribution",#         no age data from the autumn survey
                 name = "aldist.igfs",
                 weight = 1,
                 data = aldist.igfs[[1]],
  #                 #filter(year!=1989),
  #                 filter(year<1990),
                 fleetnames = c("igfs"),
                 stocknames =stock_names) %>% 
  gadget_update("catchdistribution",
                name = "ldist.dse",
                weight = 1,
                data = ldist.dse[[1]], 
                fleetnames = c("dse"),
                stocknames = stock_names) %>% 
  gadget_update("catchdistribution",  #      no data...
                name = "aldist.dse",
                weight = 1,
                data = aldist.dse[[1]]%>% 
                filter(year > minyear_cutoff)%>% 
                filter(age > minage_cutoff), 
                fleetnames = c("dse"),
                stocknames = stock_names) %>%
  # gadget_update("catchdistribution", #                no data...
  #               name = "aldist.gil",
  #               weight = 1,
  #               data = aldist.gil[[1]] %>% 
  #                 filter(year<1990),
  #               fleetnames = c("gil"),
  #               stocknames = stock_names) %>% 
  gadget_update("catchdistribution",
                name = "ldist.bmt",
                weight = 1,
                data = ldist.bmt[[1]],
                    # !(year==1982&step==4),
                    #      !(year==1984&step==1),
                    #      !(year==1992&step==4),
                    #      !(year==1994&step==1),
                    #      !(year==1998&step==3),
                    #      !(year==1989&step==3)),
                fleetnames = c("bmt"),
                stocknames = stock_names) %>% 
  gadget_update("catchdistribution",
                name = "aldist.bmt",
                weight = 1,
                data = aldist.bmt[[1]]%>% 
                  filter(year>minyear_cutoff)%>% 
                  filter(age>minage_cutoff), 
                fleetnames = c("bmt"),
                stocknames = stock_names) %>% 
  gadget_update("stockdistribution",
                name = "matp.igfs",
                weight = 1,
                data = matp.igfs[[1]],
                fleetnames = c("igfs"),
                stocknames =stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.10-20",
                weight = 1,
                data = igfs.SI1[[1]],
                #fittype = 'loglinearfit',
                fittype = 'fixedslopeloglinearfit',
                slope=1,
                stocknames = stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.20-25",
                weight = 1,
                data = igfs.SI2a[[1]],
                #fittype = 'loglinearfit',
                fittype = 'fixedslopeloglinearfit',
                slope=1,
                stocknames = stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.25-30",
                weight = 1,
                data = igfs.SI2b[[1]],
                #fittype = 'loglinearfit',
                fittype = 'fixedslopeloglinearfit',
                slope=1,
                stocknames = stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.30-35",
                weight = 1,
                data = igfs.SI3a[[1]],
                fittype = 'fixedslopeloglinearfit',
                slope=1,
                stocknames = stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.35-40",
                weight = 1,
                data = igfs.SI3b[[1]],
                fittype = 'fixedslopeloglinearfit',
                slope=1,
                stocknames = stock_names) %>% 
  gadget_update("surveyindices",
                name = "si.40-45",
                weight = 1,
                data = igfs.SI3c[[1]],
                fittype = 'fixedslopeloglinearfit',
                slope=1,
                stocknames = stock_names) %>% 
  gadget_update("surveyindices",
               name = "si.45-60",
               weight = 1,
                data = igfs.SI3d[[1]],
               fittype = 'fixedslopeloglinearfit',
                slope=1,
                stocknames = stock_names) %>% 
  write.gadget.file(gd)

