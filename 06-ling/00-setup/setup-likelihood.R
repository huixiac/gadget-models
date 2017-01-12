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
                weight = "100") %>% write.gadget.file(gd$dir)
gadget_likelihood_component("catchdistribution",
                            name = "ldist.igfs",
                            weight = 1,
                            data = ldist.igfs[[1]],
                            fleetnames = c("igfs"),
                            stocknames = c("lingimm", "lingmat")) %>%   gadget_dir_write(gd,.)
gadget_likelihood_component("catchdistribution",
                            name = "aldist.igfs",
                            weight = 1,
                            data = aldist.igfs[[1]],
                            fleetnames = c("igfs"),
                            stocknames = c("lingimm", "lingmat")) %>% gadget_dir_write(gd,.)
gadget_likelihood_component("stockdistribution",
                            name = "matp.igfs",
                            weight = 1,
                            data = matp.igfs[[1]],
                            fleetnames = c("igfs"),
                            stocknames = c("lingimm", "lingmat")) %>% gadget_dir_write(gd,.)
gadget_likelihood_component("catchdistribution",
                            name = "ldist.comm",
                            weight = 1,
                            data = ldist.comm[[1]],
                            fleetnames = c("comm"),
                            stocknames = c("lingimm", "lingmat")) %>% gadget_dir_write(gd,.)
gadget_likelihood_component("catchdistribution",
                            name = "aldist.comm",
                            weight = 1,
                            data = aldist.comm[[1]],
                            fleetnames = c("comm"),
                            stocknames = c("lingimm", "lingmat")) %>% gadget_dir_write(gd,.)
gadget_likelihood_component("surveyindices",
                            name = "si.20-50",
                            weight = 1,
                            data = igfs.SI1[[1]],
                            fittype = 'fixedslopeloglinearfit',
                            slope=1,
                            stocknames = c("lingimm","lingmat")) %>% gadget_dir_write(gd,.)
gadget_likelihood_component("surveyindices",
                            name = "si.50-70",
                            weight = 1,
                            data = igfs.SI2[[1]],
                            fittype = 'fixedslopeloglinearfit',
                            slope=1,
                            stocknames = c("lingimm","lingmat")) %>% gadget_dir_write(gd,.)
gadget_likelihood_component("surveyindices",
                            name = "si.70-180",
                            weight = 1,
                            data = igfs.SI3[[1]],
                            fittype = 'fixedslopeloglinearfit',
                            slope=1,
                            stocknames = c("lingimm","lingmat")) %>% gadget_dir_write(gd,.)

