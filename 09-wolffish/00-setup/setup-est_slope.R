est_slope <- gadget.variant.dir(gd$dir,'est_slope')
gadgetlikelihood('likelihood',est_slope) %>% 
  gadget_update("surveyindices",
                name = "si.20-50",
                weight = 1,
                data = igfs.SI1[[1]],
                fittype = 'loglinearfit',
                stocknames = c("lingimm","lingmat")) %>% 
  gadget_update("surveyindices",
                name = "si.50-70",
                weight = 1,
                data = igfs.SI2[[1]],
                fittype = 'loglinearfit',
                stocknames = c("lingimm","lingmat")) %>% 
  gadget_update("surveyindices",
                name = "si.70-180",
                weight = 1,
                data = igfs.SI3[[1]],
                fittype = 'loglinearfit',
                stocknames = c("lingimm","lingmat")) -> tmp
attr(tmp,'file_config')$mainfile_overwrite = TRUE
write.gadget.file(tmp,est_slope)