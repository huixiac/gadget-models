fixed_slope <- gadget.variant.dir(gd$dir,'fixed_slope')
gadgetlikelihood('likelihood',fixed_slope) %>% 
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
                stocknames = stock_names) -> tmp
attr(tmp,'file_config')$mainfile_overwrite = TRUE
write.gadget.file(tmp,fixed_slope)