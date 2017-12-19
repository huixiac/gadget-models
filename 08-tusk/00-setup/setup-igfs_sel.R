igfs.sel <- gadget.variant.dir(gd$dir,variant_dir = 'igfs_sel')

igfs.landings <- 
  structure(data.frame(year=defaults$year,step=2,area=1,number=1),
            area_group = mfdb_group(`1` = 1))

fleet <- 
  gadgetfleet(path = gd$dir,'Modelfiles/fleet') %>% 
  gadget_update('totalfleet',
                name = 'igfs',
                livesonareas=1,
                suitability = 
                  paste0('\n',
                         paste(c('tuskimm','tuskmat'),
                               'function','andersenfleet',
                               '#tusk.igfs.p0',to.gadget.formulae(quote(log(110/tusk.igfs.lmode))),'#tusk.igfs.p2',
                               '#tusk.igfs.p3','#tusk.igfs.p4','110',
                               collapse='\n')),  
                #   list(c('tuskimm','function','exponentiall50','#tusk.igfs.alpha'), 
                # gadgetfile('igfsL50',
                #            file_type = 'timevariable',
                #            components=list(list('igfsL50',
                #                                 data = 
                #                                   tibble::data_frame(year = c(1982,1996,2005),
                #                                                      step = 1,
                #                                                      value = c("#tusk.igfs.l50",'#tusk.igfs.l50.2','#tusk.igfs.l50'))))),
                # c('tuskmat','function','exponentiall50','#tusk.igfs.alpha'), 
                # gadgetfile('igfsL50',
                #            file_type = 'timevariable',
                #            components=list(list('igfsL50',
                #                                 data = 
                #                                   tibble::data_frame(year = c(1982,1996,2005),
                #                                                      step = 1,
                #                                                      value = c("#tusk.igfs.l50",'#tusk.igfs.l50.2','#tusk.igfs.l50')))))),
                data = igfs.landings)
attr(fleet,'file_config')$mainfile_overwrite = TRUE
fleet %>% 
  write.gadget.file(igfs.sel)


Sys.setenv(GADGET_WORKING_DIR=normalizePath(gd$dir))
callGadget(s=1,log = 'init.log',i='params.init',main = 'igfs_sel/main')
read.gadget.parameters(sprintf('%s/params.out',igfs.sel)) %>% 
  init_guess('p0',0,0,1,1) %>% 
  init_guess('p2',1,0,1,1) %>% 
  init_guess('p3',1,0.01,100,1) %>% 
  init_guess('p4',1,0.01,100,1) %>% 
  init_guess('mode',70,20,90,1) %>%
  write.gadget.parameters(.,file=sprintf('%s/%s/params.in',igfs.sel,attr(igfs.sel,'variant_dir')))
  