# example
# timedat<-data_frame(year = rep(year_range, each=4), 
#                     step = rep(1:4, length(year_range)), 
#                     value = parse(text=sprintf('0.001*had.k.%s',yr_tmp)) %>%
#                       map(to.gadget.formulae)%>% 
#                       unlist())


gadgetfile('Modelfiles/timevariableK.mat',
           file_type = 'timevariable',
           components = list(list('annualgrowth',
                                  data= data_frame(year = rep(year_range, each=6), 
                                                   step = rep(1:6, length(year_range)), 
                                                   value = parse(text=sprintf(rep(c(rep('0.001*hadmat.k.%s',3), 
                                                                                    rep('0.001*hadimm.k.%s',1),
                                                                                    rep('0.001*hadmat.k.%s',1),
                                                                                    rep('0.001*hadimm.k.%s',1)), 
                                                                                  length(year_range)),rep(c(12,12,3,4,5,6), 
                                                                                                          length(year_range)))) %>%
                                                     map(to.gadget.formulae)%>% 
                                                     unlist()))
           )) %>% 
  write.gadget.file(gd$dir)

gadgetfile('Modelfiles/timevariableK.imm',
           file_type = 'timevariable',
           components = list(list('annualgrowth',
                                  data= data_frame(year = rep(year_range, each=6), 
                                                   step = rep(1:6, length(year_range)), 
                                                   value = parse(text=sprintf('0.001*hadimm.k.%s',rep(c(1,2,3,4,5,6), length(year_range)))) %>%
                                                     map(to.gadget.formulae)%>% 
                                                     unlist()))
           )) %>% 
  write.gadget.file(gd$dir)


#create two time periods for suitability, roughly when it appears that lg fish were reduced in catches (beginning 1993)
#possibly as a result of a regulation introduction (no fishing during cod spawning periods?)
#
bmt_yr_eff<-c(rep(2, 14*6), rep(1, (length(year_range)-14)*6))

gadgetfile('Modelfiles/timevariablep0.bmt',
           file_type = 'timevariable',
           components = list(list('suitability.bmt.p0',
                                  data= data_frame(year = rep(year_range, each=6), 
                                                   step = rep(1:6, length(year_range)), 
                                                   value = parse(text=sprintf('had.bmt.p0.%s', bmt_yr_eff)) %>%
                                                     map(to.gadget.formulae)%>% 
                                                     unlist()))
           )) %>% 
  write.gadget.file(gd$dir)

gadgetfile('Modelfiles/timevariablep2.bmt',
           file_type = 'timevariable',
           components = list(list('suitability.bmt.p2',
                                  data= data_frame(year = rep(year_range, each=6), 
                                                   step = rep(1:6, length(year_range)), 
                                                   value = parse(text=sprintf('had.bmt.p2.%s', bmt_yr_eff)) %>%
                                                     map(to.gadget.formulae)%>% 
                                                     unlist()))
           )) %>% 
  write.gadget.file(gd$dir)

gadgetfile('Modelfiles/timevariablep3.bmt',
           file_type = 'timevariable',
           components = list(list('suitability.bmt.p3',
                                  data= data_frame(year = rep(year_range, each=6), 
                                                   step = rep(1:6, length(year_range)), 
                                                   value = parse(text=sprintf('had.bmt.p3.%s', bmt_yr_eff)) %>%
                                                     map(to.gadget.formulae)%>% 
                                                     unlist()))
           )) %>% 
  write.gadget.file(gd$dir)

gadgetfile('Modelfiles/timevariablep4.bmt',
           file_type = 'timevariable',
           components = list(list('suitability.bmt.p4',
                                  data= data_frame(year = rep(year_range, each=6), 
                                                   step = rep(1:6, length(year_range)), 
                                                   value = parse(text=sprintf('had.bmt.p4.%s', bmt_yr_eff)) %>%
                                                     map(to.gadget.formulae)%>% 
                                                     unlist()))
           )) %>% 
  write.gadget.file(gd$dir)

gadgetfile('Modelfiles/timevariablelmode.bmt',
           file_type = 'timevariable',
           components = list(list('suitability.bmt.lmode',
                                  data= data_frame(year = rep(year_range, each=6), 
                                                   step = rep(1:6, length(year_range)), 
                                                   value = parse(text=sprintf('log(100/had.bmt.lmode.%s)', bmt_yr_eff)) %>%
                                                     map(to.gadget.formulae)%>% 
                                                     unlist()))
           )) %>% 
  write.gadget.file(gd$dir)



gadgetfile('Modelfiles/timevariablep0.ott',
           file_type = 'timevariable',
           components = list(list('suitability.ott.p0',
                                  data= data_frame(year = rep(year_range, each=6), 
                                                   step = rep(1:6, length(year_range)), 
                                                   value = parse(text=sprintf('had.ott.p0.%s', bmt_yr_eff)) %>%
                                                     map(to.gadget.formulae)%>% 
                                                     unlist()))
           )) %>% 
  write.gadget.file(gd$dir)

gadgetfile('Modelfiles/timevariablep2.ott',
           file_type = 'timevariable',
           components = list(list('suitability.ott.p2',
                                  data= data_frame(year = rep(year_range, each=6), 
                                                   step = rep(1:6, length(year_range)), 
                                                   value = parse(text=sprintf('had.ott.p2.%s', bmt_yr_eff)) %>%
                                                     map(to.gadget.formulae)%>% 
                                                     unlist()))
           )) %>% 
  write.gadget.file(gd$dir)

gadgetfile('Modelfiles/timevariablep3.ott',
           file_type = 'timevariable',
           components = list(list('suitability.ott.p3',
                                  data= data_frame(year = rep(year_range, each=6), 
                                                   step = rep(1:6, length(year_range)), 
                                                   value = parse(text=sprintf('had.ott.p3.%s', bmt_yr_eff)) %>%
                                                     map(to.gadget.formulae)%>% 
                                                     unlist()))
           )) %>% 
  write.gadget.file(gd$dir)

gadgetfile('Modelfiles/timevariablep4.ott',
           file_type = 'timevariable',
           components = list(list('suitability.ott.p4',
                                  data= data_frame(year = rep(year_range, each=6), 
                                                   step = rep(1:6, length(year_range)), 
                                                   value = parse(text=sprintf('had.ott.p4.%s', bmt_yr_eff)) %>%
                                                     map(to.gadget.formulae)%>% 
                                                     unlist()))
           )) %>% 
  write.gadget.file(gd$dir)

gadgetfile('Modelfiles/timevariablelmode.ott',
           file_type = 'timevariable',
           components = list(list('suitability.ott.lmode',
                                  data= data_frame(year = rep(year_range, each=6), 
                                                   step = rep(1:6, length(year_range)), 
                                                   value = parse(text=sprintf('log(100/had.ott.lmode.%s)', bmt_yr_eff)) %>%
                                                     map(to.gadget.formulae)%>% 
                                                     unlist()))
           )) %>% 
  write.gadget.file(gd$dir)

gadgetfile('Modelfiles/timevariablealpha.gil',
           file_type = 'timevariable',
           components = list(list('suitability.gil.alpha',
                                  data= data_frame(year = rep(year_range, each=6), 
                                                   step = rep(1:6, length(year_range)), 
                                                   value = parse(text=sprintf('had.gil.alpha.%s', bmt_yr_eff)) %>%
                                                     map(to.gadget.formulae)%>% 
                                                     unlist()))
           )) %>% 
  write.gadget.file(gd$dir)

gadgetfile('Modelfiles/timevariablel50.gil',
           file_type = 'timevariable',
           components = list(list('suitability.gil.l50',
                                  data= data_frame(year = rep(year_range, each=6), 
                                                   step = rep(1:6, length(year_range)), 
                                                   value = parse(text=sprintf('had.gil.l50.%s',bmt_yr_eff)) %>%
                                                     map(to.gadget.formulae)%>% 
                                                     unlist()))
           )) %>% 
  write.gadget.file(gd$dir)


gadgetfile('Modelfiles/timevariablealpha.lln',
           file_type = 'timevariable',
           components = list(list('suitability.lln.alpha',
                                  data= data_frame(year = rep(year_range, each=6), 
                                                   step = rep(1:6, length(year_range)), 
                                                   value = parse(text=sprintf('had.lln.alpha.%s',bmt_yr_eff)) %>%
                                                     map(to.gadget.formulae)%>% 
                                                     unlist()))
           )) %>% 
  write.gadget.file(gd$dir)

gadgetfile('Modelfiles/timevariablel50.lln',
           file_type = 'timevariable',
           components = list(list('suitability.lln.l50',
                                  data= data_frame(year = rep(year_range, each=6), 
                                                   step = rep(1:6, length(year_range)), 
                                                   value = parse(text=sprintf('had.lln.l50.%s',bmt_yr_eff)) %>%
                                                     map(to.gadget.formulae)%>% 
                                                     unlist()))
           )) %>% 
  write.gadget.file(gd$dir)


gadgetfile('Modelfiles/timevariablealpha.dse',
           file_type = 'timevariable',
           components = list(list('suitability.dse.alpha',
                                  data= data_frame(year = rep(year_range, each=6), 
                                                   step = rep(1:6, length(year_range)), 
                                                   value = parse(text=sprintf('had.dse.alpha.%s',bmt_yr_eff)) %>%
                                                     map(to.gadget.formulae)%>% 
                                                     unlist()))
           )) %>% 
  write.gadget.file(gd$dir)

gadgetfile('Modelfiles/timevariablel50.dse',
           file_type = 'timevariable',
           components = list(list('suitability.gil.dse',
                                  data= data_frame(year = rep(year_range, each=6), 
                                                   step = rep(1:6, length(year_range)), 
                                                   value = parse(text=sprintf('had.dse.l50.%s',bmt_yr_eff)) %>%
                                                     map(to.gadget.formulae)%>% 
                                                     unlist()))
           )) %>% 
  write.gadget.file(gd$dir)


# gadgetfile('Modelfiles/timevariableLinf',
#            file_type = 'timevariable',
#            components = list(list('annualgrowth',
#                                   data= data_frame(year = rep(year_range, each=4), 
#                                                    step = rep(1:4, length(year_range)), 
#                                                    value = parse(text=sprintf('had.Linf.%s',rep(c(41,23,23,41), length(year_range)))) %>%
#                                                      map(to.gadget.formulae)%>% 
#                                                      unlist()))
#            )) %>% 
#   write.gadget.file(gd$dir)