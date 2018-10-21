library(Rgadget)
tmp <- gadget.iterative(rew.sI=TRUE,
                        main='main',
                        grouping=list(sind1.i=c("si.i.10_15","si.i.15_21","si.i.21_28","si.i.28_36"),
                                      sind2.i=c("si.i.36_45","si.i.45_55","si.i.55_97"),
                                      sind1.a=c("si.a.10_15","si.a.15_21","si.a.21_28","si.a.28_36"),
                                      sind2.a=c("si.a.36_45","si.a.45_55","si.a.55_97"),
                                      early_ages=c("aldist.igfs_e","aldist.bmt_e","aldist.ott_e", "aldist.lln_e", "aldist.dse_e","aldist.gil_e"),#comm1=c('ldist.igfs', 'aldist.igfs'),
                                      ldist.igfs=c('ldist.igfs_e', 'ldist.igfs_l'),
                                      matp.igfs=c('matp.igfs_e', 'matp.igfs_l'),
                                      ldist.bmt=c('ldist.bmt_e', 'ldist.bmt_l'),
                                      ldist.ott=c('ldist.ott_e', 'ldist.ott_l'),
                                      ldist.igfs=c('ldist.lln_e', 'ldist.lln_l'),
                                      ldist.igfs=c('ldist.gil_e', 'ldist.gil_l'),
                                      ldist.igfs=c('ldist.dse_e', 'ldist.dse_l')
                        ),
                        #cv.floor = 0.05,
                        resume.final = FALSE,
                        params.file = 'params.init',
                        wgts='WGTS')#,
                        #comp = c('ldist.aut', 'aldis.aut', indices...),inverse = TRUE) #throws out the aut survey results from likelihood

print('Running analytical retro')

gadget.retro(mainfile = 'WGTS/main.final',params.file = 'params.init')

