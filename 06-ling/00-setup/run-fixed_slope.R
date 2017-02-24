library(Rgadget)
tmp <- gadget.iterative(rew.sI=TRUE,
                        main='fixed_slope/main',
                        grouping=list(sind1=c('si.20-50','si.50-60','si.60-70'),
                                      sind2=c('si.70-80','si.80-90','si.90-100',
                                              'si.100-160'),
                                      comm=c('ldist.gil','ldist.bmt',
                                             'aldist.gil','aldist.bmt')),
                        #cv.floor = 0.05,
                        params.file = 'params.init',
                        wgts='fixed_slope/WGTS')

print('Running analytical retro')

gadget.retro(mainfile = 'fixed_slope/WGTS/main.final',params.file = 'params.init')

