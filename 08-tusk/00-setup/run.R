library(Rgadget)
tmp <- gadget.iterative(rew.sI=TRUE,
                        main='main',
                        grouping=list(sind=c('si.10-20','si.20-30','si.30-40','si.40-50','si.50-60',
                                             'si.60-70','si.70-110')),
                        #cv.floor = 0.05,
                        wgts='WGTS',
                        params.file = 'params.init')



print('Running analytical retro')

gadget.retro(mainfile = 'WGTS/main.final',params.file = 'params.init')

