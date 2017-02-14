library(Rgadget)
tmp <- gadget.iterative(rew.sI=TRUE,
                        main='main',
                        grouping=list(sind=c('si.20-50','si.50-60','si.60-70','si.80-90','si.90-100','si.100-160'),
                                      comm=c('ldist.gil','ldist.bmt',
                                             'aldist.gil','aldist.bmt')),
                        cv.floor = 0.05,
                        params.file = 'params.init',
                        wgts='WGTS')


