library(Rgadget)
tmp <- gadget.iterative(rew.sI=TRUE,
                        main='three_fleets/main',
                        grouping=list(sind=c('si.20-50','si.50-70','si.70-180'),
                          comm=c('ldist.gil','ldist.bmt',
                            'aldist.gil','aldist.bmt')),
                        cv.floor = 0.05,
                        params.file = 'three_fleets/params.in',
                        wgts='three_fleets/WGTS')


