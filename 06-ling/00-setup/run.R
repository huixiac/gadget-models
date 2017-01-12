library(Rgadget)
tmp <- gadget.iterative(rew.sI=TRUE,
                        main='main',
                        grouping=list(sind=c('si.20-50','si.50-70','si.70-180')),
                        cv.floor = 0.05,
                        wgts='WGTS')


