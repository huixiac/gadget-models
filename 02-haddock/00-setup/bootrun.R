library(Rgadget)
mclapply(1:100,
         function(x){
           tmp <- gadget.iterative(rew.sI=TRUE,
                                   main=sprintf('BS.WGTS/BS.%s/main',x),
                                   grouping=list(sind1=c('si.20-50','si.50-60','si.60-70'),
                                                 sind2=c('si.70-80','si.80-90','si.90-100',
                                                         'si.100-160'),
                                                 comm=c('ldist.gil','ldist.bmt',
                                                        'aldist.gil','aldist.bmt')),
                                   #cv.floor = 0.05,
                                   params.file = 'params.init',
                                   wgts=sprintf('BS.WGTS/BS.%s/WGTS',x))
         })
         
         